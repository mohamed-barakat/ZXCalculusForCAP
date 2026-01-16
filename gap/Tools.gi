# SPDX-License-Identifier: GPL-2.0-or-later
# ZXCalculusForCAP: The category of ZX-diagrams
#
# Implementations
#

if IsPackageMarkedForLoading( "json", "2.1.1" ) then

  InstallGlobalFunction( ExportAsQGraphString,
    function ( phi )
      local quadruple, labels, input_positions, output_positions, edges,
            input_positions_indices, output_positions_indices,
            wire_vertices, node_vertices, vertex_names, padding_length, get_vertex_name, vertex_name,
            is_input, is_output, input_position, output_position, undir_edges, edge, edge_name,
            src_vertex_name, tgt_vertex_name, qgraph, pos, edge_counter;
        
        quadruple := ZX_RemovedInnerNeutralNodes( MorphismDatum( phi ) );
        
        labels := ShallowCopy( quadruple[1] );
        input_positions := ShallowCopy( quadruple[2] );
        output_positions := ShallowCopy( quadruple[3] );
        edges := ShallowCopy( quadruple[4] );
        
        # input_positions and output_positions might be ranges, which are immutable in Julia
        # -> convert to regular lists
        input_positions := List( [ 1 .. Length( input_positions ) ], i -> input_positions[i] );
        output_positions := List( [ 1 .. Length( output_positions ) ], i -> output_positions[i] );
        
        # nodes which are simultaneously inputs and outputs or multiple inputs or outputs are not supported by PyZX
        # split such nodes into multiple input or outputs nodes connected by an edge
        for pos in [ 1 .. Length( labels ) ] do
            
            # find input and output indices corresponding to this node
            input_positions_indices := Positions( input_positions, pos - 1 );
            output_positions_indices := Positions( output_positions, pos - 1 );
            
            if Length( input_positions_indices ) = 0 and Length( output_positions_indices ) = 0 then
                
                # not an input or output node
                
                # inner neutral nodes have been removed above
                Assert( 0, labels[pos] <> "neutral" );
                
                continue;
                
            fi;
            
            Assert( 0, labels[pos] = "neutral" );
            
            if Length( input_positions_indices ) = 1 and Length( output_positions_indices ) = 0 then
                
                # normal input node
                continue;
                
            elif Length( input_positions_indices ) = 0 and Length( output_positions_indices ) = 1 then
                
                # normal output node
                continue;
                
            elif Length( input_positions_indices ) = 1 and Length( output_positions_indices ) = 1 then
                
                # simultaneously an input and an output:
                # add a new neutral node for the output and an edge between input and output
                Add( labels, "neutral" );
                output_positions[output_positions_indices[1]] := Length( labels ) - 1;
                Add( edges, [ pos - 1, Length( labels ) - 1 ] );
                
            elif Length( input_positions_indices ) = 2 and Length( output_positions_indices ) = 0 then
                
                # simultaneously two inputs:
                # add a new neutral node for a separate input and a dummy Z node to connect the two inputs
                Add( labels, "neutral" );
                input_positions[input_positions_indices[2]] := Length( labels ) - 1;
                Add( labels, "Z" );
                Add( edges, [ input_positions[input_positions_indices[1]], Length( labels ) - 1 ] );
                Add( edges, [ input_positions[input_positions_indices[2]], Length( labels ) - 1 ] );
                
            elif Length( input_positions_indices ) = 0 and Length( output_positions_indices ) = 2 then
                
                # simultaneously two outputs:
                # add a new neutral node for a separate output and a dummy Z node to connect the two outputs
                Add( labels, "neutral" );
                output_positions[output_positions_indices[2]] := Length( labels ) - 1;
                Add( labels, "Z" );
                Add( edges, [ output_positions[output_positions_indices[1]], Length( labels ) - 1 ] );
                Add( edges, [ output_positions[output_positions_indices[2]], Length( labels ) - 1 ] );
                
            else
                
                # COVERAGE_IGNORE_NEXT_LINE
                Error( "this case should not appear in a well-defined ZX-diagram" );
                
            fi;
            
        od;
        
        edges := Set( edges );
        
        wire_vertices := rec( );
        node_vertices := rec( );
        
        vertex_names := [ ];
        
        # we want to pad all numbers with zeros on the left so the order does not change when ordering them as strings
        # this helps to work around https://github.com/Quantomatic/pyzx/issues/168
        padding_length := Int( Log10( Float( Length( labels ) ) ) ) + 1;
        
        get_vertex_name := function ( prefix, record )
          local id, id_string, vertex_name;
            
            id := Length( RecNames( record ) );
            
            id_string := String( id, padding_length );
            
            vertex_name := Concatenation( prefix, ReplacedString( id_string, " ", "0" ) );
            
            Assert( 0, not IsBound( record.(vertex_name) ) );
            
            return vertex_name;
            
        end;
        
        # See https://github.com/Quantomatic/quantomatic/blob/stable/docs/json_formats.txt
        # for a rough overview of the qgraph format.
        
        for pos in [ 1 .. Length( labels ) ] do
            
            if labels[pos][1] = 'Z' then
                
                vertex_name := get_vertex_name( "v", node_vertices );
                
                node_vertices.(vertex_name) := rec(
                    annotation := rec(
                        coord := [ 1, - pos ],
                    ),
                    data := rec(
                        type := "Z",
                    )
                );
                
                if Length( labels[pos] ) > 1 then
                    
                    node_vertices.(vertex_name).data.value := labels[pos]{[ 2 .. Length( labels[pos] ) ]};
                    
                fi;
                
            elif labels[pos][1] = 'X' then
                
                vertex_name := get_vertex_name( "v", node_vertices );
                
                node_vertices.(vertex_name) := rec(
                    annotation := rec(
                        coord := [ 1, - pos ],
                    ),
                    data := rec(
                        type := "X",
                    )
                );
                
                if Length( labels[pos] ) > 1 then
                    
                    node_vertices.(vertex_name).data.value := labels[pos]{[ 2 .. Length( labels[pos] ) ]};
                    
                fi;
                
            elif labels[pos] = "H" then
                
                vertex_name := get_vertex_name( "v", node_vertices );
                
                node_vertices.(vertex_name) := rec(
                    annotation := rec(
                        coord := [ 1, - pos ],
                    ),
                    data := rec(
                        type := "hadamard",
                        # always use Hadamard edges to work around https://github.com/Quantomatic/pyzx/issues/161
                        is_edge := "true",
                        value := "\\pi",
                    ),
                );
                
            elif labels[pos] = "neutral" then
                
                vertex_name := get_vertex_name( "b", wire_vertices );
                
                is_input := (pos - 1) in input_positions;
                is_output := (pos - 1) in output_positions;
                
                if is_input and is_output then
                    
                    # COVERAGE_IGNORE_NEXT_LINE
                    Error( "found neutral node which is simultaneously an input and an output, this is not supported by PyZX" );
                    
                elif is_input then
                    
                    input_position := SafeUniquePosition( input_positions, pos - 1 ) - 1;
                    
                    wire_vertices.(vertex_name) := rec(
                        annotation := rec(
                            boundary := true,
                            coord := [ 0, - input_position ],
                            input := input_position,
                        ),
                    );
                    
                elif is_output then
                    
                    output_position := SafeUniquePosition( output_positions, pos - 1 ) - 1;
                    
                    wire_vertices.(vertex_name) := rec(
                        annotation := rec(
                            boundary := true,
                            coord := [ 2, - output_position ],
                            output := output_position,
                        ),
                    );
                    
                else
                    
                    # COVERAGE_IGNORE_NEXT_LINE
                    Error( "found inner neutral node, this is not supported by PyZX" );
                    
                fi;
                
            else
                
                # COVERAGE_IGNORE_NEXT_LINE
                Error( "unknown label ", labels[pos] );
                
            fi;
            
            Assert( 0, Length( vertex_names ) = pos - 1 );
            Add( vertex_names, vertex_name );
            
        od;
        
        Assert( 0, Length( vertex_names ) = Length( labels ) );
        
        undir_edges := rec( );
        
        for edge_counter in [ 1 .. Length( edges ) ] do
            
            edge := edges[edge_counter];
            
            edge_name := Concatenation( "e", String( edge_counter - 1 ) );
            
            src_vertex_name := vertex_names[edge[1] + 1];
            tgt_vertex_name := vertex_names[edge[2] + 1];
            
            undir_edges.(edge_name) := rec( src := src_vertex_name, tgt := tgt_vertex_name );
            
        od;
        
        qgraph := rec( wire_vertices := wire_vertices,
                       node_vertices := node_vertices,
                       undir_edges := undir_edges );
        
        return GapToJsonString( qgraph );
        
    end );
    
    InstallGlobalFunction( ExportAsQGraphFile,
      function ( phi, filename )
        local qgraph;
        
        qgraph := ExportAsQGraphString( phi );
        
        FileString( filename, qgraph );
        
        # suppress return value for julia
        return;
        
    end );
    
    InstallGlobalFunction( ImportFromQGraphString,
      function ( cat, qgraph )
        local labels, edges, wire_vertices, node_vertices, undir_edges, vertex_names,
              input_positions, output_positions, edge, src_vertex, tgt_vertex, annotation, data,
              full_type, io_positions, src_index, tgt_index, via_index, source, range, mor, name;
        
        labels := [ ];
        edges := [ ];
        
        qgraph := JsonStringToGap( qgraph );
        
        wire_vertices := qgraph.wire_vertices;
        node_vertices := qgraph.node_vertices;
        undir_edges := qgraph.undir_edges;
        
        vertex_names := [ ];
        # will be turned into lists later because Julia does not support non-dense lists
        input_positions := rec( );
        output_positions := rec( );
        
        # identify inputs or outputs connected to other inputs or outputs
        for name in SortedList( RecNames( undir_edges ) ) do
            
            edge := undir_edges.(name);
            
            if edge.src = edge.tgt then
                
                Error( "loops are currently not supported" );
                
            fi;
            
            if IsBound( wire_vertices.(edge.src) ) and IsBound( wire_vertices.(edge.tgt) ) then
                
                src_vertex := wire_vertices.(edge.src);
                tgt_vertex := wire_vertices.(edge.tgt);
                
                if IsBound( src_vertex.annotation.input ) and IsBound( tgt_vertex.annotation.input ) then
                    
                    Assert( 0, not IsBound( src_vertex.annotation.output ) );
                    Assert( 0, not IsBound( tgt_vertex.annotation.output ) );
                    
                    src_vertex.annotation.input2 := tgt_vertex.annotation.input;
                    
                elif IsBound( src_vertex.annotation.input ) and IsBound( tgt_vertex.annotation.output ) then
                    
                    Assert( 0, not IsBound( src_vertex.annotation.output ) );
                    Assert( 0, not IsBound( tgt_vertex.annotation.input ) );
                    
                    src_vertex.annotation.output := tgt_vertex.annotation.output;
                    
                elif IsBound( src_vertex.annotation.output ) and IsBound( tgt_vertex.annotation.input ) then
                    
                    Assert( 0, not IsBound( src_vertex.annotation.input ) );
                    Assert( 0, not IsBound( tgt_vertex.annotation.output ) );
                    
                    src_vertex.annotation.input := tgt_vertex.annotation.input;
                    
                elif IsBound( src_vertex.annotation.output ) and IsBound( tgt_vertex.annotation.output ) then
                    
                    Assert( 0, not IsBound( src_vertex.annotation.input ) );
                    Assert( 0, not IsBound( tgt_vertex.annotation.input ) );
                    
                    src_vertex.annotation.output2 := tgt_vertex.annotation.output;
                    
                else
                    
                    Error( "this should never happen" );
                    
                fi;
                
                Unbind( wire_vertices.(edge.tgt) );
                
                Unbind( undir_edges.(name) );
                
            fi;
            
        od;
        
        for name in SortedList( RecNames( wire_vertices ) ) do
            
            Add( vertex_names, name );
            
            annotation := wire_vertices.(name).annotation;
            
            Add( labels, "neutral" );
            
            Assert( 0, Number( [ "input", "input2", "output", "output2" ], name -> IsBound( annotation.(name) ) ) > 0 );
            Assert( 0, Number( [ "input", "input2", "output", "output2" ], name -> IsBound( annotation.(name) ) ) <= 2 );
            
            if IsBound( annotation.input ) then
                
                input_positions.(annotation.input + 1) := Length( labels ) - 1;
                
            fi;
            
            if IsBound( annotation.input2 ) then
                
                Assert( 0, IsBound( annotation.input ) );
                
                input_positions.(annotation.input2 + 1) := Length( labels ) - 1;
                
            fi;
            
            if IsBound( annotation.output ) then
                
                output_positions.(annotation.output + 1) := Length( labels ) - 1;
                
            fi;
            
            if IsBound( annotation.output2 ) then
                
                Assert( 0, IsBound( annotation.output ) );
                
                output_positions.(annotation.output2 + 1) := Length( labels ) - 1;
                
            fi;
            
        od;
        
        Assert( 0, SortedList( RecNames( input_positions ) ) = List( [ 1 .. Length( RecNames( input_positions ) ) ], i -> String( i ) ) );
        Assert( 0, SortedList( RecNames( output_positions ) ) = List( [ 1 .. Length( RecNames( output_positions ) ) ], i -> String( i ) ) );
        
        input_positions := List( [ 1 .. Length( RecNames( input_positions ) ) ], i -> BigInt( input_positions.(i) ) );
        output_positions := List( [ 1 .. Length( RecNames( output_positions ) ) ], i -> BigInt( output_positions.(i) ) );
        
        for name in SortedList( RecNames( node_vertices ) ) do
            
            Add( vertex_names, name );
            
            data := node_vertices.(name).data;
            
            if data.type = "Z" then
                
                if IsBound( data.value ) then
                    
                    full_type := Concatenation( "Z", data.value );
                    
                else
                    
                    full_type := "Z";
                    
                fi;
                
                Add( labels, full_type );
                
            elif data.type = "X" then
                
                if IsBound( data.value ) then
                    
                    full_type := Concatenation( "X", data.value );
                    
                else
                    
                    full_type := "X";
                    
                fi;
                
                Add( labels, full_type );
                
            elif data.type = "hadamard" then
                
                Add( labels, "H" );
                
            else
                
                Error( "node vertex has unkown type ", data.type );
                
            fi;
            
        od;
        
        Assert( 0, Length( labels ) = Length( vertex_names ) );
        
        io_positions := Concatenation( input_positions, output_positions );
        
        for name in SortedList( RecNames( undir_edges ) ) do
            
            edge := undir_edges.(name);
            
            src_index := BigInt( SafeUniquePosition( vertex_names, edge.src ) ) - 1;
            tgt_index := BigInt( SafeUniquePosition( vertex_names, edge.tgt ) ) - 1;
            
            if src_index in io_positions and tgt_index in io_positions then
                
                Error( "this case should have been handled above" );
                
            elif src_index in io_positions then
                
                Add( edges, [ src_index, tgt_index ] );
                
            elif tgt_index in io_positions then
                
                Add( edges, [ tgt_index, src_index ] );
                
            else
                
                Add( labels, "neutral" );
                
                via_index := BigInt( Length( labels ) ) - 1;
                
                Add( edges, [ via_index, src_index ] );
                Add( edges, [ via_index, tgt_index ] );
                
            fi;
            
        od;
        
        source := ObjectConstructor( cat, Length( input_positions ) );
        range := ObjectConstructor( cat, Length( output_positions ) );
        
        mor := MorphismConstructor( cat, source, NTuple( 4, labels, input_positions, output_positions, edges ), range );
        
        Assert( 0, IsWellDefinedForMorphisms( cat, mor ) );
        
        return mor;
        
    end );
    
    InstallGlobalFunction( ImportFromQGraphFile,
      function ( cat, filename )
        local qgraph;
        
        qgraph := StringFile( filename );
        
        Assert( 0, qgraph <> fail );
        
        return ImportFromQGraphString( cat, qgraph );
        
    end );
    
fi;

##
InstallMethod( ForgetIO,
        "for a ZX-diagram",
        [ IsMorphismInCategoryOfZXDiagrams ],
        
  function( zx_diagram )
    local zx_wo_io;
    
    zx_wo_io := UnderlyingCategoryOfZXDiagramsWithoutIO( CapCategory( zx_diagram ) );
    
    return MorphismDatum( zx_diagram ){[1,4]} / zx_wo_io;
    
end );

##
InstallMethod( DotVertexLabelledDigraph,
        "for a ZX-diagram without IO",
        [ IsObjectInCategoryOfZXDiagramsWithoutIO ],
        
  function( zx_diagram )
    local str, pair, labels, edges, sources, outer_nodes, all_inner_nodes, neutral_inner_nodes, inner_nodes,
          i, label, outer_edges, inner_edges, connected_neutral_inner_nodes;
    
    str := Concatenation(
                   "//dot\n",
                   "digraph ZX_diagram_without_IO{\n",
                   "rankdir=\"LR\"\n",
                   "minlen=0\n",
                   "layout=\"dot\"\n",
                   "node [shape=circle width=0.2 height=0 style=filled fontsize=\"10\"]\n",
                   "edge [dir=none fontsize=\"10\"]\n\n" );
    
    pair := ObjectDatum( zx_diagram );
    
    labels := pair[1];
    edges := pair[2];
    
    sources := List( edges, e -> e[1] );
    
    outer_nodes := Filtered( [ 0 .. Length( labels ) - 1 ], i -> labels[1 + i] = "neutral" and Length( Positions( sources, i ) ) <= 1 );
    
    str := Concatenation( str,
                   Concatenation( List( [ 1 .. Length( outer_nodes ) ], i ->
                           Concatenation( String( outer_nodes[i] ),
                                   " [xlabel=\"", String( outer_nodes[i] ), "\" label=\"\" width=0.1 color=\"black\"]\n" ) ) ),
                   "\n" );
    
    all_inner_nodes := Difference( [ 0 .. Length( labels ) - 1 ], outer_nodes );
    
    neutral_inner_nodes := Filtered( all_inner_nodes, i -> labels[1 + i] = "neutral" );
    
    inner_nodes := Filtered( all_inner_nodes, i -> not labels[1 + i] = "neutral" );
    
    for i in inner_nodes do
        
        label := labels[1 + i];
        str := Concatenation( str, String( i ) );
        
        if label[1] = 'H' then
            str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" shape=\"square\" color=\"orange\"]" );
        elif label[1] = 'X' then
            if Length( label ) > 1 then
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"", label{[ 2 .. Length( label )]}, "\" color=\"tomato\"]" );
            else
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" color=\"tomato\"]" );
            fi;
        elif label[1] = 'Z' then
            if Length( label ) > 1 then
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"", label{[ 2 .. Length( label )]}, "\" color=\"lightgreen\"]" );
            else
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" color=\"lightgreen\"]" );
            fi;
        fi;
        
        str := Concatenation( str, "\n" );
        
    od;
    
    str := Concatenation( str, "\n" );
        
    ## the edges from the outer nodes to the circuit cluster
    outer_edges := Filtered( edges, edge -> not edge[1] in neutral_inner_nodes );
    
    for i in outer_edges do
        str := Concatenation( str, String( i[1] ), " -> ", String( i[2] ), "\n" );
    od;
    
    str := Concatenation( str, "\n" );
    
    ## the edges within the circuit cluster
    inner_edges := Filtered( edges, edge -> edge[1] in neutral_inner_nodes );
    
    ## ignore those which are not connected to any other node, see PreCompose( CoevaluationForDual( qubit ), EvaluationForDual( qubit ) )
    connected_neutral_inner_nodes := Intersection( List( inner_edges, edge -> edge[1] ), neutral_inner_nodes );
    
    for i in connected_neutral_inner_nodes do
        pair := Filtered( inner_edges, edge -> edge[1] = i );
        Assert( 0, Length( pair ) = 2 );
        str := Concatenation( str, String( pair[1][2] ), " -> ", String( pair[2][2] ), " [label=\"", String( pair[1][1] ), "\"] \n" );
    od;
    
    str := Concatenation( str, "}\n" );
    
    return str;
    
end );

MakeShowable( [ "image/svg+xml" ], IsObjectInCategoryOfZXDiagramsWithoutIO );

##
InstallOtherMethod( SvgString,
        "for a ZX-diagram without IO",
        [ IsObjectInCategoryOfZXDiagramsWithoutIO ],
        
  function ( zx_diagram )
    
    return DotToSVG( DotVertexLabelledDigraph( zx_diagram ) );
    
end );

##
InstallOtherMethod( DotVertexLabelledDigraph,
        "for a morphism in the category of spans of ZX-diagrams without IO",
        [ IsMorphismInCategoryOfSpans ],
        
  function( span_of_zx_diagrams )
    local span, mor_source, mor_target, datum_mor_source, datum_mor_target, str, i, label, pair,
          pair_K, labels_K, edges_K, sources_K, outer_nodes_K, all_inner_nodes_K, neutral_inner_nodes_K, inner_nodes_K,
          outer_edges_K, inner_edges_K, connected_neutral_inner_nodes_K,
          pair_L, labels_L, edges_L, sources_L, outer_nodes_L, all_inner_nodes_L, neutral_inner_nodes_L, inner_nodes_L,
          outer_edges_L, inner_edges_L, connected_neutral_inner_nodes_L,
          pair_R, labels_R, edges_R, sources_R, outer_nodes_R, all_inner_nodes_R, neutral_inner_nodes_R, inner_nodes_R,
          outer_edges_R, inner_edges_R, connected_neutral_inner_nodes_R;

    Assert( 0, IsWellDefined( span_of_zx_diagrams ) );
    
    span := MorphismDatum( span_of_zx_diagrams );
    
    mor_source := span[2][1];
    mor_target := span[2][2];
    
    Assert( 0, IsMonomorphism( mor_source ) );
    Assert( 0, IsMonomorphism( mor_target ) );
    
    datum_mor_source := MorphismDatum( mor_source );
    datum_mor_target := MorphismDatum( mor_target );
    
    Assert( 0, datum_mor_source[1] = [ 0 .. Length( datum_mor_source[1] ) - 1 ] );
    Assert( 0, datum_mor_source[2] = [ 0 .. Length( datum_mor_source[2] ) - 1 ] );
    Assert( 0, datum_mor_target[1] = [ 0 .. Length( datum_mor_target[1] ) - 1 ] );
    Assert( 0, datum_mor_target[2] = [ 0 .. Length( datum_mor_target[2] ) - 1 ] );
    
    str := Concatenation(
                   "//dot\n",
                   "digraph span_of_ZX_diagrams{\n",
                   "rankdir=\"LR\"\n",
                   "minlen=0\n",
                   "newrank=true\n",
                   "layout=\"dot\"\n",
                   "node [shape=circle width=0.2 height=0 style=filled fontsize=\"10\"]\n",
                   "edge [dir=none fontsize=\"10\"]\n\n" );
    
    ## the cluster of K:
    str := Concatenation( str,
                   "subgraph cluster_K {\n",
                   "label=\"K\"\n\n" );
    
    pair_K := ObjectDatum( span[1] );
    
    labels_K := pair_K[1];
    edges_K := pair_K[2];
    
    sources_K := List( edges_K, e -> e[1] );
    
    outer_nodes_K := Filtered( [ 0 .. Length( labels_K ) - 1 ], i -> labels_K[1 + i] = "neutral" and Length( Positions( sources_K, i ) ) <= 1 );
    
    str := Concatenation( str,
                   Concatenation( List( [ 1 .. Length( outer_nodes_K ) ], i ->
                           Concatenation( String( outer_nodes_K[i] ),
                                   " [xlabel=\"", String( outer_nodes_K[i] ), "\" label=\"\" width=0.1 color=\"black\"]\n" ) ) ),
                   "\n" );
    
    all_inner_nodes_K := Difference( [ 0 .. Length( labels_K ) - 1 ], outer_nodes_K );
    
    neutral_inner_nodes_K := Filtered( all_inner_nodes_K, i -> labels_K[1 + i] = "neutral" );
    
    inner_nodes_K := Filtered( all_inner_nodes_K, i -> not labels_K[1 + i] = "neutral" );
    
    for i in inner_nodes_K do
        
        label := labels_K[1 + i];
        str := Concatenation( str, String( i ) );
        
        if label[1] = 'H' then
            str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" shape=\"square\" color=\"orange\"]" );
        elif label[1] = 'X' then
            if Length( label ) > 1 then
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"", label{[ 2 .. Length( label )]}, "\" color=\"tomato\"]" );
            else
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" color=\"tomato\"]" );
            fi;
        elif label[1] = 'Z' then
            if Length( label ) > 1 then
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"", label{[ 2 .. Length( label )]}, "\" color=\"lightgreen\"]" );
            else
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" color=\"lightgreen\"]" );
            fi;
        fi;
        
        str := Concatenation( str, "\n" );
        
    od;
    
    str := Concatenation( str, "}\n\n" );
    
    ## L ↩ K :ℓ
    str := Concatenation( str,
                   "subgraph cluster_L {\n",
                   "style=rounded\n",
                   "label=\"L-K\"\n\n" );
    
    pair_L := ObjectDatum( Target( span[2][1] ) );
    
    labels_L := pair_L[1];
    edges_L := pair_L[2];
    
    sources_L := List( edges_L, e -> e[1] );
    
    outer_nodes_L := Filtered( [ 0 .. Length( labels_L ) - 1 ], i -> labels_L[1 + i] = "neutral" and Length( Positions( sources_L, i ) ) <= 1 );
    
    outer_nodes_L := Difference( outer_nodes_L, outer_nodes_K );
    
    str := Concatenation( str,
                   Concatenation( List( [ 1 .. Length( outer_nodes_L ) ], i ->
                           Concatenation( "L", String( outer_nodes_L[i] ),
                                   " [xlabel=\"", String( outer_nodes_L[i] ), "\" label=\"\" width=0.1 color=\"black\"]\n" ) ) ),
                   "\n" );
    
    all_inner_nodes_L := Difference( [ 0 .. Length( labels_L ) - 1 ], outer_nodes_L );
    
    all_inner_nodes_L := Difference( all_inner_nodes_L, all_inner_nodes_K );
    
    neutral_inner_nodes_L := Filtered( all_inner_nodes_L, i -> labels_L[1 + i] = "neutral" );
    
    inner_nodes_L := Filtered( all_inner_nodes_L, i -> not labels_L[1 + i] = "neutral" );
    
    for i in inner_nodes_L do
        
        label := labels_L[1 + i];
        str := Concatenation( str, "L", String( i ) );
        
        if label[1] = 'H' then
            str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" shape=\"square\" color=\"orange\"]" );
        elif label[1] = 'X' then
            if Length( label ) > 1 then
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"", label{[ 2 .. Length( label )]}, "\" color=\"tomato\"]" );
            else
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" color=\"tomato\"]" );
            fi;
        elif label[1] = 'Z' then
            if Length( label ) > 1 then
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"", label{[ 2 .. Length( label )]}, "\" color=\"lightgreen\"]" );
            else
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" color=\"lightgreen\"]" );
            fi;
        fi;
        
        str := Concatenation( str, "\n" );
        
    od;
    
    str := Concatenation( str, "}\n\n" );
    
    ## r: K ↪ R
    str := Concatenation( str,
                   "subgraph cluster_R {\n",
                   "style=rounded\n",
                   "label=\"R-K\"\n\n" );
    
    pair_R := ObjectDatum( Target( span[2][2] ) );
    
    labels_R := pair_R[1];
    edges_R := pair_R[2];
    
    sources_R := List( edges_R, e -> e[1] );
    
    outer_nodes_R := Filtered( [ 0 .. Length( labels_R ) - 1 ], i -> labels_R[1 + i] = "neutral" and Length( Positions( sources_R, i ) ) <= 1 );
    
    outer_nodes_R := Difference( outer_nodes_R, outer_nodes_K );
    
    str := Concatenation( str,
                   Concatenation( List( [ 1 .. Length( outer_nodes_R ) ], i ->
                           Concatenation( "R", String( outer_nodes_R[i] ),
                                   " [xlabel=\"", String( outer_nodes_R[i] ), "\" label=\"\" width=0.1 color=\"black\"]\n" ) ) ),
                   "\n" );
    
    all_inner_nodes_R := Difference( [ 0 .. Length( labels_R ) - 1 ], outer_nodes_R );
    
    all_inner_nodes_R := Difference( all_inner_nodes_R, all_inner_nodes_K );
    
    neutral_inner_nodes_R := Filtered( all_inner_nodes_R, i -> labels_R[1 + i] = "neutral" );
    
    inner_nodes_R := Filtered( all_inner_nodes_R, i -> not labels_R[1 + i] = "neutral" );
    
    for i in inner_nodes_R do
        
        label := labels_R[1 + i];
        str := Concatenation( str, "R", String( i ) );
        
        if label[1] = 'H' then
            str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" shape=\"square\" color=\"orange\"]" );
        elif label[1] = 'X' then
            if Length( label ) > 1 then
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"", label{[ 2 .. Length( label )]}, "\" color=\"tomato\"]" );
            else
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" color=\"tomato\"]" );
            fi;
        elif label[1] = 'Z' then
            if Length( label ) > 1 then
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"", label{[ 2 .. Length( label )]}, "\" color=\"lightgreen\"]" );
            else
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" color=\"lightgreen\"]" );
            fi;
        fi;
        
        str := Concatenation( str, "\n" );
        
    od;
    
    str := Concatenation( str, "}\n\n" );
    
    ## connect K with L-K:
    
    outer_nodes_L := Filtered( [ 0 .. Length( labels_L ) - 1 ], i -> labels_L[1 + i] = "neutral" and Length( Positions( sources_L, i ) ) <= 1 );
    
    all_inner_nodes_L := Difference( [ 0 .. Length( labels_L ) - 1 ], outer_nodes_L );
    
    neutral_inner_nodes_L := Filtered( all_inner_nodes_L, i -> labels_L[1 + i] = "neutral" );
    
    inner_nodes_L := Filtered( all_inner_nodes_L, i -> not labels_L[1 + i] = "neutral" );
    
    ## the edges from the outer nodes to the circuit cluster
    outer_edges_L := Filtered( edges_L, edge -> not edge[1] in neutral_inner_nodes_L );
    
    for i in outer_edges_L do
        str := Concatenation( str, String( i[1] ), " -> L", String( i[2] ), " [constraint=false]\n" );
    od;
    
    str := Concatenation( str, "\n" );
    
    ## the edges within the circuit cluster
    inner_edges_L := Filtered( edges_L, edge -> edge[1] in neutral_inner_nodes_L );
    
    ## ignore those which are not connected to any other node, see PreCompose( CoevaluationForDual( qubit ), EvaluationForDual( qubit ) )
    connected_neutral_inner_nodes_L := Intersection( List( inner_edges_L, edge -> edge[1] ), neutral_inner_nodes_L );
    
    for i in connected_neutral_inner_nodes_L do
        pair := Filtered( inner_edges_L, edge -> edge[1] = i );
        Assert( 0, Length( pair ) = 2 );
        str := Concatenation( str, "L", String( pair[1][2] ), " -> L", String( pair[2][2] ), " [label=\"", String( pair[1][1] ), "\"] \n" );
    od;
    
    str := Concatenation( str, "\n" );
    
    ## connect K with R-K:
    
    outer_nodes_R := Filtered( [ 0 .. Length( labels_R ) - 1 ], i -> labels_R[1 + i] = "neutral" and Length( Positions( sources_R, i ) ) <= 1 );
    
    all_inner_nodes_R := Difference( [ 0 .. Length( labels_R ) - 1 ], outer_nodes_R );
    
    neutral_inner_nodes_R := Filtered( all_inner_nodes_R, i -> labels_R[1 + i] = "neutral" );
    
    inner_nodes_R := Filtered( all_inner_nodes_R, i -> not labels_R[1 + i] = "neutral" );
    
    ## the edges from the outer nodes to the circuit cluster
    outer_edges_R := Filtered( edges_R, edge -> not edge[1] in neutral_inner_nodes_R );
    
    for i in outer_edges_R do
        str := Concatenation( str, String( i[1] ), " -> R", String( i[2] ), " [constraint=false]\n" );
    od;
    
    str := Concatenation( str, "\n" );
    
    ## the edges within the circuit cluster
    inner_edges_R := Filtered( edges_R, edge -> edge[1] in neutral_inner_nodes_R );
    
    ## ignore those which are not connected to any other node, see PreCompose( CoevaluationForDual( qubit ), EvaluationForDual( qubit ) )
    connected_neutral_inner_nodes_R := Intersection( List( inner_edges_R, edge -> edge[1] ), neutral_inner_nodes_R );
    
    for i in connected_neutral_inner_nodes_R do
        pair := Filtered( inner_edges_R, edge -> edge[1] = i );
        Assert( 0, Length( pair ) = 2 );
        str := Concatenation( str, "R", String( pair[1][2] ), " -> R", String( pair[2][2] ), " [label=\"", String( pair[1][1] ), "\"] \n" );
    od;
    
    str := Concatenation( str, "}\n" );
    
    return str;
    
end );

MakeShowable( [ "image/svg+xml" ], IsMorphismInCategoryOfSpans );

##
InstallOtherMethod( SvgString,
        "for a morphism in the category of spans of ZX-diagrams without IO",
        [ IsMorphismInCategoryOfSpans ],
        
  function ( span_of_zx_diagrams )
    
    return DotToSVG( DotVertexLabelledDigraph( span_of_zx_diagrams ) );
    
end );

##
InstallMethod( DotVertexLabelledDigraph,
        "for a monomorphism between ZX-diagram without IO",
        [ IsMorphismInCategoryOfZXDiagramsWithoutIO ],
        
  function( mor )
    local str, pair, labels, edges, sources, outer_nodes, all_inner_nodes, neutral_inner_nodes, inner_nodes,
          images_nodes, images_edges, i, label, outer_edges, inner_edges, connected_neutral_inner_nodes;
    
    Assert( 0, IsMonomorphism( mor ) );
    
    str := Concatenation(
                   "//dot\n",
                   "digraph subobject_ZX_diagram_without_IO{\n",
                   "rankdir=\"LR\"\n",
                   "minlen=0\n",
                   "layout=\"dot\"\n",
                   "node [shape=circle width=0.2 height=0 style=filled fontsize=\"10\"]\n",
                   "edge [dir=none fontsize=\"10\"]\n\n" );
    
    pair := ObjectDatum( Target( mor ) );
    
    labels := pair[1];
    edges := pair[2];
    
    sources := List( edges, e -> e[1] );
    
    outer_nodes := Filtered( [ 0 .. Length( labels ) - 1 ], i -> labels[1 + i] = "neutral" and Length( Positions( sources, i ) ) <= 1 );
    
    str := Concatenation( str,
                   Concatenation( List( [ 1 .. Length( outer_nodes ) ], i ->
                           Concatenation( String( outer_nodes[i] ),
                                   " [xlabel=\"", String( outer_nodes[i] ), "\" label=\"\" width=0.1 color=\"black\"]\n" ) ) ),
                   "\n" );
    
    all_inner_nodes := Difference( [ 0 .. Length( labels ) - 1 ], outer_nodes );
    
    neutral_inner_nodes := Filtered( all_inner_nodes, i -> labels[1 + i] = "neutral" );
    
    inner_nodes := Filtered( all_inner_nodes, i -> not labels[1 + i] = "neutral" );
    
    pair := MorphismDatum( mor );
    
    images_nodes := pair[1];
    images_edges := pair[2];
    
    for i in inner_nodes do
        
        label := labels[1 + i];
        str := Concatenation( str, String( i ) );
        
        if label[1] = 'H' then
            str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" shape=\"square\" color=\"orange\"" );
            ## https://graphviz.org/doc/info/colors/
            if not i in images_nodes then
               Append( str, " fontcolor=\"azure3\" color=\"lightyellow\"" );
            fi;
        elif label[1] = 'X' then
            if Length( label ) > 1 then
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"", label{[ 2 .. Length( label )]}, "\" color=\"tomato\"" );
            else
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" color=\"tomato\"" );
            fi;
            ## https://graphviz.org/doc/info/colors/
            if not i in images_nodes then
               Append( str, " fontcolor=\"azure3\" color=\"lightpink\"" );
            fi;
        elif label[1] = 'Z' then
            if Length( label ) > 1 then
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"", label{[ 2 .. Length( label )]}, "\" color=\"lightgreen\"" );
            else
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" color=\"lightgreen\"" );
            fi;
            ## https://graphviz.org/doc/info/colors/
            if not i in images_nodes then
               Append( str, " fontcolor=\"azure3\" color=\"aquamarine\"" );
            fi;
        fi;
        
        str := Concatenation( str, "]\n" );
        
    od;
    
    str := Concatenation( str, "\n" );
        
    ## the edges from the outer nodes to the circuit cluster
    outer_edges := Filtered( edges, edge -> not edge[1] in neutral_inner_nodes );
    
    for i in outer_edges do
        str := Concatenation( str, String( i[1] ), " -> ", String( i[2] ) );
        if not i in images_edges then
            Append( str, " [fontcolor=\"azure3\" color=\"azure3\"]\n" );
        fi;
    od;
    
    str := Concatenation( str, "\n" );
    
    ## the edges within the circuit cluster
    inner_edges := Filtered( edges, edge -> edge[1] in neutral_inner_nodes );
    
    ## ignore those which are not connected to any other node, see PreCompose( CoevaluationForDual( qubit ), EvaluationForDual( qubit ) )
    connected_neutral_inner_nodes := Intersection( List( inner_edges, edge -> edge[1] ), neutral_inner_nodes );
    
    for i in connected_neutral_inner_nodes do
        pair := Filtered( inner_edges, edge -> edge[1] = i );
        Assert( 0, Length( pair ) = 2 );
        str := Concatenation( str, String( pair[1][2] ), " -> ", String( pair[2][2] ), " [label=\"", String( pair[1][1] ), "\"" );
        if not i in images_edges then
            Append( str, " fontcolor=\"azure3\" color=\"azure3\"]\n" );
        fi;
    od;
    
    str := Concatenation( str, "}\n" );
    
    return str;
    
end );

MakeShowable( [ "image/svg+xml" ], IsMorphismInCategoryOfZXDiagramsWithoutIO );

##
InstallOtherMethod( SvgString,
        "for a monomorphism between ZX-diagram without IO",
        [ IsMorphismInCategoryOfZXDiagramsWithoutIO ],
        
  function ( mor )
    
    return DotToSVG( DotVertexLabelledDigraph( mor ) );
    
end );

##
InstallMethod( DotVertexLabelledDigraph,
        "for a ZX-diagram",
        [ IsMorphismInCategoryOfZXDiagrams ],
        
  function( zx_diagram )
    local str, tuple, labels, input, output, edges, inputs_and_outputs, inp, out, int, input_output, all_inner_nodes, neutral_inner_nodes, inner_nodes,
          i, label, outer_edges, inner_edges, connected_neutral_inner_nodes, pair;
    
    str := Concatenation(
                   "//dot\n",
                   "digraph ZX_diagram{\n",
                   "rankdir=\"LR\"\n",
                   "minlen=0\n",
                   "layout=\"dot\"\n",
                   "node [shape=circle width=0.2 height=0 style=filled fontsize=\"10\"]\n",
                   "edge [dir=none fontsize=\"10\"]\n\n" );
    
    tuple := MorphismDatum( zx_diagram );
    
    labels := tuple[1];
    input := tuple[2];
    output := tuple[3];
    edges := tuple[4];
    
    inputs_and_outputs := Union( input, output );
    
    ## nodes that appear twice as an input
    
    ## [ 0, 1, 0, 1, 2 ] -> [ 0, 1 ]
    inp := Filtered( DuplicateFreeList( input ), k -> Length( Positions( input, k ) ) = 2 );
    
    edges := Concatenation( edges, List( inp, k -> [ Concatenation( "i_", String( k ), "_1" ), Concatenation( "i_", String( k ), "_2" ) ] ) );
    
    ## [ 0, 1, 0, 1, 2 ] -> [ "i_0_1", "i_1_1", "i_0_2", "i_1_2", 2 ]
    input := List( [ 1 .. Length( input ) ], function( p ) local k; k := input[p];
        if not k in inp then return k; else return Concatenation( "i_", String( k ), "_", String( Position( Positions( input, k ), p ) ) ); fi; end );
    
    ## nodes that appear twice as an output
    
    ## [ 3, 4, 5, 3, 4 ] -> [ 3, 4 ]
    out := Filtered( DuplicateFreeList( output ), k -> Length( Positions( output, k ) ) = 2 );
    
    edges := Concatenation( edges, List( out, k -> [ Concatenation( "o_", String( k ), "_1" ), Concatenation( "o_", String( k ), "_2" ) ] ) );
    
    ## [ 3, 4, 5, 3, 4 ] -> [ "o_3_1", "o_4_1", 5, "o_3_2", "o_4_2" ]
    output := List( [ 1 .. Length( output ) ], function( p ) local k; k := output[p];
        if not k in out then return k; else return Concatenation( "o_", String( k ), "_", String( Position( Positions( output, k ), p ) ) ); fi; end );
    
    ## nodes that are simultaneously inputs and outputs
    int := Intersection( input, output );
    
    input := List( input, function( k ) if k in int then return Concatenation( "i_", String( k ) ); else return k; fi; end );
    output := List( output, function( k ) if k in int then return Concatenation( "o_", String( k ) ); else return k; fi; end );
    
    ## the cluster of input nodes
    str := Concatenation( str,
                   "subgraph cluster_input {\n",
                   "style=rounded\n",
                   "label=\"input\"\n" );
    
    str := Concatenation( str,
                   Concatenation( List( [ 1 .. Length( input ) ], i ->
                           Concatenation( String( input[i] ),
                                   " [xlabel=\"", String( input[i] ), "\" label=\"\" ",
                                   "pos=\"0,", String( i ),
                                   "!\" width=0.1 color=\"black\"]\n" ) ) ),
                   "}\n\n" );
    
    ## the cluster of output nodes
    str := Concatenation( str,
                   "subgraph cluster_output {\n",
                   "style=rounded\n",
                   "label=\"output\"\n" );
    
    str := Concatenation( str,
                   Concatenation( List( [ 1 .. Length( output ) ], i ->
                           Concatenation( String( output[i] ),
                                   " [xlabel=\"", String( output[i] ), "\" label=\"\" ",
                                   "pos=\"1,", String( i ),
                                   "!\" width=0.1 color=\"black\"]\n" ) ) ),
                   "}\n\n" );
    
    ## the cluster of circuit nodes
    str := Concatenation( str, "subgraph cluster_circuit{\n" );
    
    all_inner_nodes := Difference( [ 0 .. Length( labels ) - 1 ], inputs_and_outputs );
    
    neutral_inner_nodes := Filtered( all_inner_nodes, i -> labels[1 + i] = "neutral" );
    
    inner_nodes := Filtered( all_inner_nodes, i -> not labels[1 + i] = "neutral" );
    
    for i in inner_nodes do
        
        label := labels[1 + i];
        str := Concatenation( str, String( i ) );
        
        if label[1] = 'H' then
            str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" shape=\"square\" color=\"orange\"]" );
        elif label[1] = 'X' then
            if Length( label ) > 1 then
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"", label{[ 2 .. Length( label )]}, "\" color=\"tomato\"]" );
            else
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" color=\"tomato\"]" );
            fi;
        elif label[1] = 'Z' then
            if Length( label ) > 1 then
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"", label{[ 2 .. Length( label )]}, "\" color=\"lightgreen\"]" );
            else
                str := Concatenation( str, " [xlabel=\"", String( i ), "\" label=\"\" color=\"lightgreen\"]" );
            fi;
        fi;
        
        str := Concatenation( str, "\n" );
        
    od;
    
    str := Concatenation( str, "}\n\n" );
    
    ## the edges from the input and output clusters to the circuit cluster
    outer_edges := Filtered( edges, edge -> not edge[1] in neutral_inner_nodes );
    
    for i in outer_edges do
        str := Concatenation( str, String( i[1] ), " -> ", String( i[2] ), "\n" );
    od;
    
    str := Concatenation( str, "\n" );
    
    ## the edges from input to ouput bypassing the circuit cluster
    for i in int do
        str := Concatenation( str, Concatenation( "i_", String( i ) ), " -> ", Concatenation( "o_", String( i ) ), "\n" );
    od;
    
    str := Concatenation( str, "\n" );
    
    ## the edges within the circuit cluster
    inner_edges := Filtered( edges, edge -> edge[1] in neutral_inner_nodes );
    
    ## ignore those which are not connected to any other node, see PreCompose( CoevaluationForDual( qubit ), EvaluationForDual( qubit ) )
    connected_neutral_inner_nodes := Intersection( List( inner_edges, edge -> edge[1] ), neutral_inner_nodes );
    
    for i in connected_neutral_inner_nodes do
        pair := Filtered( inner_edges, edge -> edge[1] = i );
        Assert( 0, Length( pair ) = 2 );
        str := Concatenation( str, String( pair[1][2] ), " -> ", String( pair[2][2] ), " [label=\"", String( pair[1][1] ), "\"] \n" );
    od;
    
    str := Concatenation( str, "}\n" );
    
    return str;
    
end );

MakeShowable( [ "image/svg+xml" ], IsMorphismInCategoryOfZXDiagrams );

##
InstallOtherMethod( SvgString,
        "for a ZX-diagram",
        [ IsMorphismInCategoryOfZXDiagrams ],
        
  function ( zx_diagram )
    
    return DotToSVG( DotVertexLabelledDigraph( zx_diagram ) );
    
end );
