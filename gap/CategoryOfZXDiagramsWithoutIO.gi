# SPDX-License-Identifier: GPL-2.0-or-later
# ZXCalculusForCAP: The category of ZX-diagrams
#
# Implementations
#

##
InstallMethod( CategoryOfZXDiagramsWithoutIO,
        [ IsCategoryOfDecoratedQuivers ],
        
  FunctionWithNamedArguments(
  [
    [ "no_precompiled_code", false ],
    [ "FinalizeCategory", true ],
  ],
  function ( CAP_NAMED_ARGUMENTS, category_of_decorated_quivers )
    local name, category_filter, category_object_filter, category_morphism_filter,
          object_datum_type, object_constructor, object_datum,
          morphism_datum_type, morphism_constructor, morphism_datum,
          valency_of_neutral_nodes_at_most_two, full_subcat,
          modeling_tower_object_constructor, modeling_tower_object_datum,
          modeling_tower_morphism_constructor, modeling_tower_morphism_datum,
          ZX_without_IO;
    
    ##
    name := Concatenation( "CategoryOfZXDiagramsWithoutIO( ", Name( category_of_decorated_quivers ), " )" );
    
    ##
    category_filter := IsCategoryOfZXDiagramsWithoutIO;
    category_object_filter := IsObjectInCategoryOfZXDiagramsWithoutIO;
    category_morphism_filter := IsMorphismInCategoryOfZXDiagramsWithoutIO;
    
    ##
    object_datum_type :=
      CapJitDataTypeOfNTupleOf( 2,
              CapJitDataTypeOfListOf( IsStringRep ),
              CapJitDataTypeOfListOf( CapJitDataTypeOfNTupleOf( 2, IsBigInt, IsBigInt ) ) );
    
    object_constructor :=
      function ( ZX_without_IO, pair )
        
        return CreateCapCategoryObjectWithAttributes( ZX_without_IO,
                       PairOfStringsAndListOfPairsOfIntegers, pair );
        
    end;
    
    object_datum :=
      function ( ZX_without_IO, object )
        
        return PairOfStringsAndListOfPairsOfIntegers( object );
        
    end;
    
    ##
    morphism_datum_type :=
      CapJitDataTypeOfNTupleOf( 2,
              CapJitDataTypeOfListOf( IsInt ),
              CapJitDataTypeOfListOf( IsInt ) );
    
    morphism_constructor :=
      function ( ZX_without_IO, source, pair, target )
        
        return CreateCapCategoryMorphismWithAttributes( ZX_without_IO,
                       source,
                       target,
                       PairOfListsOfIntegers, pair );
        
    end;
    
    morphism_datum :=
      function ( ZX_without_IO, morphism )
        
        return PairOfListsOfIntegers( morphism );
        
    end;
    
    ## building the categorical tower:
    
    ## the full subcategory of category_of_decorated_quivers where each neutral node has valency at most 2:
    ## valency 0: either double input, or input-output, or double output
    ## valency 1: either input or output
    ## valency 2: inner node

    valency_of_neutral_nodes_at_most_two :=
      function( ambient_cat, decorated_quiver )
        local pair;
        
        pair := DefiningPairOfDecoratedQuiver( decorated_quiver );
        
        return ForAll( List( Collected( List( pair[1][3], p -> p[1] ) ), vm -> vm[2] ), m -> m <= 2 );
        
    end;
    
    full_subcat := FullSubcategoryByObjectMembershipFunction( category_of_decorated_quivers, valency_of_neutral_nodes_at_most_two );
    
    ## from the raw object data to the object in the modeling category
    modeling_tower_object_constructor :=
      function ( ZX_without_IO, pair )
        local full_subcat, category_of_decorated_quivers, labels, edges,
              nr_vertices, nr_edges, decorations_of_vertices, decorations_of_edges,
              decorated_quiver, input_morphism_decorated_quivers, output_morphism_decorated_quivers,
              input_morphism, output_morphism, morphism_pair, mor;
        
        full_subcat := ModelingCategory( ZX_without_IO );
        
        category_of_decorated_quivers := AmbientCategory( full_subcat );
        
        labels := pair[1];
        edges := pair[2];
        
        nr_vertices := Length( labels );
        nr_edges := Length( edges );
        
        decorations_of_vertices := List( labels, ZX_LabelToInteger );
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0,
                ZX_LabelToInteger( "neutral" ) = 0 and
                ForAll( edges, edge -> decorations_of_vertices[1 + edge[1]] = 0 ) ); # all edges start from a neutrally decorated vertex
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0,
                ForAll( edges, edge ->
                        S_ZX_EDGES[decorations_of_vertices[1 + edge[2]]] =
                        Pair( decorations_of_vertices[1 + edge[1]], decorations_of_vertices[1 + edge[2]] ) ) ); # the edge [ 0, i ] has position i in S_ZX_EDGES
        
        decorations_of_edges := List( edges, edge -> decorations_of_vertices[1 + edge[2]] - 1 );
        
        decorated_quiver :=
          ObjectConstructor( category_of_decorated_quivers,
                  Pair( Triple(
                          nr_vertices,
                          nr_edges,
                          edges ),
                        Pair( decorations_of_vertices,
                              decorations_of_edges ) ) );
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0, IsWellDefinedForObjects( category_of_decorated_quivers, decorated_quiver ) );
        
        return ObjectConstructor( full_subcat, decorated_quiver );
        
    end;
    
    ## from the object in the modeling category to the raw object data
    modeling_tower_object_datum :=
      function ( ZX_without_IO, decorated_quiver_in_full_subcat )
        local decorated_quiver, decorated_quiver_data,
              nr_vertices, decorations_of_vertices, labels, edges;
        
        decorated_quiver := UnderlyingCell( decorated_quiver_in_full_subcat );
        
        decorated_quiver_data := DefiningPairOfDecoratedQuiver( decorated_quiver );
        
        nr_vertices := decorated_quiver_data[1][1];
        
        decorations_of_vertices := decorated_quiver_data[2][1];
        
        labels := List( decorations_of_vertices, d -> ZX_IntegerToLabel( d ) );
        edges := decorated_quiver_data[1][3];
        
        return Pair( labels, edges );
        
    end;
    
    ## from the raw morphism data to the morphism in the modeling category
    modeling_tower_morphism_constructor :=
      function ( ZX_without_IO, source, pair, target )
        local full_subcat, category_of_decorated_quivers;
        
        full_subcat := ModelingCategory( ZX_without_IO );
        
        category_of_decorated_quivers := AmbientCategory( full_subcat );
        
        return MorphismConstructor( full_subcat,
                       source,
                       MorphismConstructor( category_of_decorated_quivers,
                               UnderlyingCell( source ),
                               pair,
                               UnderlyingCell( target ) ),
                       target );
        
    end;
    
    ## from the morphism in the modeling category to the raw morphism data
    modeling_tower_morphism_datum :=
      function ( ZX_without_IO, morphism )
        
        return PairOfListsOfIntegers( morphism );
        
    end;
    
    ZX_without_IO :=
      ReinterpretationOfCategory( full_subcat,
              rec( name := name,
                   category_filter := category_filter,
                   category_object_filter := category_object_filter,
                   category_morphism_filter := category_morphism_filter,
                   object_datum_type := object_datum_type,
                   morphism_datum_type := morphism_datum_type,
                   object_constructor := object_constructor,
                   object_datum := object_datum,
                   morphism_constructor := morphism_constructor,
                   morphism_datum := morphism_datum,
                   modeling_tower_object_constructor := modeling_tower_object_constructor,
                   modeling_tower_object_datum := modeling_tower_object_datum,
                   modeling_tower_morphism_constructor := modeling_tower_morphism_constructor,
                   modeling_tower_morphism_datum := modeling_tower_morphism_datum,
                   only_primitive_operations := true )
              : FinalizeCategory := false );
    
    if not CAP_NAMED_ARGUMENTS.no_precompiled_code then
        
        #
        
    fi;
    
    SetUnderlyingCategoryOfSpansOfZXDiagramsWithoutIO( ZX_without_IO, CategoryOfSpans( ZX_without_IO ) );
    
    if CAP_NAMED_ARGUMENTS.FinalizeCategory then
        
        Finalize( ZX_without_IO );
        
    fi;
    
    return ZX_without_IO;
    
end ) );

##
InstallMethod( ZXFusionRule,
        [ IsCategoryOfZXDiagramsWithoutIO, IsString, IsInt, IsInt, IsInt ],
        
  function ( ZX_without_IO, type, c, l1, l2 )
    local e, labels_L, edges_L, L, labels_R, edges_R, R, labels_K, K, l, r, SpanOfZX;
    
    Assert( 0, c > 0 );
    
    e := l1 + l2;
    
    labels_L := Concatenation( ListWithIdenticalEntries( e + c, "neutral" ), [ type, type ] );
    
    edges_L := Concatenation(
                       List( [ 0 .. l1 - 1 ], i -> Pair( i, e + c ) ),
                       List( [ 0 .. l2 - 1 ], i -> Pair( l1 + i, e + c + 1 ) ),
                       Concatenation( List( [ 0 .. c - 1 ], i -> [ Pair( e + i, e + c ), Pair( e + i, e + c + 1 ) ] ) ) );
    
    L := ObjectConstructor( ZX_without_IO, Pair( labels_L, edges_L ) );
    
    labels_R := Concatenation( ListWithIdenticalEntries( e, "neutral" ), [ type ] );
    
    edges_R := Concatenation(
                       List( [ 0 .. l1 - 1 ], i -> Pair( i, e ) ),
                       List( [ 0 .. l2 - 1 ], i -> Pair( l1 + i, e ) ) );
    
    R := ObjectConstructor( ZX_without_IO, Pair( labels_R, edges_R ) );
    
    labels_K := ListWithIdenticalEntries( e, "neutral" );
    
    K := ObjectConstructor( ZX_without_IO, Pair( labels_K, [ ] ) );
    
    l := MorphismConstructor( ZX_without_IO,
                 K,
                 Pair( [ 0 .. e - 1 ], [ ] ),
                 L );
    
    SetIsMonomorphism( l, true );
    
    r := MorphismConstructor( ZX_without_IO,
                 K,
                 Pair( [ 0 .. e - 1 ], [ ] ),
                 R );
    
    SetIsMonomorphism( r, true );
    
    SpanOfZX := UnderlyingCategoryOfSpansOfZXDiagramsWithoutIO( ZX_without_IO );
    
    return MorphismConstructor( SpanOfZX, l, r );
    
end );

####################################
#
# View and Display methods
#
####################################

BindGlobal( "ZX_without_IO_RemovedInnerNeutralNodes", function ( pair )
  local labels, edges, outer_nodes, pos, edge_positions, new_edge, edge_1, edge_2, remaining_indices;
    
    labels := ShallowCopy( pair[1] );
    edges := ShallowCopy( pair[2] );
    
    outer_nodes := [ ];
    
    while true do
        
        pos := PositionProperty( [ 1 .. Length( labels ) ], i -> labels[i] = "neutral" and (not i in outer_nodes));
        
        if pos = fail then
            
            break;
            
        fi;
        
        # find the edges connecting to the current node
        edge_positions := PositionsProperty( edges, e -> (pos - 1) in e );
        
        new_edge := fail;
        
        if Length( edge_positions ) = 0 then
            
            # isolated neutral node
            # this can happen when composing EvaluationForDual with CoevaluationForDual
            # simply remove below
            
        elif Length( edge_positions ) = 1 then
            
            Add( outer_nodes, pos );
            continue;
            
        elif Length( edge_positions ) = 2 then
            
            edge_1 := edges[edge_positions[1]];
            edge_2 := edges[edge_positions[2]];
            
            new_edge := [ ];
            
            if edge_1[1] = pos - 1 then
                
                Assert( 0, edge_1[2] <> pos - 1 );
                
                Add( new_edge, edge_1[2] );
                
            elif edge_1[2] = pos - 1 then
                
                Assert( 0, edge_1[1] <> pos - 1 );
                
                Add( new_edge, edge_1[1] );
                
            else
                
                # COVERAGE_IGNORE_NEXT_LINE
                Error( "this should never happen" );
                
            fi;
            
            if edge_2[1] = pos - 1 then
                
                Assert( 0, edge_2[2] <> pos - 1 );
                
                Add( new_edge, edge_2[2] );
                
            elif edge_2[2] = pos - 1 then
                
                Assert( 0, edge_2[1] <> pos - 1 );
                
                Add( new_edge, edge_2[1] );
                
            else
                
                # COVERAGE_IGNORE_NEXT_LINE
                Error( "this should never happen" );
                
            fi;
            
        else
            
            # COVERAGE_IGNORE_NEXT_LINE
            Error( "this should never happen" );
            
        fi;
        
        Remove( labels, pos );
        
        # we cannot use Remove for the two edges because the position of the second edge might change after the first is removed
        remaining_indices := Difference( [ 1 .. Length( edges ) ], edge_positions );
        edges := edges{remaining_indices};
        
        if new_edge <> fail then
            
            Add( edges, new_edge );
            
        fi;
        
        # adjust edges from/to nodes after the removed node
        edges := List( edges, function ( e )
            
            e := ShallowCopy( e );
            
            Assert( 0, e[1] <> pos - 1 );
            
            if e[1] > pos - 1 then
                
                e[1] := e[1] - 1;
                
            fi;
            
            Assert( 0, e[2] <> pos - 1 );
            
            if e[2] > pos - 1 then
                
                e[2] := e[2] - 1;
                
            fi;
            
            return e;
            
        end );
        
    od;
    
    return Pair( labels, edges );
    
end );

##
InstallMethod( DisplayString,
        "for an object in a category of ZX-diagrams without inputs and outputs",
        [ IsObjectInCategoryOfZXDiagramsWithoutIO ],
        
  function ( obj )
    local pair, labels, edges;
    
    pair := ZX_without_IO_RemovedInnerNeutralNodes( ObjectDatum( obj ) );
    
    labels := pair[1];
    edges := pair[2];
    
    return Concatenation(
        "A morphism in ", Name( CapCategory( obj ) ), " given by a ZX-diagram with ", String( Length( labels ) ), " vertex labels\n",
        "  ", PrintString( labels ), ",\n",
        "  and ", String( Length( edges ) ), " edges\n",
        "  ", PrintString( edges ), ".\n"
    );
    
end );
