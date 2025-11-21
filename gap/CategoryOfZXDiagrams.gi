# SPDX-License-Identifier: GPL-2.0-or-later
# ZXCalculusForCAP: The category of ZX-diagrams
#
# Implementations
#

# 0: IO + inner neutral nodes
# 1: green spider = Z
# 2: red spider = X
# 3: Hadamard = H

#BindGlobal( "S_ZX_NODES", [ "neutral", "Z", "X", "H" ] );
#BindGlobal( "S_ZX_EDGES", [ [ 0, 1 ], [ 0, 2 ], [ 0, 3 ] ] );

## original: BindGlobal( "S_ZX_NODES", [ "neutral", "H", "Z", "X", "Zπ", "Xπ" ] );
BindGlobal( "S_ZX_NODES", [ "neutral", "H", "Z", "X", "Zπ", "Xπ" ] );
BindGlobal( "S_ZX_EDGES", List( [ 1 .. Length( S_ZX_NODES ) - 1 ], i -> [ 0, i ] ) );

BindGlobal( "ZX_LabelToInteger", function ( label )
  local pos;
    
    pos := Position( S_ZX_NODES, label );
    
    if pos = fail then
        
        Add( S_ZX_NODES, label );
        
        Add( S_ZX_EDGES, [ 0, Length( S_ZX_NODES ) - 1 ] );
        
        return Length( S_ZX_NODES ) - 1;
        
    else
        
        return pos - 1;
        
    fi;
    
end );

CapJitAddTypeSignature( "ZX_LabelToInteger", [ IsStringRep ], IsBigInt );

BindGlobal( "ZX_IntegerToLabel", function ( int )
    
    return S_ZX_NODES[int + 1];
    
end );

CapJitAddTypeSignature( "ZX_IntegerToLabel", [ IsBigInt ], IsStringRep );

BindGlobal( "ZX_RemovedInnerNeutralNodes", function ( tuple )
  local labels, input_positions, output_positions, edges, pos, edge_positions, new_edge, edge_1, edge_2, remaining_indices;
    
    labels := ShallowCopy( tuple[1] );
    input_positions := ShallowCopy( tuple[2] );
    output_positions := ShallowCopy( tuple[3] );
    edges := ShallowCopy( tuple[4] );
    
    while true do
        
        pos := PositionProperty( [ 1 .. Length( labels ) ], i -> labels[i] = "neutral" and (not i - 1 in input_positions) and (not i - 1 in output_positions) );
        
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
        
        # adjust input positions after the removed node
        input_positions := List( input_positions, function ( i )
            
            Assert( 0, i <> pos - 1 );
            
            if i > pos - 1 then
                
                return i - 1;
                
            else
                
                return i;
                
            fi;
            
        end );
        
        # adjust output positions after the removed node
        output_positions := List( output_positions, function ( i )
            
            Assert( 0, i <> pos - 1 );
            
            if i > pos - 1 then
                
                return i - 1;
                
            else
                
                return i;
                
            fi;
            
        end );
        
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
    
    return NTuple( 4, labels, input_positions, output_positions, edges );
    
end );

InstallGlobalFunction( CategoryOfZXDiagrams, FunctionWithNamedArguments(
  [
    [ "no_precompiled_code", false ],
  ],
  function ( CAP_NAMED_ARGUMENTS )
    local ZX;
    
    if CAP_NAMED_ARGUMENTS.no_precompiled_code then
        
        if IsPackageMarkedForLoading( "FunctorCategories", ">= 2023.07-01" ) then
            
            ZX := CategoryOfZXDiagrams_as_CategoryOfCospans_CategoryOfDecoratedQuivers( : FinalizeCategory := false );
            
        else
            
            # COVERAGE_IGNORE_NEXT_LINE
            Error( "To get a version of `CategoryOfZXDiagrams` without precompiled code, the package `FunctorCategories` is required." );
            
        fi;
        
    else
        
        ZX := CreateCapCategoryWithDataTypes(
                      "CategoryOfZXDiagrams( )",
                      IsCategoryOfZXDiagrams,
                      IsObjectInCategoryOfZXDiagrams,
                      IsMorphismInCategoryOfZXDiagrams,
                      IsCapCategoryTwoCell,
                      IsBigInt,
                      CapJitDataTypeOfNTupleOf( 4,
                              CapJitDataTypeOfListOf( IsStringRep ),
                              CapJitDataTypeOfListOf( IsBigInt ),
                              CapJitDataTypeOfListOf( IsBigInt ),
                              CapJitDataTypeOfListOf( CapJitDataTypeOfNTupleOf( 2, IsBigInt, IsBigInt ) ) ),
                      fail
            : is_computable := false
        );
        
        SetIsRigidSymmetricClosedMonoidalCategory( ZX, true );
        
    fi;
    
    if not CAP_NAMED_ARGUMENTS.no_precompiled_code then
        
        ADD_FUNCTIONS_FOR_CategoryOfZXDiagrams_precompiled( ZX );
        
    fi;
    
    Finalize( ZX );
    
    return ZX;
    
end ) );

##
InstallMethod( Qubits,
        "for a category of ZX-diagram and an integer",
        [ IsCategoryOfZXDiagrams, IsInt ],
        
  function ( ZX, m )
    
    return ObjectConstructor( ZX, BigInt( m ) );
    
end );


##
InstallOtherMethod( Qubits,
        "for an integer",
        [ IsInt ],
        
  function ( m )
    
    return Qubits( ZX, m );
    
end );


##
BindGlobal( "CreateZXMorphism",
  function ( ZX, Z_or_X_or_H, phi, nr_inputs, nr_outputs )
    
    return MorphismConstructor( ZX,
                   Qubits( ZX, nr_inputs ),
                   NTuple( 4,
                           Concatenation(
                                   ListWithIdenticalEntries( nr_inputs, "neutral" ),
                                   [ Concatenation( Z_or_X_or_H, phi ) ],
                                   ListWithIdenticalEntries( nr_outputs, "neutral" ) ),
                           List( [ 0 .. nr_inputs - 1 ], i -> BigInt( i ) ),
                           List( [ nr_inputs + 1 .. nr_inputs + nr_outputs ], o -> BigInt( o ) ),
                           Concatenation(
                                   List( [ 0 .. nr_inputs - 1 ], i -> Pair( BigInt( i ), BigInt( nr_inputs ) ) ),
                                   List( [ nr_inputs + 1 .. nr_inputs + nr_outputs ], o -> Pair( BigInt( o ), BigInt( nr_inputs ) ) ) ) ),
                   Qubits( ZX, nr_outputs ) );
    
end );
  

##
InstallMethod( Z_Spider,
        "for a category of ZX-diagram, a string, and two integers",
        [ IsCategoryOfZXDiagrams, IsStringRep, IsInt, IsInt ],
        
  function ( ZX, phi, nr_inputs, nr_outputs )
    
    return CreateZXMorphism( ZX, "Z", phi, nr_inputs, nr_outputs );
    
end );

##
InstallOtherMethod( Z_Spider,
        "for a category of ZX-diagram and two integers",
        [ IsCategoryOfZXDiagrams, IsInt, IsInt ],
        
  function ( ZX, nr_inputs, nr_outputs )
    
    return Z_Spider( ZX, "", nr_inputs, nr_outputs );
    
end );

##
InstallOtherMethod( Z_Spider,
        "for a string and two integers",
        [ IsStringRep, IsInt, IsInt ],
        
  function ( phi, nr_inputs, nr_outputs )
    
    return Z_Spider( ZX, phi, nr_inputs, nr_outputs );
    
end );

##
InstallOtherMethod( Z_Spider,
        "for two integers",
        [ IsInt, IsInt ],
        
  function ( nr_inputs, nr_outputs )
    
    return Z_Spider( ZX, nr_inputs, nr_outputs );
    
end );

##
InstallMethod( X_Spider,
        "for a category of ZX-diagram, a string, and two integers",
        [ IsCategoryOfZXDiagrams, IsStringRep, IsInt, IsInt ],
        
  function ( ZX, phi, nr_inputs, nr_outputs )
    
    return CreateZXMorphism( ZX, "X", phi, nr_inputs, nr_outputs );
    
end );

##
InstallOtherMethod( X_Spider,
        "for a category of ZX-diagram and two integers",
        [ IsCategoryOfZXDiagrams, IsInt, IsInt ],
        
  function ( ZX, nr_inputs, nr_outputs )
    
    return X_Spider( ZX, "", nr_inputs, nr_outputs );
    
end );

##
InstallOtherMethod( X_Spider,
        "for a string and two integers",
        [ IsStringRep, IsInt, IsInt ],
        
  function ( phi, nr_inputs, nr_outputs )
    
    return X_Spider( ZX, phi, nr_inputs, nr_outputs );
    
end );

##
InstallOtherMethod( X_Spider,
        "for two integers",
        [ IsInt, IsInt ],
        
  function ( nr_inputs, nr_outputs )
    
    return X_Spider( ZX, nr_inputs, nr_outputs );
    
end );

##
InstallMethod( H_Gate,
        "for a category of ZX-diagram",
        [ IsCategoryOfZXDiagrams ],
        
  function ( ZX )
    
    return CreateZXMorphism( ZX, "H", "", 1, 1 );
    
end );

##
InstallOtherMethod( H_Gate,
        "",
        [ ],
        
  function ( )
    
    return H_Gate( ZX );
    
end );

##
InstallOtherMethod( \*,
        "for a morphism and an object in a category of ZX-diagrams",
        [ IsMorphismInCategoryOfZXDiagrams, IsMorphismInCategoryOfZXDiagrams ],
        
  function ( qubits, zx_diagram )
    
    return PreCompose( qubits, zx_diagram );
    
end );

##
InstallOtherMethod( \+,
        "for two objects in a category of ZX-diagrams",
        [ IsObjectInCategoryOfZXDiagrams, IsObjectInCategoryOfZXDiagrams ],
        
  function ( qubits1, qubits2 )
    
    return TensorProduct( qubits1, qubits2 );
    
end );

##
InstallOtherMethod( \+,
        "for two morphisms in a category of ZX-diagrams",
        [ IsMorphismInCategoryOfZXDiagrams, IsMorphismInCategoryOfZXDiagrams ],
        
  function ( zx_diagram1, zx_diagram2 )
    
    return TensorProduct( zx_diagram1, zx_diagram2 );
    
end );

##
InstallOtherMethod( \+,
        "for an object and a morphism in a category of ZX-diagrams",
        [ IsObjectInCategoryOfZXDiagrams, IsMorphismInCategoryOfZXDiagrams ],
        
  function ( qubits, zx_diagram )
    
    return TensorProduct( qubits, zx_diagram );
    
end );

##
InstallOtherMethod( \+,
        "for a morphism and an object in a category of ZX-diagrams",
        [ IsMorphismInCategoryOfZXDiagrams, IsObjectInCategoryOfZXDiagrams ],
        
  function ( qubits, zx_diagram )
    
    return TensorProduct( qubits, zx_diagram );
    
end );

##
InstallOtherMethod( \*,
        "for an integer and a morphism in a category of ZX-diagrams",
        [ IsInt, IsMorphismInCategoryOfZXDiagrams ],
        
  function ( m, zx_diagram )
    
    return Iterated( ListWithIdenticalEntries( m, zx_diagram ), {a,b} -> a + b, IdentityMorphism( TensorUnit( CapCategory( zx_diagram ) ) ) );
    
end );

##
InstallOtherMethod( \*,
        "for an integer and an object in a category of ZX-diagrams",
        [ IsInt, IsObjectInCategoryOfZXDiagrams ],
        
  function ( m, qubits )
    
    return Iterated( ListWithIdenticalEntries( m, qubits ), {a,b} -> a + b, TensorUnit( CapCategory( qubits ) ) );
    
end );

####################################
#
# View and Display methods
#
####################################

##
InstallMethod( ZX_DPO_Rewriting,
        "for a category of ZX-diagrams, a morphism therein and three morphisms of decorated quivers",
        [ IsCategoryOfZXDiagrams, IsMorphismInCategoryOfZXDiagrams,
          IsMorphismInCategoryOfDecoratedQuivers, IsMorphismInCategoryOfDecoratedQuivers, IsMorphismInCategoryOfDecoratedQuivers ],
        
  function ( ZX, zx_diagram, m, l, r )
    local Cospans, input_output_morphism, I, O, input_output, input, output, dpo, p1, y, new_input, new_output;
    
    Assert( 0, IsIdenticalObj( ZX, CapCategory( zx_diagram ) ) );
    
    Assert( 0, HasModelingCategory( ZX ) );
    
    Cospans := ModelingCategory( ZX );
    
    input_output_morphism := ModelingMorphism( ZX, zx_diagram );
    
    I := Source( input_output_morphism );
    O := Target( input_output_morphism );
    
    input_output := MorphismDatum( Cospans, input_output_morphism );
    
    input := input_output[1];
    output := input_output[2];
    
    dpo := DPO( m, l, r );
    
    p1 := dpo[1];
    y := dpo[3];
    
    new_input := PreCompose( Lift( input, y ), p1 );
    new_output := PreCompose( Lift( output, y ), p1 );
    
    return ReinterpretationOfMorphism( ZX,
                   Source( zx_diagram ),
                   MorphismConstructor( Cospans,
                           I,
                           Pair( new_input, new_output ),
                           O ),
                   Target( zx_diagram ) );
    

end );

##
InstallMethod( ZXFusionRule,
        "for a morphism in a category of ZX-diagrams and an integer",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ],
        
  function ( zx_diagram, i )
    local ZX, Cospans, DecoratedQuivers, lgraph, labels, inputs, outputs, arrows, pos, pair, type,
          zx_quiver, m, l, K, k, v, e, R, r;
    
    ZX := CapCategory( zx_diagram );
    
    Assert( 0, HasModelingCategory( ZX ) );
    
    Cospans := ModelingCategory( ZX );
    
    DecoratedQuivers := UnderlyingCategory( Cospans );
    
    lgraph := VertexLabeledGraph( zx_diagram );
    
    labels := lgraph[1];
    
    Assert( 0, labels[1 + i] = "neutral" );
    
    inputs := lgraph[2];
    outputs := lgraph[3];
    
    Assert( 0, not i in inputs );
    Assert( 0, not i in outputs );
    
    ## at this point we know that i stands for an inner node and now we need to check its vertices
    
    arrows := lgraph[4];
    
    pos := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][1] = i );
    
    Assert( 0, Length( pos ) = 2 );
    
    pair := List( arrows{1 + pos}, pair -> pair[2] );
    
    ## type in [ "X", "Z" ]
    type := Set( List( pair, v -> labels[1 + v][1] ) );
    
    ## the edge is either between two X-nodes or two Z-nodes
    Assert( 0, type in [ "X", "Z" ] );
    
    zx_quiver := Target( MorphismDatum( Cospans, ModelingMorphism( ZX, zx_diagram ) )[1] );
    
    m := Subobject( zx_quiver, -1 + PositionsProperty( arrows, arrow -> arrow[2] in pair ) );
    
    l := LiftAlongMonomorphism( m, Subobject( zx_quiver, Difference( AsList( m.V ), [ i, pair[1], pair[2] ] ), [ ] ) );
    
    K := Source( l );
    
    k := Length( K.V );
    
    v := ZX_LabelToInteger( type );
    e := v - 1;
    
    R := CreateDecoratedQuiver( DecoratedQuivers,
                 Pair( Triple(
                         k + 1,
                         k,
                         List( [ 0 .. k - 1 ], j -> Pair( j, k ) ) ),
                       Pair( Concatenation( ListWithIdenticalEntries( k, 0 ), [ v ] ),
                             ListWithIdenticalEntries( k, e ) ) ) );
    
    r := Subobject( R, [ 0 .. k - 1 ], [ ] );
    
    Assert( 0, Source( r ) = K );
    
    return ZX_DPO_Rewriting( ZX, zx_diagram, m, l, r );
    
end );

##
InstallMethod( ZXInverseFusionRule,
        " for a morphism in a category of ZX-diagrams and an integer ",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ],
        
  function ( zx_diagram, i )
    local ZX, Cospans, DecoratedQuivers, lgraph, labels, arrows, pos_arrows_to_i, neutral_nodes_adjacent_i, type, v, e, zx_quiver, k, m, l, K, R, r;
    
    ZX := CapCategory( zx_diagram );
    
    Assert( 0, HasModelingCategory( ZX ) );
    
    Cospans := ModelingCategory( ZX );
    
    DecoratedQuivers := UnderlyingCategory( Cospans );
    
    lgraph := VertexLabeledGraph( zx_diagram );
    
    labels := lgraph[1];
    
    type := labels[1 + i];
    
    Assert( 0, type in [ "Z", "X" ] );
     
    arrows := lgraph[4];
     
    pos_arrows_to_i := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = i );
    
    neutral_nodes_adjacent_i := List( arrows{1 + pos_arrows_to_i}, arrow -> arrow[1] );
    
    ## the gate has valency k :
    k := Length( neutral_nodes_adjacent_i );
    
    zx_quiver := Target( MorphismDatum( Cospans, ModelingMorphism( ZX, zx_diagram ) )[1] );
    
    m := Subobject( zx_quiver, pos_arrows_to_i );
    
    l := LiftAlongMonomorphism( m, Subobject( zx_quiver, neutral_nodes_adjacent_i, [ ] ) );
    
    K := Source( l );
    
    v := ZX_LabelToInteger( type );
    e := v - 1;
    
    R := CreateDecoratedQuiver( DecoratedQuivers,
                 Pair( Triple(
                         k + 3,
                         k + 2,
                         Concatenation( List( [ 0 .. k - 1 ], j -> Pair( j, k ) ), [ Pair( k + 1, k ), Pair( k + 1, k + 2 ) ] ) ),
                       Pair( Concatenation( ListWithIdenticalEntries( k, 0 ), [ v, 0, v ] ),
                             Concatenation( ListWithIdenticalEntries( k, e ), [ e, e ] ) ) ) );
    
    r := Subobject( R, [ 0 .. k - 1 ], [ ] );
    
    Assert( 0, Source( r ) = K );
    
    return ZX_DPO_Rewriting( ZX, zx_diagram, m, l, r );
    
end );

##
InstallMethod( ZXIdentityRule,
        "for a morphism in a category of ZX-diagrams and an integer",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ],
        
  function ( zx_diagram, i )
    local ZX, Cospans, DecoratedQuivers, lgraph, labels, arrows, pos, pair,
          zx_quiver, m, l, K, R, r;
    
    ZX := CapCategory( zx_diagram );
    
    Assert( 0, HasModelingCategory( ZX ) );
    
    Cospans := ModelingCategory( ZX );
    
    DecoratedQuivers := UnderlyingCategory( Cospans );
    
    lgraph := VertexLabeledGraph( zx_diagram );
    
    labels := lgraph[1];
    
    Assert( 0, labels[1 + i] in [ "Z", "X" ] );
    
    ## at this point we know that i stands for a Z or X gate with phase 0:
    
    arrows := lgraph[4];
    
    pos := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = i );
    
    Assert( 0, Length( pos ) = 2 );
    
    pair := List( arrows{1 + pos}, pair -> pair[1] );
    
    zx_quiver := Target( MorphismDatum( Cospans, ModelingMorphism( ZX, zx_diagram ) )[1] );
    
    m := Subobject( zx_quiver, pos );
    
    l := LiftAlongMonomorphism( m, Subobject( zx_quiver, pair, [ ] ) );
    
    K := Source( l );
    
    R := Source( Subobject( K, [ 0 ], [ ] ) );
    
    r := CreateDecoratedQuiverMorphism( DecoratedQuivers, K, Pair( [ 0, 0 ], [ ] ), R );
    
    Assert( 0, Source( r ) = K );
    
    return ZX_DPO_Rewriting( ZX, zx_diagram, m, l, r );
    
end );

##
InstallMethod( ZXInverseIdentityRule,
        "for a morphism in a category of ZX-diagrams and an integer",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt, IsString ],
        
  function ( zx_diagram, i, type )
    local ZX, Cospans, DecoratedQuivers, lgraph, labels, v, e, zx_quiver,
          arrows, pos, adjacent, m, l, K, R, r;
    
    Assert( 0, type in [ "Z", "X" ] );
    
    ZX := CapCategory( zx_diagram );
    
    Assert( 0, HasModelingCategory( ZX ) );
    
    Cospans := ModelingCategory( ZX );
    
    DecoratedQuivers := UnderlyingCategory( Cospans );
    
    lgraph := VertexLabeledGraph( zx_diagram );
    
    labels := lgraph[1];
    
    Assert( 0, labels[1 + i] = "neutral" );
    
    zx_quiver := Target( MorphismDatum( Cospans, ModelingMorphism( ZX, zx_diagram ) )[1] );
    
    arrows := lgraph[4];
    
    pos := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][1] = i );
    
    Assert( 0, Length( pos ) in [ 1, 2 ] );
    
    adjacent := List( arrows{1 + pos}, pair -> pair[2] );
    
    m := Subobject( zx_quiver, pos );
    
    l := LiftAlongMonomorphism( m, Subobject( zx_quiver, adjacent, [ ] ) );
    
    K := Source( l );
    
    v := ZX_LabelToInteger( type );
    e := v - 1;
    
    R := CreateDecoratedQuiver( DecoratedQuivers,
                 Pair( Triple(
                         3 + Length( pos ),
                         2 + Length( pos ),
                         Concatenation( [ Pair( 0, 2 ), Pair( 1, 2 ) ], List( [ 0 .. Length( pos ) - 1 ], k -> Pair( k, k + 3 ) ) ) ),
                       Pair( Concatenation( [ 0, 0, v ], List( labels{1 + adjacent}, ZX_LabelToInteger ) ),
                             Concatenation( [ e, e ], List( [ 0 .. Length( pos ) - 1 ], k -> -1 + ZX_LabelToInteger( labels[1 + adjacent[1 + k]] ) ) ) ) ) );
    
    r := Subobject( R, 3 + [ 0 .. Length( pos ) - 1 ], [ ] );
    
    Assert( 0, Source( r ) = K );
    
    return ZX_DPO_Rewriting( ZX, zx_diagram, m, l, r );
    
end );

##
InstallMethod( ZXColorChangeRule,
        "for a morphism in a category of ZX-diagrams and an integer",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ],
        
  function ( zx_diagram, i )
    local ZX, Cospans, DecoratedQuivers, lgraph, type, labels, arrows, pos_arrows_adjacent_to_i, inner_neutral_nodes_adjacent_to_i, k, inputs, outputs,
          pos_inner_arrows_adjacent_to_H_gates, adjacent_H_gates, pos_outer_arrows_adjacent_to_H_gates, outer_neutral_nodes_adjacent_to_H_gates,
          zx_quiver, m, l, K, v, e, R, r;
    
    ZX := CapCategory( zx_diagram );
    
    Assert( 0, HasModelingCategory( ZX ) );
    
    Cospans := ModelingCategory( ZX );
    
    DecoratedQuivers := UnderlyingCategory( Cospans );
    
    lgraph := VertexLabeledGraph( zx_diagram );
    
    labels := lgraph[1];
    
    type := labels[1 + i]{[ 1 ]};
    
    Assert( 0, type in [ "Z", "X" ] );
    
    ## at this point we know that i stands for an Z or X gate:
    
    arrows := lgraph[4];
    
    pos_arrows_adjacent_to_i := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = i );
    
    inner_neutral_nodes_adjacent_to_i := List( arrows{1 + pos_arrows_adjacent_to_i}, pair -> pair[1] );
    
    k := Length( inner_neutral_nodes_adjacent_to_i );
    
    inputs := lgraph[2];
    outputs := lgraph[3];
    
    Assert( 0, IsEmpty( Intersection( inner_neutral_nodes_adjacent_to_i, inputs ) ) );
    Assert( 0, IsEmpty( Intersection( inner_neutral_nodes_adjacent_to_i, outputs ) ) );
    
    pos_inner_arrows_adjacent_to_H_gates := Filtered( [ 0 .. Length( arrows ) - 1 ], a ->
                                                    arrows[1 + a][1] in inner_neutral_nodes_adjacent_to_i and not arrows[1 + a][2] = i );
    
    Assert( 0, Length( pos_inner_arrows_adjacent_to_H_gates ) = k );
    
    adjacent_H_gates := List( arrows{1 + pos_inner_arrows_adjacent_to_H_gates}, pair -> pair[2] );
    
    Assert( 0, ForAll( adjacent_H_gates, v -> labels[1 + v] = "H" ) );
    
    pos_outer_arrows_adjacent_to_H_gates := Filtered( [ 0 .. Length( arrows ) - 1 ], a ->
                                                    arrows[1 + a][2] in adjacent_H_gates and not arrows[1 + a][1] in inner_neutral_nodes_adjacent_to_i );
    
    Assert( 0, Length( pos_outer_arrows_adjacent_to_H_gates ) = k );
    
    outer_neutral_nodes_adjacent_to_H_gates := List( arrows{1 + pos_outer_arrows_adjacent_to_H_gates }, pair -> pair[1] );
    
    zx_quiver := Target( MorphismDatum( Cospans, ModelingMorphism( ZX, zx_diagram ) )[1] );
    
    m := Subobject( zx_quiver, Concatenation( pos_arrows_adjacent_to_i, pos_inner_arrows_adjacent_to_H_gates, pos_outer_arrows_adjacent_to_H_gates ) );
    
    l := LiftAlongMonomorphism( m, Subobject( zx_quiver, outer_neutral_nodes_adjacent_to_H_gates, [ ] ) );
    
    K := Source( l );
    
    if type = "Z" then
        v := ZX_LabelToInteger( type ) + 1;
    else
        v := ZX_LabelToInteger( type ) - 1;
    fi;
    
    e := v - 1;
    
    R := CreateDecoratedQuiver( DecoratedQuivers,
                 Pair( Triple(
                         k + 1,
                         k,
                         List( [ 0 .. k - 1 ], j -> Pair( j, k ) ) ),
                       Pair( Concatenation( ListWithIdenticalEntries( k, 0 ), [ v ] ),
                             ListWithIdenticalEntries( k, e ) ) ) );
    
    r := Subobject( R, [ 0 .. k - 1 ], [ ] );
    
    Assert( 0, Source( r ) = K );
    
    return ZX_DPO_Rewriting( ZX, zx_diagram, m, l, r );
    
end );

##
InstallMethod( ZXInverseColorChangeRule,
        "for a morphism in a category of ZX-diagrams and an integer",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ],
        
  function ( zx_diagram, i )
    local ZX, Cospans, DecoratedQuivers, lgraph, type, labels, arrows, pos_arrows_adjacent_to_i, inputs, outputs,
          inner_neutral_nodes_adjacent_to_i, k, zx_quiver, m, l, K, v, e, hv, he, R, r, dpo, p1, y;
    
    ZX := CapCategory( zx_diagram );
    
    Assert( 0, HasModelingCategory( ZX ) );
    
    Cospans := ModelingCategory( ZX );
    
    DecoratedQuivers := UnderlyingCategory( Cospans );
    
    lgraph := VertexLabeledGraph( zx_diagram );
    
    labels := lgraph[1];
    
    type := labels[1 + i];
    
    Assert( 0, type in [ "Z", "X" ] );
    
    ## at this point we know that i stands for an Z or X gate:
    
    arrows := lgraph[4];
    
    pos_arrows_adjacent_to_i := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = i );
    
    inner_neutral_nodes_adjacent_to_i := List( arrows{1 + pos_arrows_adjacent_to_i}, pair -> pair[1] );
    
    k := Length( inner_neutral_nodes_adjacent_to_i );

    inputs := lgraph[2];
    outputs := lgraph[3];
    
    Assert( 0, IsEmpty( Intersection( inner_neutral_nodes_adjacent_to_i, inputs ) ) );
    Assert( 0, IsEmpty( Intersection( inner_neutral_nodes_adjacent_to_i, outputs ) ) );
    
    zx_quiver := Target( MorphismDatum( Cospans, ModelingMorphism( ZX, zx_diagram ) )[1] );
    
    m := Subobject( zx_quiver, pos_arrows_adjacent_to_i );
    
    l := LiftAlongMonomorphism( m, Subobject( zx_quiver, inner_neutral_nodes_adjacent_to_i, [ ] ) );
    
    K := Source( l );
    
    if type = "Z" then
        v := ZX_LabelToInteger( type ) + 1;
    else
        v := ZX_LabelToInteger( type ) - 1;
    fi;
    
    e := v - 1;
    
    hv := 1;
    he := 0;
    
    R := CreateDecoratedQuiver( DecoratedQuivers,
                 Pair( Triple(
                         3 * k + 1,
                         3 * k,
                         Concatenation( List( [ 0 .. k - 1 ], j -> Pair( j, k ) ) ,
                                 List( [ 0 .. k - 1 ], j -> Pair( j, k + j + 1 ) ) ,
                                 List( [ 0 .. k - 1 ], j -> Pair( 2 * k + j + 1,  k + j + 1 ) ) ) ),
                       Pair( Concatenation( ListWithIdenticalEntries( k, 0 ), [ v ],
                               ListWithIdenticalEntries( k, hv ),
                               ListWithIdenticalEntries( k, 0 ) ),
                             Concatenation( ListWithIdenticalEntries( k, e ),
                                     ListWithIdenticalEntries( k, he ),
                                     ListWithIdenticalEntries( k, he ) ) ) ) );
    
    r := Subobject( R, [ 2 * k + 1 .. 3 * k ], [ ] );
    
    Assert( 0, Source( r ) = K );
    
    return ZX_DPO_Rewriting( ZX, zx_diagram, m, l, r );
    
end );

##
InstallMethod( ZXBialgebraRule,
        "for a morphism in a category of ZX-diagrams and four integers",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt, IsInt, IsInt, IsInt ],
        
  function ( zx_diagram, z1, z2, x1, x2 )
    local ZX, Cospans, DecoratedQuivers, lgraph, labels, arrows,
          pos_arrows_to_z1, pos_arrows_to_z2, pos_arrows_to_x1, pos_arrows_to_x2,
          neutral_nodes_adjacent_z1, neutral_nodes_adjacent_z2, neutral_nodes_adjacent_x1, neutral_nodes_adjacent_x2,
          z1_x1, z1_x2, z2_x1, z2_x2, z1_out, z2_out, x1_out, x2_out,
          zx_quiver, outer_neutral_nodes, m, l, K, R, r;
    
    ZX := CapCategory( zx_diagram );
    
    Assert( 0, HasModelingCategory( ZX ) );
    
    Cospans := ModelingCategory( ZX );
    
    DecoratedQuivers := UnderlyingCategory( Cospans );
    
    lgraph := VertexLabeledGraph( zx_diagram );
    
    labels := lgraph[1];
    
    Assert( 0, not z1 = z2 );
    Assert( 0, not x1 = x2 );
    
    Assert( 0, labels[1 + z1] = "Z" );
    Assert( 0, labels[1 + z2] = "Z" );
    Assert( 0, labels[1 + x1] = "X" );
    Assert( 0, labels[1 + x2] = "X" );
    
    ## at this point we know the colors of the four gates:
    
    arrows := lgraph[4];
    
    pos_arrows_to_z1 := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = z1 );
    pos_arrows_to_z2 := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = z2 );
    pos_arrows_to_x1 := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = x1 );
    pos_arrows_to_x2 := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = x2 );
    
    neutral_nodes_adjacent_z1 := List( arrows{1 + pos_arrows_to_z1}, arrow -> arrow[1] );
    neutral_nodes_adjacent_z2 := List( arrows{1 + pos_arrows_to_z2}, arrow -> arrow[1] );
    neutral_nodes_adjacent_x1 := List( arrows{1 + pos_arrows_to_x1}, arrow -> arrow[1] );
    neutral_nodes_adjacent_x2 := List( arrows{1 + pos_arrows_to_x2}, arrow -> arrow[1] );
    
    z1_x1 := Intersection( neutral_nodes_adjacent_z1, neutral_nodes_adjacent_x1 );
    z1_x2 := Intersection( neutral_nodes_adjacent_z1, neutral_nodes_adjacent_x2 );
    z2_x1 := Intersection( neutral_nodes_adjacent_z2, neutral_nodes_adjacent_x1 );
    z2_x2 := Intersection( neutral_nodes_adjacent_z2, neutral_nodes_adjacent_x2 );
    
    ## z1 is connected to x1 by a unique edge:
    Assert( 0, Length( z1_x1 ) = 1 );
    
    ## z1 is connected to x2 by a unique edge:
    Assert( 0, Length( z1_x2 ) = 1 );
    
    ## z2 is connected to x1 by a unique edge:
    Assert( 0, Length( z2_x1 ) = 1 );
    
    ## z2 is connected to x2 by a unique edge:
    Assert( 0, Length( z2_x2 ) = 1 );
    
    ## z1 is not connected to z2 by a unique edge:
    Assert( 0, Length( Intersection( neutral_nodes_adjacent_z1, neutral_nodes_adjacent_z2 ) ) = 0 );
    
    ## x1 is not connected to x2 by a unique edge:
    Assert( 0, Length( Intersection( neutral_nodes_adjacent_x1, neutral_nodes_adjacent_x2 ) ) = 0 );
    
    ## all four gates have valency 3:
    Assert( 0, Length( neutral_nodes_adjacent_z1 ) = 3 );
    Assert( 0, Length( neutral_nodes_adjacent_z2 ) = 3 );
    Assert( 0, Length( neutral_nodes_adjacent_x1 ) = 3 );
    Assert( 0, Length( neutral_nodes_adjacent_x2 ) = 3 );
    
    z1_out := Difference( neutral_nodes_adjacent_z1, Concatenation( z1_x1, z1_x2 ) );
    z2_out := Difference( neutral_nodes_adjacent_z2, Concatenation( z2_x1, z2_x2 ) );
    x1_out := Difference( neutral_nodes_adjacent_x1, Concatenation( z1_x1, z2_x1 ) );
    x2_out := Difference( neutral_nodes_adjacent_x2, Concatenation( z1_x2, z2_x2 ) );
    
    Assert( 0, Length( z1_out ) = 1 );
    Assert( 0, Length( z2_out ) = 1 );
    Assert( 0, Length( x1_out ) = 1 );
    Assert( 0, Length( x2_out ) = 1 );
    
    zx_quiver := Target( MorphismDatum( Cospans, ModelingMorphism( ZX, zx_diagram ) )[1] );
    
    outer_neutral_nodes := Concatenation( z1_out, z2_out, x1_out, x2_out );
    
    m := Subobject( zx_quiver, Concatenation( pos_arrows_to_z1, pos_arrows_to_z2, pos_arrows_to_x1, pos_arrows_to_x2 ) );
    
    l := LiftAlongMonomorphism( m,
                 ## hotfix
                 CreateDecoratedQuiverMorphism(
                         Source( Subobject( zx_quiver, outer_neutral_nodes, [ ] ) ),
                         outer_neutral_nodes,
                         [ ],
                         Target( m ) ) );
    
    K := Source( l );
    
    R := CreateDecoratedQuiver( DecoratedQuivers,
                 Pair( Triple(
                         7,
                         6,
                         [ Pair( 0, 5 ), Pair( 1, 5 ), Pair( 4, 5 ), Pair( 2, 6 ), Pair( 3, 6 ), Pair( 4, 6 ) ] ),
                       Pair( [ 0, 0, 0, 0, 0, 3, 2 ],
                             [ 2, 2, 2, 1, 1, 1 ] ) ) );
    
    r := Subobject( R, [ 0 .. 3 ], [ ] );
    
    Assert( 0, Source( r ) = K );
    
    return ZX_DPO_Rewriting( ZX, zx_diagram, m, l, r );
    
end );

##
InstallMethod( ZXInverseBialgebraRule,
        "for a morphism in a category of ZX-diagrams and two integers",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt, IsInt ],
        
  function ( zx_diagram, i, j )
    local ZX, Cospans, DecoratedQuivers, lgraph, labels, arrows,
          pos_arrows_to_i, pos_arrows_to_j, neutral_nodes_adjacent_i, neutral_nodes_adjacent_j,
          x_z, z_out, x_out, zx_quiver, outer_neutral_nodes, m, l, K, R, r;
    
    ZX := CapCategory( zx_diagram );
    
    Assert( 0, HasModelingCategory( ZX ) );
    
    Cospans := ModelingCategory( ZX );
    
    DecoratedQuivers := UnderlyingCategory( Cospans );
    
    lgraph := VertexLabeledGraph( zx_diagram );
    
    labels := lgraph[1];
    
    Assert( 0, labels[1 + i] = "X" );
    Assert( 0, labels[1 + j] = "Z" );
    
    arrows := lgraph[4];
    
    pos_arrows_to_i := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = i );
    pos_arrows_to_j := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = j );
    
    neutral_nodes_adjacent_i := List( arrows{1 + pos_arrows_to_i}, arrow -> arrow[1] );
    neutral_nodes_adjacent_j := List( arrows{1 + pos_arrows_to_j}, arrow -> arrow[1] );
    
    x_z := Intersection( neutral_nodes_adjacent_i, neutral_nodes_adjacent_j );
    
    ## x is connected to z by a unique edge:
    Assert( 0, Length( x_z ) = 1 );
    
    ## all two gates have valency 3:
    Assert( 0, Length( neutral_nodes_adjacent_i ) = 3 );
    Assert( 0, Length( neutral_nodes_adjacent_j ) = 3 );
    
    x_out := Difference( neutral_nodes_adjacent_i, x_z );
    z_out := Difference( neutral_nodes_adjacent_j, x_z );
    
    Assert( 0, Length( x_out ) = 2 );
    Assert( 0, Length( z_out ) = 2 );
    
    zx_quiver := Target( MorphismDatum( Cospans, ModelingMorphism( ZX, zx_diagram ) )[1] );
    
    outer_neutral_nodes := Concatenation( x_out, z_out );
    
    m := Subobject( zx_quiver, Concatenation( pos_arrows_to_i, pos_arrows_to_j ) );
    
    l := LiftAlongMonomorphism( m,
                 ## hotfix
                 CreateDecoratedQuiverMorphism(
                         Source( Subobject( zx_quiver, outer_neutral_nodes, [ ] ) ),
                         outer_neutral_nodes,
                         [ ],
                         Target( m ) ) );
    
    K := Source( l );
    
    R := CreateDecoratedQuiver( DecoratedQuivers,
                 Pair( Triple(
                         12,
                         12,
                         [ Pair( 0, 8 ), Pair( 1, 8 ), Pair( 2, 8 ), Pair( 3, 9 ), Pair( 4, 9 ), Pair( 5, 9 ),
                           Pair( 1, 10 ), Pair( 4, 10 ), Pair( 6, 10 ), Pair( 2, 11 ), Pair( 5, 11 ), Pair( 7, 11 ) ] ),
                       Pair( [ 0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 3, 3 ],
                             [ 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2 ] ) ) );
    
    r := Subobject( R, [ 0, 3, 6, 7 ], [ ] );
    
    Assert( 0, Source( r ) = K );
    
    return ZX_DPO_Rewriting( ZX, zx_diagram, m, l, r );
    
end );

##
InstallMethod( ZXCopyRule,
        "for a morphism in a category of ZX-diagrams and two integers",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt, IsInt ],
        
  function ( zx_diagram, i, j )
    local ZX, Cospans, DecoratedQuivers, lgraph, labels, arrows, v, e,
          pos_arrows_to_i, pos_arrows_to_j, neutral_nodes_adjacent_i, neutral_nodes_adjacent_j,
          x_z, z_out, x_out, zx_quiver, outer_neutral_nodes, m, l, K, R, r;
    
    ZX := CapCategory( zx_diagram );
    
    Assert( 0, HasModelingCategory( ZX ) );
    
    Cospans := ModelingCategory( ZX );
    
    DecoratedQuivers := UnderlyingCategory( Cospans );
    
    lgraph := VertexLabeledGraph( zx_diagram );
    
    labels := lgraph[1];
    
    Assert( 0, labels[1 + i] in [ "Z", "X" ] );
    Assert( 0, labels[1 + j] in [ "Z", "X" ] );
    Assert( 0, not labels[1 + i] = labels[1 + j] );
    
    arrows := lgraph[4];
    
    pos_arrows_to_i := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = i );
    pos_arrows_to_j := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = j );
    
    neutral_nodes_adjacent_i := List( arrows{1 + pos_arrows_to_i}, arrow -> arrow[1] );
    neutral_nodes_adjacent_j := List( arrows{1 + pos_arrows_to_j}, arrow -> arrow[1] );
    
    x_z := Intersection( neutral_nodes_adjacent_i, neutral_nodes_adjacent_j );
    
    ## x is connected to z by a unique edge:
    Assert( 0, Length( x_z ) = 1 );
    
    ## the gates have valency 2 and 3:
    Assert( 0, Length( neutral_nodes_adjacent_i ) = 1 );
    Assert( 0, Length( neutral_nodes_adjacent_j ) = 3 );
    
    x_out := Difference( neutral_nodes_adjacent_i, x_z );
    z_out := Difference( neutral_nodes_adjacent_j, x_z );
    
    Assert( 0, Length( x_out ) = 0 );
    Assert( 0, Length( z_out ) = 2 );
    
    zx_quiver := Target( MorphismDatum( Cospans, ModelingMorphism( ZX, zx_diagram ) )[1] );
    
    m := Subobject( zx_quiver, Concatenation( pos_arrows_to_i, pos_arrows_to_j ) );
    
    l := LiftAlongMonomorphism( m, Subobject( zx_quiver, z_out, [ ] ) );
    
    K := Source( l );
    
    v := ZX_LabelToInteger( labels[1 + i] );
    
    e := v - 1;
    
    R := CreateDecoratedQuiver( DecoratedQuivers,
                 Pair( Triple(
                         4,
                         2,
                         [ Pair( 0, 2 ), Pair( 1, 3 ) ] ),
                       Pair( [ 0, 0, v, v ],
                             [ e, e ] ) ) );
    
    r := Subobject( R, [ 0, 1 ], [ ] );
    
    Assert( 0, Source( r ) = K );
    
    return ZX_DPO_Rewriting( ZX, zx_diagram, m, l, r );
    
end );

##
InstallMethod( ZXCongruenceRule,
        "for a morphism in a category of ZX-diagrams and two integers",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt, IsInt ],
        
  function ( zx_diagram, i, j )
    local ZX, Cospans, DecoratedQuivers, lgraph, labels, arrows, v, e,
          pos_arrows_to_i, pos_arrows_to_j, neutral_nodes_adjacent_i, neutral_nodes_adjacent_j,
          x_z, z_out, x_out, zx_quiver, outer_neutral_nodes, m, l, K, R, r;
    
    ZX := CapCategory( zx_diagram );
    
    Assert( 0, HasModelingCategory( ZX ) );
    
    Cospans := ModelingCategory( ZX );
    
    DecoratedQuivers := UnderlyingCategory( Cospans );
    
    lgraph := VertexLabeledGraph( zx_diagram );
    
    labels := lgraph[1];
    
    Assert( 0, labels[1 + i] in [ "Z", "X" ] );
    Assert( 0, labels[1 + j] in [ "Z", "X" ] );
    
    arrows := lgraph[4];
    
    pos_arrows_to_i := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = i );
    pos_arrows_to_j := Filtered( [ 0 .. Length( arrows ) - 1 ], a -> arrows[1 + a][2] = j );
    
    neutral_nodes_adjacent_i := List( arrows{1 + pos_arrows_to_i}, arrow -> arrow[1] );
    neutral_nodes_adjacent_j := List( arrows{1 + pos_arrows_to_j}, arrow -> arrow[1] );
    
    x_z := Intersection( neutral_nodes_adjacent_i, neutral_nodes_adjacent_j );
    
    ## x is connected to z by a unique edge:
    Assert( 0, Length( x_z ) = 1 );
    
    ## the gates have valency 2 and 3:
    Assert( 0, Length( neutral_nodes_adjacent_i ) = 1 );
    Assert( 0, Length( neutral_nodes_adjacent_j ) = 1 );
    
    x_out := Difference( neutral_nodes_adjacent_i, x_z );
    z_out := Difference( neutral_nodes_adjacent_j, x_z );
    
    Assert( 0, Length( x_out ) = 0 );
    Assert( 0, Length( z_out ) = 0 );
    
    zx_quiver := Target( MorphismDatum( Cospans, ModelingMorphism( ZX, zx_diagram ) )[1] );
    
    m := Subobject( zx_quiver, Concatenation( pos_arrows_to_i, pos_arrows_to_j ) );
    
    l := LiftAlongMonomorphism( m, Subobject( zx_quiver, z_out, [ ] ) );
    
    K := Source( l );
    
    R := CreateDecoratedQuiver( DecoratedQuivers,
                 Pair( Triple(
                         0,
                         0,
                         [  ] ),
                       Pair( [  ],
                             [  ] ) ) );
    
    r := Subobject( R, [ ], [ ] );
    
    Assert( 0, Source( r ) = K );
    
    return ZX_DPO_Rewriting( ZX, zx_diagram, m, l, r );
    
end );

####################################
#
# View and Display methods
#
####################################

##
InstallMethod( ViewString,
        "for an object in a category of ZX-diagrams",
        [ IsObjectInCategoryOfZXDiagrams ],
        
  function ( obj )
    
    return Concatenation( "<An object in ", Name( CapCategory( obj ) ), " representing ", String( AsInteger( obj ) ), " input/output vertices>" );
    
end );

##
InstallMethod( DisplayString,
        "for an object in a category of ZX-diagrams",
        [ IsObjectInCategoryOfZXDiagrams ],
        
  function ( obj )
    
    return Concatenation( "An object in ", Name( CapCategory( obj ) ), " representing ", String( AsInteger( obj ) ), " input/output vertices.\n" );
    
end );

##
InstallMethod( DisplayString,
        "for a morphism in a category of ZX-diagrams",
        [ IsMorphismInCategoryOfZXDiagrams ],
        
  function ( phi )
    local tuple, labels, input_positions, output_positions, edges;
    
    tuple := ZX_RemovedInnerNeutralNodes( MorphismDatum( phi ) );
    
    labels := tuple[1];
    input_positions := tuple[2];
    output_positions := tuple[3];
    edges := tuple[4];
    
    return Concatenation(
        "A morphism in ", Name( CapCategory( phi ) ), " given by a ZX-diagram with ", String( Length( labels ) ), " vertex labels\n",
        "  ", PrintString( labels ), ",\n",
        "  inputs\n",
        "  ", PrintString( input_positions ), ",\n",
        "  outputs\n",
        "  ", PrintString( output_positions ), ",\n",
        "  and ", String( Length( edges ) ), " edges\n",
        "  ", PrintString( edges ), ".\n"
    );
    
end );
