# SPDX-License-Identifier: GPL-2.0-or-later
# ZXCalculusForCAP: The category of ZX-diagrams
#
# Implementations
#

##
InstallGlobalFunction( CategoryOfZXDiagrams_as_CategoryOfCospans_CategoryOfDecoratedQuivers, FunctionWithNamedArguments(
  [
    [ "no_precompiled_code", false ],
    [ "FinalizeCategory", true ],
  ],
  function ( CAP_NAMED_ARGUMENTS )
    local name, category_filter, category_object_filter, category_morphism_filter,
          object_datum_type, object_constructor, object_datum,
          morphism_datum_type, morphism_constructor, morphism_datum,
          S_ZX, decorated_quivers, FinalizeCategory, csp,
          modeling_tower_object_constructor, modeling_tower_object_datum,
          modeling_tower_morphism_constructor, modeling_tower_morphism_datum,
          ZX;
    
    ##
    name := "CategoryOfZXDiagrams( )";
    
    ##
    category_filter := IsCategoryOfZXDiagrams;
    category_object_filter := IsObjectInCategoryOfZXDiagrams;
    category_morphism_filter := IsMorphismInCategoryOfZXDiagrams;
    
    ##
    object_datum_type := IsBigInt;
    
    object_constructor :=
      function ( ZX, integer )
        local obj;
        
        if not (IsBigInt( integer ) and integer >= 0) then
            
            # COVERAGE_IGNORE_NEXT_LINE
            Error( "The object datum in the category of ZX-diagrams must be a non-negative integer." );
            
        fi;
        
        obj := CreateCapCategoryObjectWithAttributes( ZX, AsInteger, integer );
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0, IsWellDefinedForObjects( ZX, obj ) );
        
        return obj;
        
    end;
    
    object_datum :=
      function ( ZX, object )
        
        return AsInteger( object );
        
    end;
    
    ##
    morphism_datum_type :=
      CapJitDataTypeOfNTupleOf( 4,
              CapJitDataTypeOfListOf( IsStringRep ),
              CapJitDataTypeOfListOf( IsBigInt ),
              CapJitDataTypeOfListOf( IsBigInt ),
              CapJitDataTypeOfListOf( CapJitDataTypeOfNTupleOf( 2, IsBigInt, IsBigInt ) ) );
    
    morphism_constructor :=
      function ( ZX, source, quadruple, target )
        local mor;
        
        mor := CreateCapCategoryMorphismWithAttributes( ZX, source, target, VertexLabeledGraph, quadruple );
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0, IsWellDefinedForMorphisms( ZX, mor ) );
        
        return mor;
        
    end;
    
    morphism_datum :=
      function ( ZX, morphism )
        
        return VertexLabeledGraph( morphism );
        
    end;
    
    ## building the categorical tower:
    
    # expanded version of: S_ZX := CreateQuiver( Length( S_ZX_NODES ), S_ZX_EDGES );
    S_ZX := CreateCapCategoryObjectWithAttributes( FinQuivers,
                    DefiningTripleOfQuiverEnrichedOverSkeletalFinSets, Triple( Length( S_ZX_NODES ), Length( S_ZX_EDGES ), S_ZX_EDGES ) );
    
    Assert( 0, IsWellDefinedForObjects( FinQuivers, S_ZX ) );
    
    decorated_quivers := CategoryOfDecoratedQuivers( S_ZX,
        List( S_ZX_NODES, function ( label )
            
            if label = "neutral" then
                
                return "black";
                
            elif label = "H" then
                
                return "orange";
                
            elif StartsWith( label, "Z" ) then
                
                return "green";
                
            elif StartsWith( label, "X" ) then
                
                return "red";
                
            else
                
                # COVERAGE_IGNORE_NEXT_LINE
                Error( "unknown label: ", label );
                
            fi;
            
        end ),
        ListWithIdenticalEntries( Length( S_ZX_EDGES ), Immutable( "black" ) )
        : FinalizeCategory := true
    );
    
    # Display( ENHANCED_SYNTAX_TREE( x -> CreateCapCategoryObjectWithAttributes( FinQuivers, DefiningTripleOfQuiverEnrichedOverSkeletalFinSets, Triple( 4, 3, CapJitTypedExpression( S_ZX_EDGES, { } -> CapJitDataTypeOfListOf( CapJitDataTypeOfNTupleOf( 2, IsBigInt, IsBigInt ) ) ) ) ) ).bindings.BINDING_RETURN_VALUE );
    ModelingCategory( decorated_quivers )!.compiler_hints.category_attribute_resolving_functions := rec(
        BaseObject := { } -> rec(
            args := rec(
                1 := rec(
                    gvar := "FinQuivers",
                    type := "EXPR_REF_GVAR"
                ),
                2 := rec(
                    gvar := "DefiningTripleOfQuiverEnrichedOverSkeletalFinSets",
                    type := "EXPR_REF_GVAR"
                ),
                3 := rec(
                    args := rec(
                        1 := rec(
                            type := "EXPR_INT",
                            value := 4
                        ),
                        2 := rec(
                            type := "EXPR_INT",
                            value := 3
                        ),
                        3 := rec(
                            data_type := rec(
                                element_type := rec(
                                    element_types := [
                                        rec(
                                            filter := IsBigInt
                                        ),
                                        rec(
                                            filter := IsBigInt
                                        )
                                    ],
                                    filter := IsNTuple
                                ),
                                filter := IsList
                            ),
                            gvar := "S_ZX_EDGES",
                            type := "EXPR_REF_GVAR"
                        ),
                        length := 3,
                        type := "SYNTAX_TREE_LIST"
                    ),
                    funcref := rec(
                        gvar := "Triple",
                        type := "EXPR_REF_GVAR"
                    ),
                    type := "EXPR_FUNCCALL"
                ),
                length := 3,
                type := "SYNTAX_TREE_LIST"
            ),
            funcref := rec(
                gvar := "CreateCapCategoryObjectWithAttributes",
                type := "EXPR_REF_GVAR"
            ),
            type := "EXPR_FUNCCALL"
        ),
    );
    
    csp := CategoryOfCospans_for_ZXCalculus( decorated_quivers : FinalizeCategory := true );
    
    ## from the raw object data to the object in the modeling category
    modeling_tower_object_constructor :=
      function ( ZX, integer )
        local csp, decorated_quivers, decorated_quiver, obj;
        
        if not (IsBigInt( integer ) and integer >= 0) then
            
            # COVERAGE_IGNORE_NEXT_LINE
            Error( "The object datum in the category of ZX-diagrams must be a non-negative integer." );
            
        fi;
        
        csp := ModelingCategory( ZX );
        
        decorated_quivers := UnderlyingCategory( csp );
        
        decorated_quiver := ObjectConstructor( decorated_quivers, Pair(
            # (nr_vertices, nr_edges, edges)
            Triple( integer, BigInt( 0 ), CapJitTypedExpression( [ ], { } -> CapJitDataTypeOfListOf( CapJitDataTypeOfNTupleOf( 2, IsBigInt, IsBigInt ) ) ) ),
            # (decorations_of_vertices, deocorations_of_edges)
            Pair( ListWithIdenticalEntries( integer, BigInt( 0 ) ), CapJitTypedExpression( [ ], { } -> CapJitDataTypeOfListOf( IsBigInt ) ) ) ) );
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0, IsWellDefinedForObjects( decorated_quivers, decorated_quiver ) );
        
        obj := ObjectConstructor( csp, decorated_quiver );
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0, IsWellDefinedForObjects( csp, obj ) );
        
        return obj;
        
    end;
    
    ## from the object in the modeling category to the raw object data
    modeling_tower_object_datum := function ( ZX, object )
      local csp, decorated_quivers, pair, integer;
        
        csp := ModelingCategory( ZX );
        
        decorated_quivers := UnderlyingCategory( csp );
        
        pair := ObjectDatum( decorated_quivers, ObjectDatum( csp, object ) );
        
        integer := pair[1][1]; # nr_vertices
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0,
                pair = Pair(
                        # (nr_vertices, nr_edges, edges)
                        Triple( integer, BigInt( 0 ), [ ] ),
                        # (decorations_of_vertices, deocorations_of_edges)
                        Pair( ListWithIdenticalEntries( integer, BigInt( 0 ) ), [ ] ) ) );
        
        return integer;
        
    end;
    
    ## from the raw morphism data to the morphism in the modeling category
    modeling_tower_morphism_constructor :=
      function ( ZX, source, quadruple, target )
        local csp, decorated_quivers, labels, edges, input_positions, output_positions,
              source_decorated_quiver, source_pair, target_decorated_quiver, target_pair,
              nr_vertices, nr_edges, decorations_of_vertices, decorations_of_edges,
              central_decorated_quiver, input_morphism, output_morphism, morphism_pair, mor;
        
        csp := ModelingCategory( ZX );
        
        decorated_quivers := UnderlyingCategory( csp );
        
        labels := quadruple[1];
        input_positions := quadruple[2];
        output_positions := quadruple[3];
        edges := quadruple[4];
        
        source_decorated_quiver := ObjectDatum( csp, source );
        source_pair := ObjectDatum( decorated_quivers, source_decorated_quiver );
        
        target_decorated_quiver := ObjectDatum( csp, target );
        target_pair := ObjectDatum( decorated_quivers, target_decorated_quiver );
        
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
        
        central_decorated_quiver :=
          ObjectConstructor( decorated_quivers,
                  Pair( Triple(
                          nr_vertices,
                          nr_edges,
                          edges ),
                        Pair( decorations_of_vertices,
                              decorations_of_edges ) ) );
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0, IsWellDefinedForObjects( decorated_quivers, central_decorated_quiver ) );
        
        input_morphism := MorphismConstructor( decorated_quivers,
                                  source_decorated_quiver,
                                  Pair( input_positions, CapJitTypedExpression( [ ], { } -> CapJitDataTypeOfListOf( IsBigInt ) ) ),
                                  central_decorated_quiver );
        
        output_morphism := MorphismConstructor( decorated_quivers,
                                   target_decorated_quiver,
                                   Pair( output_positions, CapJitTypedExpression( [ ], { } -> CapJitDataTypeOfListOf( IsBigInt ) ) ),
                                   central_decorated_quiver );
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0, IsWellDefinedForMorphisms( decorated_quivers, input_morphism ) );
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0, IsWellDefinedForMorphisms( decorated_quivers, output_morphism ) );
        
        morphism_pair := Pair( input_morphism, output_morphism );
        
        mor := MorphismConstructor( csp, source, morphism_pair, target );
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0, IsWellDefinedForMorphisms( csp, mor ) );
        
        return mor;
        
    end;
    
    ## from the morphism in the modeling category to the raw morphism data
    modeling_tower_morphism_datum :=
      function ( ZX, morphism )
        local csp, decorated_quivers, morphism_pair, input_morphism, output_morphism,
              source_decorated_quiver, target_decorated_quiver, central_decorated_quiver, central_decorated_quiver_data, nr_vertices, decorations_of_vertices, input_positions, output_positions, labels, edges;
        
        csp := ModelingCategory( ZX );
        
        decorated_quivers := UnderlyingCategory( csp );
        
        morphism_pair := MorphismDatum( csp, morphism );
        
        input_morphism := morphism_pair[1];
        output_morphism := morphism_pair[2];
        
        source_decorated_quiver := Source( input_morphism );
        target_decorated_quiver := Source( output_morphism );
        
        central_decorated_quiver := Target( input_morphism );
        
        central_decorated_quiver_data := ObjectDatum( decorated_quivers, central_decorated_quiver );
        
        nr_vertices := central_decorated_quiver_data[1][1];
        
        decorations_of_vertices := central_decorated_quiver_data[2][1];
        
        input_positions := MorphismDatum( decorated_quivers, input_morphism )[1];
        output_positions := MorphismDatum( decorated_quivers, output_morphism )[1];
        
        labels := List( decorations_of_vertices, d -> ZX_IntegerToLabel( d ) );
        edges := central_decorated_quiver_data[1][3];
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0, ForAll( input_positions, pos -> labels[pos + 1] = "neutral" ) );
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        Assert( 0, ForAll( output_positions, pos -> labels[pos + 1] = "neutral" ) );
        
        return NTuple( 4, labels, input_positions, output_positions, edges );
        
    end;
    
    ZX :=
      ReinterpretationOfCategory( csp,
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
    
    SetUnderlyingCategoryOfZXDiagramsWithoutIO( ZX, CategoryOfZXDiagramsWithoutIO( decorated_quivers ) );
    
    if not CAP_NAMED_ARGUMENTS.no_precompiled_code then
        
        ADD_FUNCTIONS_FOR_CategoryOfZXDiagrams_precompiled( ZX );
        
    fi;
    
    if CAP_NAMED_ARGUMENTS.FinalizeCategory then
        
        Finalize( ZX );
        
    fi;
    
    return ZX;
    
end ) );
