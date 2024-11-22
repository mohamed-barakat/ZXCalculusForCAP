# SPDX-License-Identifier: GPL-2.0-or-later
# ZXCalculusForCAP: The category of ZX-diagrams
#
# Declarations
#
#! @Chapter The category of ZX-diagrams

####################################
##
#! @Section Filters
##
####################################

#! @Description
#!   The filter of categories of ZX-diagrams.
DeclareFilter( "IsCategoryOfZXDiagrams", IsCapCategory );

#! @Description
#!   The filter of objects in a category of ZX-diagrams.
DeclareFilter( "IsObjectInCategoryOfZXDiagrams", IsCapCategoryObject );

#! @Description
#!   The filter of morphisms in a category of ZX-diagrams.
DeclareFilter( "IsMorphismInCategoryOfZXDiagrams", IsCapCategoryMorphism );

####################################
##
#! @Section Constructors
##
####################################

#! @Description
#!   Creates a category of ZX-diagrams.
#! @Arguments
#! @Returns a category of ZX-diagrams
DeclareGlobalFunction( "CategoryOfZXDiagrams" );

#! @Description
#!  The default instance of the category of ZX-diagrams.
#!  It is automatically created while loading this package.
DeclareGlobalName( "ZX" );

#! @Description
#!  Create <A>n</A> qubits as an object in the category <A>zx</A> of ZX-diagrams.
#! @Arguments zx, n
#! @Returns an object
DeclareOperation( "Qubits",
        [ IsCategoryOfZXDiagrams, IsInt ] );

#! @Description
#!  Create an $Z$-spider of phase <A>phi</A>
#!  with <A>nr_inputs</A> inputs and <A>nr_outputs</A> outputs
#!  as a morphism in the category <A>zx</A> of ZX-diagrams.
#! @Arguments zx, phi, nr_inputs, nr_outputs
#! @Returns a morphism
DeclareOperation( "Z_Spider",
        [ IsCategoryOfZXDiagrams, IsStringRep, IsInt, IsInt ] );

#! @Description
#!  Create an $X$-spider of phase <A>phi</A>
#!  with <A>nr_inputs</A> inputs and <A>nr_outputs</A> outputs
#!  as a morphism in the category <A>zx</A> of ZX-diagrams.
#! @Arguments zx, phi, nr_inputs, nr_outputs
#! @Returns a morphism
DeclareOperation( "X_Spider",
        [ IsCategoryOfZXDiagrams, IsStringRep, IsInt, IsInt ] );

#! @Description
#!  Create an H-gate with $1$ input and $1$ output
#!  as a morphism in the category <A>zx</A> of ZX-diagrams.
#! @Arguments zx
#! @Returns a morphism
DeclareOperation( "H_Gate",
        [ IsCategoryOfZXDiagrams ] );

#! @Description
#!  Apply the fusion rule to the <A>i</A>-th edge in the ZX-diagram <A>mor</A>.
#! @Arguments mor, i
#! @Returns a morphism
DeclareOperation( "ZXFusionRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ] );

#! @Description
#!  Apply the identity rule to the <A>i</A>-th gate in the ZX-diagram <A>mor</A>.
#! @Arguments mor, i
#! @Returns a morphism
DeclareOperation( "ZXIdentityRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ] );

#! @Description
#!  Apply the color change rule to the <A>i</A>-th gate in the ZX-diagram <A>mor</A>.
#! @Arguments mor, i
#! @Returns a morphism
DeclareOperation( "ZXColorChangeRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ] );

####################################
##
#! @Section Attributes
##
####################################

#! @Description
#!   The integer defining an object in a category of ZX-diagrams.
#! @Arguments obj
DeclareAttribute( "AsInteger", IsObjectInCategoryOfZXDiagrams );

#! @Description
#!   The labeled graph defining a morphism in a category of ZX-diagrams.
#! @Arguments mor
DeclareAttribute( "VertexLabeledGraph", IsMorphismInCategoryOfZXDiagrams );

CapJitAddTypeSignature( "VertexLabeledGraph", [ IsMorphismInCategoryOfZXDiagrams ], CapJitDataTypeOfNTupleOf( 4, CapJitDataTypeOfListOf( IsStringRep ), CapJitDataTypeOfListOf( IsBigInt ), CapJitDataTypeOfListOf( IsBigInt ), CapJitDataTypeOfListOf( CapJitDataTypeOfNTupleOf( 2, IsBigInt, IsBigInt ) ) ) );

####################################
##
## helpers
##
####################################

DeclareGlobalName( "ZX_LabelToInteger" );
DeclareGlobalName( "ZX_IntegerToLabel" );
DeclareGlobalName( "ZX_RemovedInnerNeutralNodes" );
DeclareGlobalName( "S_ZX_EDGES" );

DeclareGlobalFunction( "SKELETAL_FIN_SETS_ExplicitCoequalizer_primitive_input" );
CapJitAddTypeSignature( "SKELETAL_FIN_SETS_ExplicitCoequalizer_primitive_input", [ IsInt, IsList ], CapJitDataTypeOfListOf( CapJitDataTypeOfListOf( IsInt ) ) );
