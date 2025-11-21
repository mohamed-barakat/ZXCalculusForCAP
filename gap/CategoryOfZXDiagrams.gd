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
#!  The input is a category <A>ZX</A> of ZX-diagrams, a ZX-diagram therein <A>zx_diagram</A>,
#!  and three morphisms in the underlying category of decorated quivers, of which
#!  the first is the matching morphism <A>m</A>, the second is left rule <A>l</A>,
#!  and the third is the right rule <A>r</A>.
#! @Arguments ZX, zx_diagram, m, l, r
#! @Returns a morphism
DeclareOperation( "ZX_DPO_Rewriting",
        [ IsCategoryOfZXDiagrams, IsMorphismInCategoryOfZXDiagrams,
          IsMorphismInCategoryOfDecoratedQuivers, IsMorphismInCategoryOfDecoratedQuivers, IsMorphismInCategoryOfDecoratedQuivers ] );

#! @Description
#!  Apply the fusion rule to the <A>i</A>-th edge in the ZX-diagram <A>mor</A>.
#! @Arguments mor, i
#! @Returns a morphism
DeclareOperation( "ZXFusionRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ] );

#! @Description
#!  Apply the inverse of fusion rule to the <A>i</A>-th edge in the ZX-diagram <A>mor</A>.
#! @Arguments mor, i
#! @Returns a morphism
DeclareOperation( "ZXInverseFusionRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ] );

#! @Description
#!  Apply the identity rule to the <A>i</A>-th gate in the ZX-diagram <A>mor</A>.
#! @Arguments mor, i
#! @Returns a morphism
DeclareOperation( "ZXIdentityRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ] );

#! @Description
#!  Apply the identity rule to the <A>i</A>-th gate in the ZX-diagram <A>mor</A>.
#! @Arguments mor, i
#! @Returns a morphism
DeclareOperation( "ZXInverseIdentityRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt, IsString ] );

#! @Description
#!  Apply the color change rule to the <A>i</A>-th gate in the ZX-diagram <A>mor</A>.
#! @Arguments mor, i
#! @Returns a morphism
DeclareOperation( "ZXColorChangeRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ] );

#! @Description
#!  Apply the color change rule to the <A>i</A>-th gate in the ZX-diagram <A>mor</A>.
#! @Arguments mor, i
#! @Returns a morphism
DeclareOperation( "ZXInverseColorChangeRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt ] );

#! @Description
#!  Apply the bialgebra rule to the two Z-spiders <A>z1</A>, <A>z2</A> and the two X-spiders <A>x1</A>, <A>x2</A> in the ZX-diagram <A>mor</A>.
#! @Arguments mor, z1, z2, x1, x2
#! @Returns a morphism
DeclareOperation( "ZXBialgebraRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt, IsInt, IsInt, IsInt ] );

#! @Description
#!  Apply the rule to the <A>i, j</A>-th gate in the ZX-diagram <A>mor</A>.
#! @Arguments mor, i, j
#! @Returns a morphism
DeclareOperation( "ZXInverseBialgebraRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt, IsInt ] );

#! @Description
#!  Apply the copy rule to the <A>i, j</A>-th gate in the ZX-diagram <A>mor</A>.
#! @Arguments mor, i, j
#! @Returns a morphism
DeclareOperation( "ZXCopyRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt, IsInt ] );

#! @Description
#!  Apply the copy rule to the <A>i, j</A>-th gate in the ZX-diagram <A>mor</A>.
#! @Arguments mor, i, j
#! @Returns a morphism
DeclareOperation( "ZXCongruenceRule",
        [ IsMorphismInCategoryOfZXDiagrams, IsInt, IsInt ] );

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
