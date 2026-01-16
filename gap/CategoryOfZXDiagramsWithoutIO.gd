# SPDX-License-Identifier: GPL-2.0-or-later
# ZXCalculusForCAP: The category of ZX-diagrams
#
# Declarations
#
#! @Chapter The category of ZX-diagrams without inputs and outputs

####################################
##
#! @Section Filters
##
####################################

#! @Description
#!   The filter of categories of ZX-diagrams without inputs and outputs.
DeclareFilter( "IsCategoryOfZXDiagramsWithoutIO", IsCapCategory );

#! @Description
#!   The filter of objects in a category of ZX-diagrams without inputs and outputs.
DeclareFilter( "IsObjectInCategoryOfZXDiagramsWithoutIO", IsCapCategoryObject );

#! @Description
#!   The filter of morphisms in a category of ZX-diagrams without inputs and outputs.
DeclareFilter( "IsMorphismInCategoryOfZXDiagramsWithoutIO", IsCapCategoryMorphism );

####################################
##
#! @Section Constructors
##
####################################

#! @Description
#!   Creates a category of ZX-diagrams without inputs and outpus.
#! @Arguments
#! @Returns a &CAP; category
DeclareOperation( "CategoryOfZXDiagramsWithoutIO",
        [ IsCategoryOfDecoratedQuivers ] );

####################################
##
#! @Section Attributes
##
####################################

#! @Description
#!  The input is a ZX-diagram <A>zx_diagram</A> withtout inputs and outputs.
#!  The output is the datum of <A>zx_diagram</A>.
#!  It is a pair, with first entry a list of strings,
#!  and second entry a list of pairs of integers.
#! @Arguments zx_diagram
DeclareAttribute( "PairOfStringsAndListOfPairsOfIntegers", IsObjectInCategoryOfZXDiagramsWithoutIO );

CapJitAddTypeSignature( "PairOfStringsAndListOfPairsOfIntegers", [ IsObjectInCategoryOfZXDiagramsWithoutIO ],
        CapJitDataTypeOfNTupleOf( 2,
                CapJitDataTypeOfListOf( IsStringRep ),
                CapJitDataTypeOfListOf( CapJitDataTypeOfNTupleOf( 2, IsBigInt, IsBigInt ) ) ) );

#! @Description
#!  The input is a morphism <A>mor</A> of ZX-diagrams withtout inputs and outputs.
#!  The output is the datum of <A>mor</A>.
#!  It is a pair, with each entry a list of integers.
#! @Arguments mor
DeclareAttribute( "PairOfListsOfIntegers", IsMorphismInCategoryOfZXDiagramsWithoutIO );

CapJitAddTypeSignature( "PairOfListsOfIntegers", [ IsMorphismInCategoryOfZXDiagramsWithoutIO ],
        CapJitDataTypeOfNTupleOf( 2,
                CapJitDataTypeOfListOf( IsInt ),
                CapJitDataTypeOfListOf( IsInt ) ) );

####################################
##
#! @Section Operations
##
####################################

#! @Description
#!  Create the fusion rule.
#! @Arguments ZX, type, c, l1, l2
#! @Returns a pair of morphisms
DeclareOperation( "ZXFusionRule",
        [ IsCategoryOfZXDiagramsWithoutIO, IsString, IsInt, IsInt, IsInt ] );

####################################
##
#! @Section Attributes
##
####################################

#! @Description
#!   The input is a category of ZX-diagrams without inputs and outputs.
#!   The output is the underlying category of spans of ZX-diagrams without inputs and outputs,
#!   this is the category in which the rewriting rules of the ZX-calculus are implemented.
#! @Arguments zx_without_io
DeclareAttribute( "UnderlyingCategoryOfSpansOfZXDiagramsWithoutIO", IsCategoryOfZXDiagramsWithoutIO );
