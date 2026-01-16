# SPDX-License-Identifier: GPL-2.0-or-later
# ZXCalculusForCAP: The category of ZX-diagrams
#
# Declarations
#
#! @Chapter The category of ZX-diagrams

#! @Section Constructors

#! @Description
#!   Creates a category of ZX-diagrams as a tower.
#!   Only available if the package `FunctorCategories` is available.
#! @Arguments
#! @Returns a category of ZX-diagrams
DeclareGlobalFunction( "CategoryOfZXDiagrams_as_CategoryOfCospans_CategoryOfDecoratedQuivers" );

####################################
##
#! @Section Attributes
##
####################################

#! @Description
#!   The input is a category of ZX-diagrams.
#!   The output is the underlying category of ZX-diagrams without inputs and outputs,
#!   this is the category in which the left and right production rules of the
#!   rewriting rules of the ZX-calculus are implemented.
#! @Arguments zx_cat
DeclareAttribute( "UnderlyingCategoryOfZXDiagramsWithoutIO", IsCategoryOfZXDiagrams );
