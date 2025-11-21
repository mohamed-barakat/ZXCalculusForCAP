LoadPackage( "ZXCalculusForCAP", false );
zx := CategoryOfZXDiagrams_as_CategoryOfCospans_CategoryOfDecoratedQuivers( );
Display( zx );
one := 1 / zx;
id := IdentityMorphism( one );
braid := Braiding( one, one );

diagram5 := X_Spider( zx, 0, 1 )* Z_Spider( zx, 1, 2 );
diagram_CopyRule := ZXCopyRule( diagram5, 0, 2 );

Z_0_1 := Z_Spider( zx, 0, 1 );
Z_1_2 := Z_Spider( zx, 1, 2 );
X_1_0 := X_Spider( zx, 1, 0 );
X_2_1 := X_Spider( zx, 2, 1 );

diagram_bialgebra := (Z_0_1 * Z_1_2 + Z_1_2) * (id + braid + id ) * ( X_2_1 * X_1_0 + X_2_1 );
#diagram_bialgebra_rewritten := ZXBialgebraRule( diagram_bialgebra, 6, 2, 9, 12 );
#IsWellDefined( diagram_bialgebra_rewritten );
#diagram_InverseBilalgebra := ZXInverseBialgebraRule( diagram_bialgebra_rewritten, 7, 8 );

diagram7 := ZXInverseIdentityRule( diagram_bialgebra, 7, 1 );
diagram8 := ZXInverseColorChangeRule( diagram7, 10 );
diagram9 := ZXInverseFusionRule( diagram8, 2 );

diagram6 := Z_1_2 * X_2_1;
diagram_corollary := ZXInverseIdentityRule( diagram6, 2, 3 );
diagram_corollary1 := ZXInverseIdentityRule( diagram_corollary, 7, 1 );
diagram_corollary2 := ZXInverseFusionRule( diagram_corollary1, 6 );
diagram_corollary3 := ZXInverseFusionRule( diagram_corollary2, 7 );
Assert( 0, IsWellDefined( diagram_corollary3 ) );
diagram_corollary4 := ZXBialgebraRule( diagram_corollary3, 11, 1, 8, 3 );
diagram_corollory5 := ZXCopyRule( diagram_corollary4, 3, 8 );
diagram_corollory6 := ZXCopyRule( diagram_corollory5, 3, 5 );
diagram_corollory7 := ZXCongruenceRule( diagram_corollory6, 4, 6 );
