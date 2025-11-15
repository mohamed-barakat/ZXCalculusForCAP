#! @Chunk CategoryOfZXDiagramsAsTower

#! @Example
LoadPackage( "ZXCalculusForCAP", false );
#! true

zx := CategoryOfZXDiagrams_as_CategoryOfCospans_CategoryOfDecoratedQuivers( );
#! CategoryOfZXDiagrams( )

zero := 0 / zx;
#! <An object in CategoryOfZXDiagrams( ) representing 0 input/output vertices>
one := 1 / zx;
#! <An object in CategoryOfZXDiagrams( ) representing 1 input/output vertices>
two := 2 / zx;
#! <An object in CategoryOfZXDiagrams( ) representing 2 input/output vertices>
three := 3 / zx;
#! <An object in CategoryOfZXDiagrams( ) representing 3 input/output vertices>

three = one + two;
#! true
three = zero + three;
#! true
three = 3 * one;
#! true

id := IdentityMorphism( one );
#! <An identity morphism in CategoryOfZXDiagrams( )>
id3 := IdentityMorphism( three );
#! <An identity morphism in CategoryOfZXDiagrams( )>
id3 = 3 * id;
#! true
ev := EvaluationForDual( three );
#! <A morphism in CategoryOfZXDiagrams( )>
coev := CoevaluationForDual( three );
#! <A morphism in CategoryOfZXDiagrams( )>

PreCompose( ev, IdentityMorphism( zero ) );
#! <A morphism in CategoryOfZXDiagrams( )>
PreCompose( IdentityMorphism( TensorProduct( three, three ) ), ev );
#! <A morphism in CategoryOfZXDiagrams( )>

PreCompose( coev, IdentityMorphism( TensorProduct( three, three ) ) );
#! <A morphism in CategoryOfZXDiagrams( )>
PreCompose( IdentityMorphism( zero ), coev );
#! <A morphism in CategoryOfZXDiagrams( )>

Display( PreCompose( coev, ev ) );
#! A morphism in CategoryOfZXDiagrams( ) given by a ZX-diagram \
#! with 0 vertex labels
#!   [  ],
#!   inputs
#!   [  ],
#!   outputs
#!   [  ],
#!   and 0 edges
#!   [  ].
Display( PreCompose( ev, coev ) );
#! A morphism in CategoryOfZXDiagrams( ) given by a ZX-diagram \
#! with 6 vertex labels
#!   [ "neutral", "neutral", "neutral", "neutral", "neutral", "neutral" ],
#!   inputs
#!   [ 0, 1, 2, 0, 1, 2 ],
#!   outputs
#!   [ 3, 4, 5, 3, 4, 5 ],
#!   and 0 edges
#!   [  ].

IdentityMorphism( one ) + IdentityMorphism( two ) = id3;
#! true

AssociatorLeftToRight( zero, one, two ) = id3;
#! true

AssociatorRightToLeft( zero, one, two ) = id3;
#! true

LeftUnitor( three ) = id3;
#! true

LeftUnitorInverse( three ) = id3;
#! true

RightUnitor( three ) = id3;
#! true

RightUnitorInverse( three ) = id3;
#! true

Braiding( one, two ) = BraidingInverse( two, one );
#! true

X_1_1 := X_Spider( zx, 1, 1 );
#! <A morphism in CategoryOfZXDiagrams( )>
IsWellDefined( X_1_1 );
#! true

Z_1_1 := Z_Spider( zx, 1, 1 );
#! <A morphism in CategoryOfZXDiagrams( )>
IsWellDefined( Z_1_1 );
#! true

H := H_Gate( zx );
#! <A morphism in CategoryOfZXDiagrams( )>
IsWellDefined( H );
#! true

X_1_2 := X_Spider( zx, 1, 2 );
#! <A morphism in CategoryOfZXDiagrams( )>
IsWellDefined( X_1_2 );
#! true

Z_2_1 := Z_Spider( zx, 2, 1 );
#! <A morphism in CategoryOfZXDiagrams( )>
IsWellDefined( Z_2_1 );
#! true

X_1_2_Z_2_1 := PreCompose( X_1_2, Z_2_1 );
#! <A morphism in CategoryOfZXDiagrams( )>
IsWellDefined( X_1_2_Z_2_1 );
#! true

GHZ := ( 3 * X_Spider( zx, 0, 1 ) ) * ( id + H + id ) *
       ( id + Z_Spider( zx, 1, 2 ) + X_1_2 ) *
       ( X_Spider( zx, 2, 1 ) + Z_2_1 + id );
#! <A morphism in CategoryOfZXDiagrams( )>
IsWellDefined( GHZ );
#! true

GHZ_rewritten1 := ZXFusionRule( GHZ, 1 );
GHZ_rewritten2 := ZXFusionRule( GHZ_rewritten1, 3 );
GHZ_rewritten3 := ZXFusionRule( GHZ_rewritten2, 6 );
GHZ_rewritten4 := ZXIdentityRule( GHZ_rewritten3, 9 );
GHZ_rewritten5 := ZXIdentityRule( GHZ_rewritten4, 8 );
GHZ_rewritten6 := ZXColorChangeRule( GHZ_rewritten5, 0 );
GHZ_rewritten7 := ZXFusionRule( GHZ_rewritten6, 0 );

GHZ_quiver := Target( MorphismDatum( ModelingMorphism( zx, GHZ ) )[1] );
#! <An object in CategoryOfDecoratedQuivers( decorating_quiver )>
IsWellDefined( GHZ_quiver );
#! true

braid := Braiding( one, one );
Z_0_1 := Z_Spider( zx, 0, 1 );
X_1_0 := X_Spider( zx, 1, 0 );
Z_1_2 := Z_Spider( zx, 1, 2 );
X_2_1 := X_Spider( zx, 2, 1 );

diagram_bialgebra := (Z_0_1 * Z_1_2 + Z_1_2) * (id + braid + id ) * ( X_2_1 * X_1_0 + X_2_1 );

diagram_bialgebra_rewritten := ZXBialgebraRule( diagram_bialgebra, 6, 2, 9, 12 );
Assert( 0, IsWellDefined( diagram_bialgebra_rewritten ) );

DotVertexLabelledDigraph( diagram_bialgebra_rewritten );

tmp_dir := DirectoryTemporary( );;

# test ExportAsQGraphFile
ExportAsQGraphFile( id, Filename( tmp_dir, "id" ) );
ExportAsQGraphFile( ev, Filename( tmp_dir, "ev" ) );
ExportAsQGraphFile( coev, Filename( tmp_dir, "coev" ) );
ExportAsQGraphFile( X_1_1, Filename( tmp_dir, "X_1_1" ) );
ExportAsQGraphFile( Z_1_1, Filename( tmp_dir, "Z_1_1" ) );
ExportAsQGraphFile( H, Filename( tmp_dir, "H" ) );
ExportAsQGraphFile( X_1_2, Filename( tmp_dir, "X_1_2" ) );
ExportAsQGraphFile( Z_2_1, Filename( tmp_dir, "Z_2_1" ) );
ExportAsQGraphFile( X_1_2_Z_2_1, Filename( tmp_dir, "X_1_2_Z_2_1" ) );

#
# test ImportFromQGraphFile
ImportFromQGraphFile( zx, Filename( tmp_dir, "id" ) );;
ImportFromQGraphFile( zx, Filename( tmp_dir, "ev" ) );;
ImportFromQGraphFile( zx, Filename( tmp_dir, "coev" ) );;
ImportFromQGraphFile( zx, Filename( tmp_dir, "X_1_1" ) );;
ImportFromQGraphFile( zx, Filename( tmp_dir, "Z_1_1" ) );;
ImportFromQGraphFile( zx, Filename( tmp_dir, "H" ) );;
ImportFromQGraphFile( zx, Filename( tmp_dir, "X_1_2" ) );;
ImportFromQGraphFile( zx, Filename( tmp_dir, "Z_2_1" ) );;
ImportFromQGraphFile( zx, Filename( tmp_dir, "X_1_2_Z_2_1" ) );;

# due to reordering during import/export, we have to make some roundtrips
# until import/export is stable
test_inverse :=
  function( mor0 )
    local json, mor1, json2, mor2, json3, mor3;
    json := ExportAsQGraphString( mor0 );
    mor1 := ImportFromQGraphString( zx, json );
    json2 := ExportAsQGraphString( mor1 );
    mor2 := ImportFromQGraphString( zx, json2 );
    json3 := ExportAsQGraphString( mor2 );
    mor3 := ImportFromQGraphString( zx, json3 );
    return IsEqualForMorphisms( mor2, mor3 ) and json2 = json3;
end;

test_inverse( id );
#! true
test_inverse( ev );
#! true
test_inverse( coev );
#! true
test_inverse( X_1_1 );
#! true
test_inverse( Z_1_1 );
#! true
test_inverse( H );
#! true
test_inverse( X_1_2 );
#! true
test_inverse( Z_2_1 );
#! true
test_inverse( X_1_2_Z_2_1 );
#! true

#@if ValueOption( "no_precompiled_code" ) <> true
# The following JSON is obtained from PyZX and encoded in Base64 to avoid conversion errors to Julia.
# It contains a non-zero phase of an X-spider and hence enhances S_ZX_EDGES dynamically. This only works when using the precompiled code.
# """{"wire_vertices": {"b0": {"annotation": {"boundary": true, "coord": [-3.75, 4.75], "input": 0}}, "b1": {"annotation": {"boundary": true, "coord": [-3.75, 2.75], "input": 1}}, "b2": {"annotation": {"boundary": true, "coord": [0.75, 4.75], "output": 0}}, "b3": {"annotation": {"boundary": true, "coord": [0.75, 2.75], "output": 1}}}, "node_vertices": {"v0": {"annotation": {"coord": [-2.25, 4.75]}, "data": {"type": "X"}}, "v1": {"annotation": {"coord": [-0.5, 2.75]}, "data": {"type": "X", "value": "\u03c0"}}, "v2": {"annotation": {"coord": [-2.25, 2.75]}, "data": {"type": "Z"}}}, "undir_edges": {"e0": {"src": "b0", "tgt": "v0"}, "e1": {"src": "b1", "tgt": "v2"}, "e2": {"src": "b2", "tgt": "v0"}, "e3": {"src": "b3", "tgt": "v1"}, "e4": {"src": "v0", "tgt": "v2"}, "e5": {"src": "v1", "tgt": "v2"}}}"""
succ_mod_4 := ImportFromQGraphString( zx, StringBase64( "eyJ3aXJlX3ZlcnRpY2VzIjogeyJiMCI6IHsiYW5ub3RhdGlvbiI6IHsiYm91bmRhcnkiOiB0cnVlLCAiY29vcmQiOiBbLTMuNzUsIDQuNzVdLCAiaW5wdXQiOiAwfX0sICJiMSI6IHsiYW5ub3RhdGlvbiI6IHsiYm91bmRhcnkiOiB0cnVlLCAiY29vcmQiOiBbLTMuNzUsIDIuNzVdLCAiaW5wdXQiOiAxfX0sICJiMiI6IHsiYW5ub3RhdGlvbiI6IHsiYm91bmRhcnkiOiB0cnVlLCAiY29vcmQiOiBbMC43NSwgNC43NV0sICJvdXRwdXQiOiAwfX0sICJiMyI6IHsiYW5ub3RhdGlvbiI6IHsiYm91bmRhcnkiOiB0cnVlLCAiY29vcmQiOiBbMC43NSwgMi43NV0sICJvdXRwdXQiOiAxfX19LCAibm9kZV92ZXJ0aWNlcyI6IHsidjAiOiB7ImFubm90YXRpb24iOiB7ImNvb3JkIjogWy0yLjI1LCA0Ljc1XX0sICJkYXRhIjogeyJ0eXBlIjogIlgifX0sICJ2MSI6IHsiYW5ub3RhdGlvbiI6IHsiY29vcmQiOiBbLTAuNSwgMi43NV19LCAiZGF0YSI6IHsidHlwZSI6ICJYIiwgInZhbHVlIjogIs+AIn19LCAidjIiOiB7ImFubm90YXRpb24iOiB7ImNvb3JkIjogWy0yLjI1LCAyLjc1XX0sICJkYXRhIjogeyJ0eXBlIjogIloifX19LCAidW5kaXJfZWRnZXMiOiB7ImUwIjogeyJzcmMiOiAiYjAiLCAidGd0IjogInYwIn0sICJlMSI6IHsic3JjIjogImIxIiwgInRndCI6ICJ2MiJ9LCAiZTIiOiB7InNyYyI6ICJiMiIsICJ0Z3QiOiAidjAifSwgImUzIjogeyJzcmMiOiAiYjMiLCAidGd0IjogInYxIn0sICJlNCI6IHsic3JjIjogInYwIiwgInRndCI6ICJ2MiJ9LCAiZTUiOiB7InNyYyI6ICJ2MSIsICJ0Z3QiOiAidjIifX19" ) );
#! <A morphism in CategoryOfZXDiagrams( )>
test_inverse( succ_mod_4 );
#! true
zx_quiver := Target( MorphismDatum( ModelingMorphism( zx, succ_mod_4 ) )[1] );
#! <An object in CategoryOfDecoratedQuivers( decorating_quiver )>
#@fi

#! @EndExample
