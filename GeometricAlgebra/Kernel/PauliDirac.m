Package["Wolfram`GeometricAlgebra`PauliDirac`"]

PackageImport["Wolfram`GeometricAlgebra`"]

PackageExport[$PauliAlgebra]
PackageExport[$PauliBasis]
PackageExport[$PauliSpectralBasis]
PackageExport[$PauliSpinorBasis]
PackageExport[$DiracAlgebra]
PackageExport[$DiracSpectralBasis]
PackageExport[$DiracCovariantSpectralBasis]
PackageExport[$DiracSpinorBasis]

PackageExport[DiracMatrix]

PackageExport[SpinorMultivector]
PackageExport[PauliSpinor]
PackageExport[DiracSpinor]

PackageExport[$STA]
PackageExport[SpacetimeSplit]
PackageExport[Reversion]



$PauliAlgebra = GeometricAlgebra[3,
    "Format" -> "\[DoubleStruckCapitalP]",
    "FormatIndex" -> Function[Switch[#, {}, "", {1, 2, 3}, "\[ScriptCapitalI]", _, Subscript["\[Sigma]", Row @ Riffle[#, "\[InvisibleComma]"]]]]
]

{s1, s2, s3, is1, is2, is3} = $PauliAlgebra["Basis", {1, 2}]


$PauliBasis = $PauliAlgebra["Basis", 1]

$PauliSpectralBasis = MultivectorArray[{1, s1}] ** ((1 / 2) * (1 + s3)) ** MultivectorArray[{1, s1}, -2]
 
$PauliSpinorBasis = Prepend[$PauliAlgebra["Scalar"]] @ $PauliAlgebra["PseudoBasis", 1]


$DiracAlgebra = $STA = GeometricAlgebra[1, 3,
    "Format" -> "\[DoubleStruckCapitalD]",
    "FormatIndex" -> Function[Switch[#, {}, "", {1, -3, -2, -1}, Subscript["\[Gamma]", 5], _, Subscript["\[Gamma]", Row @ Riffle[# /. {1 -> 0, -1 -> 3, -2 -> 2, -3 -> 1}, "\[InvisibleComma]"]]]]
]

{g0, g1, g2, g3, g01, g02, g03, g12, g13, g23} = $DiracAlgebra["Basis", {1, 2}]

$DiracSpectralBasis = LeftKroneckerProduct[MultivectorArray[{1, - g13}], MultivectorArray[{1, g03}]] ** ((1 / 4) (g0 - 1) ** (1 + I g12)) ** RightKroneckerProduct[MultivectorArray[{1, g03}, -2], MultivectorArray[{1, g13}, -2]]

$DiracCovariantSpectralBasis = LeftKroneckerProduct[MultivectorArray[{1, - g13}], MultivectorArray[{1, - g03}]] ** ((1 / 4) (g0 - 1) ** (1 + I g12)) ** RightKroneckerProduct[MultivectorArray[{1, - g03}, -2], MultivectorArray[{1, g13}, -2]]

$DiracSpinorBasis = $DiracSpectralBasis["Components"][[All, 1]]


DiracMatrix[0] := KroneckerProduct[PauliMatrix[3], PauliMatrix[0]]

DiracMatrix[i_Integer] /; 1 <= i <= 3 := I KroneckerProduct[PauliMatrix[2], PauliMatrix[i]]

DiracMatrix[4 | 5] := KroneckerProduct[PauliMatrix[1], PauliMatrix[1]]


PauliSpinor[v_Multivector] /; v["GeometricAlgebra"] == $PauliAlgebra := Block[{a0, a1, a2, a3},
    a0 = v["Scalar"];
    {a1, a2, a3} = v["Coordinates", 2];
    {
        {a0 + a1 I},
        {a2 + a3 I}
    }
]


DiracSpinor[v_Multivector] /; v["GeometricAlgebra"] == $DiracAlgebra := ({#1["Scalar"]} &) /@ MultivectorMatrix[v, "Basis" -> $DiracSpectralBasis]["Components"][[All, 1]]


SpinorMultivector[a_ ? VectorQ /; Dimensions[a] == {4}] := a . $PauliSpinorBasis

SpinorMultivector[a_ ? MatrixQ /; Dimensions[a] == {2, 1}] := ComplexExpand[Flatten @ Map[ReIm, a, {2}]] . $PauliAlgebra["Basis", "Even"]

SpinorMultivector[a_ ? MatrixQ /; Dimensions[a] == {4, 1}] := Flatten[a] . $DiracSpinorBasis


SpacetimeSplit[v_Multivector] /; GeometricAlgebra[v] == $STA := GeometricProduct[v, $STA[1]]

Reversion[v_Multivector] /; GeometricAlgebra[v] == $STA := GeometricProduct[$STA[1], v, $STA[1]]
