Package["Wolfram`GeometricAlgebra`ProjectiveGeometry`"]

PackageImport["Wolfram`GeometricAlgebra`"]

PackageExport[PGA]
PackageExport[$PGA]
PackageExport[$2DPGA]
PackageExport[PGAOrigin]
PackageExport[RegionPGA]
PackageExport[PGARegions]

PackageExport[PGAVector]
PackageExport[PGAMagnitude]
PackageExport[PGAPoint]
PackageExport[PGALine]
PackageExport[PGAPlane]

PackageExport[PGATranslator]
PackageExport[PGAReflector]
PackageExport[PGARotor]
PackageExport[PGAMotor]
PackageExport[PGAFlector]

PackageExport[PGAAttitude]
PackageExport[PGASupport]
PackageExport[PGAAntisupport]

PackageExport[PGAExpansion]
PackageExport[PGAContraction]

PackageExport[PGAProjection]
PackageExport[PGARejection]

PackageExport[PGADistance]

PackageExport[PGATransform]



$PGA = $3DPGA = e = e3 = GeometricAlgebra[3, 0, 1, "Format" -> "PGA",
    "FormatIndex" -> Function[$DefaultMultivectorFormatFunction[#] /. {Subscript[_, Row[{1, 2, 3, 4}, _]] -> "\[DoubleStruckOne]"}],
    "Ordering" -> {{}, {1}, {2}, {3}, {4}, {4, 1}, {4, 2}, {4, 3}, {2, 3}, {3, 1}, {1, 2}, {4, 2, 3}, {4, 3, 1}, {4, 1, 2}, {3, 2, 1}, {1, 2, 3, 4}}
]

$2DPGA = e2 = GeometricAlgebra[2, 0, 1, "Format" -> Subscript["PGA", "2D"],
    "FormatIndex" -> Function[$DefaultMultivectorFormatFunction[#] /. {Subscript[_, Row[{3, 2, 1}, _]] -> "\[DoubleStruckOne]"}],
    "Ordering" -> {{}, {1}, {2}, {3}, {2, 3}, {3, 1}, {1, 2}, {3, 2, 1}}
]

PGA[3] := $3DPGA
PGA[2] := $2DPGA
PGA[n_Integer ? Positive] := GeometricAlgebra[n, 0, 1, "Format" -> Subscript["PGA", n],
    "FormatIndex" -> With[{i = Range[n + 1]}, Function[$DefaultMultivectorFormatFunction[#] /. {Subscript[_, Row[i, _]] -> "\[DoubleStruckOne]"}]]
]

i = e[1, 2, 3, 4]
i2 = e2[1, 2, 3]

moment = e[{{2, 3}, {3, 1}, {1, 2}}]


PGA2DQ[x : _GeometricAlgebra | _Multivector] := x["Signature"] === {2, 0, 1}

PGA2DQ[___] := False

PGA3DQ[x : _GeometricAlgebra | _Multivector] := x["Signature"] === {3, 0, 1}

PGA3DQ[___] := False


PGAOrigin[_ ? PGA2DQ] := $2DPGA[3]

PGAOrigin[_ ? PGA3DQ] := $3DPGA[4]


pseudoVector[v : {_, _, _}] := Reverse[v] {1, -1, 1}

(* Constructors *)

PGAVector[v_Multivector ? PGA2DQ] := v[{{1}, {2}}]
PGAVector[v_Multivector ? PGA3DQ] := v[{{1}, {2}, {3}}]
PGAVector[v : {_, _}] := Grade[v, 1, $2DPGA]
PGAVector[v : {_, _, _}] := Grade[v, 1, $PGA]

PGAMagnitude[x_ : 1, y_ : 1] := x + y i

PGAPoint[Point[p_], w_ : 1] := PGAPoint[p, w]
PGAPoint[x_Multivector ? PGA2DQ] := With[{p = x[{{1}, {2}}], w = x[3]},
    Switch[w == 0, True, Missing["Point"], _, Point[p / w]]
]
PGAPoint[x_Multivector ? PGA3DQ] := With[{p = x[{{1}, {2}, {3}}], w = x[4]},
    Switch[w == 0, True, Missing["Point"], _, Point[p / w]]
]
PGAPoint[p_List, w_ : 1] := Grade[Append[p, w], 1, PGA[Length[p]]]

PGALine[(Line | InfiniteLine)[{p_, q_}]] := Wedge[PGAPoint[p], PGAPoint[q]]
PGALine[InfiniteLine[p : {_, _, _}, v : {_, _, _}]] := PGALine[v, Cross[p, v]]
PGALine[InfiniteLine[p : {_, _}, {x_, y_}]] := PGALine[{y, -x}, Norm[p]]
PGALine[x_Multivector ? PGA2DQ] := Enclose[InfiniteLine[First @ Confirm @ PGAPoint[PGASupport[x]], x[{{3, 1}, {3, 2}}]], Missing["Line"] &]
PGALine[x_Multivector ? PGA3DQ] := Enclose[InfiniteLine[First @ Confirm @ PGAPoint[PGASupport[x]], x[{{4, 1}, {4, 2}, {4, 3}}]], Missing["Line"] &]
PGALine[n : {_, _}, d_ : 0] := n . e2[{{2, 3}, {3, 1}}] + d e2[1, 2] 
PGALine[v : {_, _, _}, m : {_, _, _}] := e[4] ** PGAVector[v] + m . moment

PGAPlane[InfinitePlane[p : {_, _, _}, {u : {_, _, _}, v : {_, _, _}}]] := Wedge[PGAPoint[p], PGAPoint[p + u] PGAPoint[p + v]]
PGAPlane[(Triangle | InfinitePlane)[{p1 : {_, _, _}, p2 : {_, _, _}, p3 : {_, _, _}}]] := Wedge[PGAPoint[p1], PGAPoint[p2], PGAPoint[p3]]
PGAPlane[Hyperplane[n : {_, _, _}, p : {_, _, _}]] := PGAPlane[n, - n . p / Norm[n]]
PGAPlane[Hyperplane[n : {_, _, _}, d_]] := PGAPlane[n, - d]
PGAPlane[x_Multivector ? PGA3DQ] := With[{n = x[{{4, 2, 3}, {4, 3, 1}, {4, 1, 2}}], w = x[3, 2, 1]},
    Switch[Norm[n] != 0, False, Missing["Plane"], _, Hyperplane[n, - w]]
]
PGAPlane[n : {_, _, _} : {0, 0, 1}, w_ : 1] := Grade[Prepend[pseudoVector[n], - w], 3, $PGA]

QuaternionToRotationMatrix[r_, x_, y_, z_] := With[{aim = Norm[{x, y, z}]},
	If[aim == 0, Return[IdentityMatrix[3]]];
	First @ LinearAlgebra`Private`MatrixPolynomial[
		{Prepend[2 aim {r, aim} / (r ^ 2 + aim ^ 2), 1]},
		- HodgeDual[{x, y, z} / aim]
	]
]

PGATranslator[t : {_, _, _}] := t . moment + i
PGATranslator[t_Multivector] := TranslationTransform[2 t[{{2, 3}, {3, 1}, {1, 2}}]]
PGATranslator[t_TransformationFunction] := PGATranslator[t["AffineVector"] / 2]

PGAReflector[n : {_, _, _}, w_ : 1] := PGAPlane[n, w]
PGAReflector[g : _Triangle | _InfinitePlane | _Hyperplane] := PGAPlane[g]
PGAReflector[r_Multivector ? PGA3DQ] := With[{n = r[{{4, 2, 3}, {4, 3, 1}, {4, 1, 2}}], w = r[3, 2, 1]},
    ReflectionTransform[n, - w * n / n . n]
]

PGARotor[l_Multivector, phi_] := WeightUnitize[l] * Sin[phi / 2] + GeometricAlgebra[l]["Pseudoscalar"] * Cos[phi / 2]
PGARotor[line : _InfiniteLine | _Line, phi_] := PGARotor[PGALine[line], phi]
PGARotor[r_Multivector] := AffineTransform[
    If[ PGA3DQ[r],
        QuaternionToRotationMatrix @@ r[{{1, 2, 3, 4}, {4, 1}, {4, 2}, {4, 3}}]
    ]
]

PGAMotor[v : {_, _, _}, m : {_, _, _}, vw_ : 1, mw_ : 1] := mw + v . e[{{4, 1}, {4, 2}, {4, 3}}] + m . e[{{2, 3}, {3, 1}, {1, 2}}] + vw i
PGAMotor[line : _InfiniteLine | _Line | _Multivector, t : {_, _, _}, phi_ : 0] := PGAMotor[line, PGATranslator[t], phi]
PGAMotor[line : _InfiniteLine | _Line | _Multivector, t_Multivector, phi_ : 0] := AntiGeometricProduct[t, PGARotor[line, phi]]
PGAMotor[m_Multivector] := PGATranslator[AntiGeometricProduct[m, AntiReverse[Weight[m]]]] @* PGARotor[m]

PGAFlector[p : {_, _, _}, g : {_, _, _}, pw_ : 1, gw_ : 1] := PGAVector[Append[p, pw]] + GeometricProduct[e[4], g . moment] + gw e[3, 2, 1]
PGAFlector[line : _InfiniteLine | _Line, g : _Triangle | _InfinitePlane | _Hyperplane, phi_ : 0] := AntiGeometricProduct[PGARotor[line, phi], g]
PGAFlector[p : _Point | _Multivector, g : _Triangle | _InfinitePlane | _Hyperplane, phi_ : 0] := PGAFlector[PGALine[PGAExpansion[p, g]], g, phi]
PGAFlector[f_Multivector] := PGAReflector[f] @* PGARotor[AntiGeometricProduct[f, AntiReverse[PGAPlane[f]]]]


(* Unary operations *)

PGAAttitude[v_Multivector] := Vee[v, OverBar[PGAOrigin[v]]]

PGASupport[v_Multivector] := OrthoProjection[PGAOrigin[v], v]

PGAAntisupport[v_Multivector] := CentralAntiprojection[OverBar[PGAOrigin[v]], v]


(* Binary operations *)

PGAProjection[a_Multivector, b_Multivector] := LeftInteriorProduct[RightInteriorProduct[Weight[b], a], b]

PGARejection[a_Multivector, b_Multivector] := LeftInteriorAntiProduct[RightInteriorAntiProduct[Weight[b], a], b]


PGAExpansion = WeightExpansion

PGAContraction = WeightContraction


PGADistance[a_Multivector, b_Multivector] := BulkNorm[PGAAttitude[Wedge[a, b]]] + WeightNorm[Wedge[a, PGAAttitude[b]]]


(* Transforms *)

PGATransform[x_, q_] := AntiGeometricProduct[q, x, AntiReverse[q]]


(* Region constructions *)

Attributes[RegionPGA] = {Listable}

RegionPGA[point : _Point] := PGAPoint[point]

RegionPGA[line : _Line | _InfiniteLine] := PGALine[line]

RegionPGA[plane : _Triangle | _InfinitePlane | _Hyperplane] := PGAPlane[plane]


(* Region export *)

PGARegions[v_Multivector] /; PGA2DQ[v] := DeleteMissing @ <|"Vector" -> Arrow[{{0, 0}, PGAVector[v]}], "Point" -> PGAPoint[v] , "Line" -> PGALine[v]|>

PGARegions[v_Multivector] /; PGA3DQ[v] := DeleteMissing @ <|"Vector" -> Arrow[{{0, 0, 0}, PGAVector[v]}], "Point" -> PGAPoint[v] ,"Line" -> PGALine[v] ,"Plane" -> PGAPlane[v]|>


(* Overwrite mulitivector functions for regions *)

$PGARegion = _Point | _InfiniteLine | _Triangle | _InfinitePlane | _Hyperplane

Scan[
    Function[f,
        With[{rule = HoldPattern[f[left___, x_ ? (MatchQ[$PGARegion]), right___]] :> f[left, RegionPGA[x], right]},
            If[! MemberQ[DownValues[f], Verbatim[rule]], PrependTo[DownValues[f], rule]]
        ]
    ],
    {
        AntiReverse,
        Grade, AntiGrade,
        GeometricProduct, AntiGeometricProduct,
        WedgeProduct, AntiWedgeProduct,
        DotProduct, AntiDotProduct,
        ScalarProduct, CrossProduct,
        InnerProduct, AntiInnerProduct,
        LeftInteriorProduct, RightInteriorProduct, LeftInteriorAntiProduct, RightInteriorAntiProduct,
        BulkExpansion, BulkContraction,
        WeightExpansion, WeightContraction,
        Bulk, Weight,
        BulkNorm, WeightNorm,
        WeightUnitize,
        LeftComplement, RightComplement,
        Sandwich, AntiSandwich,
        Rejection,
        OrthoProjection, CentralProjection,
        OrthoAntiprojection, CentralAntiprojection,

        PGAProjection, PGARejection,
        PGASupport, PGAAntisupport, PGAAttitude
    }
]

