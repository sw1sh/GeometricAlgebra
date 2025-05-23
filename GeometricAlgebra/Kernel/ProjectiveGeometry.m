Package["Wolfram`GeometricAlgebra`ProjectiveGeometry`"]

PackageImport["Wolfram`GeometricAlgebra`"]

PackageExport[PGA]
PackageExport[PGAQ]
PackageExport[$PGA]
PackageExport[$2DPGA]
PackageExport[PGADimension]
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

PackageExport[PGAExpansion]
PackageExport[PGAContraction]

PackageExport[PGAProjection]
PackageExport[PGARejection]

PackageExport[PGADistance]

PackageExport[PGATransform]



PGA[3] = $PGA = $3DPGA = e = e3 = GeometricAlgebra[3, 0, 1, "Format" -> "PGA",
    "FormatIndex" -> Function[$DefaultMultivectorFormatFunction[#] /. {Subscript[_, Row[{1, 2, 3, 4}, _]] -> "\[DoubleStruckOne]"}],
    "Ordering" -> {{}, {1}, {2}, {3}, {4}, {4, 1}, {4, 2}, {4, 3}, {2, 3}, {3, 1}, {1, 2}, {4, 2, 3}, {4, 3, 1}, {4, 1, 2}, {3, 2, 1}, {1, 2, 3, 4}}
]

PGA[2] = $2DPGA = e2 = GeometricAlgebra[2, 0, 1, "Format" -> Subscript["PGA", "2D"],
    "FormatIndex" -> Function[$DefaultMultivectorFormatFunction[#] /. {Subscript[_, Row[{3, 2, 1}, _]] -> "\[DoubleStruckOne]"}],
    "Ordering" -> {{}, {1}, {2}, {3}, {2, 3}, {3, 1}, {1, 2}, {3, 2, 1}}
]

PGA[n_Integer ? Positive] := GeometricAlgebra[n, 0, 1, "Format" -> Subscript["PGA", n],
    "FormatIndex" -> With[{i = Range[n + 1]}, Function[$DefaultMultivectorFormatFunction[#] /. {Subscript[_, Row[i, _]] -> "\[DoubleStruckOne]"}]]
]

i = e[1, 2, 3, 4]
i2 = e2[1, 2, 3]

moment = e[{{2, 3}, {3, 1}, {1, 2}}]


PGAQ[x : _GeometricAlgebra | _Multivector] := MatchQ[x["Signature"], {_, 0, 1}]

PGA2DQ[x : _GeometricAlgebra | _Multivector] := x["Signature"] === {2, 0, 1}

PGA2DQ[___] := False

PGA3DQ[x : _GeometricAlgebra | _Multivector] := x["Signature"] === {3, 0, 1}

PGA3DQ[___] := False


PGADimension[x : _GeometricAlgebra | _Multivector] := Which[CGAQ[x], x["Dimension"] - 2, PGAQ[x], x["Dimension"] - 1, True, x["Dimension"]]


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

PGALine[InfiniteLine[{p_, q_}]] := Wedge[PGAPoint[p], PGAPoint[q]]
PGALine[InfiniteLine[p : {_, _, _}, v : {_, _, _}]] := PGALine[v, Cross[p, v]]
PGALine[InfiniteLine[p : {_, _}, {x_, y_}]] := PGALine[{y, -x}, Norm[p]]
PGALine[x_Multivector ? PGA2DQ] := Enclose[InfiniteLine[First @ Confirm @ PGAPoint[Support[x]], x[{{3, 1}, {3, 2}}]], Missing["Line"] &]
PGALine[x_Multivector ? PGA3DQ] := Enclose[InfiniteLine[First @ Confirm @ PGAPoint[Support[x]], x[{{4, 1}, {4, 2}, {4, 3}}]], Missing["Line"] &]
PGALine[n : {_, _}, d_ : 0] := n . e2[{{2, 3}, {3, 1}}] + d e2[1, 2] 
PGALine[v : {_, _, _}, m : {_, _, _}] := GeometricProduct[e[4], PGAVector[v]] + m . moment

PGAPlane[InfinitePlane[p : {_, _, _}, {u : {_, _, _}, v : {_, _, _}}]] := Wedge[PGAPoint[p], PGAPoint[p + u], PGAPoint[p + v]]
PGAPlane[(Triangle | InfinitePlane)[{p1 : {_, _, _}, p2 : {_, _, _}, p3 : {_, _, _}}]] := Wedge[PGAPoint[p1], PGAPoint[p2], PGAPoint[p3]]
PGAPlane[Hyperplane[n : {_, _, _}, p : {_, _, _}]] := PGAPlane[n, - n . p / Norm[n]]
PGAPlane[Hyperplane[n : {_, _, _}, d_]] := PGAPlane[n, - d]
PGAPlane[x_Multivector ? PGA3DQ] := With[{n = x[{{4, 2, 3}, {4, 3, 1}, {4, 1, 2}}], w = x[3, 2, 1]},
    Switch[Norm[n] != 0, False, Missing["Plane"], _, Hyperplane[n, - w]]
]
PGAPlane[n : {_, _, _}, w_ : 1] := n . e[{{4, 2, 3}, {4, 3, 1}, {4, 1, 2}}] + w e[3, 2, 1]

QuaternionToRotationMatrix[r_, x_, y_, z_] := With[{aim = Norm[{x, y, z}]},
	If[aim == 0, Return[IdentityMatrix[3]]];
	First @ LinearAlgebra`Private`MatrixPolynomial[
		{Prepend[2 aim {r, aim} / (r ^ 2 + aim ^ 2), 1]},
		- HodgeDual[{x, y, z} / aim]
	]
]

PGATranslator[t : {_, _}] := ({1, -1} t) . e2[{2, 1}] + i2
PGATranslator[t : {_, _, _}] := t . moment + i
PGATranslator[t_Multivector] := TranslationTransform[2 t[{{2, 3}, {3, 1}, {1, 2}}]]
PGATranslator[t_TransformationFunction] := PGATranslator[t["AffineVector"] / 2]

PGAReflector[n : {_, _, _}, w_ : 1] := PGAPlane[n, w]
PGAReflector[g : _Triangle | _InfinitePlane | _Hyperplane] := PGAPlane[g]
PGAReflector[r_Multivector ? PGA3DQ] := With[{n = r[{{4, 2, 3}, {4, 3, 1}, {4, 1, 2}}], w = r[3, 2, 1]},
    ReflectionTransform[n, - w * n / n . n]
]

PGARotor[l_Multivector, phi_] := WeightUnitize[l] * Sin[phi / 2] + GeometricAlgebra[l]["Pseudoscalar"] * Cos[phi / 2]
PGARotor[line : InfiniteLine[{{_, _, _}, {_, _, _}}], phi_] := PGARotor[PGALine[line], phi]
PGARotor[point : Point[{_, _}], phi_] := PGARotor[PGAPoint[point], phi]
PGARotor[r_Multivector] :=
    Which[
        PGA3DQ[r],
        AffineTransform[QuaternionToRotationMatrix @@ r[{{1, 2, 3, 4}, {4, 1}, {4, 2}, {4, 3}}]],

        PGA2DQ[r],
        RotationTransform[ArcTan[r[1, 2, 3, 4], r[3]], r[{1, 2}]]
    ]

PGAMotor[v : {_, _, _}, m : {_, _, _}, vw_ : 1, mw_ : 1] := mw + v . e[{{4, 1}, {4, 2}, {4, 3}}] + m . e[{{2, 3}, {3, 1}, {1, 2}}] + vw i
PGAMotor[r : Point[{_, _}] | _InfiniteLine | _Multivector, t : {_, _, _}, phi_ : 0] := PGAMotor[r, PGATranslator[t], phi]
PGAMotor[r : Point[{_, _}] | _InfiniteLine | _Multivector, t_Multivector, phi_ : 0] := AntiGeometricProduct[t, PGARotor[r, phi]]
PGAMotor[m_Multivector] := PGATranslator[AntiGeometricProduct[m, AntiReverse[Weight[m]]]] @* PGARotor[m]

PGAFlector[p : {_, _, _}, g : {_, _, _}, pw_ : 1, gw_ : 1] := PGAVector[Append[p, pw]] + GeometricProduct[e[4], g . moment] + gw e[3, 2, 1]
PGAFlector[r : InfiniteLine[{{_, _, _}, {_, _, _}}], g : _Triangle | _InfinitePlane | _Hyperplane, phi_ : 0] := AntiGeometricProduct[PGARotor[r, phi], g]
PGAFlector[r : Point[{_, _}], l : _InfiniteLine, phi_ : 0] := AntiGeometricProduct[PGARotor[r, phi], l]
PGAFlector[p : Point[{_, _, _}] | _Multivector, g : _Triangle | _InfinitePlane | _Hyperplane, phi_ : 0] := PGAFlector[PGALine[PGAExpansion[p, g]], g, phi]
PGAFlector[f_Multivector] := PGAReflector[f] @* PGARotor[AntiGeometricProduct[f, AntiReverse[PGAPlane[f]]]]


(* Unary operations *)



(* Binary operations *)

PGAProjection[a_Multivector, b_Multivector] := LeftInteriorProduct[RightInteriorProduct[Weight[b], a], b]

PGARejection[a_Multivector, b_Multivector] := LeftInteriorAntiProduct[RightInteriorAntiProduct[Weight[b], a], b]


PGAExpansion = WeightExpansion

PGAContraction = WeightContraction


PGADistance[a_Multivector, b_Multivector] := BulkNorm[Attitude[Wedge[a, b]]] + WeightNorm[Wedge[a, Attitude[b]]]


(* Transforms *)

PGATransform[x_, qs__] := AntiGeometricProduct[Sequence @@ Reverse[{qs}], x, Sequence @@ AntiReverse /@ {qs}]


(* Region constructions *)

Attributes[RegionPGA] = {Listable}

RegionPGA[point : _Point] := PGAPoint[point]

RegionPGA[line : _InfiniteLine] := PGALine[line]

RegionPGA[plane : _Triangle | _InfinitePlane | _Hyperplane] := PGAPlane[plane]

RegionPGA[b : _Disk | _Ball] := CGARoundPoint[b]

RegionPGA[s_Sphere] := CGASphere[s]

RegionPGA[d_Line | d_Tube] := CGADipole[d]

RegionPGA[c : _Circle | Inactive[ResourceFunction["Circle3D"]][__]] := CGACircle[c]


(* Region export *)

PGARegions[v_Multivector] /; PGA2DQ[v] := <|"Vector" -> Arrow[{{0, 0}, PGAVector[v]}], "Point" -> PGAPoint[v] , "Line" -> PGALine[v]|>

PGARegions[v_Multivector] /; PGA3DQ[v] := <|"Vector" -> Arrow[{{0, 0, 0}, PGAVector[v]}], "Point" -> PGAPoint[v] ,"Line" -> PGALine[v] ,"Plane" -> PGAPlane[v]|>



(* Overwrite mulitivector functions for regions *)

$RGARegion = _Point | _InfiniteLine | _Triangle | _InfinitePlane | _Hyperplane

$CGARegion = _Disk | _Ball | _Line | _Tube | _Circle | Inactive[ResourceFunction["Circle3D"]][__] | _Sphere

$PGARegion = $RGARegion | $CGARegion


Scan[
    Function[f,
        Scan[
            rule |-> If[! MemberQ[DownValues[f], Verbatim[rule]], PrependTo[DownValues[f], rule]],
            {
                HoldPattern[f[left___, x_ ? (MatchQ[$PGARegion]), right___]] :> f[left, RegionPGA[x], right],
                HoldPattern[f[left___, vs__Multivector, right___] /; AnyTrue[{vs}, PGAQ] && AnyTrue[{vs}, CGAQ]] :>
                    With[{g = largestGeometricAlgebra[vs]},
                        f[left, ##, right] & @@ ToCGA /@ {vs} 
                    ]
            }
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

        FlatPart, RoundPart,
        RoundBulk, FlatBulk,
        RoundWeight, FlatWeight,
        Carrier, Cocarrier, Container,
        Partner,

        PGAProjection, PGARejection
    }
]

