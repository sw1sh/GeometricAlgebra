Package["Wolfram`GeometricAlgebra`ProjectiveGeometry`"]

PackageImport["Wolfram`GeometricAlgebra`"]

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



$PGA = $3DPGA = e = e3 = GeometricAlgebra[3, 0, 1, "Format" -> "PGA",
    "FormatIndex" -> Function[$DefaultMultivectorFormatFunction[#] /. {Subscript[_, Row[{1, 2, 3, 4}, _]] -> "\[DoubleStruckOne]"}],
    "Ordering" -> {{}, {1}, {2}, {3}, {4}, {4, 1}, {4, 2}, {4, 3}, {2, 3}, {3, 1}, {1, 2}, {4, 2, 3}, {4, 3, 1}, {4, 1, 2}, {3, 2, 1}, {1, 2, 3, 4}}
]

$2DPGA = e2 = GeometricAlgebra[2, 0, 1, "Format" -> Subscript["PGA", "2D"],
    "FormatIndex" -> Function[$DefaultMultivectorFormatFunction[#] /. {Subscript[_, Row[{3, 2, 1}, _]] -> "\[DoubleStruckOne]"}],
    "Ordering" -> {{}, {1}, {2}, {3}, {2, 3}, {3, 1}, {1, 2}, {3, 2, 1}}
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

PGAVector[v_Multivector ? PGA2DQ] := With[{p = v[{{1}, {2}}], w = v[3]},
    Switch[w == 0, True, Missing["Vector"], _, p / w]
]
PGAVector[v_Multivector ? PGA3DQ] := With[{p = v[{{1}, {2}, {3}}], w = v[4]},
    Switch[w == 0, True, Missing["Vector"], _, p / w]
]
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
PGAPoint[p : {_, _}, w_ : 1] := Grade[Append[p, w], 1, $2DPGA]
PGAPoint[p : {_, _, _}, w_ : 1] := Grade[Append[p, w], 1, $PGA]

PGALine[(Line | InfiniteLine)[{p_, q_}]] := Wedge[PGAPoint[p], PGAPoint[q]]
PGALine[InfiniteLine[p_, v_]] := PGALine[v, Cross[p, v]]
PGALine[x_Multivector ? PGA2DQ] := With[{n = x[{{2, 3}, {3, 1}}], p = PGAVector[PGASupport[x]]},
    Switch[Norm[n] == 0, True, Missing["Line"], _, InfiniteLine[p, {1, -1} Reverse[n]]]
]
PGALine[x_Multivector ? PGA3DQ] := With[{v = x[{{4, 1}, {4, 2}, {4, 3}}], m = x[{{2, 3}, {3, 1}, {1, 2}}]},
    Switch[m == {0, 0, 0} || Chop[v . m] == 0 && v . v != 0 && m . m != 0, False, Missing["Line"], _,
        InfiniteLine[PGAVector[PGASupport[x]], v]
    ]
]
PGALine[n : {_, _}, d_ : 0] := n . e2[{{2, 3}, {3, 1}}] + d e2[1, 2] 
PGALine[v : {_, _, _}, m : {_, _, _} : {0, 0, 0}] := e[4] ** PGAVector[v] + m . e[{{2, 3}, {3, 1}, {1, 2}}]

PGAPlane[InfinitePlane[p : {_, _, _}, {u : {_, _, _}, v : {_, _, _}}]] := Wedge[PGAPoint[p], PGAPoint[p + u] PGAPoint[p + v]]
PGAPlane[(Triangle | InfinitePlane)[{p1 : {_, _, _}, p2 : {_, _, _}, p3 : {_, _, _}}]] := Wedge[PGAPoint[p1], PGAPoint[p2], PGAPoint[p3]]
PGAPlane[Hyperplane[n : {_, _, _}, p : {_, _, _}]] := PGAPlane[n, - n . p / Norm[n]]
PGAPlane[Hyperplane[n : {_, _, _}, d_]] := PGAPlane[n, - d]
PGAPlane[x_Multivector ? PGA3DQ] := With[{n = x[{{4, 2, 3}, {4, 3, 1}, {4, 1, 2}}], w = x[3, 2, 1]},
    Switch[Norm[n] != 0, False, Missing["Plane"], _, Hyperplane[n, - w]]
]
PGAPlane[n : {_, _, _} : {0, 0, 1}, w_ : 1] := Grade[Prepend[pseudoVector[n], - w], 3, $PGA]

PGAMotor[v : {_, _, _}, m : {_, _, _}, vw_ : 1, mw_ : 1] := mw + v . e[{{4, 1}, {4, 2}, {4, 3}}] + m . e[{{2, 3}, {3, 1}, {1, 2}}] + vw i

PGAFlector[p : {_, _, _}, g : {_, _, _}, pw_ : 1, gw_ : 1] := PGAVector[Append[p, pw]] + e[4] ** (g . moment) + gw e[3, 2, 1]


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


(* Region constructions *)

Attributes[RegionPGA] = {Listable}

RegionPGA[point : _Point] := PGAPoint[point]

RegionPGA[line : _Line | _InfiniteLine] := PGALine[line]

RegionPGA[plane : _Triangle | _InfinitePlane | _Hyperplane] := PGAPlane[plane]


(* Region export *)

PGARegions[v_Multivector] /; PGA3DQ[v] := DeleteMissing @ <|"Point" -> PGAPoint[v] , "Line" -> PGALine[v] , "Plane" -> PGAPlane[v]|>
