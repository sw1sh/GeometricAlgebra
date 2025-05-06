Package["Wolfram`GeometricAlgebra`ProjectiveGeometry`"]

PackageImport["Wolfram`GeometricAlgebra`"]

PackageExport[$PGA]
PackageExport[RegionPGA]
PackageExport[PGARegions]
PackageExport[PGAVectorQ]

PackageExport[PGAMagnitude]
PackageExport[PGAPoint]
PackageExport[PGALine]
PackageExport[PGAPlane]
PackageExport[PGAMotor]
PackageExport[PGAFlector]

PackageExport[PGABulk]
PackageExport[PGARightBulkDual]
PackageExport[PGALeftBulkDual]
PackageExport[PGAWeight]
PackageExport[PGARightWeightDual]
PackageExport[PGALeftWeightDual]
PackageExport[PGAAttitude]

PackageExport[PGABulkNorm]
PackageExport[PGAWeightNorm]
PackageExport[PGANorm]

PackageExport[RightInteriorProduct]
PackageExport[LeftInteriorProduct]
PackageExport[RightInteriorAntiProduct]
PackageExport[LeftInteriorAntiProduct]
PackageExport[PGAProjection]
PackageExport[PGARejection]
PackageExport[PGABulkContraction]
PackageExport[PGAWeightContraction]
PackageExport[PGABulkExpansion]
PackageExport[PGAWeightExpansion]



$PGA = e = GeometricAlgebra[3, 0, 1, "Format" -> "PGA",
    "FormatIndex" -> {
        {{}, {1}, {2}, {3}, {4}, {4, 1}, {4, 2}, {4, 3}, {2, 3}, {3, 1}, {1, 2}, {4, 2, 3}, {4, 3, 1}, {4, 1, 2}, {3, 2, 1}, {1, 2, 3, 4}},
        Function[$DefaultMultivectorFormatFunction[#] /. {Subscript[_, Row[{1, 2, 3, 4}, _]] -> "\[DoubleStruckOne]"}]
    }
]

eps = e[4]
i = e[1, 2, 3, 4]


PGAVectorQ[v_Multivector] := v["Signature"] === {3, 0, 1}

pseudoVector[v : {_, _, _}] := Reverse[v] {1, -1, 1}


(* Constructors *)

PGAMagnitude[x_ : 1, y_ : 1] := x + y i

PGAPoint[p : {_, _, _} : {0, 0, 0}, w_ : 1] := Grade[Append[p, w], 1, $PGA]

PGALine[v : {_, _, _}, m : {_, _, _}] := eps ** Grade[v, 1, $PGA] + Grade[- m, 1, $PGA]["Dual"]

PGAPlane[n : {_, _, _} : {0, 0, 1}, d_ : 0] := Grade[Prepend[pseudoVector[n], - d], 3, $PGA]

PGAMotor[v : {_, _, _}, m : {_, _, _}, vw_ : 1, mw_ : 1] := mw + v . e[{{4, 1}, {4, 2}, {4, 3}}] + m . e[{{2, 3}, {3, 1}, {1, 2}}] + vw i

PGAFlector[p : {_, _, _}, g : {_, _, _}, pw_ : 1, gw_ : 1] := Grade[Append[p, pw], 1, $PGA] + eps ** Grade[- g, 1, $PGA]["Dual"] + gw e[3, 2, 1]


(* Unary operations *)

PGABulk[v_Multivector] := Multivector[KeySelect[v["Association"], FreeQ[4]], v["GeometricAlgebra"]]

PGARightBulkDual[v_Multivector] := OverBar[PGABulk[v]]

PGALeftBulkDual[v_Multivector] := UnderBar[PGABulk[v]]

PGAWeight[v_Multivector] := Multivector[KeySelect[v["Association"], Not @* FreeQ[4]], v["GeometricAlgebra"]]

PGARightWeightDual[v_Multivector] := OverBar[PGAWeight[v]]

PGALeftWeightDual[v_Multivector] := UnderBar[PGAWeight[v]]

PGAAttitude[v_Multivector] := Vee[v, OverBar[eps]]


(* Norms *)

PGABulkNorm[a_Multivector] := Sqrt[a ** OverTilde[a]]

PGAWeightNorm[a_Multivector] := Sqrt[AntiGeometricProduct[a, AntiReverse[a]]]
 
PGANorm[a_Multivector] := PGABulkNorm[a] + PGAWeightNorm[a]


(* Binary operations *)

RightInteriorProduct[a_Multivector, b_Multivector] := Vee[a, OverBar[b]]

LeftInteriorProduct[a_Multivector, b_Multivector] := Vee[UnderBar[a], b]

RightInteriorAntiProduct[a_Multivector, b_Multivector] := Wedge[a, OverBar[b]]

LeftInteriorAntiProduct[a_Multivector, b_Multivector] := Wedge[UnderBar[a], b]

PGAProjection[a_Multivector, b_Multivector] := LeftInteriorProduct[RightInteriorProduct[PGAWeight[b], a], b]

PGARejection[a_Multivector, b_Multivector] := LeftInteriorAntiProduct[RightInteriorAntiProduct[PGAWeight[b], a], b]

PGABulkContraction[a_Multivector, b_Multivector] := Vee[a, PGARightBulkDual[b]]

PGAWeightContraction[a_Multivector, b_Multivector] := Vee[a, PGARightWeightDual[b]]

PGABulkExpansion[a_Multivector, b_Multivector] := Wedge[a, PGARightBulkDual[b]]

PGAWeightExpansion[a_Multivector, b_Multivector] := Wedge[a, PGARightWeightDual[b]]


(* Region constructions *)

Attributes[RegionPGA] = {Listable}

RegionPGA[Point[p_]] := PGAPoint[p]

RegionPGA[(Line | InfiniteLine)[{p_, q_}]] := Wedge[RegionPGA[Point[p]], RegionPGA[Point[q]]]

RegionPGA[InfiniteLine[p_, v_]] := PGALine[v, Cross[p, v]]

RegionPGA[(Triangle | InfinitePlane)[{p_, q_, r_}]] := Wedge[RegionPGA[Point[p]], RegionPGA[Point[q]], RegionPGA[Point[r]]]

RegionPGA[InfinitePlane[p_, {u_, v_}]] := Wedge[RegionPGA[Point[p]], RegionPGA[Point[p + u]] RegionPGA[Point[p + v]]]

RegionPGA[Hyperplane[n_, d_]] := PGAPlane[n, d]


(* Region export *)

PGARegions[v_Multivector] /; PGAVectorQ[v] := Quiet @ With[{
    c1 = v["Coordinates", 1],
    c2 = v["Coordinates", 2],
    c3 = v["Coordinates", 3]
},
    <|
        "Point" -> Check[
            Point[Normal[c1[[;; 3]] / c1[[4]]]],
            Missing[]
        ]
    ,
        "Line" -> Check[
            With[{p = Normal @ PGAProjection[PGAPoint[{0, 0, 0}], v["Grade", 2]]["Coordinates", 1]},
                InfiniteLine[p[[;; 3]] / p[[4]], - c2[[{3, 5, 6}]] // Normal]
            ],
            Missing[]
        ]
    ,
        "Plane" -> Check[Hyperplane[Normal @ pseudoVector @ c3[[2 ;;]], c3[[1]]], Missing[]]
    |>
]

