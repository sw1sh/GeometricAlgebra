Package["Wolfram`GeometricAlgebra`ProjectiveGeometry`"]

PackageImport["Wolfram`GeometricAlgebra`"]

PackageExport[$PGA]
PackageExport[RegionPGA]
PackageExport[PGARegions]
PackageExport[PGAVectorQ]
PackageExport[PGAPoint]
PackageExport[PGALine]
PackageExport[PGAPlane]

PackageExport[PGABulk]
PackageExport[PGARightBulkDual]
PackageExport[PGALeftBulkDual]
PackageExport[PGAWeight]
PackageExport[PGARightWeightDual]
PackageExport[PGALeftWeightDual]

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



$PGA = GeometricAlgebra[3, 0, 1, "Format" -> "PGA"]

eps = Multivector[<|{4} -> 1|>, $PGA]


PGAVectorQ[v_Multivector] := v["Signature"] === {3, 0, 1}

pseudoVector[v : {_, _, _}] := Reverse[v] {1, -1, 1}


(* Constructors *)

PGAPoint[p : {_, _, _}, w_ : 1] := Grade[Append[p, w], 1, $PGA]

PGALine[v : {_, _, _}, m : {_, _, _}] := eps ** Grade[v, 1, $PGA] + Grade[- m, 1, $PGA]["Dual"]

PGAPlane[n : {_, _, _}, d_] := Grade[Prepend[pseudoVector[n], - d], 3, $PGA]


(* Unary operations *)

PGABulk[v_Multivector] := Multivector[KeySelect[v["Association"], FreeQ[4]], v["GeometricAlgebra"]]

PGARightBulkDual[v_Multivector] := PGABulk[v]["RightComplement"]

PGALeftBulkDual[v_Multivector] := PGABulk[v]["LeftComplement"]

PGAWeight[v_Multivector] := Multivector[KeySelect[v["Association"], Not @* FreeQ[4]], v["GeometricAlgebra"]]

PGARightWeightDual[v_Multivector] := PGAWeight[v]["RightComplement"]

PGALeftWeightDual[v_Multivector] := PGAWeight[v]["LeftComplement"]

(* Norms *)

PGABulkNorm[a_Multivector] := Sqrt[a ** a["Reverse"]]

PGAWeightNorm[a_Multivector] := Sqrt[AntiGeometricProduct[a, a["AntiReverse"]]]
 
PGANorm[a_Multivector] := PGABulkNorm[a] + PGAWeightNorm[a]

(* Binary operations *)

RightInteriorProduct[a_Multivector, b_Multivector] := Vee[a, b["RightComplement"]]

LeftInteriorProduct[a_Multivector, b_Multivector] := Vee[a["LeftComplement"], b]

RightInteriorAntiProduct[a_Multivector, b_Multivector] := Wedge[a, b["RightComplement"]]

LeftInteriorAntiProduct[a_Multivector, b_Multivector] := Wedge[a["LeftComplement"], b]

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

