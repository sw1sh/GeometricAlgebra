Package["Wolfram`GeometricAlgebra`ConformalGeometry`"]

PackageImport["Wolfram`GeometricAlgebra`"]

PackageExport[$CGA0]
PackageExport[$CGA]
PackageExport[CGAQ]

PackageExport[CGARoundBulk]
PackageExport[CGAFlatBulk]
PackageExport[CGARoundWeight]
PackageExport[CGAFlatWeight]
PackageExport[CGAAttitude]
PackageExport[Carrier]
PackageExport[Cocarrier]

PackageExport[RegionCGA]
PackageExport[CGARegions]
PackageExport[CGAFlatPoint]
PackageExport[CGALine]
PackageExport[CGAPlane]
PackageExport[CGARoundPoint]
PackageExport[CGADipole]
PackageExport[CGACircle]
PackageExport[CGASphere]



$CGA0 = GeometricAlgebra[4, 1, "Format" -> "CGA0",
    "FormatIndex" -> Function[$DefaultMultivectorFormatFunction[#] /. {4 -> "-", UnderBar[1] -> "+", Subscript[_, Row[{1, 2, 3, 4, UnderBar[1]}, _]] -> "\[DoubleStruckOne]"}],
    "VectorBasis" -> {{1, 0, 0, 0, 0}, {0, 1, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 0, 1}, {0, 0, 0, 1, 0}}
]

$CGA = GeometricAlgebra[4, 1, "Format" -> "CGA",
    "FormatIndex" -> Function[$DefaultMultivectorFormatFunction[#] /. {UnderBar[1] -> 5, Subscript[_, Row[{1, 2, 3, 4, UnderBar[1]}, _]] -> "\[DoubleStruckOne]"}],
    "VectorBasis" -> {{1, 0, 0, 0, 0}, {0, 1, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, - 1, 1 / 2}, {0, 0, 0, 1, 1 / 2}}
]
e = $CGA

CGAQ[v_Multivector] := GeometricAlgebra[v] == $CGA
CGAQ[___] := False


(* Representations *)

CGAFlatPoint[p : {_, _, _}, w_ : 1] := p . {e[1, 5], e[2, 5], e[3, 5]} + w e[4, 5]

CGALine[m : {_, _, _}, v : {_, _, _}] := m . e[{{2, 3, 5}, {3, 1, 5}, {1, 2, 5}}] + v . e[{{4, 1, 5}, {4, 2, 5}, {4, 3, 5}}]

CGAPlane[n : {_, _, _}, c_] := n . e[{{4, 2, 3, 5}, {4, 3, 1, 5}, {4, 1, 2, 5}}] + c e[3, 2, 1, 5]

CGARoundPoint[p : {_, _, _}, r_ : 1, w_ : 1] := p . {e[1], e[2], e[3]} + w e[4] + (p . p + r ^ 2) / 2 e[5]

CGADipole[p : {_, _, _}, n : {_, _, _}, r_ : 0] :=
	n . {e[4, 1], e[4, 2], e[4, 3]} + Cross[p, n] . {e[2, 3], e[3, 1], e[1, 2]} + p . n CGAFlatPoint[p] - (p . p + r ^ 2) / 2 n . {e[1, 5], e[2, 5], e[3, 5]}

CGACircle[p : {_, _, _}, n : {_, _, _}, r_ : 1] :=
    n . {e[4, 2, 3], e[4, 3, 1], e[4, 1, 2]} + Cross[p, n] . {e[4, 1, 5], e[4, 2, 5], e[4, 3, 5]} + p . n (p . {e[2, 3, 4], e[3, 1, 5], e[1, 2, 5]} - e[3, 2, 1]) - (p . p - r ^ 2) / 2 n . {e[2, 3, 5], e[3, 1, 5], e[1, 2, 5]}

CGASphere[p : {_, _, _}, r_ : 1] := p . {e[4, 2, 3, 5], e[4, 3, 1, 5], e[4, 1, 2, 5]} - e[1, 2, 3, 4] - (p . p - r ^ 2) / 2 e[3, 2, 1, 5]


(* Unary operations *)

CGARoundBulk[v_Multivector ? CGAQ] := Multivector[KeySelect[v["Association"], FreeQ[4 | -1]], $CGA]

CGAFlatBulk[v_Multivector ? CGAQ] := Multivector[KeySelect[v["Association"], FreeQ[#, 4] && ! FreeQ[#, -1] &], $CGA]

CGARoundWeight[v_Multivector ? CGAQ] := Multivector[KeySelect[v["Association"], ! FreeQ[#, 4] && FreeQ[#, -1] &], $CGA]

CGAFlatWeight[v_Multivector ? CGAQ] := Multivector[KeySelect[v["Association"], ! FreeQ[#, 4] && ! FreeQ[#, -1] &], $CGA]

CGAAttitude[v_Multivector ? CGAQ] := Vee[v, e[3, 2, 1, 5]]

Carrier[v_Multivector ? CGAQ] := Wedge[v, e[5]]

Cocarrier[v_Multivector ? CGAQ] := Wedge[v, e[5]]


(* Binary operations *)



(* Region constructions *)

RegionCGA[Point[r_]] := CGAFlatPoint[r]

RegionCGA[(Line | InfiniteLine)[{p : {_, _, _}, q : {_, _, _}}]] := RegionCGA[InfiniteLine[p, q - p]]

RegionCGA[InfiniteLine[p : {_, _, _}, d : {_, _, _}]] := CGALine[Cross[p, d], d]

RegionCGA[Hyperplane[n : {_, _, _}, p : {_, _, _}]] := RegionCGA[Hyperplane[n, Norm[p]]]

RegionCGA[Hyperplane[n : {_, _, _}, c_]] := CGAPlane[n, c]

RegionCGA[Sphere[c : {_, _, _}, r_]] := CGASphere[c, r]

RegionCGA[Tube[{{p1 : {_, _, _}, p2 : {_, _, _}}, r_}]] := CGADipole[(p1 + p2) / 2, (p2 - p1) / 2, r]


(* Region export *)

CGARegions[v_Multivector ? CGAQ] := Quiet @ <|
    "FlatPoint" -> With[{w = v[4, 5]}, If[w == 0, Missing[], Point[v[{{1, 5}, {2, 5}, {3, 5}}] / w]]],
    "RoundPoint" -> With[{w = v[4]}, If[w == 0, Missing[], Point[v[{{1}, {2}, {3}}] / w]]],
    "Line" -> With[{m = v[{{2, 3, 5}, {3, 1, 5}, {1, 2, 5}}], d = v[{{4, 1, 5}, {4, 2, 5}, {4, 3, 5}}]},
        If[Norm[d] == 0, Missing[], InfiniteLine[Cross[d, m] / d . d, d]]
    ],
    "Plane" -> With[{n = v[{{4, 2, 3, 5}, {4, 3, 1, 5}, {4, 1, 2, 5}}]},
        If[Norm[n] == 0, Missing[], Hyperplane[n, v[3, 2, 1, 5]]]
    ],
    "Sphere" -> With[{w = v[1, 2, 3, 4]},
        If[ w == 0,
            Missing[],
            With[{c = - v[{{4, 2, 3, 5}, {4, 3, 1, 5}, {4, 1, 2, 5}}] / w},
                Sphere[c, Sqrt[c . c + 2 v[3, 2, 1, 5]]]
            ]
        ]
    ],
    ResourceFunction["CompoundScope"][
        n = v[{{4, 2, 3}, {4, 3, 1}, {4, 1, 2}}];
        nn = n . n;
        If[nn == 0, Return["Circle" -> Missing[]]];
        pn = - v[3, 2, 1];
        x = v[{{4, 1, 5}, {4, 2, 5}, {4, 3, 5}}];
        p = (Cross[n, x] + pn * n) / nn;
        r = Sqrt[2 (v[{{2, 3, 5}, {3, 1, 5}, {1, 2, 5}}] - pn * p) . n / nn + p . p];
        psi = - ArcTan[n[[1]], n[[3]]];
        zeta = ArcSin[n[[2]] / Sqrt[nn]]
        ,
        "Circle" :> ResourceFunction["Circle3D"][p, Abs[{r, r}], psi, zeta]
    ],
    "Dipole" -> ResourceFunction["CompoundScope"][
        n = v[{{4, 1}, {4, 2}, {4, 3}}];
        nn = n . n;
        If[nn == 0, Return[Missing[]]];
        pn = v[4, 5];
        x = v[{{2, 3}, {3, 1}, {1, 2}}];
        p = (Cross[n, x] + pn * n) / nn;
        r = Sqrt[2 (pn * p - v[{{1, 5}, {2, 5}, {3, 5}}]) . n / nn - p . p];
        ,
        Tube[{{p - r n / nn, p + r n / nn}}, r]
    ]
|>

