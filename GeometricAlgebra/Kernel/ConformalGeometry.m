Package["Wolfram`GeometricAlgebra`ProjectiveGeometry`"]

PackageImport["Wolfram`GeometricAlgebra`"]

PackageExport[CGA]
PackageExport[$CGA0]
PackageExport[$2DCGA]
PackageExport[$CGA]
PackageExport[CGAQ]

PackageExport[CGARegions]
PackageExport[CGAFlatPoint]
PackageExport[CGALine]
PackageExport[CGAPlane]
PackageExport[CGARoundPoint]
PackageExport[CGADipole]
PackageExport[CGACircle]
PackageExport[CGASphere]

PackageScope[CGA3DQ]
PackageScope[ToCGA]





$CGA0 = GeometricAlgebra[4, 1, "Format" -> "CGA0",
    "FormatIndex" -> Function[$DefaultMultivectorFormatFunction[#] /. {4 -> "-", UnderBar[1] -> "+", Subscript[_, Row[{1, 2, 3, 4, UnderBar[1]}, _]] -> "\[DoubleStruckOne]"}],
    "VectorBasis" -> {{1, 0, 0, 0, 0}, {0, 1, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, 0, 1}, {0, 0, 0, 1, 0}}
]

e = $3DCGA = $CGA = GeometricAlgebra[4, 1, "Format" -> "CGA",
    "FormatIndex" -> Function[$DefaultMultivectorFormatFunction[#] /. {UnderBar[1] -> 5, Subscript[_, Row[{1, 2, 3, 4, UnderBar[1]}, _]] -> "\[DoubleStruckOne]"}],
    "VectorBasis" -> {{1, 0, 0, 0, 0}, {0, 1, 0, 0, 0}, {0, 0, 1, 0, 0}, {0, 0, 0, - 1, 1 / 2}, {0, 0, 0, 1, 1 / 2}},
    "Ordering" -> {
        {}, {1}, {2}, {3}, {4}, {-1}, {4, 1}, {4, 2}, {4, 3}, {2, 3}, {3, 1}, {1, 2}, {1, -1}, {2, -1}, {3, -1}, {4, -1}, 
        {4, 2, 3}, {4, 3, 1}, {4, 1, 2}, {3, 2, 1}, {4, 1, -1}, {4, 2, -1}, {4, 3, -1}, {2, 3, -1}, {3, 1, -1}, {1, 2, -1},
        {1, 2, 3, 4}, {4, 2, 3, -1}, {4, 3, 1, -1}, {4, 1, 2, -1}, {3, 2, 1, -1}, {1, 2, 3, 4, -1}
    }
]

CGA[3] = $3DCGA

CGA[n_Integer ? Positive] := GeometricAlgebra[n + 1, 1, "Format" -> Subscript["CGA", n],
    "FormatIndex" -> Function[$DefaultMultivectorFormatFunction[#] /. {UnderBar[1] -> n + 2, Subscript[_, Row[Append[Range[n + 1], UnderBar[1]], _]] -> "\[DoubleStruckOne]"}],
    "VectorBasis" -> BlockDiagonalMatrix[{IdentityMatrix[n], {{- 1, 1 / 2}, {1, 1 / 2}}}]
]

$2DCGA = e2 = GeometricAlgebra[CGA[2],
    "Ordering" -> {
        {}, {1}, {2}, {3}, {4}, {2, 3}, {3, 1}, {1, 2}, {4, 1}, {4, 2}, {4, 3},
        {3, 2, 1}, {4, 2, 3}, {4, 3, 1}, {4, 1, 2}, {1, 2, 3, 4}
    }
]


CGAQ[x : _GeometricAlgebra | _Multivector] := MatchQ[x["Signature"], {_, 1, 0}]

CGA2DQ[x : _GeometricAlgebra | _Multivector] := x["Signature"] === {3, 1, 0}

CGA2DQ[___] := False

CGA3DQ[x : _GeometricAlgebra | _Multivector] := x["Signature"] === {4, 1, 0}

CGA3DQ[___] := False


ToCGA[v_Multivector] := With[{g = CGA[v["NonNegativeDimension"] - 1]},
    Which[
        CGAQ[v], v,
        PGAQ[v], Wedge[Multivector[v, g], g["Infinity"]],
        True, Multivector[v, g]
    ]
]

ToCGA[r : $CGARegion] := r

ToCGA[___] := Missing[]


(* Representations *)

CGAFlatPoint[args___] := ToCGA @ PGAPoint[args]
CGAFlatPoint[x_Multivector ? CGAQ] := With[{d = PGADimension[x]}, {p = x[{#, -1} & /@ Range[d]], w = x[d + 1, -1]},
    Switch[w == 0, True, Missing["FlatPoint"], _, Point[p / w]]
]

CGALine[args___] := ToCGA @ PGALine[args]
CGALine[x_Multivector ? CGAQ] := With[{d = PGADimension[x]},
    Enclose[InfiniteLine[First @ Confirm @ CGARoundPoint[Support[x]], x[{d + 1, #, -1} & /@ Range[d]]], Missing["Line"] &]
]

CGAPlane[args___] := ToCGA @ PGAPlane[args]
CGAPlane[x_Multivector ? CGA3DQ] := With[{n = x[{{4, 2, 3, 5}, {4, 3, 1, 5}, {4, 1, 2, 5}}], w = x[3, 2, 1, 5]},
    Switch[Norm[n] != 0, False, Missing["Plane"], _, Hyperplane[n, - w]]
]

CGARoundPoint[p_List, r_ : 0, w_ : 1] := With[{d = Length[p]}, {g = CGA[d]}, p . g[Range[d]] + w g["Origin"] + (p . p + r ^ 2) / 2 g["Infinity"]]
CGARoundPoint[Point[p_], r_ : 0, w_ : 1] := CGARoundPoint[p, r, w]
CGARoundPoint[Ball[p_, r_ : 0], w_ : 1] := CGARoundPoint[p, r, w]
CGARoundPoint[x_Multivector ? CGAQ] := With[{d = PGADimension[x]}, {p = x[Range[d]], w = x[d + 1]}, {r = Sqrt[2 x[-1] - p . p]},
    Switch[w == 0, True, Missing["RoundPoint"], _, Ball[p / w, r]]
]

CGADipole[p : {_, _, _}, v : {_, _, _}, m : {_, _, _}, pw_ : 1] := PGALine[v, m] + CGAFlatPoint[p, pw]
CGADipole[p : {_, _}, n : {_, _}, r_ : 0, pw_ : 1] :=
    n . e2[{{2, 3}, {3, 1}}] - Det[{p, n}] (p . e2[{{4, 1}, {4, 2}}] + e2[4, 3]) - p . n e2[1, 2] + (p . p + r ^ 2) / 2 n . e2[{{2, 4}, {4, 1}}]
CGADipole[p : {_, _, _}, n : {_, _, _}, r_ : 0, pw_ : 1] :=
	n . e[{{4, 1}, {4, 2}, {4, 3}}] + Cross[p, n] . e[{{2, 3}, {3, 1}, {1, 2}}] + p . n CGAFlatPoint[p, pw] - (p . p + r ^ 2) / 2 n . e[{{1, 5}, {2, 5}, {3, 5}}]
CGADipole[p_Point, q_Point] := Wedge[CGARoundPoint[p], CGARoundPoint[q]]
CGADipole[Line[{p1 : {_, _}, p2 : {_, _}}]] := With[{p = (p1 + p2) / 2, n = {-1, 1} Reverse[(p1 - p2) / 2]}, CGADipole[p, n, Norm[n]]]
CGADipole[Tube[{p1_List, p2_List}, r_]] := With[{p = (p1 + p2) / 2, n = (p2 - p1) / 2}, CGADipole[p, r * Normalize[n], r]]
CGADipole[Tube[{p1_List, p2_List}]] := With[{p = (p1 + p2) / 2, n = (p2 - p1) / 2}, CGADipole[p, n, Norm[n]]]
CGADipole[v_Multivector ? CGA2DQ] := ResourceFunction["CompoundScope"][
    n = v[{{2, 3}, {3, 1}}];
    nn = n . n;
    If[nn == 0, Return[Missing["Dipole"]]];
    pn = - v[1, 2];
    x = - v[4, 3];
    p = {{pn, x}, {-x, pn}} . n / nn;
    r = Abs @ Sqrt[2 (v[{{2, 4}, {4, 1}}] + x * Reverse[p] {-1, 1}) . n / nn - p . p];
    d = Reverse[n] {-1, 1} / Sqrt[nn]
    ,
    Line[{p - r d, p + r d}]
]
CGADipole[v_Multivector ? CGA3DQ] := ResourceFunction["CompoundScope"][
    n = v[{{4, 1}, {4, 2}, {4, 3}}];
    nn = n . n;
    If[nn == 0, Return[Missing["Dipole"]]];
    pn = v[4, 5];
    x = v[{{2, 3}, {3, 1}, {1, 2}}];
    p = (Cross[n, x] + pn * n) / nn;
    r = Abs @ Sqrt[2 (pn * p - v[{{1, 5}, {2, 5}, {3, 5}}]) . n / nn - p . p];
    ,
    Tube[{p - r n / Sqrt[nn], p + r n / Sqrt[nn]}]
]

CGACircle[n : {_, _, _}, v : {_, _, _}, m : {_, _, _}, w_ : 1] := PGAPlane[n, w] + CGALine[v, m]
CGACircle[p : {_, _}, r_ : 0] :=
    p . e2[{{4, 2, 3}, {4, 3, 1}}] - e2[3, 2, 1] - (p . p - r ^ 2) / 2 e2[4, 1, 2]
CGACircle[p : {_, _, _}, n : {_, _, _}, r_ : 1] :=
    n . e[{{4, 2, 3}, {4, 3, 1}, {4, 1, 2}}] + Cross[p, n] . e[{{4, 1, 5}, {4, 2, 5}, {4, 3, 5}}] + p . n (p . e[{{2, 3, 5}, {3, 1, 5}, {1, 2, 5}}] - e[3, 2, 1]) - (p . p - r ^ 2) / 2 n . e[{{2, 3, 5}, {3, 1, 5}, {1, 2, 5}}]
CGACircle[Circle[p : {_, _}, r_ : 1]] := CGACircle[p, r]
CGACircle[Inactive[ResourceFunction["Circle3D"]][p : {_, _, _}, {r_, _}, psi_, zeta_]] := CGACircle[p, {Cos[psi] Cos[zeta], Sin[zeta], -Cos[zeta] Sin[psi]}, r]
CGACircle[v_Multivector ? CGA2DQ] := With[{p = v[{{4, 2, 3}, {4, 3, 1}}], w = - v[3, 2, 1]}, If[w == 0, Return[Missing["Circle"]]]; Circle[p / w, Abs[Sqrt[2 v[4, 1, 2] / w + p . p]]]]
CGACircle[v_Multivector ? CGA3DQ] := ResourceFunction["CompoundScope"][
    n = v[{{4, 2, 3}, {4, 3, 1}, {4, 1, 2}}];
    nn = n . n;
    If[nn == 0, Return[Missing["Circle"]]];
    pn = - v[3, 2, 1];
    x = v[{{4, 1, 5}, {4, 2, 5}, {4, 3, 5}}];
    p = (Cross[n, x] + pn * n) / nn;
    r = Abs @ Sqrt[2 (v[{{2, 3, 5}, {3, 1, 5}, {1, 2, 5}}] - pn * p) . n / nn + p . p];
    psi = - ArcTan[n[[1]], n[[3]]];
    zeta = ArcSin[n[[2]] / Sqrt[nn]]
    ,
    Inactive[ResourceFunction["Circle3D"]][p, Abs[{r, r}], psi, zeta]
]

CGASphere[n : {_, _, _}, u_, w_] := CGAPlane[n, w] + u e[1, 2, 3, 4]
CGASphere[p : {_, _, _}, r_ : 1] := p . {e[4, 2, 3, 5], e[4, 3, 1, 5], e[4, 1, 2, 5]} - e[1, 2, 3, 4] - (p . p - r ^ 2) / 2 e[3, 2, 1, 5]
CGASphere[Sphere[c : {_, _, _}, r_ : 1]] := CGASphere[c, r]
CGASphere[v_Multivector ? CGA3DQ] := With[{w = v[1, 2, 3, 4]},
    If[ w == 0,
        Missing["Sphere"],
        With[{c = - v[{{4, 2, 3, 5}, {4, 3, 1, 5}, {4, 1, 2, 5}}] / w},
            Sphere[c, Sqrt[c . c - 2 v[3, 2, 1, 5] / w]]
        ]
    ]
]


(* Unary operations *)



(* Region export *)

CGARegions[v_Multivector ? CGAQ] := <|
    "FlatPoint" -> CGAFlatPoint[v],
    "RoundPoint" -> CGARoundPoint[v],
    "Line" -> CGALine[v],
    "Plane" -> CGAPlane[v],
    "Sphere" -> CGASphere[v],
    Activate["Circle" :> Evaluate[CGACircle[v]]],
    "Dipole" -> CGADipole[v]
|>

