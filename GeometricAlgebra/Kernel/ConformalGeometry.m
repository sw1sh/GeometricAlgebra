Package["Wolfram`GeometricAlgebra`ProjectiveGeometry`"]

PackageImport["Wolfram`GeometricAlgebra`"]

PackageExport[CGA]
PackageExport[$CGA0]
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


CGAQ[x : _GeometricAlgebra | _Multivector] := MatchQ[x["Signature"], {_, 1, 0}]

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

ToCGA[___] := $$Failed


(* Representations *)

CGAFlatPoint[args___] := ToCGA @ PGAPoint[args]
CGAFlatPoint[x_Multivector ? CGA3DQ] := With[{p = x[{{1, 5}, {2, 5}, {3, 5}}], w = x[4, 5]},
    Switch[w == 0, True, Missing["FlatPoint"], _, Point[p / w]]
]

CGALine[args___] := ToCGA @ PGALine[args]
CGALine[x_Multivector ? CGA3DQ] := Enclose[InfiniteLine[First @ Confirm @ CGARoundPoint[Support[x]], x[{{4, 1, 5}, {4, 2, 5}, {4, 3, 5}}]], Missing["Line"] &]

CGAPlane[args___] := ToCGA @ PGAPlane[args]
CGAPlane[x_Multivector ? CGA3DQ] := With[{n = x[{{4, 2, 3, 5}, {4, 3, 1, 5}, {4, 1, 2, 5}}], w = x[3, 2, 1, 5]},
    Switch[Norm[n] != 0, False, Missing["Plane"], _, Hyperplane[n, - w]]
]

CGARoundPoint[p : {_, _, _}, r_ : 0, w_ : 1] := p . {e[1], e[2], e[3]} + w e[4] + (p . p + r ^ 2) / 2 e[5]
CGARoundPoint[Point[p_], r_ : 0, w_ : 1] := CGARoundPoint[p, r, w]
CGARoundPoint[Ball[p : {_, _, _}, r_ : 0], w_ : 1] := CGARoundPoint[p, r, w]
CGARoundPoint[x_Multivector ? CGA3DQ] := With[{p = x[{{1}, {2}, {3}}], w = x[4]}, {r = Sqrt[2 x[5] - p . p]},
    Switch[w == 0, True, Missing["RoundPoint"], _, Ball[p / w, r]]
]

CGADipole[p : {_, _, _}, v : {_, _, _}, m : {_, _, _}, pw_ : 1] := PGALine[v, m] + CGAFlatPoint[p, pw]
CGADipole[p : {_, _, _}, n : {_, _, _}, r_ : 0, pw_ : 1] :=
	n . e[{{4, 1}, {4, 2}, {4, 3}}] + Cross[p, n] . e[{{2, 3}, {3, 1}, {1, 2}}] + p . n CGAFlatPoint[p, pw] - (p . p + r ^ 2) / 2 n . e[{{1, 5}, {2, 5}, {3, 5}}]
CGADipole[p_Point, q_Point] := Wedge[CGARoundPoint[p], CGARoundPoint[q]]
CGADipole[Tube[{p1 : {_, _, _}, p2 : {_, _, _}}, r_]] := With[{p = (p1 + p2) / 2, n = (p2 - p1) / 2}, CGADipole[p, r * Normalize[n], r]]
CGADipole[Tube[{p1 : {_, _, _}, p2 : {_, _, _}}]] := With[{p = (p1 + p2) / 2, n = (p2 - p1) / 2}, CGADipole[p, n, Norm[n]]]
CGADipole[v_Multivector ? CGAQ] := ResourceFunction["CompoundScope"][
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
CGACircle[p : {_, _, _}, n : {_, _, _}, r_ : 1] :=
    n . e[{{4, 2, 3}, {4, 3, 1}, {4, 1, 2}}] + Cross[p, n] . e[{{4, 1, 5}, {4, 2, 5}, {4, 3, 5}}] + p . n (p . e[{{2, 3, 5}, {3, 1, 5}, {1, 2, 5}}] - e[3, 2, 1]) - (p . p - r ^ 2) / 2 n . e[{{2, 3, 5}, {3, 1, 5}, {1, 2, 5}}]
CGACircle[Inactive[ResourceFunction["Circle3D"]][p : {_, _, _}, {r_, _}, psi_, zeta_]] := CGACircle[p, {Cos[psi] Cos[zeta], Sin[zeta], -Cos[zeta] Sin[psi]}, r]
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

CGARegions[v_Multivector ? CGAQ] := DeleteMissing @ <|
    "FlatPoint" -> CGAFlatPoint[v],
    "RoundPoint" -> CGARoundPoint[v],
    "Line" -> CGALine[v],
    "Plane" -> CGAPlane[v],
    "Sphere" -> CGASphere[v],
    If[MissingQ[#], "Circle" -> #, "Circle" :> ResourceFunction["Circle3D"][##] & @@ #] & @ CGACircle[v],
    "Dipole" -> CGADipole[v]
|>

