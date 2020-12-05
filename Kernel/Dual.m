Package["GeometricAlgebra`"]

PackageExport["Dual"]
PackageExport["DualRe"]
PackageExport["DualEps"]


Needs["GeneralUtilities`"]


Attributes[Dual] = {Listable};


Dual /: DualRe[Dual[x_, _]] := x

DualRe[x : Except[_Dual]] := x

Attributes[DualRe] = {Listable, NumericFunction};


Dual /: DualEps[Dual[_, y_]] := y

DualEps[Except[_Dual]] := 0

Attributes[DualEps] = {Listable, NumericFunction};


Dual[x : Except[_Dual]] := Dual[x, 0]


Dual[x_, 0] := x


Dual /: Dual[d_Dual] := d


(* Dual magic here *)
applyDuals[f_, values_List] := With[{
    n = Length @ values,
    x = DualRe @ values,
    y = DualEps @ values
},
    Dual[f @@ x, Total @
        MapThread[
            (* Hack: instead of taking limit of exact derivative, 
                assume indeterminate expression is zero and suppress the warning: Derivative[0, 1][Power][0, 0] should be zero *)
            #2 Quiet[#1 @@ x, Infinity::indet] /. Indeterminate -> 0 &,
            {Map[Apply[Derivative, UnitVector[n, #]][f] &, Range@n], y}
        ]
    ]
]


Dual /: expr : f_[___, _Dual, ___] /; MatchQ[f, _Function] || numericFunctionQ[f] || ! hasDefinitionsQ[f] := 
    applyDuals[f, Dual /@ List @@ Unevaluated[expr]]


Dual[v_Multivector, w_Multivector] := With[{G = v["GeometricAlgebra"]},
    Multivector[MapThread[Dual, {v["Coordinates"], Multivector[w, G]["Coordinates"]}], G]
]

Dual[v_Multivector, w_] := Dual[v, Multivector[w, v["GeometricAlgebra"]]]

Dual[v_, w_Multivector] := Dual[Multivector[v, w["GeometricAlgebra"]], w]



MakeBoxes[d : Dual[x_, y_], fmt_] := Module[{z, zbox, sign},
    If[NumericQ[y] && Negative[y] || MatchQ[y, -_],
        sign = "-";
        z = -y,
        sign = If[x === 0, Nothing, "+"];
        z = y
    ];
    zbox = If[z === 1, Nothing, Parenthesize[#, fmt, Plus] &[z]];
    InterpretationBox[#, d, Tooltip -> "\[DoubleStruckCapitalD]"] &[
        If[x === 0,
            If[z === 0, 0, RowBox[{sign, zbox, "\[Epsilon]"}]],
            If[z === 0, ToBoxes[x, fmt],
                RowBox[{ToBoxes[x, fmt], sign, zbox, "\[Epsilon]"}]
            ]
        ]
    ]
]



numericFunctionQ[f_] := MemberQ[Attributes[f], NumericFunction]


hasDefinitionsQ[f_] := GeneralUtilities`HasDefinitionsQ[f] || Length[Attributes[f]] > 0
