Package["GeometricAlgebra`"]

PackageExport["Dual"]
PackageExport["DualCoordinates"]
PackageExport["DualRe"]
PackageExport["DualEps"]


Needs["GeneralUtilities`"]


Attributes[Dual] = {Listable};



Dual /: DualCoordinates[HoldPattern[Dual[xs__]]] := {xs}

DualCoordinates[x : Except[_Dual]] := {x}

Attributes[DualCoordinates] = {Listable, NumericFunction};


Dual /: DualRe[Dual[x_, __]] := x

DualRe[x : Except[_Dual]] := x

Attributes[DualRe] = {Listable, NumericFunction};


Dual /: DualEps[Dual[_, y_]] := y

Dual /: DualEps[Dual[_, y__]] := {y}

DualEps[Except[_Dual]] := 0

Attributes[DualEps] = {Listable, NumericFunction};


Dual[x : Except[_Dual]] := Dual[x, 0]


Dual[x_, 0] := x


Dual /: Dual[d_Dual] := d


(* Dual magic here *)
applyDuals[f_, values_List] := Module[{
    coords = DualCoordinates /@ values,
    xs, h, hs, coeffs,
    n
},
    n = Ceiling @ Log2[Max[Length /@ coords]];
    xs = PadRight[#, 2 ^ n, 0] & /@ coords;
    hs = Array[h, n];
    coeffs = Array[Times @@ (h @* First /@ Position[Reverse @ IntegerDigits[#, 2, n], 1]) &, 2 ^ n, 0];
    Dual @@
        SortBy[
            CoefficientRules[
                Normal @ Series[
                    Apply[f, Total[coeffs #] & /@ xs],
                    Sequence @@ Map[{#, 0, 1} &, hs]
                ],
                hs
            ],
            FromDigits[#[[1]], 2] &
        ][[All, 2]]
  ]


Dual /: expr : f_[___, _Dual, ___] /; MatchQ[f, _Function] || numericFunctionQ[f] || ! hasDefinitionsQ[f] :=
    applyDuals[f, Dual /@ List @@ Unevaluated[expr]]


Dual[v_Multivector, w_Multivector] := With[{G = v["GeometricAlgebra"]},
    Multivector[MapThread[Dual, {v["Coordinates"], Multivector[w, G]["Coordinates"]}], G]
]

Dual[v_Multivector, w_] := Dual[v, Multivector[w, v["GeometricAlgebra"]]]

Dual[v_, w_Multivector] := Dual[Multivector[v, w["GeometricAlgebra"]], w]



negativeQ[x_] := NumericQ[x] && Quiet[Check[Negative[x], False]] || MatchQ[x, - _]

MakeBoxes[d : HoldPattern[Dual[xs__]], fmt_] := Module[{
    n, zboxes, displayBox
},
    n = Ceiling @ Log2[Length @ {xs}];
    zboxes = Parenthesize[#, fmt, Plus] & /@ {xs};
    displayBox = RowBox @ MapAt[Replace["-" | "+" -> Nothing], 1] @
        MapIndexed[Function[{x, i}, With[{k = i[[1]]},
            Splice @ {
                If[ x === 0,
                    Nothing,
                    Splice @ {If[negativeQ[x], Nothing, "+"], Slot[k],
                    If[ k > 1,
                        If[ n > 1,
                            SubscriptBox[
                                "\[Epsilon]",
                                RowBox @ Riffle[First @ Subsets[Range[n], All, {k}], "\[InvisibleSpace]"]
                            ],
                            "\[Epsilon]"
                        ],
                        Nothing
                    ]}
                ]
            }
            ]],
            {xs}
        ];
    TemplateBox[
        zboxes,
        "Dual",
        DisplayFunction -> Function[Evaluate[displayBox]],
        InterpretationFunction -> Dual,
        Tooltip -> SuperscriptBox["\[DoubleStruckCapitalD]", n]
    ]
]
