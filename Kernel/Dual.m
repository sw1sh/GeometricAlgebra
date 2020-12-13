Package["GeometricAlgebra`"]

PackageExport["Dual"]
PackageExport["DualCoordinates"]
PackageExport["DualRe"]
PackageExport["DualEps"]
PackageExport["DualDimension"]
PackageExport["DualBasis"]

PackageScope["dualFunction"]
PackageScope["applyDualFunction"]
PackageScope["multivectorDuals"]
PackageScope["antiProductSigns"]


Needs["GeneralUtilities`"]


Attributes[Dual] = {Listable};



Dual /: DualCoordinates[HoldPattern[d : Dual[xs__]], pad_ : False] :=
    If[TrueQ[pad], PadRight[{xs}, 2 ^ DualDimension[d]], {xs}]

DualCoordinates[x : Except[_Dual], ___] := {x}

Attributes[DualCoordinates] = {Listable};


Dual /: DualRe[Dual[x_, __]] := x

DualRe[x : Except[_Dual]] := x

Attributes[DualRe] = {Listable, NumericFunction};


Dual /: DualEps[Dual[_, y_]] := y

Dual /: DualEps[Dual[_, y__]] := {y}

DualEps[Except[_Dual]] := 0

Attributes[DualEps] = {Listable, NumericFunction};


DualDimension[HoldPattern[Dual[xs__]]] := Ceiling @ Log2[Length @ {xs}]

DualDimension[Except[_Dual]] := 1


DualBasis[n_Integer] := Table[Dual @@ UnitVector[2 ^ n, i], {i, 2 ^ n}]


Dual[x : Except[_Dual]] := Dual[x, 0]


Dual[x_, 0 ...] := x

Dual[] := 0


Dual /: Dual[ds : PatternSequence[___, _Dual, ___]] :=
    Total @ MapIndexed[Function[{x, i}, x ** Dual @@ UnitVector[Length[{ds}], First[i]], HoldAllComplete], {ds}]


dualFunction[f_, arity_Integer, n_Integer] := dualFunction[f, arity, n] = With[{
    es = Array[\[FormalE], n],
    ps = Array[Times @@ (\[FormalE] @* First /@ Position[Reverse @ IntegerDigits[#, 2, n], 1]) &, 2 ^ n, 0],
    coeffs = Array[Function[{i, j}, Slot[i * 2 ^ n + j + 1]], {arity, 2 ^ n}, 0]
},
    Dual @@ Map[
        Function[subset,
            D[Apply[f, Total[ps #] & /@ coeffs], Sequence @@ subset] /. Alternatives @@ es -> 0
        ],
        Subsets[es]
    ] // Evaluate // Function
]


applyDualFunction[f_, coeffs_, n_Integer] := With[{
    es = Array[\[FormalE], n],
    ps = Array[Times @@ (\[FormalE] @* First /@ Position[Reverse @ IntegerDigits[#, 2, n], 1]) &, 2 ^ n, 0]
},
    Dual @@ Map[
        Function[subset,
            D[f[Total[ps coeffs]], Sequence @@ subset] /. Alternatives @@ es -> 0
        ],
        Subsets[es]
    ]
]


(* Dual magic here *)
applyDuals[f_, values_List] := Module[{
    coords = DualCoordinates /@ values,
    xs, arity, n
},
    arity = Length[values];
    n = Ceiling @ Log2[Max[Length /@ coords]];
    xs = PadRight[#, 2 ^ n, 0] & /@ coords;
    Quiet[dualFunction[f, arity, n] @@ Catenate @ xs, {General::infy, General::indet}] /. Indeterminate -> 0
  ]


Dual /: expr : f_[___, _Dual, ___] /; MatchQ[f, _Function] || numericFunctionQ[f] || ! hasDefinitionsQ[f] :=
    applyDuals[f, Dual /@ List @@ Unevaluated[expr]]


Dual[vs__Multivector] := With[{G = First @ MaximalBy[#["GeometricAlgebra"] & /@ {vs}, #["Order"] &]},
    Multivector[MapThread[Dual, Multivector[#, G]["Coordinates"] & /@ {vs}], G]
]

Dual[vs : PatternSequence[___, v_Multivector, ___]] := Apply[Dual, Multivector[#, v["GeometricAlgebra"]] & /@ {vs}]


multivectorDuals[v_Multivector] := With[{n = v["CoordinateDimension"]},
    Multivector[#, v["GeometricAlgebra"]] & /@ Transpose[PadRight[#, 2 ^ n] & /@ DualCoordinates /@ v["Coordinates"]]
]

multivectorDuals[x : Except[_Multivector]] := multivectorDuals[Multivector[x, 0]]


subsetOrders[n_Integer] := Length /@ Subsets[Range[n]]

antiProductSigns[n_Integer, m_Integer] := With[{
    leftOrders = subsetOrders @ n,
    rightOrders = subsetOrders @ m
},
  Partition[(-1) ^ Times @@@ Tuples[{leftOrders, rightOrders}], 2 ^ m]
]



negativeQ[x_] := NumericQ[x] && Quiet[Check[Negative[x], False]] || MatchQ[x, - _]


MakeBoxes[d : HoldPattern[Dual[xs__]], fmt_] := Module[{
    n, zboxes, displayBox
},
    n = Ceiling @ Log2[Length @ {xs}];
    zboxes = Parenthesize[#, fmt, Plus] & /@ {xs};
    displayBox = RowBox @ MapAt[Replace["+" -> Nothing], 1] @
        MapIndexed[Function[{x, i}, With[{k = i[[1]]},
            Splice @ {
                If[ x === 0,
                    Nothing,
                    Splice @ {If[negativeQ[x], Nothing, "+"], Which[k > 1 && x === 1, Nothing, k > 1 && x === - 1, "-", True, Slot[k]] ,
                    If[ k > 1,
                        If[ n > 1,
                            SubscriptBox[
                                "\[Epsilon]",
                                RowBox @ Riffle[First @ Subsets[Range[n], All, {k}], "\[InvisibleSpace]"]
                            ],
                            "\[Epsilon]"
                        ],
                        Nothing
                    ], "\[InvisibleSpace]"}
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
