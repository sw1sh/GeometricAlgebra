Package["Wolfram`GeometricAlgebra`"]


PackageExport["GeometricAlgebra"]
PackageExport["GeometricAlgebraQ"]

PackageScope["$GeometricAlgebraProperties"]
PackageScope["lowerGeometricAlgebra"]
PackageScope["higherGeometricAlgebra"]


GeometricAlgebra::usage = "GeometricAlgebra[p, q] gives an underlying algebra object for use with Multivector";


Options[GeometricAlgebra] = {"Signature" -> {3, 0, 0}, "VectorBasis" -> Automatic, "Format" -> Automatic, "FormatIndex" -> Automatic}


$GeometricAlgebraProperties = {
    "Format",
    "FormatIndex",

    "Signature",
    "ComplexSignature",
    "DualSignature",

    "Dimension",
    "DualDimension",
    "ComplexDimension",

    "Order",
    "ComplexOrder",
    "DualOrder",

    "VectorBasis",
    "Metric",
    "MatricSignature",
    "Indices",
    "DualIndices",
    "ReIndices",
    "ImIndexSigns",

    "Basis",
    "PseudoBasis",

    "MultiplicationMatrix",
    "MultiplicationTable",
    "ExomorphismMatrix",
    "AntiExomorphismMatrix",

    "PseudoscalarSquare",
    "ComplexAlgebra",

    "Zero",
    "Identity"
};


geometricAlgebraQ[HoldPattern[GeometricAlgebra[data_Association ? AssociationQ]]] :=
    MatchQ[data, KeyValuePattern["Signature" -> {Repeated[_Integer ? NonNegative, {3}]}]];

geometricAlgebraQ[___] := False


GeometricAlgebraQ[A_GeometricAlgebra] := System`Private`HoldValidQ[A] || geometricAlgebraQ[Unevaluated[A]]

GeometricAlgebraQ[___] := False


GeometricAlgebra[p_Integer, q_Integer: 0, r_Integer: 0, opts: OptionsPattern[]] :=
    GeometricAlgebra["Signature" -> {p, q, r}, opts]

GeometricAlgebra[{p_Integer, q___Integer}, opts: OptionsPattern[]] := GeometricAlgebra[p, q, opts]

GeometricAlgebra[A_GeometricAlgebra, opts: OptionsPattern[]] :=
    GeometricAlgebra[Merge[Join[FilterRules[{opts}, Options[GeometricAlgebra]], Options[A]], First]]

GeometricAlgebra[v_Multivector] := v["GeometricAlgebra"]

GeometricAlgebra[] := OptionValue[Multivector, "GeometricAlgebra"] (* current default GeometricAlgebra *)

GeometricAlgebra[opts : OptionsPattern[]] := GeometricAlgebra[Association[opts]]

GeometricAlgebra /: HoldPattern[Options[GeometricAlgebra[data_] ? GeometricAlgebraQ]] := Normal[data]

GeometricAlgebra /: HoldPattern[Options[GeometricAlgebra[data_] ? GeometricAlgebraQ, filter_]] := FilterRules[Normal[data], filter]

A_GeometricAlgebra[opt_String] /; KeyExistsQ[Options[GeometricAlgebra], opt] := With[{value = OptionValue[{Options[A], Options[GeometricAlgebra]}, opt]},
    Switch[opt,
        "VectorBasis",
        Replace[value, Automatic :> If[A["Dimension"] == 0, {{}}, IdentityMatrix[A["Dimension"]]]],
        "FormatIndex",
        Replace[value, Except[{{{___Integer} ..}, _}] :> {Automatic, value}],
        _,
        value
    ]
]

A_GeometricAlgebra["Metric"] := Replace[A["VectorBasis"], {
    Automatic :> If[A["Dimension"] == 0, {{}}, DiagonalMatrix[A["MetricSignature"]]],
    b_ ? SquareMatrixQ :> Transpose[b] . DiagonalMatrix[A["MetricSignature"]] . b,
    b_ ? VectorQ :> A["MetricSignature"] * b ^ 2
}]

A_GeometricAlgebra["ComplexSignature"] := A["Signature"][[;; 2]]

A_GeometricAlgebra["DualSignature"] := A["Signature"][[-1]]

A_GeometricAlgebra["Dimension"] := Total @ A["Signature"]

A_GeometricAlgebra["ComplexDimension"] := Total @ A["ComplexSignature"]

A_GeometricAlgebra["DualDimension"] := A["DualSignature"]

A_GeometricAlgebra["Order"] := 2 ^ A["Dimension"]

A_GeometricAlgebra["ComplexOrder"] := 2 ^ A["ComplexDimension"]

A_GeometricAlgebra["DualOrder"] := 2 ^ A["DualDimension"]

A_GeometricAlgebra["MetricSignature"] :=
    Module[{p, q, r},
        {p, q, r} = A["Signature"];
        Join[ConstantArray[1, p], ConstantArray[0, r], ConstantArray[-1, q]]
    ]

A_GeometricAlgebra["MetricMatrix"] := Map[Lookup[#, Key[{}], 0] &, A["MultiplicationTable"], {2}]

A_GeometricAlgebra["Indices"] := A["Indices"] = Block[{
    p, q, r
},
    {p, q, r} = A["Signature"];
    Subsets[Join[Range[p], p + Range[r], Range[- q, -1]]]
]

A_GeometricAlgebra["FormatIndices"] := Replace[A["FormatIndex"][[1]], Automatic :> A["Indices"]]

A_GeometricAlgebra["DualIndices"] := With[{i = Last @ A["Indices"]},
     Map[DeleteElements[i, #] &, A["Indices"]]
]

A_GeometricAlgebra["PseudoscalarSquare"] := Module[{p, q},
    {p, q} = A["ComplexSignature"];
    (- 1) ^ ((p - q) * (p - q - 1) / 2)
]


A_GeometricAlgebra["ComplexAlgebra"] := With[{n = Floor[A["ComplexDimension"] / 2], r = A["DualSignature"]},
    If[ OddQ[A["ComplexDimension"]],
        If[ A["PseudoscalarSquare"] == 1,
            GeometricAlgebra[n + 1 + r, n + r],
            GeometricAlgebra[n + r, n + 1 + r]
        ],
        GeometricAlgebra[n + r, n + r]
    ]
]


middleIndex[A_GeometricAlgebra] := Module[{p, q},
    {p, q} = A["ComplexSignature"];
    Join[Range[p], Range[-q, -1]][[Ceiling[(p + q) / 2]]]
]

A_GeometricAlgebra["ReIndices"] := Cases[A["Indices"], _List ? (FreeQ[#, middleIndex[A]] &)]

A_GeometricAlgebra["ImIndexSigns"] := With[{i = Last @ A["Indices"]}, Rule @@ KeyMap[Reverse] @ multiplyIndices[i, #, A["Metric"]] & /@ A["ReIndices"]]


GeometricAlgebra /: Equal[gs__GeometricAlgebra]:= Equal @@ (#["Signature"] & /@ {gs})


A_GeometricAlgebra /; System`Private`HoldNotValidQ[A] && geometricAlgebraQ[Unevaluated[A]] := System`Private`SetNoEntry[System`Private`HoldSetValid[A]]


(* Boxes *)


GeometricAlgebra /: MakeBoxes[A_GeometricAlgebra /; GeometricAlgebraQ[Unevaluated[A]], form___] := With[{
    box = Replace[A["Format"], Automatic :> SubscriptBox["\[DoubleStruckCapitalG]", RowBox @ Riffle[ToString /@ Replace[MapAt[Replace[0 -> Nothing], A["Signature"], {3}], {p_, 0} :> {p}], ","]]],
    tooltip = RowBox[{"Geometric Algebra", ToBoxes[A["Signature"], form]}]
},
    InterpretationBox[box, A, Tooltip -> tooltip]
]


(* Utility functions *)

lowerGeometricAlgebra[G_GeometricAlgebra] := Module[{
    p, q, r
},
    {p, q, r} = G["Signature"];
    GeometricAlgebra @ If[p >= q, {Max[p - 1, 0], q, r}, {p, q - 1, r}]
]

higherGeometricAlgebra[G_GeometricAlgebra] := Module[{
    p, q, r
},
    {p, q, r} = G["Signature"];
    GeometricAlgebra @ If[p > q, {p, q + 1, r}, {p + 1, q, r}]
]
