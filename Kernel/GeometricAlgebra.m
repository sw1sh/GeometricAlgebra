Package["GeometricAlgebra`"]


PackageExport["GeometricAlgebra"]

PackageScope["$GeometricAlgebraProperties"]
PackageScope["lowerGeometricAlgebra"]
PackageScope["higherGeometricAlgebra"]


GeometricAlgebra::usage = "GeometricAlgebra[p, q] gives an underlying algebra object for use with Multivector";


Options[GeometricAlgebra] = {"Signature" -> {3, 0}, "FormatIndex" -> Automatic};


$GeometricAlgebraProperties = {
    "Signature",
    "FormatIndex",

    "Dimension",
    "Order",

    "Metric",
    "Indices",
    "ReIndices",
    "ImIndexSigns",

    "Basis",
    "PseudoBasis",

    "MultiplicationTable",
    "SignMatrix",
    "PseudoscalarSquare",
    "ComplexAlgebra",

    "Zero",
    "Identity"
};


GeometricAlgebra[p_Integer, q_Integer: 0, opts: OptionsPattern[GeometricAlgebra]] :=
    GeometricAlgebra["Signature" -> {p, q}, Sequence @@ FilterRules[{opts}, Except["Signature"]]]

GeometricAlgebra[{p_Integer, q___Integer}, opts: OptionsPattern[GeometricAlgebra]] := GeometricAlgebra[p, q, opts]

GeometricAlgebra[A_GeometricAlgebra, opts: OptionsPattern[]] :=
    GeometricAlgebra @@ Normal @ Merge[Join[FilterRules[{opts}, Options[GeometricAlgebra]], Options[A]], First]

GeometricAlgebra[] := OptionValue[Multivector, "GeometricAlgebra"] (* current default GeometricAlgebra *)

A_GeometricAlgebra[opt_String] /; KeyExistsQ[Options[GeometricAlgebra], opt] := Lookup[Join[Options[A], Options[GeometricAlgebra]], opt]

A_GeometricAlgebra["Dimension"] := Total @ A["Signature"]

A_GeometricAlgebra["Order"] := 2 ^ A["Dimension"]

A_GeometricAlgebra["Metric"] :=
    Module[{p, q},
        {p, q} = A["Signature"];
        Join[ConstantArray[1, p], ConstantArray[-1, q]]
    ]

A_GeometricAlgebra["Indices"] := Subsets[Join[Range[A["Signature"][[1]]], Range[-A["Signature"][[2]], -1]]]

A_GeometricAlgebra["SignMatrix"] := A["SignMatrix"] = A["MultiplicationTable"][[All, All, 1]]


A_GeometricAlgebra["PseudoscalarSquare"] := Module[{p, q},
    {p, q} = A["Signature"];
    (- 1) ^ ((p - q) * (p - q - 1) / 2)
]


A_GeometricAlgebra["ComplexAlgebra"] := With[{n = Floor[A["Dimension"] / 2]},
    If[ OddQ[A["Dimension"]],
        If[ A["PseudoscalarSquare"] == 1,
            GeometricAlgebra[n + 1, n],
            GeometricAlgebra[n, n + 1]
        ],
        GeometricAlgebra[n, n]
    ]
]


middleIndex[A_GeometricAlgebra] := Module[{p, q},
    {p, q} = A["Signature"];
    Join[Range[p], Range[-q, -1]][[Ceiling[(p + q) / 2]]]
]

A_GeometricAlgebra["ReIndices"] := Cases[A["Indices"], _List ? (FreeQ[#, middleIndex[A]] &)]

A_GeometricAlgebra["ImIndexSigns"] := With[{i = Last @ A["Indices"]}, Rule @@ Reverse @ multiplyIndices[i, #, A["Metric"]] & /@ A["ReIndices"]]


(* Boxes *)


GeometricAlgebra /: MakeBoxes[A_GeometricAlgebra, StandardForm] := With[{
    box = SubscriptBox["\[DoubleStruckCapitalG]", RowBox @ Riffle[ToString /@ A["Signature"], ","]]
},
    InterpretationBox[box, A, Tooltip -> "Geometric Algebra"]
]


(* Utility functions *)

lowerGeometricAlgebra[G_GeometricAlgebra] := Module[{
    p, q
},
    {p, q} = G["Signature"];
    GeometricAlgebra @ If[p >= q, {Max[p - 1, 0], q}, {p, q - 1}]
]

higherGeometricAlgebra[G_GeometricAlgebra] := Module[{
    p, q
},
    {p, q} = G["Signature"];
    GeometricAlgebra @ If[p > q, {p, q + 1}, {p + 1, q}]
]
