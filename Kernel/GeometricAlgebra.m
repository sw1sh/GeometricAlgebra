Package["GeometricAlgebra`"]


PackageExport["GeometricAlgebra"]

PackageScope["$GeometricAlgebraProperties"]
PackageScope["lowerGeometricAlgebra"]
PackageScope["higherGeometricAlgebra"]


GeometricAlgebra::usage = "GeometricAlgebra[p, q] gives an underlying algebra object for use with Multivector";


Options[GeometricAlgebra] = {"Signature" -> {3, 0, 0}, "FormatIndex" -> Automatic};


$GeometricAlgebraProperties = {
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

    "Metric",
    "Indices",
    "ReIndices",
    "ImIndexSigns",

    "Basis",
    "PseudoBasis",

    "MultiplicationTable",
    "MultiplicationSigns",
    "MultiplicationTableWithSigns",

    "PseudoscalarSquare",
    "ComplexAlgebra",

    "Zero",
    "Identity"
};


GeometricAlgebra[p_Integer, q_Integer: 0, r_Integer: 0, opts: OptionsPattern[GeometricAlgebra]] :=
    GeometricAlgebra["Signature" -> {p, q, r}, Sequence @@ FilterRules[{opts}, Except["Signature"]]]

GeometricAlgebra[{p_Integer, q___Integer}, opts: OptionsPattern[GeometricAlgebra]] := GeometricAlgebra[p, q, opts]

GeometricAlgebra[A_GeometricAlgebra, opts: OptionsPattern[]] :=
    GeometricAlgebra @@ Normal @ Merge[Join[FilterRules[{opts}, Options[GeometricAlgebra]], Options[A]], First]

GeometricAlgebra[] := OptionValue[Multivector, "GeometricAlgebra"] (* current default GeometricAlgebra *)

A_GeometricAlgebra[opt_String] /; KeyExistsQ[Options[GeometricAlgebra], opt] := Lookup[Join[Options[A], Options[GeometricAlgebra]], opt]

A_GeometricAlgebra["ComplexSignature"] := A["Signature"][[;; 2]]

A_GeometricAlgebra["DualSignature"] := A["Signature"][[-1]]

A_GeometricAlgebra["Dimension"] := Total @ A["Signature"]

A_GeometricAlgebra["ComplexDimension"] := Total @ A["ComplexSignature"]

A_GeometricAlgebra["DualDimension"] := A["DualSignature"]

A_GeometricAlgebra["Order"] := 2 ^ A["Dimension"]

A_GeometricAlgebra["ComplexOrder"] := 2 ^ A["ComplexDimension"]

A_GeometricAlgebra["DualOrder"] := 2 ^ A["DualDimension"]

A_GeometricAlgebra["Metric"] :=
    Module[{p, q, r},
        {p, q, r} = A["Signature"];
        Join[ConstantArray[1, p], ConstantArray[0, r], ConstantArray[-1, q]]
    ]

A_GeometricAlgebra["Indices"] := A["Indices"] = Module[{
    p, q, r
},
    {p, q, r} = A["Signature"];
    Subsets[Join[Range[p], p + Range[r], Range[- q, -1]]]
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

A_GeometricAlgebra["ImIndexSigns"] := With[{i = Last @ A["Indices"]}, Rule @@ Reverse @ multiplyIndices[i, #, A["Metric"]] & /@ A["ReIndices"]]


GeometricAlgebra /: Equal[gs__GeometricAlgebra]:= Equal @@ (#["Signature"] & /@ {gs})


(* Boxes *)


GeometricAlgebra /: MakeBoxes[A_GeometricAlgebra, StandardForm] := With[{
    box = SubscriptBox["\[DoubleStruckCapitalG]", RowBox @ Riffle[ToString /@ A["Signature"], ","]]
},
    InterpretationBox[box, A, Tooltip -> "Geometric Algebra"]
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
