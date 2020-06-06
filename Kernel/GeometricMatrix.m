Package["GeometricAlgebra`"]


PackageExport["MultivectorFunction"]
PackageExport["CanonicalGeometricAlgebra"]
PackageExport["CanonicalGeometricIndices"]
PackageExport["ConvertGeometricAlgebra"]
PackageExport["CanonicalMultivector"]
PackageExport["RealMultivector"]
PackageExport["ComplexMultivector"]
PackageExport["MultivectorMatrix"]
PackageExport["MatrixMultivector"]

PackageScope["kroneckerProduct"]
PackageScope["nullToStandardMatrix"]
PackageScope["spectralToStandardMatrix"]


PackageScope["kroneckerProduct"]


Options[kroneckerProduct] = {"Direction" -> Left, "Flatten" -> True};

kroneckerProduct[va_MultivectorArray, wa_MultivectorArray, OptionsPattern[kroneckerProduct]] := With[{
    a = va ** wa,
    r = va["Rank"],
    s1 = va["Shape"],
    s2 = wa["Shape"],
    dir = OptionValue["Direction"]
},
    If[OptionValue["Flatten"],
        MultivectorArray[
            Flatten[If[dir === Left, Transpose[#, r <-> r + 1] &, Identity][a["Components"]], {{r, r + 1}}],
            Join[s1[[;; -2]], {If[dir === Left, Sign[s1[[-1]]], Sign[s2[[1]]]] Abs[s1[[-1]] s2[[1]]]}, s1[[2 ;;]]]
        ],
        MultivectorArray[
            If[dir === Left, Transpose[#, r <-> r + 1] &, Identity][a["Components"]],
            Join[s1, s2]
        ]
    ]
]

kroneckerProduct[va_MultivectorArray, OptionsPattern[kroneckerProduct]] := va

kroneckerProduct[vas__MultivectorArray, opts : OptionsPattern[kroneckerProduct]] := Fold[kroneckerProduct[##, opts] &, {vas}]

kroneckerProduct[OptionsPattern[kroneckerProduct]] := MultivectorArray[{Multivector[{1}, {0, 0}]}, {If[OptionValue["Direction"] === Left, - 1, 1]}]


CanonicalGeometricAlgebra[G_GeometricAlgebra] := Module[{
    p, q, n, n1, n2, indexConversion, newIndex
},
    {p, q} = G["Signature"];
    n = p + q;
    n1 = Floor[n / 2];
    n2 = Ceiling[n / 2];
    indexConversion = Association @ CanonicalGeometricIndices[G];
    newIndex = Map[
        With[{c = indexConversion[#][[1]], box = geometricIndexBoxes[G, #]},
            indexConversion[#][[2]] -> Switch[c,
                -1 | -I, RowBox[{"(", ToBoxes[c], ")", box}],
                I, RowBox[{"\[ImaginaryI]", box}],
                _, box
            ]
        ] &,
        G["Indices"]
    ];
    GeometricAlgebra[{n1, n2}, "FormatIndex" -> newIndex]
]


CanonicalGeometricIndices[G_GeometricAlgebra] := Module[{
    n1, n2, p, q, n, complexIndices, newIndex
},
    {p, q} = G["Signature"];
    n = p + q;
    n1 = Floor[n / 2];
    n2 = Ceiling[n / 2];
    If[p > q,
        complexIndices = Range[n1 + 1, p];
        newIndex = Map[
            #1 -> {
                With[{c = I ^ Count[#1, _ ? (MemberQ[complexIndices, #] &)]}, c],
                Map[If[MemberQ[complexIndices, #], - n + # - 1, #] &, #1]
            } &,
            G["Indices"]
        ],
        complexIndices = Range[-n2 - 1, -q, -1];
        newIndex = Map[
            #1 -> {
                With[{c = I ^ Count[#1, _ ? (MemberQ[complexIndices, #] &)]}, c],
                Map[If[MemberQ[complexIndices, #], n + # + 1, #] &, #1]
            } &,
            G["Indices"]
        ]
     ];
    newIndex
    ]


Options[ConvertGeometricAlgebra] = {"Pseudoscalar" -> I};

ConvertGeometricAlgebra[
    v_Multivector,
    G_GeometricAlgebra,
    OptionsPattern[ConvertGeometricAlgebra]] := Module[{
        toCanonicConversion, fromCanonicConversion, canonicCoordinates
},
    toCanonicConversion = CanonicalGeometricIndices[v["GeometricAlgebra"]];
    fromCanonicConversion = CanonicalGeometricIndices[G];
    canonicCoordinates = Association @ MapThread[#2[[2]] -> #1 #2[[1]] &,
        {v["Coordinates"], toCanonicConversion[[All, 2]]}
    ];

    Total @ Map[
        Apply[With[{c = canonicCoordinates[#2[[2]]] Conjugate[#2[[1]]]},
            Multivector[<|#1 -> Re[c] + Im[c] OptionValue["Pseudoscalar"]|>, G]
        ]
        &], fromCanonicConversion
    ]
]


CanonicalMultivector[v_Multivector, opts : OptionsPattern[]] :=
    ConvertGeometricAlgebra[
        v,
        GeometricAlgebra @ CanonicalGeometricAlgebra[v["GeometricAlgebra"]]["Signature"],
        opts
    ]


RealMultivector[v_Multivector] := RealMultivector[v, v["GeometricAlgebra"]]

RealMultivector[v_Multivector, g_GeometricAlgebra] := With[{G = GeometricAlgebra[g["Signature"] + {0, 1}]},
    v[Re] + G["Pseudoscalar"] ** v[Im]
]

ComplexMultivector[v_Multivector] := ComplexMultivector[v, v["GeometricAlgebra"]]

ComplexMultivector[v_Multivector, G_GeometricAlgebra] := With[{g = GeometricAlgebra[G["Signature"] - {0, 1}]},
    Multivector[v, g] - I Multivector[v ** G["Pseudoscalar"], g]
]


Options[MultivectorMatrix] = {"Basis" -> "Null"}

MultivectorMatrix[v_Multivector, opts: OptionsPattern[MultivectorMatrix]] := Module[{
    A, p, q, reIndex, re, im, X, M
},
    A = v["GeometricAlgebra"];
    {p, q} = A["Signature"];
    If[p != q && q != p + 1,
        Return[
            MultivectorMatrix[CanonicalMultivector[v, FilterRules[{opts}, Options[CanonicalMultivector]]], opts]
        ]
    ];

    If[ q == p + 1,
        reIndex = Catenate @ Position[A["Indices"], _List ? (FreeQ[#, -q] &), {1}];
        re = v["Coordinates"][[reIndex]];
        im = - (v ** A["Pseudoscalar"])["Coordinates"][[reIndex]];

        X = MapThread[Multivector[{#1, #2}, {0, 1}] &, {re, im}],

        (* Else *)
        X = Multivector[{#}, {0}] & /@ v["Coordinates"];
    ];
    M = PseudoInverse[If[OptionValue["Basis"] == "Null", nullToStandardMatrix[p], spectralToStandardMatrix[p]]];
    MultivectorArray[Partition[M . X, 2 ^ p], {2 ^ p, - 2 ^ p}]
]


Options[MatrixMultivector] = {"Basis" -> "Null", Method -> "Matrix"};

MatrixMultivector::unknownMethod = "Method should be one of {\"Multivector\", \"Matrix\"}";
MatrixMultivector::nonsq = "Not a square matrix";
MatrixMultivector::non2pow = "Matrix dimension `1` is not a power of 2";

MatrixMultivector[mat_MultivectorArray, opts: OptionsPattern[MatrixMultivector]] := Module[{
    dim, n, g, G, m, A, At, u, B, Bt, sa, M
},
    dim = Dimensions[mat];
    If[ Length[dim] != 2 || Not[Equal @@ dim],
        Message[MatrixMultivector::nonsq];
        Return[$Failed]
    ];
    n = Log2[First @ dim];
    If[ Not[IntegerQ[n]],
        Message[MatrixMultivector::non2pow, n];
        Return[$Failed]
    ];

    g = mat["GeometricAlgebra"];
    G = GeometricAlgebra[g["Signature"] + {n, n}];
    If[g["Dimension"] > 1,
        m = Floor[g["Dimension"] / 2];
        Return @ MatrixMultivector[
            MultivectorArray[
                Flatten[
                    Map[
                        MultivectorMatrix[#, Sequence @@ FilterRules[{opts}, Options[MultivectorMatrix]]]["Components"] &,
                        mat["Components"],
                        {mat["Rank"]}
                    ],
                    {{1, 3}, {2, 4}}
                ],
                {2 ^ (n + m), - 2 ^ (n + m)}
            ],
            opts
        ]
    ];

    Switch[OptionValue[Method],

        "Multivector",
         At = Apply[
             kroneckerProduct[##] &,
             Table[
                 MultivectorArray[{Multivector[1, G], If[OptionValue["Basis"] == "Null", G["Null", i], Multivector[<|{i} -> 1|>, G]]}, {-2}],
                 {i, 1, n}
             ]
         ];
         u = Apply[
             GeometricProduct,
             Table[
                 If[OptionValue["Basis"] == "Null", G["Idempotent", i], Multivector[<|{} -> 1 / 2, {-i} -> I / 2|>, G]],
                 {i, 1, n}
             ]
         ];
         B = Apply[
             kroneckerProduct[##, "Direction" -> Right] &,
             Reverse @ Table[
                 MultivectorArray[{Multivector[1, G], If[OptionValue["Basis"] == "Null", G["Null", - i], Multivector[<|{i} -> 1|>, G]]}],
                 {i, 1, n}
             ]
         ];
         sa = mapComponents[
             Multivector[SparseArray[{1 -> #["Scalar"], -1 -> #["Pseudoscalar"]}, G["Order"]], G] &,
             mat
         ];
         ((At ** u) ** (sa ** B))["Components"],

         "MultivectorTranspose",
         A = Apply[
             kroneckerProduct[##] &,
             Table[
                 MultivectorArray[{Multivector[1, G], If[OptionValue["Basis"] == "Null", G["Null", i], Multivector[<|{i} -> 1|>, G]]}],
                 {i, 1, n}
             ]
         ];
         u = Apply[
             GeometricProduct,
             Table[
                 If[OptionValue["Basis"] == "Null", G["Idempotent", i], Multivector[<|{} -> 1 / 2, {-i} -> I / 2|>, G]],
                 {i, 1, n}
             ]
         ];
         Bt = Apply[
             kroneckerProduct[##, "Direction" -> Right] &,
             Reverse @ Table[
                 MultivectorArray[{Multivector[1, G], If[OptionValue["Basis"] == "Null", G["Null", - i], Multivector[<|{i} -> 1|>, G]]}, {-2}],
                 {i, 1, n}
             ]
         ];
         Total[(A ** u ** Bt)["Components"] mat["Numeric"], 2]["Real"],

         "Matrix",
         M = If[OptionValue["Basis"] == "Null", nullToStandardMatrix[n], spectralToStandardMatrix[n]];
         Map[
             Multivector[SparseArray[{1 -> #["Scalar"], {-1} -> #["Pseudoscalar"]}, G["Order"]], G] &,
             M . (Flatten @ mat["Components"])
         ] . GeometricAlgebra[n, n]["MultivectorBasis"],

         _,
         Message[MatrixMultivector::unknownMethod];
         $Failed
    ]

]

MatrixMultivector[mat_MultivectorArray, G_GeometricAlgebra, opts: OptionsPattern[MatrixMultivector]] :=
    Module[{v},
        v = MatrixMultivector[mat, opts];
        v = Multivector[v, GeometricAlgebra[v["Signature"] + {0, G["Dimension"] - v["Dimension"]}]];
        ConvertGeometricAlgebra[v, G]
    ]


MultivectorFunction[f_, v_Multivector, opts: OptionsPattern[]] :=
    MatrixMultivector[
        MultivectorArray[
            MatrixFunction[f,
                MultivectorMatrix[v, Sequence @@ FilterRules[{opts}, Options[MultivectorMatrix]]]["Numeric"]
            ]
        ],
        Sequence @@ FilterRules[{opts}, Options[MatrixMultivector]]
    ]


(* Utility functions *)


nullToStandardMatrix[n_Integer] := nullToStandardMatrix[n] = Module[{
    m = 2 ^ n, sa, s
},
    sa = Array[s[##] &, {m, m}];

    Coefficient[#, Flatten @ sa] & /@ Normal[
        Multivector[MatrixMultivector[MultivectorArray[sa], Method -> "MultivectorTranspose"], GeometricAlgebra[n, n]][Map[ComplexExpand]]
    ]
]


spectralToStandardMatrix[n_Integer] := spectralToStandardMatrix[n] = Module[{
    m = 2 ^ n, sa, s
},
    sa = Array[s[##] &, {m, m}];

    Coefficient[#, Flatten @ sa] & /@ Normal[
        Multivector[MatrixMultivector[MultivectorArray[sa], "Basis" -> "Spectral", Method -> "MultivectorTranspose"], GeometricAlgebra[n, n]][Map[ComplexExpand]]
    ]
]