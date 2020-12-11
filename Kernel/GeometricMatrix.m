Package["GeometricAlgebra`"]


PackageExport["MultivectorFunction"]
PackageExport["CanonicalGeometricAlgebra"]
PackageExport["CanonicalGeometricIndices"]
PackageExport["ConvertGeometricAlgebra"]
PackageExport["CanonicalMultivector"]
PackageExport["MultivectorMatrix"]
PackageExport["MatrixMultivector"]
PackageExport["MultivectorBlock"]
PackageExport["LeftKroneckerProduct"]
PackageExport["RightKroneckerProduct"]

PackageScope["nilpotentBasis"]
PackageScope["nilpotentMatrix"]
PackageScope["multivectorBasisMatrix"]


Options[kroneckerProduct] = {"Direction" -> Left, "Flatten" -> True};

kroneckerProduct[va_MultivectorArray, wa_MultivectorArray, OptionsPattern[]] := With[{
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

kroneckerProduct[va_MultivectorArray, OptionsPattern[]] := va

kroneckerProduct[vas__MultivectorArray, opts : OptionsPattern[]] := Fold[kroneckerProduct[##, opts] &, {vas}]

kroneckerProduct[OptionsPattern[]] := MultivectorArray[{Multivector[{1}, {0, 0}]}, {If[OptionValue["Direction"] === Left, - 1, 1]}]


LeftKroneckerProduct[vas___MultivectorArray] := kroneckerProduct[vas, "Direction" -> Left]

RightKroneckerProduct[vas___MultivectorArray] := kroneckerProduct[vas, "Direction" -> Right]


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
        toCanonicConversion, fromCanonicConversion, canonicCoordinates, i
},
    toCanonicConversion = CanonicalGeometricIndices[v["GeometricAlgebra"]];
    fromCanonicConversion = CanonicalGeometricIndices[G];
    canonicCoordinates = Association @ MapThread[Function[{x, y}, y[[2]] -> x y[[1]], HoldAllComplete],
        {v["Coordinates"], toCanonicConversion[[All, 2]]}
    ];
    i = OptionValue["Pseudoscalar"];

    Total @ Map[
        Apply[With[{c = canonicCoordinates[#2[[2]]] Conjugate[#2[[1]]]},
            Multivector[<|#1 -> If[i != I, Re[c] + Im[c] i, c]|>, G]
        ]
        &], fromCanonicConversion
    ]
]

ConvertGeometricAlgebra[v_Multivector, args: Except[OptionsPattern[]], opts: OptionsPattern[]] :=
    ConvertGeometricAlgebra[v, GeometricAlgebra[args, FilterRules[{opts}, Options[GeomegricAlgebra]]], opts]


CanonicalMultivector[v_Multivector, opts : OptionsPattern[]] :=
    ConvertGeometricAlgebra[
        v,
        GeometricAlgebra @ CanonicalGeometricAlgebra[v["GeometricAlgebra"]]["Signature"],
        opts
    ]


fromRealCanonicalMultivector[v_Multivector, A_GeometricAlgebra] /;
        CanonicalGeometricAlgebra[v["GeometricAlgebra"]]["Signature"] == v["Signature"] := Module[{
    assoc, G, is, j},
    G = v["GeometricAlgebra"];
    assoc = v["Association"];
    is = Association[
        # -> multiplyIndices[#, Last @ G["Indices"], G["Metric"]] & /@
        Cases[CanonicalGeometricIndices[A], HoldPattern[_ -> {c_, i_} /; MatchQ[c, I | -I]] :> i]
    ];
    j = With[{keys = Complement[v["Indices"], Keys[is]]}, AssociationThread[keys, Lookup[assoc, Key /@ keys, 0]]];
    Multivector[
        Association[I #[[1]] Lookup[assoc, Key @ #[[2]], 0] & /@ is, j],
        G
    ]
]


Options[MultivectorMatrix] = {"Basis" -> Automatic};

MultivectorMatrix[v_Multivector, opts: OptionsPattern[]] := Module[{
    p, q, n, X, M, mat
},
    {p, q} = v["Signature"];

    n = Floor[(p + q) / 2];

    M = Inverse @ If[
        OptionValue["Basis"] === Automatic,
        nilpotentMatrix[n],

        multivectorBasisMatrix[OptionValue["Basis"]]
    ];
    X = MultivectorNumber /@ ConvertGeometricAlgebra[v, v["ComplexAlgebra"]]["ComplexCoordinates"];
    mat = MultivectorArray[Partition[M . X, 2 ^ n]];

    mat
]


Options[MultivectorBlock] = {}

MultivectorBlock[v_Multivector, opts: OptionsPattern[]] := Module[{
    G, n, p, q, X, F, B
},
    {p, q} = v["Signature"];

    n = Floor[(p + q) / 2];
    If[ n > 0,
        G = GeometricAlgebra @ MapThread[Max, {v["ComplexAlgebra"]["Signature"] - {1, 1}, {0, 0}}];
        X = MultivectorNumber[#, G["ComplexAlgebra"]] & /@ ConvertGeometricAlgebra[v, v["ComplexAlgebra"]]["ComplexCoordinates"];
        F = Inverse @ nilpotentMatrix[n];
        B = nilpotentMatrix[n - 1];
        BlockMap[
            Multivector[AssociationThread[G[If[OddQ[p + q], "ReIndices", "Indices"]], (B . Flatten[#, 1]) . X], G]["Flatten"] &,
            Partition[F, 2 ^ n],
            {2 ^ (n - 1), 2 ^ (n - 1)}
        ],

        {{v}}
    ] // MultivectorArray
]

MultivectorBlock[v_Multivector, n_Integer /; n > 0, opts: OptionsPattern[MultivectorMatrix]] :=
    With[{
        blocks = MultivectorBlock[v, opts]
    },
        If[ n > 1,
            MultivectorArray @ Flatten[Map[MultivectorBlock[#, n - 1, opts]["Components"] &, blocks["Components"], {2}], {{1, 3}, {2, 4}}],
            blocks
        ]
    ]

MultivectorBlock[v_Multivector, 0, ___] := MultivectorArray[{{v}}]


Options[MatrixMultivector] = {"Basis" -> Automatic, Method -> "Matrix"};

MatrixMultivector::unknownMethod = "Method should be one of {\"Basis\", \"Matrix\"}";
MatrixMultivector::nonsq = "Not a square matrix";
MatrixMultivector::non2pow = "Matrix dimension `1` is not a power of 2";
MatrixMultivector::invalidBasis = "Specified basis is not a multivector of right dimensions";

MatrixMultivector[mat_MultivectorArray, opts: OptionsPattern[]] := Module[{
    dim, n, g, G, m, basis, M, X
},
    dim = Dimensions[mat];
    If[ Length[dim] != 2 || Not[Equal @@ dim],
        Message[MatrixMultivector::nonsq];
        Return[$Failed]
    ];
    n = Log2[First @ dim];
    If[ Not[IntegerQ[n]],
        Message[MatrixMultivector::non2pow, dim];
        Return[$Failed]
    ];

    g = mat["GeometricAlgebra"];
    If[ g["Dimension"] > 1,
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

    Switch[
        OptionValue[Method],

        "Basis",

        If[
            OptionValue["Basis"] === Automatic,

            (* Construct nilpotent basis *)
            basis = nilpotentBasis[n],

            (* Explicit basis *)
            If[
                Not[MatchQ[OptionValue["Basis"], _MultivectorArray] && Dimensions[OptionValue["Basis"]] == Dimensions[mat]],

                Message[MatrixMultivector::invalidBasis];
                Return[$Failed],

                basis = OptionValue["Basis"][CanonicalMultivector]
            ]

        ];
        M = mat[MultivectorNumber]["Components"];
        Total[MapThread[#2[Map[Curry[Times][#1]]] &, {M, basis["Components"]}, 2], 2],

        "Matrix",
        G = GeometricAlgebra[{n, n}];
        X = Flatten @ mat[MultivectorNumber]["Components"];
        M = If[
            OptionValue["Basis"] === Automatic,

            nilpotentMatrix[n],

            If[
                Not[MatchQ[OptionValue["Basis"], _MultivectorArray] && Dimensions[OptionValue["Basis"]] == Dimensions[mat]],

                Message[MatrixMultivector::invalidBasis];
                Return[$Failed],

                multivectorBasisMatrix[OptionValue["Basis"]]

            ]
        ];
        Multivector[
            M . X,
            G
        ],

        _,
        Message[MatrixMultivector::unknownMethod];
        $Failed
    ]
]

MatrixMultivector[mat_MultivectorArray, G_GeometricAlgebra, opts: OptionsPattern[]] :=
    ConvertGeometricAlgebra[MatrixMultivector[mat, opts][Map[NumberMultivector[#, G["ComplexAlgebra"]] &]]["Flatten"], G]


MultivectorFunction[F_ /; MatchQ[F, _Function] || numericFunctionQ[F], v_Multivector, opts: OptionsPattern[]] := Module[{
    f = MatrixFunction[F, #] &, X, g, re, im, a, b, Y, w
},
    X = MultivectorMatrix[v, Sequence @@ FilterRules[{opts}, Options[MultivectorMatrix]]]["Components"];
    g = v["ComplexAlgebra"];

    re = Map[#["Scalar"] &, X, {2}];
    im = Map[#["Pseudoscalar"] &, X, {2}];

    Check[
        If[ v["PseudoscalarSquare"] == 1,
            (* hyperbolic (split-complex) case *)
            With[{
                aDuals = DualCoordinates[re + im],
                bDuals = DualCoordinates[re - im]},
                With[{n = Ceiling @ Log2[Max[Map[Length, Join[aDuals bDuals], {2}]]]},
                    a = applyDualFunction[MatrixFunction[F, #] &, Transpose[Map[PadRight[#, 2 ^ n] &, aDuals, {2}], 3 <-> 1], n];
                    b = applyDualFunction[MatrixFunction[F, #] &, Transpose[Map[PadRight[#, 2 ^ n] &, bDuals, {2}], 3 <-> 1], n];
                ]
            ];
            Y = MapThread[Function[{x, y}, Multivector[{x, y}, GeometricAlgebra[1, 0]], HoldAllComplete], {a + b, a - b} / 2, 2],

            (* complex case *)
            With[{
                aDuals = DualCoordinates[re + I im],
                bDuals = DualCoordinates[re - I im]},
                With[{n = Ceiling @ Log2[Max[Map[Length, Join[aDuals bDuals], {2}]]]},
                    a = applyDualFunction[MatrixFunction[F, #] &, Transpose[Map[PadRight[#, 2 ^ n] &, aDuals, {2}], 3 <-> 1], n];
                    b = applyDualFunction[MatrixFunction[F, #] &, Transpose[Map[PadRight[#, 2 ^ n] &, bDuals, {2}], 3 <-> 1], n];
                ]
            ];
            Y = MapThread[Function[{x, y}, Multivector[{x, - I y}, GeometricAlgebra[0, 1]], HoldAllComplete], {a + b, a - b} / 2, 2]
        ],
        Return[$Failed]
    ];
    w = MatrixMultivector[
        MultivectorArray[Y],
        g,
        Sequence @@ FilterRules[{opts}, Options[MatrixMultivector]]
    ];
    ConvertGeometricAlgebra[w, v["GeometricAlgebra"]]
]


v_Multivector["Matrix"] := MultivectorMatrix[v]


(* Utility functions *)


nilpotentBasis[0] := MultivectorArray[{{1}}]

nilpotentBasis[n_Integer] := Module[{A, u, Bt, G, i},
    G = GeometricAlgebra[n, n + 1];
    A = Apply[LeftKroneckerProduct,
        Table[
            MultivectorArray[{Multivector[1, G], G["Nilpotent", i]}],
            {i, 1, n}
        ]
    ];
    u = Apply[GeometricProduct, Table[G["Idempotent", i], {i, 1, n}]];
    Bt = Apply[RightKroneckerProduct,
        Reverse @ Table[
            MultivectorArray[{Multivector[1, G], G["Nilpotent", -i]}, {-2}],
            {i, 1, n}
        ]
    ];
    A ** u ** Bt
]


multivectorBasisMatrix[mat_MultivectorArray] := multivectorBasisMatrix[mat] = Module[{
    n, m, sa, s
},
    n = Log2[mat["Dimension"]] / 2;
    m = 2 ^ n;
    If[Not @ mat["DoubleSquareQ"], Return[$Failed]];
    sa = Array[s[##] &, {m, m}];

    Coefficient[#["Scalar"], Flatten @ sa] & /@
        MatrixMultivector[MultivectorArray[sa], Method -> "Basis", "Basis" -> mat[CanonicalMultivector]]["ComplexCoordinates"]
]


nilpotentMatrix[n_Integer] := nilpotentMatrix[n] = multivectorBasisMatrix[nilpotentBasis[n]]
