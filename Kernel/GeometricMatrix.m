Package["GeometricAlgebra`"]


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


PackageExport["CanonicalGeometricAlgebra"]

CanonicalGeometricAlgebra[G_GeometricAlgebra] := Module[{
    p, q, n, n1, n2, indexConversion, newIndex
},
    {p, q} = G["Signature"];
    n = p + q;
    n1 = Floor[n / 2];
    n2 = Ceiling[n / 2];
    indexConversion = Association @ CanonicalGeometricIndices[G];
    newIndex = Map[
        With[{c = indexConversion[#][[1]], box = GeometricIndexBoxes[G, #]},
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


PackageExport["CanonicalGeometricIndices"]

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
                With[{c = I^Count[#1, _?(MemberQ[complexIndices, #] &)]}, c],
                Map[If[MemberQ[complexIndices, #], -n2 + (# - n1 - 1), #] &, #1]
            } &,
            G["Indices"]
        ],
        complexIndices = Range[-n2 - 1, -q, -1];
        newIndex = Map[
            #1 -> {
                With[{c = I^Count[#1, _?(MemberQ[complexIndices, #] &)]}, c],
                Map[If[MemberQ[complexIndices, #], p + (-# - n2), #] &, #1]
            } &,
            G["Indices"]
        ]
     ];
    newIndex
    ]


PackageExport["ConvertGeometricAlgebra"]

Options[ConvertGeometricAlgebra] = {"Reals" -> True};

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
    If[OptionValue["Reals"],
        Total @ Map[
            Apply[With[{c = canonicCoordinates[#2[[2]]] Conjugate[#2[[1]]]}, 
                Multivector[<|#1 -> Re[c]|>, G] + G["Pseudoscalar"] ** Multivector[<|#1 -> Im[c]|>, G]]
            &], fromCanonicConversion
        ],
        Multivector[
            Association[Apply[#1 -> canonicCoordinates[#2[[2]]] Conjugate[#2[[1]]] &] /@ fromCanonicConversion],
            G
        ]
     ]
    ]


PackageExport["CanonicalMultivector"]

CanonicalMultivector[v_Multivector, opts : OptionsPattern[]] :=
    ConvertGeometricAlgebra[
        v,
        GeometricAlgebra @ CanonicalGeometricAlgebra[v["GeometricAlgebra"]]["Signature"],
        opts
    ]


PackageExport["RealMultivector"]

RealMultivector[v_Multivector] := RealMultivector[v, v["GeometricAlgebra"]]
RealMultivector[v_Multivector, G_GeometricAlgebra] :=
    Multivector[Re[v["Coordinates"]], v["GeometricAlgebra"]] +
    Pseudoscalar[G] ** Multivector[Im[v["Coordinates"]], v["GeometricAlgebra"]]


PackageExport["MultivectorMatrix"]

MultivectorMatrix[v_Multivector, OptionsPattern[MultivectorMatrix]] := Module[{
    A, p, q, n, w, reIndex, re, im, X, M
},
    A = v["GeometricAlgebra"];
    {p, q} = A["Signature"];
    If[p != q && q != p + 1,
        Return[MultivectorMatrix[CanonicalMultivector[v]]]
    ];
    n = 2 p;
    w = RealMultivector[v];
    If[q == p + 1,
        reIndex = Catenate @ Position[A["Indices"], _List ? (FreeQ[#, -q] &), {1}];
        re = Multivector[w["Coordinates"][[reIndex]], {p, p}];
        im = ((re - w) ** A["Pseudoscalar"]);
        re = Normal[re];
        im = im[ "Coordinates"][[reIndex]];
        X = re + I im,

        X = w["Coordinates"];
    ];
    M = PseudoInverse[If[OptionValue["Basis"] == "Null", nullToStandardMatrix[p], spectralToStandardMatrix[p]]];
    Partition[M . X, 2 ^ p]
]


PackageExport["MatrixMultivector"]

Options[MatrixMultivector] = {"Reals" -> Automatic, Method -> "Matrix"};

MatrixMultivector::unknownMethod = "Method should be one of {\"Multivector\", \"Matrix\"}";

MatrixMultivector[mat_, OptionsPattern[MatrixMultivector]] := Module[{
    n, isReal, G, At, u, B, sa, M
},
    n = Log2[Length[mat]];
    If[Not[IntegerQ[n]],
        Return[$Failed]
    ];
    isReal = OptionValue["Reals"];
    If[isReal === Automatic, 
        isReal = MatrixQ[mat, NumericQ] && Element[mat, Reals]
    ];
    G = GeometricAlgebra[{n, If[isReal, n, n + 1]}];
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
         sa = MultivectorArray[
             Map[
                 Multivector[
                     {#}
                     (*If[isReal, {#}, SparseArray[{1 -> Re[#], -1 -> Im[#]}, G["Order"]]]*),
                     G
                 ] &,
                 mat,
                 {2}
             ],
             {2^n, -2^n}
         ];
         If[isReal, RealMultivector, Identity][((At ** u) ** (sa ** B))["Components"]],

         "Matrix",
         M = If[OptionValue["Basis"] == "Null", nullToStandardMatrix[n], spectralToStandardMatrix[n]];
         If[isReal,
            Multivector[M . Flatten[mat], G],
            RealMultivector @ Multivector[Multivector[M . Flatten[mat], GeometricAlgebra[n, n]], G]
         ],

         _,
         Message[MatrixMultivector::unknownMethod];
         $Failed
    ]

]

MatrixMultivector[mat_, G_GeometricAlgebra, opts: OptionsPattern[MatrixMultivector]] := Module[{
    n, isReal
},
    n = Log2[Length[mat]];
    isReal = G["Dimension"] == 2 n;
    If[Not[IntegerQ[n]] || (G["Dimension"] != 2 n && G["Dimension"] != 2 n + 1),
        Return[$Failed]
    ];
    ConvertGeometricAlgebra[
        MatrixMultivector[mat,
            "Reals" -> isReal,
            Sequence @@ FilterRules[{opts}, Except["Reals"]]
        ],
        G,
        "Reals" -> OptionValue["Reals"]
    ]
]


PackageExport["MultivectorFunction"]

MultivectorFunction[f_, v_Multivector] := ConvertGeometricAlgebra[
    MatrixMultivector[
        MatrixFunction[f, MultivectorMatrix[v]], 
        "Reals" -> EvenQ[v["GeometricAlgebra"]["Dimension"]]
    ],
    v["GeometricAlgebra"],
    "Reals" -> False
]


PackageScope["standardToNullMatrix"]


nullToStandardMatrix[n_Integer] := nullToStandardMatrix[n] = Module[{
    m = 2^n, sa, s
},
    sa = Array[s[##] &, {m, m}];

    Coefficient[#, Flatten @ sa] & /@ ComplexExpand @ Normal[MatrixMultivector[sa, "Reals" -> True, Method -> "Multivector"]]
]


spectralToStandardMatrix[n_Integer] := spectralToStandardMatrix[n] = Module[{
    m = 2^n, sa, s
},
    sa = Array[s[##] &, {m, m}];

    Coefficient[#, Flatten @ sa] & /@ ComplexExpand @ Normal[MatrixMultivector[sa, "Reals" -> True, "Basis" -> "Spectral", Method -> "Multivector"]]
]