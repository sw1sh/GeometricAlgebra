Package["Wolfram`GeometricAlgebra`"]

PackageExport["MultivectorTransform"]
MultivectorTransform::usage = "MultivectorTransform[v, t] applies transformation t to multivector v";


MultivectorTransform[v_Multivector, "Conformal"] := Module[{p, q, A, e1, e2, o, n, w},
    {p, q} = v["GeometricAlgebra"]["Signature"];
    A = GeometricAlgebra["Signature" -> {p + 1, q + 1}];

    Internal`InheritedBlock[{Multivector},
        SetOptions[Multivector, "GeometricAlgebra" -> A];
        e1 = Multivector[<|{p + 1} -> 1|>];
        e2 = Multivector[<|{- q - 1} -> 1|>];
        o = (e2 - e1)/2;
        n = e1 + e2;
        w = Multivector[v, A];
        o + w + 1/ 2 w^2 ** n
    ]
]
