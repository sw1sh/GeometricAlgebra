Package["GeometricAlgebra`"]


PackageExport["MultivectorBasis"]


PackageScope["binomialSum"]
PackageScope["gradeIndices"]


MultivectorBasis::usage = "MultivectorBasis[A, g] gives a list of multivectors from canonical basis of geometric algebra A with grade g";


MultivectorBasis[A_GeometricAlgebra, n_Integer ? Positive | All] := With[{
    from = If[n === All, 1, binomialSum[A["Dimension"], n - 1] + 1], 
    to = If[n === All, A["Order"], binomialSum[A["Dimension"], n]]
    },
    Table[Multivector[SparseArray[{k -> 1}, A["Order"]], "GeometricAlgebra" -> A], {k, from, to}]
]

MultivectorBasis[A_GeometricAlgebra, n_Integer ? Negative] := MultivectorBasis[A, A["Dimension"] + n + 1]

MultivectorBasis[A_GeometricAlgebra, 0] := {Multivector[1, A]}

MultivectorBasis[A_GeometricAlgebra] := MultivectorBasis[A, All]

MultivectorBasis[A_GeometricAlgebra, {args___}] := MultivectorBasis[A, #] & /@ {args}

MultivectorBasis[v_Multivector, args___] := MultivectorBasis[v["GeometricAlgebra"], args]

MultivectorBasis[args__] := MultivectorBasis[GeometricAlgebra[], args]

MultivectorBasis[] := MultivectorBasis[All]


A_GeometricAlgebra["Basis"] := MultivectorBasis[A]

A_GeometricAlgebra["PseudoBasis"] := A["Pseudoscalar"] ** # & /@ MultivectorBasis[A]


(* Utility functions *)

binomialSum[n_Integer, k_Integer] := Module[{i}, Evaluate[Sum[Binomial[n, i], {i, 0, k}]]]
