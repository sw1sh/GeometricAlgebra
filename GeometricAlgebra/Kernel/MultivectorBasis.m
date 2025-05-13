Package["Wolfram`GeometricAlgebra`"]


PackageExport["MultivectorBasis"]





MultivectorBasis::usage = "MultivectorBasis[A, g] gives a list of multivectors from canonical basis of geometric algebra A with grade g";


MultivectorBasis[g_GeometricAlgebra, n_Integer ? Positive | All] := With[{
    from = If[n === All, 1, binomialSum[g["Dimension"], n - 1] + 1],
    to = If[n === All, g["Order"], binomialSum[g["Dimension"], n]]
    },
    Table[Multivector[SparseArray[{k -> 1}, g["Order"]], "GeometricAlgebra" -> g], {k, from, to}]
]

MultivectorBasis[g_GeometricAlgebra, n_Integer ? Negative] := MultivectorBasis[g, g["Dimension"] + n + 1]

MultivectorBasis[g_GeometricAlgebra, 0] := {Multivector[1, g]}

MultivectorBasis[g_GeometricAlgebra, "Even"] := Catenate[MultivectorBasis[g, #] & /@ Range[0, g["Dimension"], 2]]

MultivectorBasis[g_GeometricAlgebra, "Odd"] := Catenate[MultivectorBasis[g, #] & /@ Range[1, g["Dimension"], 2]]

MultivectorBasis[g_GeometricAlgebra] := MultivectorBasis[g, All]

MultivectorBasis[g_GeometricAlgebra, {args___}] := Catenate[MultivectorBasis[g, #] & /@ {args}]

MultivectorBasis[v_Multivector, args___] := MultivectorBasis[v["GeometricAlgebra"], args]

MultivectorBasis[args__] := MultivectorBasis[GeometricAlgebra[], args]

MultivectorBasis[] := MultivectorBasis[All]


(* Basis indexing *)

g_GeometricAlgebra[] := Multivector[1, g]

g_GeometricAlgebra[indices__Integer] := Multivector[<|{indices} -> 1|>, g]

g_GeometricAlgebra[indices : {___Integer}] := g /@ indices

g_GeometricAlgebra[indices : {{___Integer} ...}] := g @@@ indices

g_GeometricAlgebra["Basis", args___] := MultivectorBasis[g, args]

g_GeometricAlgebra["PseudoBasis", args___] := GeometricProduct[g["Pseudoscalar"], #] & /@ MultivectorBasis[g, args]

g_GeometricAlgebra["OrderedBasis"] := g[g["OrderedIndices"]]

