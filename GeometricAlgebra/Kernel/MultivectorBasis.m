Package["Wolfram`GeometricAlgebra`"]


PackageExport["MultivectorBasis"]


PackageScope["binomialSum"]
PackageScope["gradeIndices"]
PackageScope["indexSpan"]
PackageScope["normalIndex"]
PackageScope["positiveIndex"]


MultivectorBasis::usage = "MultivectorBasis[A, g] gives a list of multivectors from canonical basis of geometric algebra A with grade g";


MultivectorBasis[A_GeometricAlgebra, n_Integer ? Positive | All] := With[{
    from = If[n === All, 1, binomialSum[A["Dimension"], n - 1] + 1],
    to = If[n === All, A["Order"], binomialSum[A["Dimension"], n]]
    },
    Table[Multivector[SparseArray[{k -> 1}, A["Order"]], "GeometricAlgebra" -> A], {k, from, to}]
]

MultivectorBasis[A_GeometricAlgebra, n_Integer ? Negative] := MultivectorBasis[A, A["Dimension"] + n + 1]

MultivectorBasis[A_GeometricAlgebra, 0] := {Multivector[1, A]}

MultivectorBasis[A_GeometricAlgebra, "Even"] := Catenate[MultivectorBasis[A, #] & /@ Range[0, A["Dimension"], 2]]

MultivectorBasis[A_GeometricAlgebra, "Odd"] := Catenate[MultivectorBasis[A, #] & /@ Range[1, A["Dimension"], 2]]

MultivectorBasis[A_GeometricAlgebra] := MultivectorBasis[A, All]

MultivectorBasis[A_GeometricAlgebra, {args___}] := Catenate[MultivectorBasis[A, #] & /@ {args}]

MultivectorBasis[v_Multivector, args___] := MultivectorBasis[v["GeometricAlgebra"], args]

MultivectorBasis[args__] := MultivectorBasis[GeometricAlgebra[], args]

MultivectorBasis[] := MultivectorBasis[All]


A_GeometricAlgebra[] := Multivector[1, A]

A_GeometricAlgebra[index_Integer] := Multivector[SparseArray[{{positiveIndex[index, A["Signature"]]} -> 1}, A["Order"]], A]

A_GeometricAlgebra[indices__Integer] := GeometricProduct @@ A /@ {indices}

A_GeometricAlgebra[indices : {___Integer}] := A @@ indices

A_GeometricAlgebra[indices : {{___Integer} ...}] := A @@@ indices

A_GeometricAlgebra["Basis", args___] := MultivectorBasis[A, args]

A_GeometricAlgebra["PseudoBasis", args___] := A["Pseudoscalar"] ** # & /@ MultivectorBasis[A, args]


(* Utility functions *)

binomialSum[n_Integer, k_Integer] := Module[{i}, Evaluate[Sum[Binomial[n, i], {i, 0, k}]]]


gradeIndices[A_GeometricAlgebra, k_Integer] := SparseArray[
    Thread[Range[binomialSum[A["Dimension"], k - 1] + 1, binomialSum[A["Dimension"], k]] -> 1],
    A["Order"]
]


indexSpan[v_Multivector, n_Integer] :=
    binomialSum[v["GeometricAlgebra"]["Dimension"], n - 1] + 1 ;; binomialSum[v["GeometricAlgebra"]["Dimension"], n]

indexSpan[_Multivector, All] := All


positiveIndex[index_Integer, {p_, q_, _}] := 1 + If[index >= 0, index, Min[- index, q] + p]

positiveIndex[indices : {___Integer}, signature_] := positiveIndex[#, signature] & /@ indices


normalIndex[index_Integer, {p_, q_, r_}] := If[index < 0, Max[index, - q], If[index > p + r, Max[p + r - index, - q], Min[index, p + r]]]

normalIndex[indices : {___Integer}, signature_] := normalIndex[#, signature] & /@ indices

