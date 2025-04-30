Package["Wolfram`GeometricAlgebra`"]

PackageScope[$ElementwiseFunction]
PackageScope[numericFunctionQ]
PackageScope[hasDefinitionsQ]
PackageScope[permutationSignature]


$ElementwiseFunction = Simplify | FullSimplify | Expand | ComplexExpand | TrigExpand | ExpToTrig | TrigToExp

numericFunctionQ[f_] := MatchQ[f, _Symbol] && MemberQ[Attributes[f], NumericFunction]


hasDefinitionsQ[f_Symbol] := GeneralUtilities`HasDefinitionsQ[f] || Length[Attributes[f]] > 0

hasDefinitionsQ[f_] := GeneralUtilities`HasDefinitionsQ[f]


permutationSignature[x_List, y_List] := Signature[PermutationList[FindPermutation[x, y]]]
