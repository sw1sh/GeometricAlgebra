Package["Wolfram`GeometricAlgebra`"]

PackageExport[ExteriorMatrix]

PackageScope[elementwiseFunctionQ]
PackageScope[numericFunctionQ]
PackageScope[hasDefinitionsQ]
PackageScope[permutationSignature]


elementwiseFunctionQ = MatchQ[Simplify | FullSimplify | Expand | ComplexExpand | TrigExpand | ExpToTrig | TrigToExp | Chop]

numericFunctionQ[f_] := MatchQ[f, _Symbol] && MemberQ[Attributes[f], NumericFunction]


hasDefinitionsQ[f_Symbol] := GeneralUtilities`HasDefinitionsQ[f] || Length[Attributes[f]] > 0

hasDefinitionsQ[f_] := GeneralUtilities`HasDefinitionsQ[f]


permutationSignature[x_List, y_List] := Signature[PermutationList[FindPermutation[x, y]]]


ExteriorMatrix[matrix_ ? SquareMatrixQ] := With[{n = Length[matrix]},
	BlockDiagonalMatrix @ Table[
		With[{subsets = Subsets[Range[n], {k}]},
			SparseArray @ Map[columns |->
				Total[Signature[#] * Times @@ MapThread[Part, {columns, #}] & /@ Permutations[#]] & /@ subsets,
				Map[matrix[[All, #]] &, subsets, {2}]
			]
		],
		{k, 0, n}
	]
]
