Package["GeometricAlgebra`"]

PackageScope["numericFunctionQ"]
PackageScope["hasDefinitionsQ"]



numericFunctionQ[f_] := MemberQ[Attributes[f], NumericFunction]


hasDefinitionsQ[f_Symbol] := GeneralUtilities`HasDefinitionsQ[f] || Length[Attributes[f]] > 0

hasDefinitionsQ[f_] := GeneralUtilities`HasDefinitionsQ[f]
