Package["Wolfram`GeometricAlgebra`"]


PackageExport["Multivector"]
Multivector::usage = "Multivector[coords, ga] gives a multivector in GeometricAlgebra ga";

PackageExport["MultivectorQ"]

PackageExport["NumberMultivector"]
NumberMultivector::usage = "NumberMultivector[x, ga] gives a multivector corresponding to a complex number x in geometric algebra ga";

PackageExport["MultivectorNumber"]
MultivectorNumber::usage = "MultivectorNumber[v] gives a complex number based on scalar and pseudoscalar parts of multivector";

PackageExport["ComplexMultivector"]

PackageExport["GeometricProduct"]
GeometricProduct::usage = "GeometricProduct[vs__] computes geometric product of multivectors";

PackageExport["Grade"]
Grade::usage = "Grade[v, n] gives a nth grade of a Multivector v or converts a list to a multivector";

PackageExport["AntiGrade"]
AntiGrade::usage = "AntiGrade[v, n] gives a nth anti grade of a Multivector v or converts a list to a multivector";

PackageExport["GradeList"]
GradeList::usage = "GradeList[v] gives a list of all grades of multivector";

PackageExport["LeftContraction"]
LeftContraction::usage = "LeftContraction[v, w] gives a left contraction of multivectors v and w";

PackageExport["RightContraction"]
RightContraction::usage = "RightContraction[v, w] gives a right contraction of multivectors v and w";

PackageExport["ScalarProduct"]
ScalarProduct::usage = "ScalarProduct[v, w] gives a scalar product of multivectors v and w";

PackageExport["InnerProduct"]
InnerProduct::usage = "InnerProduct[v, w] gives an inner product of multivectors v and w";

PackageExport["AntiInnerProduct"]
AntiInnerProduct::usage = "AntiInnerProduct[v, w] gives an anti inner product of multivectors v and w";

PackageExport["BulkExpansion"]
BulkExpansion::usage = "BulkExpansion[v] gives a bulk expansion of multivector v";

PackageExport["WeightExpansion"]
WeightExpansion::usage = "WeightExpansion[v] gives a weight expansion of multivector v";

PackageExport["BulkContraction"]
BulkContraction::usage = "BulkContraction[v, w] gives a bulk contraction of multivectors v and w";

PackageExport["WeightContraction"]
WeightContraction::usage = "WeightContraction[v, w] gives a weight contraction of multivectors v and w";

PackageExport["LeftDual"]
LeftDual::usage = "LeftDual[v] gives a left dual of multivector v";

PackageExport["RightDual"]
RightDual::usage = "RightDual[v] gives a right dual of multivector v";

PackageExport["Bulk"]
Bulk::usage = "Bulk[v] gives a bulk of multivector v";

PackageExport["Weight"]
Weight::usage = "Weight[v] gives a weight of multivector v";

PackageExport["RightBulkDual"]
RightBulkDual::usage = "RightBulkDual[v] gives a right bulk dual of multivector v";

PackageExport["RightWeightDual"]
RightWeightDual::usage = "RightWeightDual[v] gives a right weight dual of multivector v";

PackageExport["LeftBulkDual"]
LeftBulkDual::usage = "LeftBulkDual[v] gives a left bulk dual of multivector v";

PackageExport["LeftWeightDual"]
LeftWeightDual::usage = "LeftWeightDual[v] gives a left weight dual of multivector v";

PackageExport["BulkNorm"]
BulkNorm::usage = "BulkNorm[v] gives a bulk norm of multivector v";

PackageExport["WeightNorm"]
WeightNorm::usage = "WeightNorm[v] gives a weight norm of multivector v";

PackageExport["MultivectorCosAngle"]
MultivectorCosAngle::usage = "MultivectorCosAngle[v, w] gives a cosine of angle between multivectors v and w";

PackageExport["Involute"]
Involute::usage = "Involute[v] gives a multivector with its odd grades multiplied by -1";

PackageExport["Rejection"]
Rejection::usage = "Rejection[v, w] gives a rejection of multivector v on w";

PackageExport["AntiReverse"]
AntiReverse::usage = "AntiReverse[v] gives a multivector with its even grades multiplied by -1";

PackageExport["AntiDot"]
AntiDot::usage = "AntiDot[v, w] gives an anti dot product of multivectors v and w";

PackageExport["AntiGeometricProduct"]
AntiGeometricProduct::usage = "AntiGeometricProduct[v, w] gives an anti geometric product of multivectors v and w";

PackageExport["$DefaultMultivectorFormatFunction"]
$DefaultMultivectorFormatFunction::usage = "$DefaultMultivectorFormatFunction is a default function for formatting multivectors";

PackageScope["zeroMultivector"]
PackageScope["identityMultivector"]
PackageScope["geometricIndexFormat"]
PackageScope["multiplyIndices"]
PackageScope["orderAndContract"]

PackageScope["switchDualSide"]


Options[Multivector] = {
    "GeometricAlgebra" -> GeometricAlgebra[3],
    "Coordinates" -> Automatic
}


multivectorQ[HoldPattern[Multivector[opts : OptionsPattern[]]]] := MatchQ[
    Unevaluated[{opts}],
    KeyValuePattern[{"Coordinates" -> coords_, "GeometricAlgebra" -> A_}] /;
        GeometricAlgebraQ[Unevaluated[A]] && VectorQ[coords] && Length[coords] == A["Order"]
]

multivectorQ[___] := False


MultivectorQ[v_Multivector] := System`Private`HoldValidQ[v] || multivectorQ[Unevaluated[v]]

MultivectorQ[___] := False


$MultivectorProperties = {
    "GeometricAlgebra",
    "Coordinates",

    "Coordinate",
    "Association",
    "Span",
    "Grade",
    "Flatten",
    "Scalar",
    "Pseudoscalar",
    "Real",
    "ComplexCoordinates",

    "Reverse",
    "Involute",
    "Conjugate",
    "LeftComplement",
    "RightComplement",
    "DoubleComplement",
    "Squared",
    "Norm",
    "Normalized",

    "Inverse",

    "LeftDual",
    "RightDual",
    "Tr",
    "Det"
}


Multivector::truncCoord = "Coordinates are incompatible with `1`. Number of coordinates should be less than `2`. Truncating excessive coordinates.";


Multivector[coords_ ? VectorQ, opts: OptionsPattern[]] :=
    With[{A = GeometricAlgebra @ OptionValue["GeometricAlgebra"]},
        If[ Length @ coords > A["Order"],
            Message[Multivector::truncCoord, A, A["Order"]]
        ];
        Multivector["Coordinates" -> SparseArray[coords, A["Order"]], "GeometricAlgebra" -> A]
    ]


Multivector[assoc_Association, opts : OptionsPattern[]] := With[{A = GeometricAlgebra[OptionValue["GeometricAlgebra"]]},
    Multivector[Lookup[KeyValueMap[{index, x} |-> #1 -> x * #2 & @@ orderAndContract[normalIndex[index, A["Signature"]], A["Metric"]], assoc], A["Indices"], 0], opts]
]


Multivector[x : Except[_Association | _ ? VectorQ | _ ? NumericQ | OptionsPattern[]], opts: OptionsPattern[]] := Multivector[{x}, opts]


NumberMultivector[x_, A_GeometricAlgebra] :=
    Multivector[SparseArray[{1 -> Re[x], -1 -> If[A["PseudoscalarSquare"] == 1, I, 1] Im[x]}, A["ComplexOrder"]], A]

NumberMultivector[v_Multivector, A_GeometricAlgebra] :=
    Multivector[SparseArray[{1 -> v["Scalar"], -1 -> If[v["PseudoscalarSquare"] != A["PseudoscalarSquare"], I, 1] v["Pseudoscalar"]}, A["ComplexOrder"]], A]

NumberMultivector[x_] := NumberMultivector[x, GeometricAlgebra[0, 1]]


MultivectorNumber[v_Multivector, A_GeometricAlgebra] :=
    Multivector[SparseArray[{1 -> v["Scalar"], -1 -> v["Pseudoscalar"]}, A["ComplexOrder"]], A]

MultivectorNumber[v_Multivector] := MultivectorNumber[v, GeometricAlgebra @ If[v["PseudoscalarSquare"] == 1, {1, 0}, {0, 1}]]

MultivectorNumber[x_, ___] := x


ComplexMultivector[v_Multivector] := ComplexMultivector[v, v["GeometricAlgebra"]]

ComplexMultivector[v_Multivector, A_GeometricAlgebra] := ConvertGeometricAlgebra[MultivectorNumber[v, A["ComplexAlgebra"]], A]


Multivector[x_ ? NumericQ, opts: OptionsPattern[]] := NumberMultivector[x, OptionValue["GeometricAlgebra"]]

Multivector[v_Multivector, opts: OptionsPattern[]] := Multivector[mergeOptions[{{opts}, Options[v]}]]

Multivector[v_Multivector, args__] := Multivector[v, GeometricAlgebra[args]]

Multivector[arg: Except[_Multivector], A_GeometricAlgebra] := Multivector[arg, "GeometricAlgebra" -> A]

Multivector[arg_, p_Integer, q_Integer: 0, r_Integer: 0] := Multivector[arg, GeometricAlgebra[p, q, r]]

Multivector[arg_, {p_Integer, q_Integer: 0, r_Integer: 0}] := Multivector[arg, p, q, r]

Multivector[] := Multivector[{}]


v_Multivector[key : {___Integer}] := #2 Lookup[v["Association"], Key[#1], 0] & @@ orderIndexWithSign[normalIndex[key, v["Signature"]], v["Dimension"]]

v_Multivector[key___Integer] := v[{key}]

v_Multivector[keys : {{___Integer} ..}] := v /@ keys


v_Multivector[f_] := mapCoordinates[f, v]



Multivector /: v_Multivector[opt: Alternatives @@ Keys @ Options @ Multivector] := Lookup[Options[v], opt]

Multivector /: v_Multivector[opt: Alternatives @@ $GeometricAlgebraProperties] := v["GeometricAlgebra"][opt]


Multivector /: Normal[v_Multivector] := Normal @ v["Coordinates"]


_Multivector["Properties"] := $MultivectorProperties

v_Multivector["Coordinates", n_Integer] := v["Coordinates"][[indexSpan[v, n]]]

v_Multivector["Coordinates", {ns__Integer}] := Join @@ (v["Coordinates", #] & /@ {ns})


v_Multivector["Coordinate", n_Integer] := v["Coordinates"][[n]]

v_Multivector["Coordinate", {ns__Integer}] := v["Coordinates"][[{ns}]]

Multivector /: Part[v_Multivector, keys___] := v["Coordinate", keys]


v_Multivector["CoordinateDimension"] := Max[DualDimension /@ v["Coordinates"]]


v_Multivector["Association"] := Association @ Map[Apply[Function[{x, y}, v["Indices"][[First[x]]] -> y, HoldAllComplete]], Most @ ArrayRules[v["Coordinates"]]]


v_Multivector["Span"] := MapThread[GeometricProduct, {v["Coordinates"], MultivectorBasis[v["GeometricAlgebra"]]}]

v_Multivector["Span", n_Integer] := MapThread[GeometricProduct, {v["Coordinates"][[indexSpan[v, n]]], MultivectorBasis[v["GeometricAlgebra"], n]}]

v_Multivector["Span", {ns__Integer}] := Catenate[v["Span", #] & /@ {ns}]


v_Multivector["Flatten"] := Inner[GeometricProduct, v["Coordinates"], v["Basis"]]


v_Multivector["Real"] := v[Re] + v["GeometricAlgebra"]["Pseudoscalar"] ** v[Im]


v_Multivector["Numeric"] := If[v["ComplexDimension"] > 0 && v["PseudoscalarSquare"] == 1,
    v["Scalar"] IdentityMatrix[2] +  v["Pseudoscalar"] Reverse @ IdentityMatrix[2],
    v["Scalar"] +  I v["Pseudoscalar"]
]


v_Multivector["ComplexCoordinates"] := Module[{
    A, re, im
},
    A = v["GeometricAlgebra"];

    If[ OddQ[A["Dimension"]],
        re = Lookup[v["Association"], A["ReIndices"], 0];
        im = Lookup[(v ** A["Pseudoscalar"])["Association"], A["ReIndices"], 0];

        re + A["PseudoscalarSquare"] im ** A["Pseudoscalar"],

        (* Else *)
        v["Coordinates"]
    ]
]


v : Multivector[opts: OptionsPattern[]] /; ! multivectorQ[Unevaluated[v]] := With[{
    coords = Lookup[{opts}, "Coordinates"],
    A = GeometricAlgebra[Lookup[{opts}, "GeometricAlgebra"]]
},
    With[{
        newOpts = Sequence[
            "Coordinates" -> If[VectorQ[coords], If[Length[coords] != A["Order"], PadRight[coords, A["Order"]], coords], SparseArray[{}, A["Order"]]],
            "GeometricAlgebra" -> A
        ]
    },
        Multivector[newOpts]
    ]
]

v_Multivector /; System`Private`HoldNotValidQ[v] && multivectorQ[Unevaluated[v]] := System`Private`SetNoEntry[System`Private`HoldSetValid[v]]


(* Coersion *)

Multivector[v_Multivector, A_GeometricAlgebra] /; v["GeometricAlgebra"] === A := v

Multivector[v_Multivector, A_GeometricAlgebra] := Multivector[
    SparseArray[
        Map[Apply[With[{
                pos = Position[A["Indices"], Extract[v["GeometricAlgebra"]["Indices"], #1]]
            },
                If[Length[pos] > 0, pos[[1]] -> #2, Nothing]
            ] &],
            Most @ ArrayRules @ v["Coordinates"]
        ],
        A["Order"]
    ],
    A
]


(* Addition *)

zeroMultivector[A_GeometricAlgebra] := Multivector[{}, A]
zeroMultivector[v_Multivector] := zeroMultivector[v["GeometricAlgebra"]]


Multivector /: Plus[vs__Multivector] /; Length[{vs}] > 1 := Module[{
    A = mergeGeometricAlgebra[vs],
    ws
},
    ws = Multivector[#, A] & /@ {vs};
    Multivector[
        Total[#["Coordinates"] & /@ ws],
        A
    ][Map[reduceFunctions]]
]

Multivector /: Plus[x: Except[_Multivector], v_Multivector] := x identityMultivector[v] + v


A_GeometricAlgebra["Zero"] := zeroMultivector[A]


(* Scalar multiplication *)

identityMultivector[A_GeometricAlgebra] := Multivector[{1}, A]

identityMultivector[v_Multivector] := identityMultivector[v["GeometricAlgebra"]]


Multivector /: Times[x: Except[_Multivector], v_Multivector] := mapCoordinates[x # &, v]


v_Multivector["Scalar"] := v["Coordinate", 1]


v_Multivector["Pseudoscalar"] := v["Coordinate", -1]


A_GeometricAlgebra["Identity"] := identityMultivector[A]


(* Geometric Product *)

A_GeometricAlgebra["BasisMatrix"] := A["BasisMatrix"] = ExteriorMatrix[A["VectorBasis"]]

A_GeometricAlgebra["InverseBasisMatrix"] := A["InverseBasisMatrix"] = ExteriorMatrix[MatrixInverse[A["VectorBasis"]]]

A_GeometricAlgebra["MultiplicationTensor"] := A["MultiplicationTensor"] = With[{indices = A["Indices"], metric = A["MetricSignature"]}, {index = PositionIndex[indices]},
    SparseArray[Outer[SparseArray[Normal @ KeyMap[Lookup[index, Key[#]] &, multiplyIndices[#1, #2, metric]], Length[indices]] &, indices, indices, 1]]
]

A_GeometricAlgebra["MetricMultiplicationTensor"] := A["MetricMultiplicationTensor"] = With[{a = Transpose @ A["BasisMatrix"], b = Transpose @ A["InverseBasisMatrix"]},
    SparseArray[Transpose[b . Transpose[b . A["MultiplicationTensor"]]] . a]
]

A_GeometricAlgebra["ExomorphismMatrix"] := A["ExomorphismMatrix"] =
    SparseArray @ With[{metric = A["Metric"]}, Wedge[##]["Coordinates"] & @@@ Replace[Map[Grade[Transpose[metric[[All, #]]], 1, A] &, A["Indices"], {2}], {} -> {A[]}, 1]]

A_GeometricAlgebra["AntiExomorphismMatrix"] := With[{perm = FindPermutation[A["Indices"], A["DualIndices"]]},
    Permute[Transpose[Permute[A["ExomorphismMatrix"], perm]], perm]
]
    

(* A_GeometricAlgebra["AntiExomorphismMatrix"] := A["AntiExomorphismMatrix"] = Det[A["Metric"]] * SparseArray[PseudoInverse[A["ExomorphismMatrix"]]] *)

Bulk[v_Multivector] := With[{A = GeometricAlgebra[v]}, Multivector[A["ExomorphismMatrix"] . v["Coordinates"], A]]

v_Multivector["Bulk"] := Bulk[v]

Weight[v_Multivector] := With[{A = GeometricAlgebra[v]}, Multivector[A["AntiExomorphismMatrix"] . v["Coordinates"], A]]

v_Multivector["Weight"] := Weight[v]

v_Multivector["BulkDual" | "RightBulkDual"] := RightBulkDual[v]

v_Multivector["LeftBulkDual"] := LeftBulkDual[v]

v_Multivector["WeightDual" | "RightWeightDual"] := RightWeightDual[v]

v_Multivector["LeftWeightDual"] := LeftWeightDual[v]

RightBulkDual[v_] := OverBar[Bulk[v]]

RightWeightDual[v_] := OverBar[Weight[v]]

LeftBulkDual[v_] := UnderBar[Bulk[v]]

LeftWeightDual[v_] := UnderBar[Weight[v]]


switchDualSide[v_Multivector] :=
    Multivector[
        MapThread[Function[{signs, x}, With[{coords = DualCoordinates[x]}, Dual @@ (Take[signs, Length[coords]] coords)], HoldAllComplete], {antiProductSigns[v["Dimension"], v["CoordinateDimension"]], Normal @ v["Coordinates"]}],
        v["GeometricAlgebra"]
    ]

A_GeometricAlgebra["MultiplicationTable"] := ResourceFunction["GridTableForm"][
    Map[Multivector[#, A] &, A["MultiplicationTensor"], {2}],
    TableHeadings -> {A["Basis"], A["Basis"]}
]

A_GeometricAlgebra["MetricMultiplicationTable"] := ResourceFunction["GridTableForm"][
    Map[Multivector[#, A] &, A["MetricMultiplicationTensor"], {2}],
    TableHeadings -> {A["Basis"], A["Basis"]}
]


GeometricProduct::usage = "GeometricProduct[v, w] or (v ** w) gives a geometric product of multivectors v and w";

GeometricProduct[v_Multivector, w_Multivector] := With[{
    A = mergeGeometricAlgebra[v, w]
},
    Multivector[
        Flatten[Outer[coordinateTimes, Multivector[v, A]["Coordinates"], Multivector[w, A]["Coordinates"], 1], 1] . Flatten[A["MetricMultiplicationTensor"], 1],
        A
    ][Map[reduceFunctions]]
]

GeometricProduct[x_, y_] := x * y

GeometricProduct[left___, v_Multivector, right___] := Fold[GeometricProduct, {left, v, right}]

GeometricProduct[] := Multivector[{1}, {0, 0}]


Multivector /: Times[vs__Multivector] := GeometricProduct[vs]


Multivector /: Power[v_Multivector, n_Integer] := If[n < 0, Power[Inverse[v], -n], Nest[# ** v &, identityMultivector[v], n]]


Multivector /: Equal[vs__Multivector] := With[{A = GeometricAlgebra[First[{vs}]]}, And @@ MapThread[Equal, Normal /@ Map[Multivector[#, A] &, {vs}]]]


Multivector /: (f_Symbol ? elementwiseFunctionQ)[v_Multivector, args___] := v[Map[f[#, args] &]]


Multivector /: N[v_Multivector ? MultivectorQ, args___] := With[{coords = N[v["Coordinates"], args]}, Multivector[coords, v["GeometricAlgebra"]] /; coords =!= v["Coordinates"]]

SetAttributes[Multivector, NHoldAll]


(* Tensor product *)

Multivector /: TensorProduct[v_Multivector, w_Multivector] := Module[{
    p, q, r
},
    {p, q, r} = v["Signature"];
    v ** Multivector[
        KeyMap[# /. {i_ ? Positive :> i + p, i_ ? Negative :> i - q} &, w["Association"]],
        GeometricAlgebra[{p, q} + w["Signature"]]
    ]
]


(* infix notation *)

Multivector /: NonCommutativeMultiply[left___, v_Multivector, right___] := GeometricProduct[left, v, right]


(* Product contractions *)

gradeProduct[v_Multivector, w_Multivector] := Outer[GeometricProduct, GradeList[v], GradeList[w]]

gradeFunctionContraction[f_, vs__Multivector] := Fold[Total[MapIndexed[Grade[#1, f[#2 - 1]] &, gradeProduct[##], {2}], 2] &, {vs}]

LeftContraction[vs__Multivector] := gradeFunctionContraction[Apply[Subtract] @* Reverse, vs]

RightContraction[vs__Multivector] := gradeFunctionContraction[Apply[Subtract], vs]

Multivector /: Dot[vs__Multivector] := gradeFunctionContraction[Abs @* Apply[Subtract], vs]

Multivector /: Wedge[vs__Multivector] := gradeFunctionContraction[Apply[Plus], vs]

Multivector /: Vee[vs__Multivector] := OverBar[Wedge @@ UnderBar /@ {vs}]

Multivector /: Cross[vs__Multivector] := UnderBar[Wedge[vs]]

ScalarProduct[vs__Multivector] := Grade[GeometricProduct[vs], 0]

AntiDot[vs__Multivector] := OverBar[Dot @@ UnderBar /@ {vs}]

InnerProduct[v_Multivector, w_Multivector] := With[{A = mergeGeometricAlgebra[v, w]},
    Multivector[Multivector[w, A]["Coordinates"] . A["ExomorphismMatrix"] . Multivector[v, A]["Coordinates"], A]
]

AntiInnerProduct[v_Multivector, w_Multivector] := With[{A = mergeGeometricAlgebra[v, w]},
    Grade[{Multivector[w, A]["Coordinates"] . A["AntiExomorphismMatrix"] . Multivector[v, A]["Coordinates"]}, -1, A]
]

(* AntiInnerProduct[v_Multivector, w_Multivector] := OverBar[InnerProduct[UnderBar[v], UnderBar[w]]] *)

AntiGeometricProduct[vs__Multivector] := OverBar[GeometricProduct @@ UnderBar /@ {vs}]

BulkExpansion[v_Multivector, w_Multivector] := Wedge[v, RightBulkDual[w]]

WeightExpansion[v_Multivector, w_Multivector] := Wedge[v, RightWeightDual[w]]

BulkContraction[v_Multivector, w_Multivector] := Vee[v, RightBulkDual[w]]

WeightContraction[v_Multivector, w_Multivector] := Vee[v, RightWeightDual[w]]

MultivectorDistance[v_Multivector, w_Multivector] := Vee[v, w] + WeightNorm[Wedge[v, Attitude[w]]]

MultivectorCosAngle[v_Multivector, w_Multivector] := WeightContraction[v, w] + WeightNorm[v] ** WeightNorm[w]


(* Inversions *)

reverseIndexCoordinate[A_GeometricAlgebra, indexPos_, x_] := Module[{newIndex, sign}, 
    {newIndex, sign} = orderIndexWithSign[Reverse[Extract[A["Indices"], indexPos]], A["Dimension"]];
    newIndex -> sign x
]

v_Multivector["Reverse"] := Multivector[
    Association[reverseIndexCoordinate[v["GeometricAlgebra"], #1, #2] & @@@ Most @ ArrayRules @ v["Coordinates"]],
    v["GeometricAlgebra"]
]

OverTilde[v_Multivector] ^:= v["Reverse"]

AntiReverse[v_Multivector] := v["Reverse"]["DoubleComplement"]

v_Multivector["AntiReverse"] := AntiReverse[v]


Involute[v_Multivector] := mapCoordinates[((-1) ^ # & @* Length /@ v["GeometricAlgebra"]["Indices"]) # &, v]

v_Multivector["Involute"] = Involute[v]


v_Multivector["Conjugate"] = v["Involute"]["Reverse"]

SuperStar[v_Multivector] ^:= v["Conjugate"]


v_Multivector["LeftComplement"] := With[{i = Last @ v["Indices"]},
     Multivector[
        Association @ KeyValueMap[
            Function[{j, x}, With[{k = DeleteElements[i, j]}, k -> permutationSignature[i, Join[k, j]] x], HoldAllComplete],
            v["Association"]
        ],
        v["GeometricAlgebra"]
    ]
]

v_Multivector["RightComplement"] := With[{i = Last @ v["Indices"]},
     Multivector[
        Association @ KeyValueMap[
            Function[{j, x}, With[{k = DeleteElements[i, j]}, k -> permutationSignature[i, Join[j, k]] x], HoldAllComplete],
            v["Association"]
        ],
        v["GeometricAlgebra"]
    ]
]


Multivector /: UnderBar[v_Multivector] := v["LeftComplement"]

Multivector /: OverBar[v_Multivector] := v["RightComplement"]


v_Multivector["DoubleComplement"] := v["RightComplement"]["RightComplement"]


Multivector /: Projection[v_Multivector, w_Multivector] := w ** (v . w)


Rejection[v_Multivector, w_Multivector] := (v \[Wedge] w) ** w


v_Multivector["Squared"] = v ** v["Involute"]


(* Inverse *)

Multivector /: Inverse[v_Multivector] := MultivectorFunction[# ^ -1 &, v]

v_Multivector["Inverse"] = Inverse[v]

Multivector /: Divide[v_, w_Multivector] := Multivector[v, w["GeometricAlgebra"]] ** Inverse[w]


(* Root and Power *)

Multivector /: Sqrt[v_Multivector] := v ^ (1 / 2)

Multivector /: Root[v_Multivector, n_Integer] := MultivectorFunction[Power[#, 1 / n] &, v]

Multivector /: Power[v_Multivector, p_Rational] := With[{n = Numerator[p], d = Denominator[p]}, Root[v ^ n, d]]

Multivector /: Power[v_Multivector, x_] := MultivectorPower[v, x]


(* Grade *)


Grade[v_Multivector, n_Integer] /; n < 0 || n > v["Dimension"] := zeroMultivector[v]

Grade[v_Multivector, n_Integer] := With[{vector = gradeVector[v["GeometricAlgebra"], n]}, mapCoordinates[# * vector &, v]]

Grade[v_Multivector] := With[{grades = Length /@ Extract[v["Indices"], SparseArray[v["Coordinates"]]["ExplicitPositions"]]},
    If[Equal @@ grades, First[grades], Indeterminate]
]

GradeList[v_Multivector] := Grade[v, #] & /@ Range[0, v["Dimension"]]

Grade[coords_List, n_Integer, args___] := Block[{
    G = GeometricAlgebra[args], d,
    skipDimension, bladeDimension
},
    d = G["Dimension"];
    k = Mod[n, d + 1];
    skipDimension = binomialSum[d, k - 1];
    bladeDimension = Binomial[d, k];
    Multivector[SparseArray[MapIndexed[Function[{x, i}, skipDimension + i -> x, HoldAllComplete], Take[coords, UpTo[bladeDimension]]], G["Order"]], G]
]

Grade[x_, n_Integer, args___] := Grade[{x}, n, args]

Grade[v_Multivector, "Even"] := Total[Grade[v, #] & /@ Range[0, v["Dimension"], 2]]

Grade[v_Multivector, "Odd"] := Total[Grade[v, #] & /@ Range[1, v["Dimension"], 2]]


AntiGrade[v_Multivector] := v["Dimension"] - Grade[v]

AntiGrade[args___] := Grade[args]["Dual"]


v_Multivector["Grade", arg_] := Grade[v, arg]


(* Special multivectors *)

A_GeometricAlgebra["Scalar"] := Multivector[1, A]


pseudoscalar[A_GeometricAlgebra] := Block[{
    p, q, r
},
    {p, q, r} = A["Signature"];
    Multivector[SparseArray[{{-1} -> 1}, A["Order"]], A]
]

pseudoscalar[v_Multivector] := pseudoscalar[v["GeometricAlgebra"]]

pseudoscalar[] := pseudoscalar[GeometricAlgebra[]]

A_GeometricAlgebra["Pseudoscalar"] := pseudoscalar[A]


A_GeometricAlgebra["Nilpotent", n_Integer] := With[{
    i = Min[Min[A["ComplexSignature"]], Abs[n]]
},
    Multivector[<|{i} -> 1 / 2, {-i} -> Sign[n] 1 / 2|>, A]
]


A_GeometricAlgebra["Idempotent", n_Integer] := A["Nilpotent", - n] ** A["Nilpotent", n]


(* Duals *)

LeftDual[v_Multivector] := LeftContraction[v, pseudoscalar[v]["Reverse"]]

v_Multivector["LeftDual"] := LeftDual[v]


RightDual[v_Multivector] := RightContraction[pseudoscalar[v], v]

v_Multivector["RightDual"] := RightDual[v]


v_Multivector["Dual"] := LeftDual[v]


(* Norms *)

v_Multivector["BulkNorm"] := BulkNorm[v]

BulkNorm[v_Multivector] := InnerProduct[v, v][Sqrt]

v_Multivector["WeightNorm"] := WeightNorm[v]

WeightNorm[v_Multivector] := AntiInnerProduct[v, v][Sqrt]

v_Multivector["Norm" | "GeometricNorm"] := v["BulkNorm"] + v["WeightNorm"]

Multivector /: Norm[v_Multivector] := v["Norm"]

\[LeftDoubleBracketingBar] v_Multivector \[RightDoubleBracketingBar] := v["Norm"]

v_Multivector["CoordinateNorm"] := Norm[v["Coordinates"]]

v_Multivector["Normalize"] := v / v["Norm"]

Multivector /: Normalize[v_Multivector] := v["Normalize"]

v_Multivector["Unitize" | "WeightUnitize"] := v / WeightNorm[v]["Pseudoscalar"]

Multivector /: OverHat[v_Multivector] := v["Unitize"]

v_Multivector["Tr"] := v + v["Conjugate"]

Multivector /: Tr[v_Multivector] := v["Tr"]


v_Multivector["Det"] := v ** v["Conjugate"]

Multivector /: Det[v_Multivector] := v["Det"]

\[LeftBracketingBar] v_Multivector \[RightBracketingBar] := v["Det"]


(* Formatting *)

$DefaultMultivectorFormatFunction = Function[index,
    If[ index === {},
        "", (* don't display zero coefficient terms *)
        Subscript["e", Row[If[# > 0, #, UnderBar[Abs[#]]] & /@ index, "\[InvisibleComma]"]]
    ]
]


geometricIndexFormat[A_GeometricAlgebra, index_] := With[{format = A["FormatIndex"][[2]]},
    Switch[format,
        Automatic,
        $DefaultMultivectorFormatFunction[index]
        ,
        "Positive",
        $DefaultMultivectorFormatFunction[positiveIndex[index, A["Signature"]] - 1]
        ,
        _Function,
        format[index]
        ,
        _,
        index /. Append[_ -> $DefaultMultivectorFormatFunction[index]] @ DeleteCases[Except[_Rule]] @ Developer`ToList[format]
    ]
]

geometricIndexFormat[index_] := geometricIndexFormat[Lookup[Options[Multivector], "GeometricAlgebra"], index]

geometricIndexFormat[A_GeometricAlgebra, {indices__List}] := geometricIndexFormat[A, #] & /@ {indices}

geometricIndexFormat[A_GeometricAlgebra] := geometricIndexFormat[A, A["Indices"]]

geometricIndexFormat[] := geometricIndexFormat[Lookup[Options[Multivector], "GeometricAlgebra"]]


SetAttributes[holdSparseArray, {HoldAll}];

holdSparseArray[a : SparseArray[{}, ___]] := a
holdSparseArray[SparseArray[{xs__}, opts___]] := SparseArray[List @@ MapAt[Hold, Hold[xs], {All, 2}], opts]

Multivector /: MakeBoxes[v : HoldPattern[Multivector[opts___]] /; MultivectorQ[Unevaluated[v]], _] := Block[{
    A = GeometricAlgebra[v],
    holdCoords, coords,
    rules,
    nonZeroPositions,
    d, n,
    indices, metric,
    display, interpret,
    boxes,
    optBoxes
},
    d = A["Dimension"];
    indices = A["FormatIndices"];
    metric = A["Metric"];
    holdCoords = Lookup[List @@ RuleDelayed @@@ Hold[opts], "Coordinates", None, Hold];
    coords =  Which[
        MatchQ[holdCoords, Hold[SparseArray[Automatic, ___]]], (* don't hold elements of a SparseArray object *)
        Map[Hold, ReleaseHold @ holdCoords],
        MatchQ[holdCoords, Hold[SparseArray[___]]], (* hold each element of a SparseArray constructor *)
        First[holdSparseArray /@ holdCoords],
        True,
        First @ MapAt[Hold, holdCoords, {1, All}] (* hold elements of a List *)
    ];
    coords = Extract[coords, Lookup[PositionIndex[A["Indices"]], SortBy[#, Mod[#, d + 1] &] & /@ indices, Nothing]];
    rules = Cases[ArrayRules[coords], ({i_Integer} -> c_) /; If[c != Hold[0], True, False, True] :> {i, c}];
    optBoxes = ToBoxes /@ FilterRules[{opts}, Except["Coordinates"]];
    If[Length[optBoxes] > 0,
        optBoxes = Riffle[optBoxes, ",", {1, 2 Length[optBoxes], 2}]
    ];
    nonZeroPositions = rules[[All, 1]];
    n = Length @ nonZeroPositions;
    coords = Riffle[MakeBoxes /@ MapThread[Rule, {nonZeroPositions, Slot /@ Range[n]}], ","];
    boxes = ReleaseHold @ Map[
            Apply[
                Function[{i, holdCoord}, If[ i > 1,
                    Function[x, With[{coord = x * orderAndContract[indices[[i]], metric][[2]]}, Switch[coord,
                        1, InterpretationBox["\[InvisibleSpace]", coord],
                        -1, InterpretationBox["-", coord],
                        _, If[MatchQ[coord, _Multivector | _Dual], RowBox[{"(", ToBoxes @ coord, ")"}], Parenthesize[coord, StandardForm, Times]]
                    ]], HoldAllComplete] @@ holdCoord,
                    MakeBoxes @@ holdCoord
                ], HoldRest]
            ],
            rules
        ];
    display = RowBox @
    If[ n > 0,
        Riffle[
            MapThread[
                RowBox[{
                    #1,
                    StyleBox[ToBoxes[geometricIndexFormat[A, #2]], "ShowStringCharacters" -> False]}
                ] &,
                { Slot /@ Range[n], indices[[nonZeroPositions]]}
            ],
        "+"
        ],
        {0} (* all zeros displayed as just zero *)
    ];
    interpret = RowBox[{"Multivector", "[",
        "\"Coordinates\"", "->", 
        "SparseArray", "[", "{",
            Sequence @@ If[n > 0, 
                Riffle[MapThread[RowBox[{ToBoxes[#1], "->", #2}] &, {nonZeroPositions, Slot /@ Range[n]}], ","],
                {}
            ],
            "}", ",", ToBoxes[A["Order"]],
        "]",
        Sequence @@ optBoxes, "]"}
    ];
    TemplateBox[
        boxes,
        "Multivector",
        DisplayFunction -> (Evaluate @ display &),
        InterpretationFunction -> (Evaluate @ interpret &),
        Tooltip -> RowBox[{"Multivector", " ", ToBoxes @ A}],
        Editable -> True
    ]]


(* Frontend *)

UsingFrontEnd[
    SetOptions[EvaluationNotebook[],
        InputAliases -> {"gp" ->
        TemplateBox[{"\[Placeholder]", "\[Placeholder]"},
            "GeometricProduct",
            InterpretationFunction -> (RowBox[{#1, "**", #2}] &),
            DisplayFunction -> (RowBox[{#1, " ", #2}] &)]
        }
    ]
]
