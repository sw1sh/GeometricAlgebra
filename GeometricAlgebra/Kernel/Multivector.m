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

PackageExport["GradeList"]
GradeList::usage = "GradeList[v] gives a list of all grades of multivector";

PackageExport["LeftContraction"]
LeftContraction::usage = "LeftContraction[v, w] gives a left contraction of multivectors v and w";

PackageExport["RightContraction"]
RightContraction::usage = "RightContraction[v, w] gives a right contraction of multivectors v and w";

PackageExport["ScalarProduct"]
ScalarProduct::usage = "ScalarProduct[v, w] gives a scalar product of multivectors v and w";

PackageExport["LeftDual"]
LeftDual::usage = "LeftDual[v] gives a left dual of multivector v";

PackageExport["RightDual"]
RightDual::usage = "RightDual[v] gives a right dual of multivector v";

PackageExport["Involute"]
Involute::usage = "Involute[v] gives a multivector with its odd grades multiplied by -1";

PackageExport["Projection"]

PackageExport["Rejection"]
Rejection::usage = "Rejection[v, w] gives a rejection of multivector v on w";

PackageExport["AntiReverse"]

PackageExport["AntiDot"]

PackageExport["AntiGeometricProduct"]

PackageScope["zeroMultivector"]
PackageScope["identityMultivector"]
PackageScope["geometricIndexBoxes"]
PackageScope["multiplyIndices"]
PackageScope["$defaultMultivectorFormatFunction"]

PackageScope["switchDualSide"]


Options[Multivector] = {
    "GeometricAlgebra" -> GeometricAlgebra[3],
    "Coordinates" -> Automatic
}


multivectorQ[HoldPattern[Multivector[opts : OptionsPattern[]]]] := MatchQ[
    Unevaluated[{opts}],
    KeyValuePattern[{"Coordinates" -> coords_, "GeometricAlgebra" -> A_}] /;
        GeometricAlgebraQ[Unevaluated[A]] && VectorQ[Unevaluated[coords]] && Length[Unevaluated[coords]] == A["Order"]
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


Multivector[assoc_Association, opts: OptionsPattern[]] :=
    Multivector[Lookup[assoc, GeometricAlgebra[OptionValue["GeometricAlgebra"]]["Indices"], 0], opts]


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

Multivector /: f_Symbol[v_Multivector] /; MemberQ[Attributes[f], NumericFunction] := v[f]


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


v_Multivector["Pseudoscalar"] := If[v["ComplexDimension"] > 0, v["Coordinate", 2 ^ v["ComplexDimension"]], 0]


A_GeometricAlgebra["Identity"] := identityMultivector[A]


(* Geometric Product *)

A_GeometricAlgebra["MultiplicationMatrix"] := A["MultiplicationMatrix"] = With[{indices = A["Indices"], metric = A["Metric"]}, {index = PositionIndex[indices]},
    SparseArray @ Outer[SparseArray[Normal @ KeyMap[Lookup[index, Key[#]] &, multiplyIndices[#1, #2, metric]], Length[indices]] &, indices, indices, 1]
]


switchDualSide[v_Multivector] :=
    Multivector[
        MapThread[Function[{signs, x}, With[{coords = DualCoordinates[x]}, Dual @@ (Take[signs, Length[coords]] coords)], HoldAllComplete], {antiProductSigns[v["Dimension"], v["CoordinateDimension"]], Normal @ v["Coordinates"]}],
        v["GeometricAlgebra"]
    ]

A_GeometricAlgebra["MultiplicationTable"] := ResourceFunction["GridTableForm"][
    Map[Multivector[#, A] &, A["MultiplicationMatrix"], {2}],
    TableHeadings -> {A["Basis"], A["Basis"]}
]


GeometricProduct::usage = "GeometricProduct[v, w] or (v ** w) gives a geometric product of multivectors v and w";

GeometricProduct[v_Multivector, w_Multivector] := Module[{
    A = mergeGeometricAlgebra[v, w],
    x, y,
    coords
},
    x = Multivector[v, A]["Coordinates"];
    y = Multivector[w, A]["Coordinates"];

    coords = Outer[coordinateTimes, x, y];
    Multivector[
        Flatten[coords, 1] . Flatten[A["MultiplicationMatrix"], 1],
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

AntiGeometricProduct[vs__Multivector] := OverBar[GeometricProduct @@ UnderBar /@ {vs}]


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
            Function[{j, x}, With[{k = DeleteCases[i, Alternatives @@ j]}, k -> permutationSignature[i, Join[k, j]] x], HoldAllComplete],
            v["Association"]
        ],
        v["GeometricAlgebra"]
    ]
]

v_Multivector["RightComplement"] := With[{i = Last @ v["Indices"]},
     Multivector[
        Association @ KeyValueMap[
            Function[{j, x}, With[{k = DeleteCases[i, Alternatives @@ j]}, k -> permutationSignature[i, Join[j, k]] x], HoldAllComplete],
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


(* Root *)

Multivector /: Root[v_Multivector, n_Integer] := MultivectorFunction[MatrixPower[#, 1 / n] &, v]

Multivector /: Power[v_Multivector, p_Rational] := With[{n = Numerator[p], d = Denominator[p]}, Root[v ^ n, d]]


(* Grade *)


Grade[v_Multivector, n_Integer] /; n < 0 || n > v["GeometricAlgebra"]["Dimension"] := zeroMultivector[v]

Grade[v_Multivector, n_Integer] := mapCoordinates[# gradeIndices[v["GeometricAlgebra"], n] &, v]

GradeList[v_Multivector] := Grade[v, #] & /@ Range[0, v["GeometricAlgebra"]["Dimension"]]

Grade[coords_List, k_Integer, A___] := With[{G = GeometricAlgebra[A]}, With[{
        skipDimension = binomialSum[G["Dimension"], k - 1],
        bladeDimension = Binomial[G["Dimension"], k]
    },
    Multivector[SparseArray[MapIndexed[Function[{x, i}, skipDimension + i -> x, HoldAllComplete], Take[coords, UpTo[bladeDimension]]], G["Order"]], G]
]]

Grade[v_Multivector, "Even"] := Total[Grade[v, #] & /@ Range[0, v["GeometricAlgebra"]["Dimension"], 2]]

Grade[v_Multivector, "Odd"] := Total[Grade[v, #] & /@ Range[1, v["GeometricAlgebra"]["Dimension"], 2]]


v_Multivector["Grade", arg_] := Grade[v, arg]


(* Special multivectors *)

A_GeometricAlgebra["Scalar"] := Multivector[1, A]


pseudoscalar[A_GeometricAlgebra] := Module[{
    p, q
},
    {p, q} = A["ComplexSignature"];
    Multivector[<|Join[Range[p], Range[- q, - 1]] -> 1|>, A]
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


(* Various multivector functions *)

v_Multivector["Norm"] := Sqrt[v ^ 2]

Multivector /: Norm[v_Multivector] := v["Det"]


v_Multivector["Normalize"] := v / v["Norm"]

Multivector /: Normalize[v_Multivector] := v["Det"]


v_Multivector["Tr"] := v + v["Conjugate"]

Multivector /: Tr[v_Multivector] := v["Det"]


v_Multivector["Det"] := v ** v["Conjugate"]

Multivector /: Det[v_Multivector] := v["Det"]


(* Utility functions *)

mergeOptions[opts_, drop_: False] := Sequence @@ Normal @ Merge[If[drop, DeleteCases[Join @@ opts, _ -> Automatic], Join @@ opts], First]


mergeGeometricAlgebra[vs__Multivector] := GeometricAlgebra[
    MapThread[Max, #["GeometricAlgebra"]["Signature"] & /@ {vs}],
    mergeOptions[Normal @ KeyDrop[Options[#["GeometricAlgebra"]], "Signature"] & /@ {vs}, True]
]


constantFunction[f_Function] := f
constantFunction[x_] := Function[x]


functionBody[Function[body_]] := body
functionBody[x_] := x


reduceFunctions[expr_] := Activate @ FixedPoint[Function[x, ReplaceRepeated[x, {
    HoldPattern[(f: Function[x_])[y_]] :> With[{e = Inactivate[f[y], D]}, Function[e] /; True],
    f: HoldPattern[Function[x_]] /; FreeQ[Hold[x], _Slot, {0, \[Infinity]}] :> x,
    (* f: HoldPattern[Function[{xs__}, x_]] /; FreeQ[x, Alternatives[xs], {0, \[Infinity]}]:> x,*)
    (* HoldPattern[Function[x_]] :> With[{e = Inactivate[x, D]}, Function[e] /; True],*)
    HoldPattern[Function[Function[x_]]] :> With[{e = Simplify @ Inactivate[x, D]}, Function[e] /; True],
    HoldPattern[Plus[xs___, f_Function, ys___]] :> With[{e = Plus @@ (functionBody /@ Inactivate[{xs, f, ys}, D])}, Function[e] /; True],
    HoldPattern[Times[xs___, f_Function, ys___]] /; FreeQ[{xs, ys}, _Function, {0, \[Infinity]}] :> With[{e = Times @@ (functionBody /@ Inactivate[{xs, f, ys}, D])}, Function[e] /; True]
}], HoldAllComplete], expr]


mapCoordinates[f_, v_Multivector] := Multivector[reduceFunctions[f[v["Coordinates"]]], v["GeometricAlgebra"]]


coordinateTimes[f: Function[x_], Function[y_]] := reduceFunctions[Function[f[y]]]

coordinateTimes[f_Function, y_] := f[y]

coordinateTimes[x_, Function[y_]] := reduceFunctions[Function[GeometricProduct[x, y]]]

coordinateTimes[v_, w_] := GeometricProduct[v, w]


solveCoordinates[f_Function, A_GeometricAlgebra] := Module[{w, sol},
    Block[{x},
        w = Array[Subscript[x, #] &, A["Order"]];
        sol = Solve[Thread[Normal[f[Multivector[w, A]]] == Normal[A["Zero"]]], w];
        If[ Length[sol] == 0 || Not[FreeQ[sol, ComplexInfinity | Indeterminate, Infinity]],
            $Failed,
            w /. First[sol] /. Thread[w -> 0]
        ]
    ]
]


dotIndices[{}, __] := 1
dotIndices[_, {}, _] := 1
dotIndices[u_, v_, g_] := Det[Outer[g[[##]] &, u, v]]

orderIndexWithSign[index_List, n_Integer] := With[{order = OrderingBy[index, Mod[#, n + 1] &]}, {index[[order]], Signature @ order}];

contractBlade[index_, g_] := Block[{newIndex, squares},
    squares = First[Reap[newIndex = SequenceReplace[index, {x_, x_} :> (Sow[x]; Nothing)]][[2]], {}];
    {newIndex, Times @@ (g[[#, #]] & /@ squares)}
]

orderAndContract[index_, g_] := ({j, x} |-> {#1, x * #2} & @@ contractBlade[j, g]) @@ orderIndexWithSign[index, Length[g]]

orderAndContractBlades[g_][indices_] := Merge[Mean] @ KeyValueMap[{k, x} |-> #1 -> x * #2 & @@ orderAndContract[k, g], indices]

multiplyIndices[uu : {___Integer}, vv : {___Integer}, g_] := Block[{
	n = Length[g], x, y, u, v, j, k, sigma, tau
},
	{u, x} = orderAndContract[uu, g];
	{v, y} = orderAndContract[vv, g];
	j = Length[u];
	k = Length[v];
	sigma = Permutations[v];
	tau = Permutations[u];
	x * y * DeleteCases[0] @ orderAndContractBlades[g] @ Association @ Table[
		Join[#2[[;; j - i]], #1[[i + 1 ;; k]]] ->
			Signature[Mod[#1, n + 1]] * Signature[Mod[#2, n + 1]] * dotIndices[Reverse[#2[[j - i + 1 ;; j]]], #1[[;; i]], g] & @@@
			Tuples[{
				Select[sigma, Less @@ #[[;; i]] && Less @@ #[[i + 1 ;; k]] &],
				Select[tau, Less @@ #[[;; j - i]] && Less @@ #[[j - i + 1 ;; j]] &]
			}],
		{i, 0, Min[j, k]}
	]
]


(* Boxes *)

$defaultMultivectorFormatFunction = Function[index,
    If[ index === {},
        "", (* don't display zero coefficient terms *)
        Subscript["e", Row @ Riffle[If[# > 0, #, UnderBar[Abs[#]]] & /@ index, "\[InvisibleComma]"]]
    ]
]


geometricIndexBoxes[A_GeometricAlgebra, index_] := With[{format = A["FormatIndex"]},
    ToBoxes @ Switch[format,
        Automatic,
        $defaultMultivectorFormatFunction[index]
        ,
        "Positive",
        $defaultMultivectorFormatFunction[positiveIndex[index, A["Signature"]] - 1]
        ,
        _Function,
        format[index]
        ,
        _,
        index /. Append[_ -> $defaultMultivectorFormatFunction[index]] @ DeleteCases[Except[_Rule]] @ Developer`ToList[format]
    ]
]

geometricIndexBoxes[index_] := geometricIndexBoxes[Lookup[Options[Multivector], "GeometricAlgebra"], index]

geometricIndexBoxes[A_GeometricAlgebra, {indices__List}] := geometricIndexBoxes[A, #] & /@ {indices}

geometricIndexBoxes[A_GeometricAlgebra] := geometricIndexBoxes[A, A["Indices"]]

geometricIndexBoxes[] := geometricIndexBoxes[Lookup[Options[Multivector], "GeometricAlgebra"]]


SetAttributes[holdSparseArray, {HoldAll}];

holdSparseArray[a : SparseArray[{}, ___]] := a
holdSparseArray[SparseArray[{xs__}, opts___]] := SparseArray[List @@ MapAt[Hold, Hold[xs], {All, 2}], opts]

Multivector /: MakeBoxes[v : HoldPattern[Multivector[opts___]] /; MultivectorQ[Unevaluated[v]], _] :=
    Module[{
        A = GeometricAlgebra[v],
        holdCoords, coords,
        rules,
        nonZeroPositions,
        n,
        indices,
        display, interpret,
        boxes,
        optBoxes
    },
    holdCoords = Lookup[List @@ RuleDelayed @@@ Hold[opts], "Coordinates", None, Hold];
    coords =  Which[
        MatchQ[holdCoords, Hold[SparseArray[Automatic, ___]]], (* don't hold elements of a SparseArray object *)
        Map[Hold, ReleaseHold @ holdCoords],
        MatchQ[holdCoords, Hold[SparseArray[___]]], (* hold each element of a SparseArray constructor *)
        First[holdSparseArray /@ holdCoords],
        True,
        First @ MapAt[Hold, holdCoords, {1, All}] (* hold elements of a List *)
    ];
    rules = Cases[ArrayRules[coords], ({i_Integer} -> c_) /; If[c != Hold[0], True, False, True] :> {i, c}];
    optBoxes = ToBoxes /@ FilterRules[{opts}, Except["Coordinates"]];
    If[Length[optBoxes] > 0,
        optBoxes = Riffle[optBoxes, ",", {1, 2 Length[optBoxes], 2}]
    ];
    nonZeroPositions = rules[[All, 1]];
    indices = A["Indices"][[nonZeroPositions]];
    n = Length @ nonZeroPositions;
    coords = Riffle[MakeBoxes /@ MapThread[Rule, {nonZeroPositions, Slot /@ Range[n]}], ","];
    boxes = ReleaseHold @ Map[
            Apply[
                Function[{index, holdCoord}, If[ index > 1,
                    Function[coord, Switch[coord,
                        1, InterpretationBox["\[InvisibleSpace]", coord],
                        -1, InterpretationBox["-", coord],
                        _, If[MatchQ[coord, _Multivector | _Dual], RowBox[{"(", ToBoxes @ coord, ")"}], Parenthesize[coord, StandardForm, Times]]
                    ], HoldAllComplete] @@ holdCoord,
                    MakeBoxes @@ holdCoord
                ], HoldRest]
            ],
            rules
        ];
    display = RowBox[
    If[Length[nonZeroPositions] > 0,
        Riffle[
            MapThread[
                RowBox[{
                    #1,
                    geometricIndexBoxes[A, #2]}
                ] &,
                { Slot /@ Range[n], indices}
            ],
        "+"
        ],
        {0} (* all zeros displayed as just zero *)
    ]
    ];
    interpret = RowBox[{"Multivector", "[",
        "\"Coordinates\"", "->", 
        "SparseArray", "[", "{",
            Sequence @@ If[n > 0, 
                Riffle[MapThread[RowBox[{ToBoxes[#1], "->", #2}] &, {nonZeroPositions, Slot /@ Range[n]}], ","],
                {}
            ],
            "}", ",",ToBoxes[A["Order"]],
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
