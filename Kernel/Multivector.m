Package["GeometricAlgebra`"]


PackageExport["Multivector"]
Multivector::usage = "Multivector[coords, ga] gives a multivector in GeometricAlgebra ga";

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


PackageScope["zeroMultivector"]
PackageScope["identityMultivector"]
PackageScope["geometricIndexBoxes"]
PackageScope["multiplyIndices"]
PackageScope["$defaultMultivectorFormatFunction"]


Options[Multivector] = {
    "GeometricAlgebra" -> GeometricAlgebra[3],
    "Coordinates" -> SparseArray[{}, 8]
};


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
        If[Length @ coords > A["Order"],
            Message[Multivector::truncCoord, A, A["Order"]]
        ];
        Multivector["Coordinates" -> SparseArray[coords, A["Order"]], "GeometricAlgebra" -> A]
    ]

Multivector[assoc_Association, opts: OptionsPattern[]] :=
    Multivector[Lookup[assoc, GeometricAlgebra[OptionValue["GeometricAlgebra"]]["Indices"], 0], opts]


NumberMultivector[x_, A_GeometricAlgebra] :=
    Multivector[SparseArray[{1 -> Re[x], -1 -> If[A["PseudoscalarSquare"] == 1, I, 1] Im[x]}, A["Order"]], A]

NumberMultivector[v_Multivector, A_GeometricAlgebra] :=
    Multivector[SparseArray[{1 -> v["Scalar"], -1 -> If[v["PseudoscalarSquare"] != A["PseudoscalarSquare"], I, 1] v["Pseudoscalar"]}, A["Order"]], A]

NumberMultivector[x_] := NumberMultivector[x, GeometricAlgebra[0, 1]]


MultivectorNumber[v_Multivector, A_GeometricAlgebra] :=
    Multivector[SparseArray[{1 -> v["Scalar"], -1 -> v["Pseudoscalar"]}, A["Order"]], A]

MultivectorNumber[v_Multivector] := MultivectorNumber[v, GeometricAlgebra @ If[v["PseudoscalarSquare"] == 1, {1, 0}, {0, 1}]]

MultivectorNumber[x_, ___] := x


ComplexMultivector[v_Multivector] := ComplexMultivector[v, v["GeometricAlgebra"]]

ComplexMultivector[v_Multivector, A_GeometricAlgebra] := ConvertGeometricAlgebra[MultivectorNumber[v, A["ComplexAlgebra"]], A]


Multivector[x_ ? NumericQ, opts: OptionsPattern[]] := NumberMultivector[x, OptionValue["GeometricAlgebra"]]

Multivector[v_Multivector, opts: OptionsPattern[]] := Multivector[mergeOptions[{{opts}, Options[v]}]]

Multivector[v_Multivector, args__] := Multivector[v, GeometricAlgebra[args]]

Multivector[arg: Except[_Multivector], A_GeometricAlgebra] := Multivector[arg, "GeometricAlgebra" -> A]

Multivector[arg_, p_Integer, q_Integer: 0] := Multivector[arg, GeometricAlgebra[p, q]]

Multivector[arg_, {p_Integer, q_Integer: 0}] := Multivector[arg, p, q]

Multivector[] := Multivector[{}]

v_Multivector[f_] := mapCoordinates[f, v]

Multivector /: f_Symbol[v_Multivector] /; MemberQ[Attributes[f], NumericFunction] := v[f]


Multivector /: v_Multivector[opt: Alternatives @@ Keys @ Options @ Multivector] := Lookup[Options[v], opt]

Multivector /: v_Multivector[opt: Alternatives @@ $GeometricAlgebraProperties] := v["GeometricAlgebra"][opt]


Multivector /: Normal[v_Multivector] := Normal @ v["Coordinates"]


v_Multivector["Coordinates", n_Integer] := v["Coordinates"][[indexSpan[v, n]]]

v_Multivector["Coordinates", {ns__Integer}] := Join @@ (v["Coordinates", #] & /@ {ns})


v_Multivector["Coordinate", n_Integer] := v["Coordinates"][[n]]

v_Multivector["Coordinate", {ns__Integer}] := v["Coordinates"][[{ns}]]


v_Multivector["Association"] := Association @ Map[Apply[Function[{x, y}, v["Indices"][[First[x]]] -> y, HoldAllComplete]], Most @ ArrayRules[v["Coordinates"]]]


v_Multivector["Span"] := MapThread[GeometricProduct, {v["Coordinates"], MultivectorBasis[v["GeometricAlgebra"]]}]

v_Multivector["Span", n_Integer] := MapThread[GeometricProduct, {v["Coordinates"][[indexSpan[v, n]]], MultivectorBasis[v["GeometricAlgebra"], n]}]

v_Multivector["Span", {ns__Integer}] := Catenate[v["Span", #] & /@ {ns}]


v_Multivector["Flatten"] := Inner[GeometricProduct, v["Coordinates"], v["Basis"]]


v_Multivector["Real"] := v[Re] + v["GeometricAlgebra"]["Pseudoscalar"] ** v[Im]


v_Multivector["Numeric"] := If[v["Dimension"] > 0 && v["PseudoscalarSquare"] == 1,
    v["Scalar"] IdentityMatrix[2] +  v["Pseudoscalar"] Reverse @ IdentityMatrix[2],
    v["Scalar"] +  I v["Pseudoscalar"]
]


v_Multivector["ComplexCoordinates"] := Module[{
    A, re, im
},
    A = v["GeometricAlgebra"];

    If[ OddQ[A["Dimension"]],
        re = Lookup[v["Association"], A["ReIndices"], 0];
        im = Lookup[(A["Pseudoscalar"] ** v)["Association"], A["ReIndices"], 0];

        re + A["PseudoscalarSquare"] A["Pseudoscalar"] ** im,

        (* Else *)
        v["Coordinates"]
    ]
]


Multivector[opts: OptionsPattern[]] := With[{
    A = GeometricAlgebra[OptionValue["GeometricAlgebra"]]
},
    Multivector[
        SparseArray[OptionValue["Coordinates"], A["Order"]],
        A
    ] /; MissingQ[Lookup[{opts}, "GeometricAlgebra"]] || Length[OptionValue["Coordinates"]] != A["Order"]
]


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


v_Multivector["Pseudoscalar"] := If[v["Dimension"] > 0, v["Coordinate", -1], 0]


A_GeometricAlgebra["Identity"] := identityMultivector[A]


(* Geometric Product *)

A_GeometricAlgebra["MultiplicationTableWithSigns"] := A["MultiplicationTableWithSigns"] =
    Outer[MapAt[First @ FirstPosition[A["Indices"], #] &, multiplyIndices[#1, #2, A["Metric"]], 2] &, A["Indices"], A["Indices"], 1]


A_GeometricAlgebra["MultiplicationTable"] := A["MultiplicationTable"] =
    Developer`ToPackedArray[A["MultiplicationTableWithSigns"][[All, All, 2]]]


A_GeometricAlgebra["MultiplicationSigns"] := A["MultiplicationSigns"] =
    Developer`ToPackedArray[A["MultiplicationTableWithSigns"][[All, All, 1]]]


GeometricProduct::usage = "GeometricProduct[v, w] or (v ** w) gives a geometric product of multivectors v and w";

GeometricProduct[v_Multivector, w_Multivector] := Module[{
    A = mergeGeometricAlgebra[v, w],
    x, y,
    coords
},
    x = Multivector[v, A]["Coordinates"];
    y = Multivector[w, A]["Coordinates"];
    coords = A["MultiplicationSigns"] Outer[coordinateTimes, x, y];
    Multivector[
        SparseArray @ Normal @ GroupBy[
            Thread[Flatten[A["MultiplicationTable"]] -> Flatten[coords]],
            First,
            Total @ #[[All, 2]] &
        ],
        A
    ][Map[reduceFunctions]]
]

GeometricProduct[x_, y_] := x * y

GeometricProduct[vs__Multivector] := Fold[GeometricProduct, {vs}]

GeometricProduct[] := Multivector[{1}, {0, 0}]


Multivector /: Times[vs__Multivector] := GeometricProduct[vs]


Multivector /: Power[v_Multivector, n_Integer] := If[n < 0, Power[Inverse[v], -n], Nest[# ** v &, identityMultivector[v], n]]


(* Tensor product *)

Multivector /: TensorProduct[v_Multivector, w_Multivector] := Module[{
    p, q
},
    {p, q} = v["Signature"];
    v ** Multivector[
        KeyMap[# /. {i_ ? Positive :> i + p, i_ ? Negative :> i - q} &, w["Association"]],
        GeometricAlgebra[{p, q} + w["Signature"]]
    ]
]


(* infix notation *)

Unprotect[NonCommutativeMultiply]

(x_? NumericQ) ** y_ := x y

x_ ** (y_? NumericQ) := x y

NonCommutativeMultiply[vs__] := Fold[GeometricProduct, {vs}]

Protect[NonCommutativeMultiply]


(* Product contractions *)

gradeProduct[v_Multivector, w_Multivector] := Outer[GeometricProduct, GradeList[v], GradeList[w]]

gradeFunctionContraction[f_, vs__Multivector] := Fold[Total[MapIndexed[Grade[#1, f[#2 - 1]] &, gradeProduct[##], {2}], 2] &, {vs}]

LeftContraction[vs__Multivector] := gradeFunctionContraction[Apply[Subtract] @* Reverse, vs]

RightContraction[vs__Multivector] := gradeFunctionContraction[Apply[Subtract], vs]

Multivector /: Dot[vs__Multivector] := gradeFunctionContraction[Abs @* Apply[Subtract], vs]

Multivector /: Wedge[vs__Multivector] := gradeFunctionContraction[Apply[Plus], vs]

Multivector /: Vee[vs__Multivector] := LeftDual[Wedge @@ RightDual /@ {vs}]

Multivector /: Cross[vs__Multivector] := Dual[Wedge[vs]]

ScalarProduct[vs__Multivector] := Grade[GeometricProduct[vs], 0]


(* Inversions *)

reverseIndexCoordinate[A_GeometricAlgebra, indexPos_, x_] := Module[{newIndex, sign}, 
    {newIndex, sign} = orderIndexWithSign[Reverse[Extract[A["Indices"], indexPos]], A["Dimension"]];
    newIndex -> sign x
]

v_Multivector["Reverse"] := Multivector[
    Association[reverseIndexCoordinate[v["GeometricAlgebra"], #1, #2] & @@@ Most @ ArrayRules @ v["Coordinates"]], 
    v["GeometricAlgebra"]
]


Involute[v_Multivector] := mapCoordinates[((-1) ^ # & @* Length /@ v["GeometricAlgebra"]["Indices"]) # &, v]

v_Multivector["Involute"] = Involute[v]


v_Multivector["Conjugate"] = v["Involute"]["Reverse"]


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

Grade[coords_List, k_Integer, opts : OptionsPattern[Multivector]] := With[{
        skipDimension = binomialSum[OptionValue["GeometricAlgebra"]["Dimension"], k - 1],
        bladeDimension = Binomial[OptionValue["GeometricAlgebra"]["Dimension"], k]
    },
    Multivector[SparseArray[MapIndexed[skipDimension + #2 -> #1 &, Take[coords, UpTo[bladeDimension]]], OptionValue["GeometricAlgebra"]["Order"]], opts]
]

Grade[v_Multivector, "Even"] := Total[Grade[v, #] & /@ Range[0, v["GeometricAlgebra"]["Dimension"], 2]]

Grade[v_Multivector, "Odd"] := Total[Grade[v, #] & /@ Range[1, v["GeometricAlgebra"]["Dimension"], 2]]


v_Multivector["Grade", arg_] := Grade[v, arg]


(* Special multivectors *)

pseudoscalar[A_GeometricAlgebra] := Multivector[SparseArray[{A["Order"] -> 1}], A]

pseudoscalar[v_Multivector] := pseudoscalar[v["GeometricAlgebra"]]

pseudoscalar[] := pseudoscalar[GeometricAlgebra[]]

A_GeometricAlgebra["Pseudoscalar"] := pseudoscalar[A]


A_GeometricAlgebra["Nilpotent", n_Integer] := With[{
    i = Min[Min[A["Signature"]], Abs[n]]
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


v_Multivector["Normalize"] := v / v["Norm"]


v_Multivector["Tr"] := v + v["Conjugate"]


v_Multivector["Det"] := v ** v["Conjugate"]


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

coordinateTimes[x_, Function[y_]] := reduceFunctions[Function[x ** y]]

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


orderIndexWithSign[index_List, n_Integer] := With[{order = OrderingBy[index, Mod[#, n + 1] &]}, {index[[order]], Signature @ order}]


multiplyIndices::badIndex = "Index `1` is incompatible with metric `2`";

checkIndex[i_Integer, m_List] := 
    If[Not[i != 0 && - Length[m] <= i <= Length[m]],
        Message[multiplyIndices::badIndex, i, m];
        $Failed
    ]

multiplyIndices[i_List, j_List, m_List] :=
    Module[{index = Join[i, j], newIndex, orderedIndex, sign, squares},
        If[AnyTrue[index, FailureQ[checkIndex[#, m]] &], Return[$Failed]];
        {orderedIndex, sign} = orderIndexWithSign[index, Length @ m];
        {newIndex, squares} = Reap @ SequenceReplace[orderedIndex, {x_, x_} :> (Sow[x]; Nothing)];
        If[ Length[squares] > 0,
            sign = sign Times @@ m[[squares[[1]]]]
        ];
        {sign, newIndex}
    ]


(* Boxes *)

$defaultMultivectorFormatFunction = Function[index,
    If[index === {},
        RowBox[{""}], (* don't display zero coefficient terms *)
        SubscriptBox["e", RowBox @ Riffle[If[Positive @ #, #, UnderscriptBox[Abs[#], "_"]] & /@ index, "\[InvisibleComma]"]]
    ]
];


geometricIndexBoxes[A_GeometricAlgebra, index_] :=
    If[A["FormatIndex"] === Automatic,
        $defaultMultivectorFormatFunction[index],
        If[Head[A["FormatIndex"]] === Function, A["FormatIndex"][index],
            index /. Append[A["FormatIndex"], _ -> $defaultMultivectorFormatFunction[index]]]]

geometricIndexBoxes[index_] := geometricIndexBoxes[Lookup[Options[Multivector], "GeometricAlgebra"], index]

geometricIndexBoxes[A_GeometricAlgebra, {indices__List}] := geometricIndexBoxes[A, #] & /@ {indices}

geometricIndexBoxes[A_GeometricAlgebra] := geometricIndexBoxes[A, A["Indices"]]

geometricIndexBoxes[] := geometricIndexBoxes[Lookup[Options[Multivector], "GeometricAlgebra"]]


SetAttributes[holdSparseArray, {HoldAll}];

holdSparseArray[a : SparseArray[{}, ___]] := a
holdSparseArray[SparseArray[{xs__}, opts___]] := SparseArray[List @@ MapAt[Hold, Hold[xs], {All, 2}], opts]

Multivector /: MakeBoxes[v: Multivector[opts: OptionsPattern[]], _] :=
    Module[{
        A = OptionValue[Multivector, {opts}, "GeometricAlgebra"],
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
    rules = Cases[ArrayRules[coords], ({i_Integer} -> c_) /; c =!= Hold[0] :> {i, c}];
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
                        _, If[Head[coord] === Multivector, RowBox[{"(", coord, ")"}], Parenthesize[coord, StandardForm, Times]]
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
