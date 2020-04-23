(* ::Package:: *)

Package["GeometricAlgebra`"]


PackageExport["GeometricAlgebra"]
GeometricAlgebra::usage = "GeometricAlgebra[p, q] gives an underlying algebra object for use with Multivector";

Options[GeometricAlgebra] = {"Signature" -> {3, 0}, "FormatIndex" -> Automatic};
Options[Multivector] = {
    "GeometricAlgebra" -> GeometricAlgebra[3],
    "Coordinates" -> SparseArray[{}, 8],
    "Inverse" -> Missing["Uncomputed"]
};

GeometricAlgebra[p_Integer, q_Integer: 0, opts: OptionsPattern[]] :=
    GeometricAlgebra[GeometricAlgebra["Signature" -> {p, q}], opts]
GeometricAlgebra[{p_Integer, q___Integer}, opts: OptionsPattern[]] := GeometricAlgebra[p, q, opts]
GeometricAlgebra[A_GeometricAlgebra, opts: OptionsPattern[]] :=
    GeometricAlgebra @@ Normal @ Merge[Join[Options[A], {opts}, Options[GeometricAlgebra]], First]
GeometricAlgebra[] := GeometricAlgebra[OptionValue[Multivector, "GeometricAlgebra"], Options[GeometricAlgebra]] (* current default GeometricAlgebra *)

A_GeometricAlgebra[opt_String] /; KeyExistsQ[Options[A], opt] := Lookup[Options[A], opt]
A_GeometricAlgebra["Metric"] :=
    Module[{p, q},
        {p, q} = A["Signature"];
        Join[ConstantArray[1, p], ConstantArray[-1, q]]
    ]
A_GeometricAlgebra["Dimension"] := Total@A["Signature"]
A_GeometricAlgebra["Order"] := 2^A["Dimension"]
A_GeometricAlgebra["Indices"] := Subsets[Join[Range[A["Signature"][[1]]], Range[-A["Signature"][[2]], -1]]]


PackageExport["Multivector"]
Multivector::usage = "Multivector[coords, ga] gives a multivector in GeometricAlgebra ga";

Multivector::truncCoord = "Coordinates are incompatible with `1`. Number of coordinates should be less than `2`. Truncating excessive coordinates.";

Multivector[coords_? VectorQ, OptionsPattern[]] :=
    With[{A = GeometricAlgebra @ OptionValue["GeometricAlgebra"]},
        If[Length@coords > A["Order"],
            Message[Multivector::truncCoord, A, A["Order"]]
        ];
        Multivector["Coordinates" -> SparseArray[coords, A["Order"]], "GeometricAlgebra" -> A]
    ]
Multivector[assoc_Association, opts: OptionsPattern[]] :=
    Multivector[Lookup[assoc, GeometricAlgebra[OptionValue["GeometricAlgebra"]]["Indices"], 0], opts]
Multivector[x_? NumericQ, opts: OptionsPattern[]] := Multivector[{x}, opts]
Multivector[v_Multivector, args__] := Multivector[v, GeometricAlgebra[args]]
Multivector[arg_, A_GeometricAlgebra] := Multivector[arg, "GeometricAlgebra" -> A]
Multivector[arg_, p_Integer, q_Integer: 0] := Multivector[arg, "GeometricAlgebra" -> GeometricAlgebra[p, q]]
Multivector[arg_, {p_Integer, q_Integer: 0}] := Multivector[arg, p, q]


Multivector[] := Multivector[{}]

Multivector /: v_Multivector[opt: Alternatives@@Keys@Options@Multivector] := Lookup[Options[v], opt]
Multivector /: v_Multivector[opt: Alternatives@@Keys@Options@GeometricAlgebra] := v["GeometricAlgebra"][opt]
Multivector /: Normal[v_Multivector] := Normal@v["Coordinates"]
v_Multivector["Coordinates", n_Integer] :=
    v["Coordinates"][[binomialSum[v["GeometricAlgebra"]["Dimension"], n - 1] + 1 ;; binomialSum[v["GeometricAlgebra"]["Dimension"], n]]]

Multivector /: Norm[v_Multivector] := With[{d = v["GeometricAlgebra"]["Dimension"]},
    Sqrt @ Total[With[{s = Squared[#]}, Total[s["Coordinates", 0] + I s["Coordinates", d]]] & /@ GradeList[v]]
]

PackageExport["MultivectorBasis"]
MultivectorBasis::usage = "MultivectorBasis[A, g] gives a list of multivectors from canonical basis of geometric algebra A with grade g";

MultivectorBasis[A_GeometricAlgebra, n_Integer ? Positive | All] := With[{
    from = If[n === All, 1, binomialSum[A["Dimension"], n - 1] + 1], 
    to = If[n === All, A["Order"], binomialSum[A["Dimension"], n]]
    },
    Table[Multivector[SparseArray[{k -> 1}, A["Order"]], "GeometricAlgebra" -> A], {k, from, to}]
]
MultivectorBasis[A_GeometricAlgebra, n_Integer ? Negative] := MultivectorBasis[A, A["Dimension"] + n + 1]
MultivectorBasis[A_GeometricAlgebra] := MultivectorBasis[A, All]
MultivectorBasis[v_Multivector, args___] := MultivectorBasis[v["GeometricAlgebra"], args]

A_GeometricAlgebra["MultiplicationTable"] := A["MultiplicationTable"] =
    Outer[MultiplyIndices[#1, #2, A["Metric"]]&, A["Indices"], A["Indices"], 1]
A_GeometricAlgebra["SignMatrix"] := A["SignMatrix"] = A["MultiplicationTable"][[All, All, 1]]

mapCoordinates[f_, v_Multivector] := Multivector[f[v["Coordinates"]], "GeometricAlgebra" -> v["GeometricAlgebra"]]


(* Addition *)

PackageExport["ZeroMultivector"]

ZeroMultivector[A_GeometricAlgebra] := Multivector[{}, "GeometricAlgebra" -> A]
ZeroMultivector[v_Multivector] := ZeroMultivector[v["GeometricAlgebra"]]

mergeOptions[opts_] := Sequence @@ Normal @ Merge[DeleteCases[Join @@ opts, _ -> Automatic], First]

mergeGeometricAlgebra[vs__Multivector] := 
    GeometricAlgebra[MapThread[Max, #["GeometricAlgebra"]["Signature"] & /@ {vs}], mergeOptions[Options[#["GeometricAlgebra"]] & /@ {vs}]]

Multivector /: Plus[vs__Multivector] /; Length[{vs}] > 1 := Module[{
    A = mergeGeometricAlgebra[vs],
    ws
},
    ws = Multivector[#, A] & /@ {vs};
    Multivector[
        Total[#["Coordinates"] & /@ ws],
        "GeometricAlgebra" -> A
    ]
]

Multivector /: Plus[x: Except[_Multivector], v_Multivector] := x IdentityMultivector[v] + v


(* Scalar multiplication *)

PackageExport["IdentityMultivector"]

IdentityMultivector[A_GeometricAlgebra] := Multivector[{1}, "GeometricAlgebra" -> A]
IdentityMultivector[v_Multivector] := IdentityMultivector[v["GeometricAlgebra"]]

Multivector /: Times[x: Except[_Multivector], v_Multivector] := mapCoordinates[x # &, v]

v_Multivector["Coordinate", n_Integer] := v["Coordinates"][[n]]
v_Multivector["Scalar"] := v["Coordinate", 1]

PackageExport["NormalizeMultivector"]

NormalizeMultivector[v_Multivector] := Normalize[v, Norm]

(* Geometric Product *)

PackageExport["GeometricProduct"]
GeometricProduct::usage = "GeometricProduct[v, w] or (v ** w) gives a geometric product of multivectors v and w";

GeometricProduct[v_Multivector, w_Multivector] := Module[{
    A = mergeGeometricAlgebra[v, w],
    x, y,
    mt,
    coords
},
    x = Multivector[v, A]["Coordinates"];
    y = Multivector[w, A]["Coordinates"];
    mt = A["MultiplicationTable"];
    coords = mt[[All, All, 1]] Outer[Times, x, y];
    Multivector[
        GroupBy[
            Flatten[MapIndexed[{#1, Extract[coords, #2]} &, mt[[All, All, 2]], {2}], 1],
            First,
            Total@#[[All, 2]]&
        ],
      "GeometricAlgebra" -> A
    ]
]

GeometricProduct[vs__Multivector] := Fold[GeometricProduct, {vs}]

(* infix notation *)
Multivector /: v_Multivector ** w_Multivector := GeometricProduct[v, w]


Multivector /: Power[v_Multivector, n_Integer] := Nest[# ** v &, IdentityMultivector[v], n]


(* Grade *)

PackageExport["Grade"]
Grade::usage = "Grade[v, n] gives a nth grade of a Multivector v or converts a list to a multivector";

binomialSum[n_Integer, k_Integer] := Module[{i}, Evaluate[Sum[Binomial[n, i], {i, 0, k}]]]

gradeIndices[A_GeometricAlgebra, k_Integer] := SparseArray[
    Thread[Range[binomialSum[A["Dimension"], k - 1] + 1, binomialSum[A["Dimension"], k]] -> 1],
    A["Order"]
]

Grade[v_Multivector, n_Integer] /; n < 0 || n > v["GeometricAlgebra"]["Dimension"] := ZeroMultivector[v]
Grade[v_Multivector, n_Integer] := mapCoordinates[# gradeIndices[v["GeometricAlgebra"], n] &, v]

GradeList[v_Multivector] := Grade[v, #] & /@ Range[0, v["GeometricAlgebra"]["Dimension"]]

Grade[coords_List, k_Integer, opts : OptionsPattern[Multivector]] := With[{
        skipDimension = binomialSum[OptionValue["GeometricAlgebra"]["Dimension"], k - 1],
        bladeDimension = Binomial[OptionValue["GeometricAlgebra"]["Dimension"], k]
    },
    Multivector[SparseArray[MapIndexed[skipDimension + #2 -> #1 &, Take[coords, UpTo[bladeDimension]]], OptionValue["GeometricAlgebra"]["Order"]], opts]
]

(* Product contractions *)

PackageExport["LeftContraction"]
LeftContraction::usage = "LeftContraction[v, w] gives a left contraction of multivectors v and w";

PackageExport["RightContraction"]
RightContraction::usage = "RightContraction[v, w] gives a right contraction of multivectors v and w";

PackageExport["ScalarProduct"]
ScalarProduct::usage = "ScalarProduct[v, w] gives a geometric product of multivectors v and w";


gradeProduct[v_Multivector, w_Multivector] := Outer[GeometricProduct, GradeList[v], GradeList[w]]
gradeFunctionContraction[f_, vs__Multivector] := Fold[Total[MapIndexed[Grade[#1, f[#2 - 1]] &, gradeProduct[##], {2}], 2] &, {vs}]

LeftContraction[vs__Multivector] := gradeFunctionContraction[Apply[Subtract] @* Reverse, vs]
RightContraction[vs__Multivector] := gradeFunctionContraction[Apply[Subtract], vs]

Multivector /: Dot[vs__Multivector] := gradeFunctionContraction[Abs @* Apply[Subtract], vs]
Multivector /: Wedge[vs__Multivector] := gradeFunctionContraction[Apply[Plus], vs]
Multivector /: Vee[vs__Multivector] := LeftDual[Wedge @@ RightDual /@ {vs}]
Multivector /: Cross[vs__Multivector] := Dual[Wedge[vs]]

ScalarProduct[vs__Multivector] := Grade[GeometricProduct[vs], 0]


(* Dual *)

PackageExport["Pseudoscalar"]
Pseudoscalar::usage = "Pseudoscalar[v] gives a pseudoscalar in the same geometric algebra as multivector v";

PackageExport["LeftDual"]
LeftDual::usage = "LeftDual[v] gives a left dual of multivector v";

PackageExport["RightDual"]
RightDual::usage = "Dual[v] gives a right dual of multivector v";

PackageExport["Dual"]
Dual::usage = "Dual[v] gives a left dual of multivector v";


Pseudoscalar[A_GeometricAlgebra] := Multivector[SparseArray[{A["Order"] -> 1}], "GeometricAlgebra" -> A]
Pseudoscalar[v_Multivector] := Pseudoscalar[v["GeometricAlgebra"]]

LeftDual[v_Multivector] := LeftContraction[v, Reverse @ Pseudoscalar[v]]
RightDual[v_Multivector] := RightContraction[Pseudoscalar[v], v]
Dual = LeftDual;


(* Multivector transformation *)

PackageExport["MultivectorTransform"]
MultivectorTransform::usage = "MultivectorTransform[v, t] applies transformation t to multivector v";

Multivector[OptionsPattern[]] := Multivector[
    Multivector[OptionValue["Coordinates"], "GeometricAlgebra" -> GeometricAlgebra[Ceiling@Log2@Length[OptionValue["Coordinates"]]]],
    GeometricAlgebra[OptionValue["GeometricAlgebra"]]
] /; Length[OptionValue["Coordinates"]] != GeometricAlgebra[OptionValue["GeometricAlgebra"]]["Order"]

Multivector[v_Multivector, A_GeometricAlgebra] /; v["GeometricAlgebra"] === A := v
Multivector[v_Multivector, A_GeometricAlgebra] := Multivector[
    SparseArray[
        Map[Apply[With[{
                pos = Position[A["Indices"], Extract[v["GeometricAlgebra"]["Indices"], #1]]
            },
                If[Length[pos] > 0, pos[[1]] -> #2, Nothing]
            ] &],
            Most@ArrayRules@v["Coordinates"]
        ],
        A["Order"]
    ],
    "GeometricAlgebra" -> A
]

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

PackageExport["Reverse"]

PackageExport["Involute"]
Involute::usage = "Involute[v] gives a multivector with its odd grades multiplied by -1";

PackageExport["Conjugate"]
PackageExport["Projection"]

PackageExport["Rejection"]
Rejection::usage = "Rejection[v, w] gives a rejection of multivector v on w";

PackageExport["Squared"]
Squared::usage = "Squared[v] gives v ** Involute[v] for multivector v";

reverseIndexCoordinate[A_GeometricAlgebra, indexPos_, x_] := Module[{newIndex, sign}, 
    {newIndex, sign} = orderIndexWithSign[Reverse[Extract[A["Indices"], indexPos]], A["Dimension"]];
    newIndex -> sign x
]

Multivector /: Reverse[v_Multivector] := Multivector[
    Association[reverseIndexCoordinate[v["GeometricAlgebra"], #1, #2] & @@@ Most@ArrayRules@v["Coordinates"]], 
    "GeometricAlgebra" -> v["GeometricAlgebra"]
]
Involute[v_Multivector] := mapCoordinates[((-1)^# &@*Length /@ v["GeometricAlgebra"]["Indices"]) # &, v]
Multivector /: Conjugate[v_Multivector] := Reverse[Involute[v]]

Multivector /: Projection[v_Multivector, w_Multivector] := w ** (v . w)
Rejection[v_Multivector, w_Multivector] := (v \[Wedge] w) ** w

Squared[v_Multivector] := v ** Involute[v]


(* Inverse *)

PackageExport[solveCoordinates]

solveCoordinates[f_Function, A_GeometricAlgebra] := Module[{w, sol},
    Block[{x},
        w = Array[Subscript[x, #] &, A["Order"]];
        sol = Solve[Thread[Normal[f[Multivector[w, A]]] == Normal[ZeroMultivector[A]]], w];
        If[ Length[sol] == 0 || Not[FreeQ[sol, ComplexInfinity | Indeterminate, Infinity]],
            $Failed,
            w /. First[N @ sol] /. Thread[w -> 0]
        ]
    ]
]

Multivector::noinv = "Can't inverse a multivector";

Multivector /: Inverse[v_Multivector] :=
    Module[{A = v["GeometricAlgebra"], coords, inv},
        inv = Lookup[Options[v], "Inverse", OptionValue[Multivector, "Inverse"]];
        If[ MissingQ[inv],
            coords = solveCoordinates[v ** # - IdentityMultivector[v] &, A];
            If[ FailureQ[coords],
                Message[Multivector::noinv]; Inverse[Multivector[v, "Inverse" -> None]],
                Multivector["Coordinates" -> coords, "Inverse" -> v, Sequence @@ Options[v]]
            ]
        ]
    ] /; Lookup[Options[v], "Inverse", OptionValue[Multivector, "Inverse"]] =!= None

Multivector /: Divide[v_, w_Multivector] := Multivector[v, w["GeometricAlgebra"]] ** Inverse[w]


(* Boxes *)

PackageExport["$DefaultMultivectorFormatFunction"]

GeometricAlgebra /: MakeBoxes[A_GeometricAlgebra, StandardForm] := With[{
    box = SubscriptBox["\[DoubleStruckCapitalG]", RowBox @ Riffle[ToString /@ A["Signature"], ","]]
},
    InterpretationBox[box, A, Tooltip -> "Geometric Algebra"]
]


$DefaultMultivectorFormatFunction = If[# === {},
    Nothing, (* don't display zero coefficient terms *)
    SubscriptBox["e", RowBox@Riffle[If[Positive@#, #, UnderscriptBox[Abs[#], "_"]] & /@ #, "\[InvisibleComma]"]]
] &;

Multivector /: MakeBoxes[v:Multivector[OptionsPattern[]], StandardForm] := Check[
    Module[{
        A = v["GeometricAlgebra"],
        rules = Cases[ArrayRules@v["Coordinates"], r:({i_Integer} -> c_) :> {i, c}],
        nonZeroPositions,
        n,
        indices
    },
    nonZeroPositions = rules[[All, 1]];
    indices = A["Indices"][[nonZeroPositions]];
    n = Length@nonZeroPositions;
    TemplateBox[{
        Splice@Map[
            Apply[
                If[#1 > 1 && #2 === 1,
                    InterpretationBox[" ", 1],
                    Parenthesize[#2, StandardForm, Times]
                ]&
            ],
            rules
        ]
    },
    "Multivector",
    DisplayFunction -> (Evaluate@RowBox[
    If[Length[nonZeroPositions] > 0,
        Riffle[
            MapThread[
                RowBox[{
                    #1,
                    If[A["FormatIndex"] === Automatic, 
                        $DefaultMultivectorFormatFunction[#2], 
                        #2 /. Append[A["FormatIndex"], _ -> $DefaultMultivectorFormatFunction[#2]]]}
                ] &,
                { Slot /@ Range[n], indices}
            ],
        "+"
        ],
        {0} (* all zeros displayed as just zero *)
    ]
    ] &),
    InterpretationFunction -> (Evaluate@RowBox[{
        "Multivector", "[",
            "<|", Splice@Riffle[MapThread[RowBox[{ToBoxes[#1], "->", #2}]&, {indices, Slot /@ Range[n]}], ","], "|>", ",",
            "\"GeometricAlgebra\"", "->", ToBoxes@A, 
        "]"
    }
    ] &),
    Tooltip -> RowBox[{"Multivector", " ", ToBoxes@A}],
    Editable -> True
    ]],
    $Failed
]

orderIndexWithSign[index_List, n_Integer] := With[{order = OrderingBy[index, Mod[#, n + 1] &]}, {index[[order]], Signature@order}]

MultiplyIndices::badIndex = "Index `1` is incompatible with metric `2`";
checkIndex[i_Integer, m_List] := 
    If[Not[i != 0 && -Length[m] <= i <= Length[m]],
        Message[MultiplyIndices::badIndex, i, m];
        $Failed
    ]
MultiplyIndices[i_List, j_List, m_List] :=
    Module[{index = Join[i, j], newIndex, orderedIndex, sign, squares},
        If[AnyTrue[index, FailureQ[checkIndex[#, m]] &], Return[$Failed]];
        {orderedIndex, sign} = orderIndexWithSign[index, Length@m];
        {newIndex, squares} = Reap@SequenceReplace[orderedIndex, {x_ ,x_} :> (Sow[x]; Nothing)];
        If[Length@squares > 0,
            sign = sign Times@@m[[squares[[1]]]]
        ];
        {sign ,newIndex}
    ]
