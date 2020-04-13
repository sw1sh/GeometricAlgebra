(* ::Package:: *)

BeginPackage["GeometricAlgebra`"]

GeometricAlgebra::usage = "GeometricAlgebra[p, q] gives an underlying algebra object for use with Multivector";
Multivector::usage = "Multivector[coords, ga] gives a multivector in GeometricAlgebra ga";
GeometricProduct::usage = "GeometricProduct[v, w] or (v ** w) gives a geometric product of multivectors v and w";
Grade::usage = "Grade[v, n] gives a nth grade of a Multivector v";
Dual::usage = "Dual[v] gives a dual of multivector v";
Pseudoscalar::usage = "Pseudoscalar[v] gives a pseudoscalar in the same geometric algebra as multivector v";
LeftContraction::usage = "LeftContraction[v, w] gives a left contraction of multivectors v and w";
RightContraction::usage = "RightContraction[v, w] gives a right contraction of multivectors v and w";
ScalarProduct::usage = "ScalarProduct[v, w] gives a geometric product of multivectors v and w";

Begin["`Private`"]

Options[GeometricAlgebra] = {"Signature" -> {3, 0}};
GeometricAlgebra[p_Integer, q_Integer: 0] := GeometricAlgebra["Signature" -> {p, q}]

A_GeometricAlgebra[opt_String] /; KeyExistsQ[Options[A], opt] := Lookup[Options[A], opt]
A_GeometricAlgebra["Metric"] :=
    Module[{p, q},
        {p, q} = A["Signature"];
        Join[ConstantArray[1, p], ConstantArray[-1, q]]
    ]
A_GeometricAlgebra["Dimension"] := Total@A["Signature"]
A_GeometricAlgebra["Order"] := 2^A["Dimension"]
A_GeometricAlgebra["Indices"] := Subsets[Range[A["Dimension"]]]

Multivector::truncCoord = "Coordinates are incompatible with `1`. Number of coordinates should be less than `2`. Truncating excessive coordinates.";
Options[Multivector] = {"GeometricAlgebra" -> GeometricAlgebra[3, 0],"Coordinates" -> SparseArray[{}, 8]};

Multivector[coords_? VectorQ, OptionsPattern[]] :=
    With[{A = OptionValue["GeometricAlgebra"]},
        If[Length@coords > A["Order"],
            Message[Multivector::truncCoord, A, A["Order"]]
        ];
        Multivector["GeometricAlgebra" -> A, "Coordinates" -> SparseArray[coords, A["Order"]]]
    ]
Multivector[assoc_Association, opts: OptionsPattern[]] :=
    Multivector[Lookup[assoc, OptionValue["GeometricAlgebra"]["Indices"], 0], opts]
Multivector[x_? NumericQ, opts: OptionsPattern[]] := Multivector[{x}, opts]
Multivector[] := Multivector[{}]

Multivector /: v_Multivector[opt: Alternatives@@Keys@Options@Multivector] := Lookup[Options[v], opt]
Multivector /: v_Multivector[opt: Alternatives@@Keys@Options@GeometricAlgebra] := v["GeometricAlgebra"][opt]
Multivector /: Normal[v_Multivector] := Normal@v["Coordinates"]

A_GeometricAlgebra["MultiplicationTable"] := A["MultiplicationTable"] =
    Outer[MultiplyIndices[#1, #2, A["Metric"]]&, A["Indices"], A["Indices"], 1]
A_GeometricAlgebra["SignMatrix"] := A["SignMatrix"] = A["MultiplicationTable"][[All, All, 1]]

mapCoordinates[f_, v_Multivector] := Multivector[f[v["Coordinates"]], "GeometricAlgebra" -> v["GeometricAlgebra"]]

(* Addition *)

ZeroMultivector[A_GeometricAlgebra] := Multivector[{}, "GeometricAlgebra" -> A]
ZeroMultivector[v_Multivector] := ZeroMultivector[v["GeometricAlgebra"]]

Multivector /:  Plus[vs__Multivector] /; Equivalent @@ (#["GeometricAlgebra"] & /@ {vs}) :=
    Multivector[
        Total[#["Coordinates"] & /@ {vs}],
        "GeometricAlgebra" -> {vs}[[1]]["GeometricAlgebra"]
    ]

(* Scalar multiplication *)

IdentityMultivector[A_GeometricAlgebra] := Multivector[{1}, "GeometricAlgebra" -> A]
IdentityMultivector[v_Multivector] := IdentityMultivector[v["GeometricAlgebra"]]

Multivector /: Times[x_, v_Multivector] := mapCoordinates[x # &, v]


(* Geometric Product *)

GeometricProduct[v_Multivector, w_Multivector] /; v["GeometricAlgebra"] == w["GeometricAlgebra"] ^:=
    Module[{
        x = v["Coordinates"], y = w["Coordinates"],
        mt = v["GeometricAlgebra"]["MultiplicationTable"],
        coords
    },
        coords = mt[[All, All, 1]] Outer[Times, x, y];
        Multivector[
            GroupBy[
                Flatten[MapIndexed[{#1, Extract[coords, #2]}&,
                    mt[[All, All, 2]], {2}], 1],
                First,
                Total@#[[All, 2]]&
            ],
          "GeometricAlgebra" -> v["GeometricAlgebra"]
        ]
    ]

GeometricProduct[vs__Multivector] := Fold[GeometricProduct, {vs}]

(* infix notation *)
Multivector /: v_Multivector ** w_Multivector := GeometricProduct[v, w]


Multivector /: Power[v_Multivector, n_Integer] := Nest[# ** v &, IdentityMultivector[v], n]

(* Grade *)

binomialSum[n_Integer, k_Integer] := Evaluate[Sum[Binomial[n, i], {i, 0, k}]]

gradeIndices[A_GeometricAlgebra, k_Integer] := SparseArray[
    Thread[Range[binomialSum[A["Dimension"], k - 1] + 1, binomialSum[A["Dimension"], k]] -> 1],
    A["Order"]
]

Grade[v_, n_Integer] /; n < 0 || n > v["GeometricAlgebra"]["Dimension"] := ZeroMultivector[v]
Grade[v_Multivector, n_Integer] := mapCoordinates[# gradeIndices[v["GeometricAlgebra"], n] &, v]

GradeList[v_Multivector] := Grade[v, #] & /@ Range[0, v["GeometricAlgebra"]["Dimension"]]

(* Product contractions *)

gradeProduct[v_Multivector, w_Multivector] := Outer[GeometricProduct, GradeList[v], GradeList[w]]
gradeFunctionContraction[f_, vs__Multivector] := Fold[Total[MapIndexed[Grade[#1, f[#2 - 1]] &, gradeProduct[##], {2}], 2] &, {vs}]

LeftContraction[vs__Multivector] := gradeFunctionContraction[Apply[Subtract], vs]
RightContraction[vs__Multivector] := gradeFunctionContraction[Apply[Subtract] @* Reverse, vs]

Multivector /: Dot[vs__Multivector] := gradeFunctionContraction[Abs @* Apply[Subtract], vs]
Multivector /: Wedge[vs__Multivector] := gradeFunctionContraction[Apply[Plus], vs]


ScalarProduct[vs__Multivector] := Grade[GeometricProduct[vs], 0]


(* Dual *)

Pseudoscalar[A_GeometricAlgebra] := Multivector[SparseArray[{A["Order"] -> 1}], "GeometricAlgebra" -> A]
Pseudoscalar[v_Multivector] := Pseudoscalar[v["GeometricAlgebra"]]

Dual[v_Multivector] := v ** Pseudoscalar[v]

(* Boxes *)

GeometricAlgebra /: MakeBoxes[A_GeometricAlgebra, StandardForm]:=
    With[{signature = A["Signature"]},
        TemplateBox[
            signature,
            "GeometricAlgebra",
            DisplayFunction -> (SubscriptBox["\[DoubleStruckCapitalG]", RowBox[{#1, ",", #2}]]&),
            Editable -> True,
            Tooltip -> "Geometric Algebra"
      ]
    ]


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
                If[#1 > 1 && #2 == 1,
                    InterpretationBox[" ", 1],
                    Parenthesize[#2, StandardForm, Plus]
                ]&
            ],
            rules
        ],
        ToBoxes@A
    },
    "Multivector",
    DisplayFunction -> (Evaluate@RowBox[
    If[Length[nonZeroPositions] > 0,
        Riffle[
            MapThread[
                RowBox[{
                    #1,
                    If[#2 === {},
                    Nothing, (* don't display zero coefficient terms *)
                    SubscriptBox["e", RowBox@Riffle[#2, "\[InvisibleComma]"]]]}
                ]&,
                { Slot /@ Range[n], indices}
            ],
        "+"
        ],
        {0} (* all zeros displayed as just zero *)
    ]
    ]&),
    InterpretationFunction -> (Evaluate@RowBox[{
        "Multivector", "[",
        "<|", Splice@Riffle[MapThread[RowBox[{ToBoxes[#1], "->", #2}]&, {indices, Slot /@ Range[n]}], ","], "|>",
        ",", "GeometricAlgebra", "->", Slot[n + 1], "]"
    }
    ]&),
    Tooltip -> RowBox[{"Multivector", " ", ToBoxes@A}],
    Editable -> True
    ]],
    $Failed
]


MultiplyIndices::badIndex = "Index `1` is incompatible with metric `2`";
checkIndex[i_Integer, m_List] := 
    If[Not[0 <= Min[i] && Max[i] <= Length[m]],
        Message[MultiplyIndices::badIndex, i, m];
        $Failed
    ]
MultiplyIndices[i_List, j_List, m_List] :=
    Module[{index = Join[i, j], newIndex, order, sign, squares},
        If[FailureQ[checkIndex[i, m]] || FailureQ[checkIndex[j, m]], Return[$Failed]];
        order = Ordering[index];
        sign = Signature[order];
        {newIndex, squares} = Reap@SequenceReplace[index[[order]] ,{x_ ,x_} :> (Sow[x]; Nothing)];
        If[Length@squares > 0,
            sign = sign Times@@m[[squares[[1]]]]
        ];
        {sign ,newIndex}
    ]


End[]

EndPackage[]
