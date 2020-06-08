Package["GeometricAlgebra`"]


PackageExport["MultivectorArray"]
MultivectorArray::usage = "MultivectorArray[vs, shape] gives a multi dimensional array of multivectors with specified shape";

PackageExport["ShapeContract"]
ShapeContract::usage = "Contract MultivectorArray indices";


PackageScope["mapComponents"]


Options[MultivectorArray] = {"Components" -> {}, "Shape" -> {}}


$MultivectorArrayProperties = {
    "Components",
    "Shape",

    "Rank",
    "GeometricAlgebra",

    "Numeric"
}


MultivectorArray::badShape = "Specified shape `1` is not cmopatible with dimensions `2`";

MultivectorArray[vs_, shape: {___Integer}] /; ArrayQ[vs, _, MatchQ[_MultivectorArray]] :=
    MultivectorArray[
        Map[#["Components"] &, vs, {ArrayDepth[vs]}],
        Join[shape, First[MaximalBy[Flatten @ vs, #["Rank"] &]]["Shape"]]
    ]

MultivectorArray[vs_, shape: {___Integer}] /; ArrayQ[vs, _, MatchQ[_Multivector]] := If[
    DeleteCases[Dimensions[vs], 0] == Abs[DeleteCases[shape, 0]],
    MultivectorArray["Components" -> vs, "Shape" -> shape],
    Message[MultivectorArray::badShape, shape, Dimensions[vs]]; $Failed
]

MultivectorArray[vs_, shape_] /; ArrayQ[vs] :=
    MultivectorArray[Map[Multivector[{#}, {0}] &, vs, {ArrayDepth[vs]}], shape]

MultivectorArray[vs_] /; ArrayQ[vs] := With[{dim = Dimensions[vs] /. 0 -> Nothing}, MultivectorArray[vs, dim * (-1) ^ Range[0, Length[dim] - 1]]]

MultivectorArray[v_Multivector, shape_: {}] := MultivectorArray["Components" -> v, "Shape" -> {}]


va_MultivectorArray[opt_String] /; KeyExistsQ[Options[va], opt] := Lookup[Options[va], opt]


va_MultivectorArray["Rank"] := Length @ va["Shape"]

va_MultivectorArray[f_] := mapComponents[f, va]


MultivectorArray /: Dimensions[va_MultivectorArray] := Abs @ va["Shape"]


va_MultivectorArray["GeometricAlgebra"] := GeometricAlgebra[MapThread[Max, Flatten[Map[#["Signature"] &, va["Components"], {va["Rank"]}], 1]]]


MultivectorArray /: f_[v_Multivector, va_MultivectorArray] := mapComponents[f[v, #] &, va]

MultivectorArray /: f_[va_MultivectorArray, v_Multivector] := mapComponents[f[#, v] &, va]

MultivectorArray /: f_[x_ ? NumericQ, va_MultivectorArray] := mapComponents[f[x, #] &, va]

MultivectorArray /: f_[va_MultivectorArray, y_ ? NumericQ] := mapComponents[f[#, y] &, va]


va_MultivectorArray["Numeric"] := Map[MultivectorNumber, va["Components"], {va["Rank"]}]


MultivectorArray /: GeometricProduct[va_MultivectorArray, vb_MultivectorArray] /; va["Rank"] > 0 && vb["Rank"] > 0 := With[{
    outer = Outer[GeometricProduct, va["Components"], vb["Components"]],
    shape = Join[va["Shape"], vb["Shape"]],
    shapeContraction = {va["Rank"], va["Rank"] + 1}
},
    If[ MatchQ[{Last @ va["Shape"], First @ vb["Shape"]}, {x_ ? Negative, y_ ? Positive} /; x == -y],
        MultivectorArray[TensorContract[outer, shapeContraction], Delete[shape, List /@ shapeContraction]],
        MultivectorArray[outer, shape]
    ]
]

GeometricProduct[va_MultivectorArray, vb_MultivectorArray] := GeometricProduct[expandDims[va, -1, 1], expandDims[vb, 1, 1]]


GeometricProduct[vs__MultivectorArray] := Fold[GeometricProduct, {vs}]


MultivectorArray /: f_Symbol[va_MultivectorArray, vb_MultivectorArray] /; MemberQ[Attributes[f], NumericFunction] && va["Shape"] == vb["Shape"] :=
    MultivectorArray[f[va["Components"], vb["Components"]], va["Shape"]]


MultivectorArray /: Plus[vas__MultivectorArray] := Fold[Plus, {vas}]


(* Transpose *)

transposeShape[shape_] := - shape

transposeShape[shape_, n_Integer] := MapAt[Minus, shape, {n}]

transposeShape[shape_List] /; Length[shape] > 1 := transposeShape[shape, 1 <-> 2]

transposeShape[shape_List, levels_List] /; Length[shape] == Length[levels] := - shape[[levels]]

transposeShape[shape_List, m_Integer <-> n_Integer] /; Length[shape] > 1 := MapAt[Minus, Permute[shape, Cycles[{{m, n}}]], {{m}, {n}}]


MultivectorArray /: Transpose[va_MultivectorArray, args___] :=
    If[va["Rank"] > 1,
      MultivectorArray[Transpose[va["Components"], args], transposeShape[va["Shape"], args]],
      MultivectorArray[va["Components"], transposeShape[va["Shape"]]]
    ]


unaryOps = Grade | Reverse | Involute | Conjugate | LeftDual | RightDual | Dual | MultivectorTransform;

MultivectorArray /: (f: unaryOps)[va_MultivectorArray, args___] :=
    mapComponents[f[#, args] &, va]


shapeContract[va_MultivectorArray] := With[{
    shapeContractions = SequencePosition[va["Shape"], {x_ ? Negative, y_ ? Positive} /; x == -y]
},
    If[Length[shapeContractions] > 0,
        MultivectorArray[TensorContract[va["Components"], shapeContractions], Delete[va["Shape"], List /@ Flatten @ shapeContractions]],
        va
    ]
]

ShapeContract[va_MultivectorArray] := FixedPoint[shapeContract, va]

shapeGridBoxes[array_, shape_] := If[shape === {},
    Slot[array],
    If[First[shape] > 0,
        RowBox[{"(", GridBox[{shapeGridBoxes[#, Rest[shape]]} & /@ array],   ")"}],
        RowBox[{"[", GridBox[{shapeGridBoxes[#, Rest[shape]] & /@ array}], "]"}]
    ]
]

MultivectorArray /: MakeBoxes[va: MultivectorArray[opts: OptionsPattern[]], _] := Module[{
    shape = va["Shape"],
    components,
    dims, size,
    boxes,
    display,
    interpret
},
    dims = Abs @ shape;
    size = Times @@ dims;
    components = First @ Map[MakeBoxes, Lookup[List @@ RuleDelayed @@@ Hold[opts], "Components", None, Hold], {va["Rank"] + 1}];
    boxes = If[va["Rank"] > 0, Flatten @ components, {components}];
    display = If[shape === {}, Slot[1], shapeGridBoxes[ArrayReshape[Range[size], dims], shape]];
    interpret = RowBox[{"MultivectorArray", "[",
        "\"Components\"", "->", If[va["Rank"] > 0, ToBoxes[ArrayReshape[Slot /@ Range[size], dims]], Slot[1]]
            /. slot_String /; StringMatchQ[slot, "#" ~~ DigitCharacter..] :> ToExpression[slot], ",",
        "\"Shape\"", "->", ToBoxes[shape],
    "]"}];
    TemplateBox[boxes,
        "MultivectorArray",
        DisplayFunction -> (Evaluate[display] &),
        InterpretationFunction -> (Evaluate[interpret] &),
        Tooltip -> RowBox[{"MultivectorArray ", ToBoxes @ shape}],
        Editable -> True
    ]
]


(* Utility functions *)

mapComponents[f_, va_MultivectorArray] := MultivectorArray[
    If[va["Rank"] == 0, f[va["Components"]], Map[f, va["Components"], {va["Rank"]}]],
    va["Shape"]
]

expandDims[va_MultivectorArray, sign_Integer: 1, dim_Integer: -1] /; sign != 0 := With[{
    shape = Insert[va["Shape"], Sign[sign], dim]
},
    MultivectorArray[If[va["Rank"] == 0, {va["Components"]}, ArrayReshape[va["Components"], Abs[shape]]], shape]
]
