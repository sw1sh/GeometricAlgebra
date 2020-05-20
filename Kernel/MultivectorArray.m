Package["GeometricAlgebra`"]

PackageExport["MultivectorArray"]

MultivectorArray::usage = "MultivectorArray[vs, shape] gives a multi dimensional array of multivectors with specified shape";
Options[MultivectorArray] = {"Components" -> {}, "Shape" -> {}}

MultivectorArray::badShape = "Specified shape `1` is not cmopatible with dimensions `2`";

MultivectorArray[vs_, shape: {___Integer}] /; ArrayQ[vs, _, MatchQ[_Multivector]] := If[
    Dimensions[vs] == Abs[DeleteCases[shape, 0]],
    MultivectorArray["Components" -> vs, "Shape" -> shape],
    Message[MultivectorArray::badShape, shape, Dimensions[vs]]; $Failed
]
MultivectorArray[vs_ ] /; ArrayQ[vs, _, MatchQ[_Multivector]] := MultivectorArray[vs, Dimensions[vs]]

MultivectorArray[v_Multivector, shape_: {}] := MultivectorArray["Components" -> v, "Shape" -> {}]

va_MultivectorArray[opt_String] /; KeyExistsQ[Options[va], opt] := Lookup[Options[va], opt]
va_MultivectorArray["Rank"] := Length @ va["Shape"]

mapComponents[f_, va_MultivectorArray] := MultivectorArray[
    If[va["Rank"] == 0, f[va["Components"]], Map[f, va["Components"], {va["Rank"]}]],
    va["Shape"]
]

expandDims[va_MultivectorArray, sign_Integer: 1, dim_Integer: -1] /; sign != 0 := With[{
    shape = Insert[va["Shape"], Sign[sign], dim]
},
    MultivectorArray[If[va["Rank"] == 0, {va["Components"]}, ArrayReshape[va["Components"], Abs[shape]]], shape]
]

MultivectorArray /: f_[va_MultivectorArray, vb_MultivectorArray] /; va["Rank"] > 0 && vb["Rank"] > 0 := With[{
    outer = Outer[f, va["Components"], vb["Components"]],
    shape = Join[va["Shape"], vb["Shape"]],
    shapeContraction = {va["Rank"], va["Rank"] + 1}
},
    If[ MatchQ[{Last @ va["Shape"], First @ vb["Shape"]}, {x_ ? Negative, y_ ? Positive} /; x == -y],
        MultivectorArray[TensorContract[outer, shapeContraction], Delete[shape, List /@ shapeContraction]],
        MultivectorArray[outer, shape]
    ]
]
MultivectorArray /: f_[va_MultivectorArray, vb_MultivectorArray] := f[expandDims[va, -1], expandDims[vb, 1, 1]]
MultivectorArray /: f[vas__MultivectorArray] := Fold[f, {vas}]


unaryOps = Grade | Reverse | Involute | Conjugate | LeftDual | RightDual | Dual | MultivectorTransform;
MultivectorArray /: (f: unaryOps)[va_MultivectorArray, args___] :=
    mapComponents[f[#, args] &, va]

PackageExport["ShapeContract"]

ShapeContract[va_MultivectorArray] := With[{
    shapeContractions = SequencePosition[va["Shape"], {x_ ? Negative, y_ ? Positive} /; x == -y]
},
    MultivectorArray[Fold[Total, va, shapeContractions], Delete[va["Shape"], Thread @ shapeContractions]]
]

shapeGridBoxes[array_, shape_] := If[shape === {},
    Slot[array],
    If[First[shape] > 0,
        RowBox[{"(", GridBox[{shapeGridBoxes[#, Rest[shape]]} & /@ array],   ")"}],
        RowBox[{"[", GridBox[{shapeGridBoxes[#, Rest[shape]] & /@ array}], "]"}]
    ]
]

MultivectorArray /: MakeBoxes[va: MultivectorArray[OptionsPattern[]], StandardForm] := Module[{
    shape = va["Shape"],
    dims, rank,
    boxes,
    display,
    interpret
},
    dims = Abs @ shape;
    rank = Times @@ dims;
    boxes = If[ListQ[va["Components"]], Map[ToBoxes, Flatten @ va["Components"]], {ToBoxes @ va["Components"]}];
    display = If[shape === {}, Slot[1], shapeGridBoxes[ArrayReshape[Range[rank], dims], shape]];
    interpret = RowBox[{"MultivectorArray", "[", 
        "\"Components\"", "->", If[ListQ[va["Components"]], ToBoxes[ArrayReshape[Slot /@ Range[rank], dims]], Slot[1]]
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


