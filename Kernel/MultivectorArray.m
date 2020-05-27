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
MultivectorArray[vs_] /; ArrayQ[vs, _, MatchQ[_Multivector]] := MultivectorArray[vs, Dimensions[vs]]

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


MultivectorArray /: f_[v_Multivector, va_MultivectorArray] := mapComponents[f[v, #] &, va]
MultivectorArray /: f_[va_MultivectorArray, v_Multivector] := mapComponents[f[#, v] &, va]

MultivectorArray /: f_[x_ ? NumericQ, va_MultivectorArray] := mapComponents[f[x, #] &, va]
MultivectorArray /: f_[va_MultivectorArray, y_ ? NumericQ] := mapComponents[f[#, y] &, va]

MultivectorArray /: (NonCommutativeMultiply | GeometricProduct)[va_MultivectorArray, vb_MultivectorArray] /; va["Rank"] > 0 && vb["Rank"] > 0 := With[{
    outer = Outer[GeometricProduct, va["Components"], vb["Components"]],
    shape = Join[va["Shape"], vb["Shape"]],
    shapeContraction = {va["Rank"], va["Rank"] + 1}
},
    If[ MatchQ[{Last @ va["Shape"], First @ vb["Shape"]}, {x_ ? Negative, y_ ? Positive} /; x == -y],
        MultivectorArray[TensorContract[outer, shapeContraction], Delete[shape, List /@ shapeContraction]],
        MultivectorArray[outer, shape]
    ]
]
MultivectorArray /: (NonCommutativeMultiply | GeometricProduct)[va_MultivectorArray, vb_MultivectorArray] := GeometricProduct[expandDims[va, -1], expandDims[vb, 1, 1]]

MultivectorArray /: f_[va_MultivectorArray, vb_MultivectorArray] /; va["Shape"] == vb["Shape"] :=
    MultivectorArray[f[va["Components"], vb["Components"]], va["Shape"]]

MultivectorArray /: Plus[vas__MultivectorArray] := Fold[Plus, {vas}]


PackageExport[MultivectorMatrix]

MultivectorMatrix[v_MultivectorArray] := Map[#["Scalar"] + I #["Coordinate", -1] &,
    v["Components"], {Length[v["Shape"]]}]


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
    components = First @ Map[MakeBoxes, Lookup[List @@ RuleDelayed @@@ Hold[opts], "Components", None, Defer], {va["Rank"] + 1}];
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


