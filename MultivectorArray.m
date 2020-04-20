Package["GeometricAlgebra`"]

PackageExport["MultivectorArray"]

MultivectorArray::usage = "MultivectorArray[vs, shape] gives a multi dimensional array of multivectors with specified shape";
Options[MultivectorArray] = {"Components" -> {}, "Shape" -> {}}

MultivectorArray::badShape = "Specified shape `1` is not cmopatible with dimensions `2`";

MultivectorArray[vs_ /; ArrayQ[vs, _, MatchQ[_Multivector]], shape: {___Integer}] := If[
    Dimensions[vs] == Abs[DeleteCases[shape, 0]],
    MultivectorArray["Components" -> vs, "Shape" -> shape],
    Message[MultivectorArray::badShape, shape, Dimensions[vs]]; $Failed
]

MultivectorArray[v_Multivector, shape: {}] := If[
    DeleteCases[shape, 0] === {},
    MultivectorArray["Components" -> v, "Shape" -> shape],
    Message[MultivectorArray::badShape, shape, {}]; $Failed
]

va_MultivectorArray[opt_String] /; KeyExistsQ[Options[va], opt] := Lookup[Options[va], opt]
va_MultivectorArray["Rank"] := Length @ va["Shape"]

GeometricProduct[va_MultivectorArray, vb_MultivectorArray] := With[{
    outer = Outer[GeometricProduct, va["Components"], vb["Components"]],
    shape = Join[va["Shape"], vb["Shape"]],
    shapeContraction = {va["Rank"], va["Rank"] + 1}
},
    If[ MatchQ[{Last @ va["Shape"], First @ vb["Shape"]}, {x_ ? Negative, y_ ? Positive} /; x == -y],
        MultivectorArray[Total[outer, shapeContraction], Delete[shape, List /@ shapeContraction]],
        MultivectorArray[outer, shape]
    ]
]


PackageExport["ShapeContract"]

ShapeContract[va_MultivectorArray] := With[{
    shapeContractions = SequencePosition[va["Shape"], {x_ ? Negative, y_ ? Positive} /; x == -y]
},
    MultivectorArray[Fold[Total, va, shapeContractions], Delete[va["Shape"], Thread @ shapeContractions]]
]

(*
MultivectorArray /: MakeBoxes[va: MultivectorArray[OptionsPattern[]], StandardForm] := With[{
    box =
        If[ va["Shape"] === {}, 
            ToBoxes[va["Components"], StandardForm],
            If[ First[va["Shape"]] > 0,
                RowBox[{"[", GridBox[{ToBoxes[MultivectorArray[#, Rest[va["Shape"]]], StandardForm]} & /@ va["Components"]], "]"}],
                RowBox[{"(", GridBox[{ToBoxes[MultivectorArray[#, Rest[va["Shape"]]], StandardForm] & /@ va["Components"]}], ")"}]
            ]
        ],
    tooltip = RowBox[{"MultivectorArray ", ToBoxes @ va["Shape"]}]
},
    InterpretationBox[box, va, Tooltip -> tooltip]
]
*)

shapeGridBoxes[array_, shape_] := If[shape === {},
    Slot[array],
    If[First[shape] > 0,
        RowBox[{"[", GridBox[{shapeGridBoxes[#, Rest[shape]]} & /@ array], "]"}],
        RowBox[{"(", GridBox[{shapeGridBoxes[#, Rest[shape]] & /@ array}], ")"}]
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


