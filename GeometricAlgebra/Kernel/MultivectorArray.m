Package["Wolfram`GeometricAlgebra`"]


PackageExport["MultivectorArray"]
MultivectorArray::usage = "MultivectorArray[vs, shape] gives a multi dimensional array of multivectors with specified shape";

PackageExport["MultivectorArrayQ"]

PackageExport["ShapeContract"]
ShapeContract::usage = "Contract MultivectorArray indices";


PackageScope["mapComponents"]


Options[MultivectorArray] = {}


multivectorArrayQ[HoldPattern[MultivectorArray[vs_, shape : {___Integer}]]] :=
    DeleteCases[Dimensions[Unevaluated[vs]], 1] == DeleteCases[Abs[shape], 1] && (shape === {} || ArrayQ[Unevaluated[vs], _, MultivectorQ])

multivectorArrayQ[___] := False


MultivectorArrayQ[va_MultivectorArray] := System`Private`HoldValidQ[va] || multivectorArrayQ[Unevaluated[va]]

MultivectorArrayQ[___] := False


$MultivectorArrayProperties = {
    "Components",
    "Shape",

    "Rank",
    "GeometricAlgebra",

    "Numeric"
}


MultivectorArray[vs_, shape : {___Integer}] /; vs =!= {} && ArrayQ[vs, _, MatchQ[_MultivectorArray]] :=
    MultivectorArray[
        Map[#["Components"] &, vs, {ArrayDepth[vs]}],
        Join[shape, First[Through[MaximalBy[Flatten @ vs, #["Rank"] &]["Shape"]], {}]]
    ]

MultivectorArray[vs_, shape_] /; vs =!= {} && ArrayQ[vs] && ! ArrayQ[vs, _, MultivectorQ] :=
    MultivectorArray[Map[If[MultivectorQ[#], #, Multivector[{#}, 0]] &, vs, {ArrayDepth[vs]}], Developer`ToList[shape]]

MultivectorArray[vs_] /; ArrayQ[vs] := With[{dim = Dimensions[vs]}, MultivectorArray[vs, dim * (-1) ^ Range[0, Length[dim] - 1]]]

MultivectorArray[x_] := MultivectorArray[x, {}]


(HoldPattern[MultivectorArray[vs_, _]] ? MultivectorArrayQ)["Components"] := vs

(HoldPattern[MultivectorArray[_, shape_]] ? MultivectorArrayQ)["Shape"] := shape


va_MultivectorArray["Dimension"] := Times @@ Abs @ va["Shape"]


va_MultivectorArray["Rank"] := With[{shape = va["Shape"]}, If[shape === {}, 1, Length[shape]]]


va_MultivectorArray["SquareQ"] := Equal @@ Dimensions[va]


va_MultivectorArray["DoubleSquareQ"] := va["SquareQ"] && IntegerQ[Log2[va["Dimension"]] / 2]


va_MultivectorArray[f_] := mapComponents[f, va]


MultivectorArray /: Dimensions[va_MultivectorArray] := Abs @ va["Shape"]


va_MultivectorArray["GeometricAlgebra"] := With[{r = va["Rank"]},
    GeometricAlgebra[MapThread[Max, Flatten[Map[#["Signature"] &, va["Components"], {r}], r - 1]]]
]

MultivectorArray /: va_MultivectorArray[opt: Alternatives @@ $GeometricAlgebraProperties] := va["GeometricAlgebra"][opt]


MultivectorArray /: f_[v_Multivector, va_MultivectorArray] := mapComponents[f[v, #] &, va]

MultivectorArray /: f_[va_MultivectorArray, v_Multivector] := mapComponents[f[#, v] &, va]

MultivectorArray /: f_[x_ ? NumericQ, va_MultivectorArray] := mapComponents[f[x, #] &, va]

MultivectorArray /: f_[va_MultivectorArray, y_ ? NumericQ] := mapComponents[f[#, y] &, va]


va_MultivectorArray["Numeric"] := Map[#["Numeric"] &, va["Components"], {2}]


va_MultivectorArray["Real"] := va[#["Real"] &]


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

GeometricProduct[x : Except[_Multivector], va_MultivectorArray] := x * va

GeometricProduct[va_MultivectorArray, x : Except[_Multivector]] := x * va


GeometricProduct[left___, va_MultivectorArray, right___] := Fold[GeometricProduct, {left, va, right}]


MultivectorArray /: f_Symbol[va_MultivectorArray ? MultivectorArrayQ, vb_MultivectorArray ? MultivectorArrayQ] /; MemberQ[Attributes[f], NumericFunction] && va["Shape"] == vb["Shape"] :=
    MultivectorArray[f[va["Components"], vb["Components"]], va["Shape"]]


MultivectorArray /: Plus[vas__MultivectorArray] := Fold[Plus, {vas}]


MultivectorArray /: NonCommutativeMultiply[left___, v_MultivectorArray, right___] := GeometricProduct[left, v, right]


MultivectorArray /: Equal[vas__MultivectorArray ? MultivectorArrayQ] := With[{shapes = Through[{vas}["Shape"]]},
    Equal @@ shapes && Apply[And, MapThread[Equal, Normal /@ {vas}, Length[First[shapes]]], All]
]

MultivectorArray /: Normal[va_MultivectorArray ? MultivectorArrayQ] := va["Components"]

MultivectorArray /: (f_Symbol ? elementwiseFunctionQ)[va_MultivectorArray, args___] := va[f[#, args] &]

MultivectorArray /: N[va_MultivectorArray ? MultivectorArrayQ, args___] := With[{components = N[va["Components"], args]},
    MultivectorArray[components, va["Shape"]] /; components =!= va["Components"]
]

SetAttributes[MultivectorArray, NHoldAll]

va_MultivectorArray /; System`Private`HoldNotValidQ[va] && multivectorArrayQ[Unevaluated[va]] := System`Private`SetNoEntry[System`Private`HoldSetValid[va]]


(* Transpose *)

transposeShape[shape_] := - shape

transposeShape[shape_, n_Integer] := MapAt[Minus, shape, {n}]

transposeShape[shape_List] /; Length[shape] > 1 := transposeShape[shape, 1 <-> 2]

transposeShape[shape_List, levels_List] /; Length[shape] == Length[levels] := - shape[[levels]]

transposeShape[shape_List, m_Integer <-> n_Integer] /; Length[shape] > 1 := MapAt[Minus, Permute[shape, Cycles[{{m, n}}]], {{m}, {n}}]


MultivectorArray /: Transpose[va_MultivectorArray, args___] :=
    If[ va["Rank"] > 1,
        MultivectorArray[Transpose[va["Components"], args], transposeShape[va["Shape"], args]],
        MultivectorArray[va["Components"], transposeShape[va["Shape"]]]
    ]


(* Contraction *)

shapeContract[va_MultivectorArray] := With[{
    shapeContractions = SequencePosition[va["Shape"], {x_ ? Negative, y_ ? Positive} /; x == -y]
},
    If[Length[shapeContractions] > 0,
        MultivectorArray[TensorContract[va["Components"], shapeContractions], Delete[va["Shape"], List /@ Flatten @ shapeContractions]],
        va
    ]
]

ShapeContract[va_MultivectorArray] := FixedPoint[shapeContract, va]


(* Boxes *)

shapeGridBoxes[array_, shape_] := If[shape === {},
    Slot[array],
    If[ First[shape] > 0,
        RowBox[{"(", GridBox[{shapeGridBoxes[#, Rest[shape]]} & /@ array],   ")"}],
        RowBox[{"[", GridBox[{shapeGridBoxes[#, Rest[shape]] & /@ array}], "]"}]
    ]
]

MultivectorArray /: MakeBoxes[va_MultivectorArray /; MultivectorArrayQ[Unevaluated[va]], form_] := Module[{
    shape = va["Shape"],
    components = va["Components"],
    dims, size,
    boxes,
    display,
    interpret
},
    dims = Abs @ shape;
    size = Times @@ dims;
    boxes = ToBoxes[#, form] & /@ Flatten[{components}];
    display = Which[size == 0, RowBox[{"(", ")"}], shape === {}, Slot[1], True, shapeGridBoxes[ArrayReshape[Range[size], dims], shape]];
    interpret = RowBox[{"MultivectorArray", "[",
        If[va["Rank"] > 0, ToBoxes[ArrayReshape[Slot /@ Range[size], dims]], Slot[1]]
            /. slot_String /; StringMatchQ[slot, "#" ~~ DigitCharacter ..] :> ToExpression[slot],
        ",",
        ToBoxes[shape, form],
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
