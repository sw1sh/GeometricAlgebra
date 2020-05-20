BeginTestSection["Test"]

SetOptions[Multivector, "GeometricAlgebra" -> GeometricAlgebra[0, 1]];

VerificationTest[
    Normal[Multivector[{1, 2}] ** Multivector[{3, -4}]],
    ReIm[(1 + 2 I) (3 - 4 I)],
    TestID -> "Complex number multiplication"
]

SetOptions[Multivector, "GeometricAlgebra" -> GeometricAlgebra[2, 3]];

VerificationTest[
    Map[Grade[Multivector[SparseArray[{1 -> 01, 3 -> 13, 9 -> 29, 25 -> 316, 28 -> 428, 30 -> 430, 32 -> 532}]], #]&, Range[0, 5]],
    {
        Multivector[SparseArray[{1 -> 01}]],
        Multivector[SparseArray[{3 -> 13}]],
        Multivector[SparseArray[{9 -> 29}]],
        Multivector[SparseArray[{25 -> 316}]],
        Multivector[SparseArray[{28 -> 428, 30 -> 430}]],
        Multivector[SparseArray[{32 -> 532}]]
    },
    TestID -> "Grade computation"
]

VerificationTest[
    MakeBoxes[x Multivector[{1,2,3}, "GeometricAlgebra" -> GeometricAlgebra[4,1]], StandardForm],
    $Failed,
    TestID -> "Boxes"
]

EndTestSection[]
