Needs["PacletManager`"];

$repoRoot = ExpandFileName[FileNameJoin[{DirectoryName[$InputFileName], ".."}]];
packageName = FileBaseName[$repoRoot];
$buildDirectory = FileNameJoin[{$repoRoot, "Build"}];

deleteBuildDirectory[] :=
  If[FileExistsQ[$buildDirectory], DeleteDirectory[$buildDirectory, DeleteContents -> True]];

copyWLSourceToBuildDirectory[] := With[{
    files = Append[Import[FileNameJoin[{$repoRoot, "Kernel"}]], FileNameJoin[{"..", "PacletInfo.m"}]]},
  If[!FileExistsQ[#], CreateDirectory[#]] & /@ {$buildDirectory, FileNameJoin[{$buildDirectory, "Kernel"}]};
  CopyFile[FileNameJoin[{$repoRoot, "Kernel", #}], FileNameJoin[{$buildDirectory, "Kernel", #}]] & /@ files;
];

packPaclet[] := Module[{pacletFileName},
  pacletFileName = PackPaclet[$buildDirectory, $repoRoot];
  If[TrueQ[FileExistsQ[FileNames[packageName <> "*.paclet"][[1]]]],
      Print[FileNames[packageName <> "*.paclet"][[-1]] <> " ... OK"],
      Print["Paclet not produced"]
    ]
]
