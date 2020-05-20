#!/usr/bin/env wolframscript

Check[
    Get[FileNameJoin[{DirectoryName[$InputFileName], "scripts", "buildInit.wl"}]];
    deleteBuildDirectory[];,

    Exit[1];
];

Check[
  copyWLSourceToBuildDirectory[];
  packPaclet[];
  deleteBuildDirectory[];
  Print["Build done."]
  ,
  Print["Build failed."];
  Exit[1]
];

