#!/usr/bin/env wolframscript
(* ::Package:: *)

srcDir=DirectoryName @ $InputFileName;
dstDir=srcDir;
CreatePacletArchive[srcDir, dstDir]
