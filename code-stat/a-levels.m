(* ::Package:: *)

(* tables *)

ngrid1=Dimensions[chi2Tab][[1]];ngrid2=Dimensions[chi2Tab][[2]];

par1TabDiff=nonconstantBX[chi2Tab[[All,1,1]]];
par2TabDiff=nonconstantBX[chi2Tab[[1,All,2]]];

chi2Aux=chi2Tab[[All,All,3]];minchi2=Min[chi2Aux];dchi2Aux=chi2Aux-minchi2;
dchi2Tab=chi2Tab;dchi2Tab[[All,All,3]]-=minchi2;dchi2TabF=Flatten[dchi2Tab,1];

(*LAuxInt=Total[KroneckerProduct[par1TabDiff,par2TabDiff]LAux,2]*)
(*LAuxInt=Sum[par1TabDiff[[i]]par2TabDiff[[j]]LAux[[i,j]],{i,1,ngrid1},{j,1,ngrid2}];*)


If[
$VersionNumber<11.3
,

LAux=Exp[-chi2Aux/2];
LAuxInt=Total[TensorProduct[par1TabDiff,par2TabDiff]LAux,2];

,

(* with SetPrecision \[Rule] 11.3 nastiness *)
LAux=Exp[SetPrecision[-chi2Aux/2,$MachinePrecision]];
LAuxInt=Total[TensorProduct[SetPrecision[par1TabDiff,$MachinePrecision],SetPrecision[par2TabDiff,$MachinePrecision]]LAux,2];

];


(* best-fit model *)

pos=Position[dchi2Tab[[All,All,3]],Min[dchi2Tab[[All,All,3]]]][[1]];
posc=dchi2Tab[[pos[[1]],pos[[2]],1;;2]];


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
