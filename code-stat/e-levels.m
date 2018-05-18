(* ::Package:: *)

(* finding max L from the interpolating function *)

maxXtab=Sort[LTabx,#1[[2]]>#2[[2]]&][[1,1]];
epsmax=0.05;
maxP=FindMaximum[Likel[x],{x,(xMin+xMax+100 maxXtab)/102,Max[{xMin,maxXtab (1-Sign[maxXtab]epsmax)}],Min[{xMax,maxXtab (1+Sign[maxXtab]epsmax)}]},AccuracyGoal->accugoal];
{maxX,maxY}={x/.maxP[[2]],maxP[[1]]};


probLe[L_?NumericQ]:=(
xL=x/.FindRoot[chi2int[x]==-2Log[L/maxY],{x,(xMin+2 maxX)/3,xMin,maxX},AccuracyGoal->accugoal(*,MaxIterations\[Rule]50*)];
xR=x/.FindRoot[chi2int[x]==-2Log[L/maxY],{x,(xMax+2 maxX)/3,maxX,xMax},AccuracyGoal->accugoal(*,MaxIterations\[Rule]50*)];
NIntegrate[Likel[x],{x,xL,xR},(*Method->{Automatic,"SymbolicProcessing"->0},*)AccuracyGoal->accugoal]
);


Off[FindRoot::reged];


vecerror={};
Do[
If[isig>0,
Lev=L/.FindRoot[probLe[L]==sigmas[[ii]],{L,maxY Exp[-gausigmas[[ii]]/2],maxY Exp[-(ii+2)^2/2],maxY Exp[-(ii-1)^2/2]},AccuracyGoal->accugoal];
,
Lev=maxY Exp[-gausigmas[[ii]]/2];
];
probLe[Lev];
{xLlev,xRlev}={xL,xR};
vecLix=Flatten[{maxX,{xLlev,xRlev}-maxX}];
check=Quiet[NIntegrate[Likel[x],{x,xLlev,xRlev}(*,Method->{Automatic,"SymbolicProcessing"->0}*)]]-sigmas[[ii]];
AppendTo[vecerror,Join[vecLix,{xLlev,xRlev,gausigmas[[ii]],-2Log[Lev/maxY],Chop[check]}]];
,{ii,1,Abs[isig]}];


On[FindRoot::reged];


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
