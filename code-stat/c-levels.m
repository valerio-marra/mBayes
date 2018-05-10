(* ::Package:: *)

(* chi2/L interpolation *)

{xMin,xMax}={pSet[[plotname,1]],pSet[[plotname,2]]};
(*{xMin,xMax}={Min[LTabx[[All,1]]],Max[LTabx[[All,1]]]};*)

(* option to reduce noise *)
(*If[plotname==If[Length[best[[2]]]==3,2,3],
chi2TabxZ=chi2Tabx;
cfit[x_]=Fit[chi2Tabx, {1,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10}, x];
chi2Tabx[[All,2]]=cfit[chi2Tabx[[All,1]]];
];*)
(*Show[ListPlot[chi2TabxZ, PlotStyle\[Rule]Red], Plot[cfit[x], {x, xMin,xMax}]];*)


Which[
noisy==0
	,
(* chi2 is naturallly locally parabolic and so InterpolationOrder\[Rule]2 should be ok *)
chi2int=Interpolation[chi2Tabx,InterpolationOrder->chi2IO,Method->"Spline"];
	,
noisy==1
,
(* low-pass filter *)
basis[n_,\[Lambda]_]:=With[{\[Omega]=(2 \[Pi])/N[\[Lambda]],k=Range[n]},Join[{1},Cos[k \[Omega] x],Sin[k \[Omega] x]]];
chi2int=Function[x,Evaluate[Fit[chi2Tabx,basis[10,8 2 Differences[MinMax[chi2Tab[[All,1]]]][[1]]],x]]];
chi2Tabx[[All,2]]=chi2int[chi2Tabx[[All,1]]];
,
noisy==2
,
chi2int=Function[x,Evaluate[Fit[chi2Tabx,{1,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10},x]]];
chi2Tabx[[All,2]]=chi2int[chi2Tabx[[All,1]]];
]


norm=NIntegrate[Exp[-chi2int[x]/2],{x,xMin,xMax}(*,Method->{Automatic,"SymbolicProcessing"->0}*),AccuracyGoal->accugoal];
Likel[x_]=Exp[-chi2int[x]/2]/norm;


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
