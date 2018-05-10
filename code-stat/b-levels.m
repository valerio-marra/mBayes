(* ::Package:: *)

(* level estimator *)

(*lev2d[z_?NumericQ]:=Module[{s,zv},
zv=z;
Lsigma=0;
Do[If[dchi2Aux[[i,j]]<zv,Lsigma+=par1TabDiff[[i]]par2TabDiff[[j]]LAux[[i,j]]],{i,1,ngrid1},{j,1,ngrid2}];
s=Lsigma/LAuxInt;
s];*)

lev2d[z_?NumericQ]:=Catch[Do[{Lsigma=0,Do[If[dchi2Aux[[i,j]]<z,Lsigma+=par1TabDiff[[i]]par2TabDiff[[j]]LAux[[i,j]]],{i,1,ngrid1},{j,1,ngrid2}],Throw[Lsigma/LAuxInt]},{1}]];


(* smoothing via fit *)

zmin=.01;zmax=50;nzstep=100;(* 18 instead of 10, 100 instead of 250 *)
(* linear space fit *)
(*g2d=Interpolation[Partition[Riffle[Range[zmin,zmax,(zmax-zmin)/nzstep],Map[lev,Range[zmin,zmax,(zmax-zmin)/nzstep]]],2],InterpolationOrder->2];*)
(* superlog space fit *)
logtab2d=Table[{logz,Log[-Log[1-lev2d[Exp[logz]]]]},{logz,Log[zmin],Log[zmax],(Log[zmax]-Log[zmin])/nzstep}];
glog2d=Interpolation[logtab2d,InterpolationOrder->1];
(*glog2dx=Fit[logtab2d, {1,x,x^2,x^3,x^4,x^5,x^6,x^7}, x];
glog2d[z_]=glog2dx/.x->z;*)
gx2d[z_]:=1-Exp[-Exp[glog2d[Log[z]]]];


ClearAll[s1f,s2f,s3f,s4f,s5f];
levels={};
errors={};

If[nlevel>=1,
s1f=Quiet[z/.FindRoot[gx2d[z]==v1t,{z,s1t 0.05,zmin,10},AccuracyGoal->accugoal]];
If[Abs[gx2d[s1f]-v1t]>10^-3,
s1f=Quiet[z/.FindRoot[gx2d[z]==v1t,{z,s1t .5,zmin,10},AccuracyGoal->accugoal]];
,Null];
If[Abs[gx2d[s1f]-v1t]>10^-3,
s1f=z/.FindRoot[gx2d[z]==v1t,{z,s1t .9,zmin,10},AccuracyGoal->accugoal];
,Null];
AppendTo[levels,Chop[s1f]];
AppendTo[errors,Chop[Abs[gx2d[s1f]-v1t]]];
];

If[nlevel>=2,
s2f=Quiet[z/.FindRoot[gx2d[z]==v2t,{z,s2t .05,s2t .02,20},AccuracyGoal->accugoal]];
If[Abs[gx2d[s2f]-v2t]>10^-3,
s2f=Quiet[z/.FindRoot[gx2d[z]==v2t,{z,s2t .6,1,20},AccuracyGoal->accugoal]];
,Null];
If[Abs[gx2d[s2f]-v2t]>10^-3,
s2f=z/.FindRoot[gx2d[z]==v2t,{z,s2t 2,1,20},AccuracyGoal->accugoal];
,Null];
AppendTo[levels,Chop[s2f]];
AppendTo[errors,Chop[Abs[gx2d[s2f]-v2t]]];
];

If[nlevel>=3,
s3f=Quiet[z/.FindRoot[gx2d[z]==v3t,{z,1.7,1.5,22},AccuracyGoal->accugoal]];
If[Abs[gx2d[s3f]-v3t]>10^-3,
s3f=Quiet[z/.FindRoot[gx2d[z]==v3t,{z,s3t .7,1.7,22},AccuracyGoal->accugoal]];
,Null];
If[Abs[gx2d[s3f]-v3t]>10^-3,
s3f=z/.FindRoot[gx2d[z]==v3t,{z,s3t .9,1.7,22},AccuracyGoal->accugoal];
,Null];
AppendTo[levels,Chop[s3f]];
AppendTo[errors,Chop[Abs[gx2d[s3f]-v3t]]];
];

If[nlevel>=4,
s4f=Quiet[z/.FindRoot[gx2d[z]==v4t,{z,s4t .7,8,40},AccuracyGoal->accugoal]];
If[Abs[gx2d[s4f]-v4t]>10^-3,
s4f=Quiet[z/.FindRoot[gx2d[z]==v4t,{z,s4t .9,8,40},AccuracyGoal->accugoal]];
,Null];
If[Abs[gx2d[s4f]-v4t]>10^-3,
s4f=z/.FindRoot[gx2d[z]==v4t,{z,s4t 1.1,8,40},AccuracyGoal->accugoal];
,Null];
AppendTo[levels,Chop[s4f]];
AppendTo[errors,Chop[Abs[gx2d[s4f]-v4t]]];
];

If[nlevel>=5,
s5f=Quiet[z/.FindRoot[gx2d[z]==v5t,{z,s5t 0.5,10,zmax},AccuracyGoal->accugoal]];
If[Abs[gx2d[s5f]-v5t]>10^-3,
s5f=Quiet[z/.FindRoot[gx2d[z]==v5t,{z,s5t .7,10,zmax},AccuracyGoal->accugoal]];
,Null];
If[Abs[gx2d[s5f]-v5t]>10^-3,
s5f=z/.FindRoot[gx2d[z]==v5t,{z,s5t 1.0,10,zmax},AccuracyGoal->accugoal];
,Null];
AppendTo[levels,Chop[s5f]];
AppendTo[errors,Chop[Abs[gx2d[s5f]-v5t]]];
];


(* diagnostic *)

If[testlev==1,
out1da=LogLogPlot[{-Log[1-lev2d[z]],-Log[1-gx2d[z]](*,-Log[1-g2d[z]]*)},{z,levels[[1]]/2,levels[[-1]]1.5},PlotPoints->20,MaxRecursion->2,Frame->True,PlotRange->All,PlotStyle->{Blue,{Red,Dashed}(*,{Black}*)},
GridLines->{Partition[Riffle[levels,Green,{2,-1,2}],2],Partition[Riffle[-Log[1-sigmas[[;;nlevel]]],Red,{2,-1,2}],2]},Axes->False,ImageSize->450,FrameStyle->20,
FrameLabel->{"\!\(\*SuperscriptBox[\(\[CapitalDelta]\[Chi]\), \(2\)]\) level","-Log[1-int. prob.]"}];
out1db=Show[ListPlot[logtab2d,Frame->True,PlotRange->All,FrameStyle->20,ImageSize->450,FrameLabel->{"Log z"},Axes->False],Plot[glog2d[z],{z,Log[zmin],Log[zmax]}]];
Print[GraphicsRow[{out1da,out1db}]];
];


(* summary table *)

outable=TableForm[Transpose[{ConstantArray[posc[[1]],nlevel],ConstantArray[posc[[2]],nlevel],gaulevels[[;;nlevel]],levels,errors}],TableHeadings->{{"1\[Sigma]","2\[Sigma]","3\[Sigma]","4\[Sigma]","5\[Sigma]"},{Row[{"x=",parnames[[plotnames[[1]]]]}],Row[{"y=",parnames[[plotnames[[2]]]]}],"Gaussian levels","Actual levels","error"}}];


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
