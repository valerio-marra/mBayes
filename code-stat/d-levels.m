(* ::Package:: *)

(* aux tables *)

(*ngridx=Length[chi2Tabx];*)
ngridx=pSet[[plotname,3]];
chi2Taby=chi2Tabx[[All,2]];
LTaby=LTabx[[All,2]];
parTabDiff=nonconstantBX[chi2Tabx[[All,1]]];
LAuxInt=Total[parTabDiff LTabx[[All,2]],2]; (* normalization *)


(* best-fit model from table *)

pos=Position[Chop[chi2Tabx[[All,2]]],0][[1]];
xbf=chi2Tabx[[pos,1]][[1]];


(* level estimator *)

lev1d[z_?NumericQ]:=Catch[Do[{Lsigma=0,Do[If[chi2Taby[[i]]<z,Lsigma+=parTabDiff[[i]]LTaby[[i]]],{i,1,ngridx}],Throw[Lsigma/LAuxInt]},{1}]];


(* smoothing via fit *)

zmin=.01;zmax=50;nzstep=100;
(* linear space fit *)
(*g1d=Interpolation[Partition[Riffle[Range[zmin,zmax,(zmax-zmin)/nzstep],Map[lev1d,Range[zmin,zmax,(zmax-zmin)/nzstep]]],2],InterpolationOrder->2];*)
(* superlog space fit *)
logtab1d=Table[{logz,Log[-Log[1-lev1d[Exp[logz]]]]},{logz,Log[zmin],Log[zmax],(Log[zmax]-Log[zmin])/nzstep}];
(*glog1d=Interpolation[logtab1d,InterpolationOrder->2];*)
glog1dx=Fit[logtab1d, {1,x,x^2,x^3,x^4,x^5,x^6,x^7}, x];
glog1d[z_]=glog1dx/.x->z;
gx1d[z_]:=1-Exp[-Exp[glog1d[Log[z]]]];


(* 1d levels *)

sfx[i_]:=Quiet[z/.FindRoot[gx1d[z]==sigmas[[i]],{z,gausigmas[[i]],Max[zmin,gausigmas[[i]]/10],Min[zmax,gausigmas[[i]]10]},AccuracyGoal->accugoal]];
If[isig>0,
levels1d=Table[sfx[i],{i,1,isig}];
,
levels1d=gausigmas[[;;-isig]];
];


(* diagnostic *)

If[testlev==1,
out1da=LogLogPlot[{-Log[1-lev1d[z]],-Log[1-gx1d[z]](*,-Log[1-g1d[z]]*)},{z,levels1d[[1]]/2,levels1d[[-1]]1.5},Frame->True,PlotRange->All,PlotStyle->{Blue,{Red,Dashed}(*,{Black}*)},
GridLines->{Partition[Riffle[levels1d,Green,{2,-1,2}],2],Partition[Riffle[-Log[1-sigmas],Red,{2,-1,2}],2]},Axes->False,ImageSize->450,FrameStyle->20,
FrameLabel->{"\!\(\*SuperscriptBox[\(\[CapitalDelta]\[Chi]\), \(2\)]\) level","-Log[1-int. prob.]"}];
out1db=Show[ListPlot[logtab1d,Frame->True,PlotRange->All,FrameStyle->20,ImageSize->450,FrameLabel->{"Log z"},Axes->False],Plot[glog1d[z],{z,Log[zmin],Log[zmax]}]];
Print[GraphicsRow[{out1da,out1db}]];
];


(* doesnt really visualize 4-5 sigma levels *)
(*LogLogPlot[{lev1d[z],gx1d[z](*,g1d[z]*)},{z,levels1d[[1]]/2,levels1d[[-1]]1.5},Frame->True,PlotRange->All,PlotStyle->{Blue,{Red,Dashed}(*,{Black}*)},
GridLines->{Partition[Riffle[levels1d,Green,{2,-1,2}],2],Partition[Riffle[sigmas,Red,{2,-1,2}],2]},Axes->False,ImageSize->450,FrameStyle->20,
FrameLabel->{"\!\(\*SuperscriptBox[\(\[CapitalDelta]\[Chi]\), \(2\)]\) level","integrated probability"}]*)


(* finding mim chi2 from the interpolating function *)

(*minXtab=Sort[chi2Tabx,#1[[2]]<#2[[2]]&][[1,1]];*)
minXtab=xbf;
epsmin=0.05;
minP=FindMaximum[Likel[x],{x,minXtab,minXtab (1-Sign[minXtab]epsmin),minXtab (1+Sign[minXtab]epsmin)},AccuracyGoal->accugoal];
{maxX,maxY}={x/.minP[[2]],minP[[1]]};
Abs[minXtab-maxX];


(* finding xL and xR relative to levels1d *)

vecerror={};
Do[
xL=x/.FindRoot[chi2int[x]==levels1d[[ii]],{x,(xMin+2 maxX)/3,xMin,maxX },AccuracyGoal->accugoal(*,MaxIterations\[Rule]50*)];
xR=x/.FindRoot[chi2int[x]==levels1d[[ii]],{x,(xMax+2 maxX)/3,maxX,xMax},AccuracyGoal->accugoal(*,MaxIterations\[Rule]50*)];
{xLlevc,xRlevc}={xL,xR};
vecLix=Flatten[{maxX,{xLlevc,xRlevc}-maxX}];
check=Quiet[NIntegrate[Likel[x],{x,xLlevc,xRlevc}(*,Method->{Automatic,"SymbolicProcessing"->0}*),AccuracyGoal->accugoal]]-sigmas[[ii]];
AppendTo[vecerror,Join[vecLix,{xLlevc,xRlevc,gausigmas[[ii]],levels1d[[ii]],Chop[check]}]];
,{ii,1,Abs[isig]}];


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
