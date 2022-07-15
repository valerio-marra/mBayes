(* ::Package:: *)

(* sigmas c.l. *)

(* v[n_]:=Integrate[PDF[NormalDistribution[0,s],x],{x,-n s,n s},Assumptions->{s>0,n>0}]; *)
v[n_]=Erf[n/Sqrt[2]];

{v1t,v2t,v3t,v4t,v5t}=Table[v[i],{i,5}];
sigmas={v1t,v2t,v3t,v4t,v5t};
gausigmas=Range[5]^2;


(* gaussian levels for chi2 with 2 dof *)

levhi2[dof_,n_]:=2 InverseGammaRegularized[dof/2.,0,Erf[n/Sqrt[2]]];
(*
FindRoot[CDF[ChiSquareDistribution[2],x]==v[n],{x,3}]
*)

(*FindRoot[v[1]==1-E^(-n/2),{n,1}]*)

(*
gencovar={{sigmax^2,rho sigmax sigmay},{rho sigmax sigmay,sigmay^2}};
Assuming[{n>0,sigmax>0,sigmay>0,0\[LessEqual]rho<1},Simplify[Integrate[PDF[MultinormalDistribution[{0,0},gencovar],{x,y}],{x,y}\[Element]Ellipsoid[{0,0},n gencovar]]]]
*)

{s1t,s2t,s3t,s4t,s5t}=Table[levhi2[2,i],{i,5}];
gaulevels={s1t,s2t,s3t,s4t,s5t};

(* gaussian sigmas for chi with 2 dof *)
(*
FindRoot[CDF[ChiSquareDistribution[2],x^2]==v[n],{x,3,0,100}]
*)
{f1t,f2t,f3t,f4t,f5t}=gaulevels^(1/2);


(*parallelizable DiscretePlot*)
(*https://mathematica.stackexchange.com/q/45587/462*)

ClearAll[discretePlot];
SetAttributes[discretePlot,HoldAll];
discretePlot[args___]:=
    Block[{System`DiscretePlotDump`flatTable},            
        SetAttributes[System`DiscretePlotDump`flatTable,HoldFirst];
        System`DiscretePlotDump`flatTable[expr_,eval_,{var_},{vals_}]:=
            ParallelTable[expr,{var,vals}];
        DiscretePlot[args]
    ];


(* stuff *)

chi2cut=10^4;
L2cut=Exp[-chi2cut/2];

ACCU=1000; (* normal is 1000, 5000 for extra accuracy - for MaxStepFraction option in NDSolve *)
pp=8;rr=3;fst=15; (* plotting options *)
chip=10^-20;
Emerald=RGBColor[0/255,155/255,119/255];
TangerineTango=RGBColor[225/255,82/255,61/255];
Iris=RGBColor[91/255, 94/255, 166/255];
pronto:=If[(tempoo=Round[dtt/60])==1,Speak["Done! It took "<>ToString[tempoo]<>" minute."],Speak["Done! It took "<>ToString[tempoo]<>" minutes."]];
pronto2:=Speak["Master, it's ready!"];
pronto3:=EmitSound[Sound[{SoundNote["C",.2],SoundNote["G",.2],SoundNote["C5",.2],SoundNote["G",.2]}]];

nneat[x_]:=NumberForm[x,2,ExponentFunction->(If[-2<#<2,Null,#]&)]; (* neat numbers *)

(* unflatten[file,{n1,n2,n3,...}] *)
(*unflatten[e_,{d__?((IntegerQ[#]&&Positive[#])&)}]:= Fold[Partition,e,Take[{d},{-1,2,-1}]] /;(Length[e]===Times[d]);*)
(*ArrayReshape[{a,b,c,d,e,f},{2,3}]*)


(* from https://gist.githubusercontent.com/lshifr/56c6fcfe7cafcd73bdf8/raw/LazyTuples.m *)

ClearAll[next];
next[{left_,_},dim_]:={left-dim*(#-1),#}&[IntegerPart[(left-1)/dim]+1];

ClearAll[multiDims];
multiDims[dims_]:=Rest@Reverse@FoldList[Times,1,Reverse@dims];

ClearAll[multiIndex];
multiIndex[pos_,dims:{__Integer}]:=Rest@FoldList[next,{pos,0},multiDims@dims][[All,2]]

ClearAll[take];
take[lists:{__List},{start_,end_}]:=With[{rend=Min[end,Times@@Map[Length,lists]]},Transpose@MapThread[Part,{lists,multiIndex[Range[start,rend],Length/@lists]}]];


(*https://mathematica.stackexchange.com/questions/22536/fullsimplify-over-reals-automatically*)
fullSimplifyReals[x_]:=Assuming[_Symbol\[Element]Reals,FullSimplify@x];
SimplifyReals[x_]:=Assuming[_Symbol\[Element]Reals,Simplify@x];


showpal:=
If[
cs==0,
Do[cs=i;Get["code-stat/colors.m"];Print[palette];,{i,1,5}];
,
Get["code-stat/colors.m"];
];


multiRiffle[x:_List..]:=Module[{i=1},Fold[Riffle[##,{++i,-1,i}]&,{x}]];


(* monitoring tools for parallel computations *)

SetAttributes[monitorParallelTable,HoldAll];

monitorParallelTable[expr_,iter__List,updatethreshold_]:=
Module[
	{counter=1,thresh=updatethreshold}
,
	SetSharedVariable[counter];
	ParallelEvaluate[localcounter=1;];
	Monitor[ParallelTable[If[localcounter>=thresh,counter=counter+localcounter;localcounter=1,localcounter++];expr,iter],counter]
];


(* this one uses "functions" *)
(*pTab[q_]:=Table[pMin[q]+(pMax[q]-pMin[q]) If[ngrid[q]==1,0,(i-1)/(ngrid[q]-1)],{i,1,ngrid[q]}];*)

(* this one uses "tables" *)
pTabX[q_,pMin_,pMax_,ngrid_]:=Table[pMin[[q]]+(pMax[[q]]-pMin[[q]]) If[ngrid[[q]]==1,0,(i-1)/(ngrid[[q]]-1)],{i,1,ngrid[[q]]}];


multiRiffle[x:_List..]:=Module[{i=1},Fold[Riffle[##,{++i,-1,i}]&,{x}]];


(* n non-constant bins for a list of n elements *)
nonconstantBX[list_]:=(
auxx=Abs[Differences[list]];
Clear[dbinsB];
dbinsB={auxx[[1]]}; (* first bin *)
Do[
	AppendTo[dbinsB,(auxx[[i]]+auxx[[i+1]])/2]
,
	{i,1,Length[auxx]-1}
];
AppendTo[dbinsB,auxx[[-1]]]; (* last bin *)
dbinsB
);


(*stepgrid[i_]:=1.(pSet[[i,2]]-pSet[[i,1]])/(ngrid[[i]]-1);*)

(*stepgridx[i_]:=(
zeros=ConstantArray[0,pardim];
zeros1=zeros;zeros1[[i]]=1;
Flatten[Differences[chi2TabPar,zeros1],pardim-1][[1,i]]
);*)


(* it does not use bin width; cannot be used for evidence evaluations *)
If[
$VersionNumber<11.3
,

marginalizeTab[pp_, tabin_] := 
Module[
	{tab, newtab, id, d = Dimensions[tabin][[-1]] - 1}
	,
	tab=Developer`ToPackedArray[tabin];
	id = ConstantArray[1, d];
	id[[pp]] = All;
	newtab = Transpose[tab[[Sequence @@ id, {Sequence @@ pp, -1}]], Ordering[pp]];
	With[
		{nn = Sequence @@ Table[Unique["n"], {Length[pp]}]}
		,
		With[
		{lim = Sequence @@ Transpose@{{nn}, Dimensions[tab][[pp]]}
		,
		mm = (id = ConstantArray[All, d]; id[[pp]] = {nn}; Sequence @@ id)},
		Do[newtab[[nn, -1]] = -2 Log[Total[Exp[-tab[[mm, -1]]/2], Infinity]];, lim];
		];
	];
	newtab
];

,

(* with SetPrecision \[Rule] 11.3 nastiness *)
marginalizeTab[pp_, tabin_] := 
Module[
	{tab, newtab, id, d = Dimensions[tabin][[-1]] - 1}
	,
	tab=Developer`ToPackedArray[tabin];
	id = ConstantArray[1, d];
	id[[pp]] = All;
	newtab = Transpose[tab[[Sequence @@ id, {Sequence @@ pp, -1}]], Ordering[pp]];
	With[
		{nn = Sequence @@ Table[Unique["n"], {Length[pp]}]}
		,
		With[
		{lim = Sequence @@ Transpose@{{nn}, Dimensions[tab][[pp]]}
		,
		mm = (id = ConstantArray[All, d]; id[[pp]] = {nn}; Sequence @@ id)},
		Do[newtab[[nn, -1]] = -2 Log[Total[Exp[SetPrecision[-tab[[mm, -1]]/2,$MachinePrecision]], Infinity]];, lim];
		];
	];
	newtab
];

];


(* experimenting... *)

(* with Rationalize \[Rule] 11.3 nastiness - it does not use bin width; cannot be used for evidence evaluations *)
marginalizeTabRatio[pp_, tabo_] := 
Module[
	{tab,newtab, id, d = Dimensions[tabo][[-1]] - 1}
	,
	id = ConstantArray[1, d];
	id[[pp]] = All;
	tab = Rationalize[tabo,0];
	newtab = Transpose[tab[[Sequence @@ id, {Sequence @@ pp, -1}]], Ordering[pp]];
	With[
		{nn = Sequence @@ Table[Unique["n"], {Length[pp]}]}
		,
		With[
		{lim = Sequence @@ Transpose@{{nn}, Dimensions[tab][[pp]]}
		,
		mm = (id = ConstantArray[All, d]; id[[pp]] = {nn}; Sequence @@ id)},
		Do[newtab[[nn, -1]] = -2 Log[N[Total[Exp[-tab[[mm, -1]]/2], Infinity],$MachinePrecision]];, lim];
		];
	];
	newtab
];


(* this one is valid for non uniform bins, it is a little slower *)
(* also, it can be used for evidence evaluations *)
marginalizeTabX[pp_, tab_] := 
Module[
	{newtab, id, d = Dimensions[tab][[-1]] - 1,parTabDiff,idmar,idx,parTensDiff}
	,
	id = ConstantArray[1, d];
	id[[pp]] = All;
	
	(* this is to create the bin step tensor parTensDiff *)
	parTabDiff=ConstantArray[1, d-Length[pp]];
	idmar=Delete[Range[d],Partition[pp,1]];
	Do[
		idx = ConstantArray[1, d];
		idx[[idmar[[k]]]]=All;
		parTabDiff[[k]]=nonconstantBX[tab[[Sequence @@ idx, idmar[[k]]]]];
	,{k,d-Length[pp]}];
	parTensDiff=TensorProduct[Sequence @@ parTabDiff];
	
	newtab = Transpose[tab[[Sequence @@ id, {Sequence @@ pp, -1}]], Ordering[pp]];
	With[
		{nn = Sequence @@ Table[Unique["n"], {Length[pp]}]}
		,
		With[
		{lim = Sequence @@ Transpose@{{nn}, Dimensions[tab][[pp]]}
		,
		mm = (id = ConstantArray[All, d]; id[[pp]] = {nn}; Sequence @@ id)},
		Do[newtab[[nn, -1]] = -2 Log[Total[parTensDiff Exp[-tab[[mm, -1]]/2], Infinity]];, lim];
		];
	];
	newtab
];

(* given a chi2 table it returns the evidence, assuming that chi2 = -2 Log[prior x likelihood] *)
evidence[tabin_]:=
Module[
	{d = Dimensions[tabin][[-1]] - 1,tab, idx, parTabDiff, parTensDiff,evid}
	,
	tab=Developer`ToPackedArray[tabin];
	parTabDiff=ConstantArray[1, d];
	Do[
		idx = ConstantArray[1, d];
		idx[[k]]=All;
		parTabDiff[[k]]=nonconstantBX[tab[[Sequence @@ idx, k]]];
	,{k,d}];
	parTensDiff=TensorProduct[Sequence @@ parTabDiff];

	evid=Total[parTensDiff Exp[-tab[[Sequence @@ ConstantArray[All, d], -1]]/2], Infinity];
	Log[evid]
];


(* calculating the exact evidence, but integrating only within an ellipsoid (from Fisher) with "level" sigmas *)

NEvidence[bestfit_,fisher_,level_]:=Module[
{center,matrix,cov,dspace,eva,eve},
center=bestfit;matrix=fisher;
cov=Inverse[matrix];
dspace=Length[center];
eva=Sqrt[Eigenvalues[cov]];
eve=Eigenvectors[cov];

(*alevel=(x/.FindRoot[CDF[ChiSquareDistribution[dspace],x]==v[level],{x,10}])^.5;*)
alevel=levhi2[dspace,level]^(1/2);

(* mapping the ellipsoid to the unit sphere *)
rescaling=Composition[TranslationTransform[center],RotationTransform[{UnitVector[dspace,1],eve[[1]]}],ScalingTransform[eva]];
intvec[vec_]:=Exp[-chi2[vec]/2];
integrand=Composition[intvec,rescaling];
ve=Array[vr,dspace];(* dummy variables *)
jac=fullSimplifyReals[CoordinateTransformData[{"Hyperspherical"->"Cartesian",dspace},"MappingJacobianDeterminant"][ve]];
tra=FromPolarCoordinates[ve];
polarIntegrand=integrand[tra] jac;
(*polarIntegrandN[vec_/;VectorQ[vec,NumberQ[#]&&!MatchQ[#,_Complex]&]]=polarIntegrand/.Table[ve[[i]]\[Rule]vec[[i]],{i,dspace}];*) (* forcing a numerical vector *)

(* polar ranges *)
pora=CoordinateChartData["Hyperspherical","CoordinateRangeAssumptions",ve];
pora1=Table[{pora[[i,1]],pora[[i,-1]]},{i,2,dspace}];
pora2=PrependTo[pora1,{0,alevel}];

inra=Sequence@@Table[Flatten[{ve[[i]],pora2[[i]]}],{i,dspace}];
NIntegrate[polarIntegrand,Evaluate@inra,Method->{Automatic,"SymbolicProcessing"->0},AccuracyGoal->5] Times@@eva
(*aux2=NIntegrate[polarIntegrandN[ve],Evaluate@inra,Method->{Automatic,"SymbolicProcessing"\[Rule]0},AccuracyGoal\[Rule]5] Times@@eva;*)

(* test function *)
(*chi2x[vec_?VectorQ]:=(vec[[1]]-1)^2+(vec[[2]]-.7)^2+(vec[[3]]-.5)^2;
intvec[vec_]:=Exp[-chi2x[vec]/2];*)

(* integration region - alternative approach *)
(*intreg=Region[Parallelepiped[pora2[[All,1]],Table[UnitVector[dspace,i](pora2[[i,2]]-pora2[[i,1]]),{i,dspace}]]];*)
(* crashes if i use "SymbolicProcessing"\[Rule]0 ... *)
(*aux3=NIntegrate[polarIntegrand,ve\[Element]intreg,AccuracyGoal\[Rule]5] Times@@eva;*)
(*aux4=NIntegrate[polarIntegrandN[ve],ve\[Element]intreg,AccuracyGoal\[Rule]5] Times@@eva;*)
];


ellipsgen[i_,j_,n_,col_,mmean_,covamat_]:=Graphics[{Opacity[0.3],col,EdgeForm[{Thin,col}](*,Transparent*),Ellipsoid[mmean[[{i,j}]],gaulevels[[n]] covamat[[{i,j},{i,j}]]]}];


(* tests to restrict the exploration of the grid within an ellipsoid *)

gtest[vec_,level_,mat_,bf_]:=(vec-bf).mat.(vec-bf)<level^2;
btest[vec_,level_,mat_,bf_]:=Boole[gtest[vec,level,mat,bf]];


(* 2d level routine *)
blev:=(
If[nlevel>0
,
Get["code-stat/b-levels.m"];
,
levels=gaulevels[[;;-nlevel]];
outable="Gaussian is good...";
];
If[summary==1,Print[outable];];
);


boostres[tab_,points_,order_]:=
(
faux=Interpolation[tab,InterpolationOrder->order];
{mi1,ma1}=MinMax[tab[[All,1]]];
Table[{x,faux[x]},{x,mi1,ma1,(ma1-mi1)/(points-1)}]
);


(* 1d level routine *)
clev:=
If[isig!=0,
Which[
method==1,Get["code-stat/d-levels.m"];, (* based on table for levels and based on FindRoot on chi2 for x determination *)
method==2,Get["code-stat/e-levels.m"];(* based on FindRoot for levels and based on FindRoot on chi2 for x determination *)
];

Ltgrids={Flatten[vecerror[[All,{4,5}]]],maxY Exp[-vecerror[[All,7]]/2]};

xgrids=Join[{maxX},vecerror[[All,4]],vecerror[[All,5]]];
Lygrids=Join[{maxY},maxY Exp[-vecerror[[All,7]]/2]];
cygrids=Join[{0},vecerror[[All,7]]];

gridcolor=Table[Blend[{Blue,Red},x],{x,0,1,If[Abs[isig]==1,2,1/(Abs[isig]-1)]}];
xgrids=Join[{{maxX,Black}},Partition[Riffle[vecerror[[All,4]],gridcolor],2],Partition[Riffle[vecerror[[All,5]],gridcolor],2]];
Lygrids=Join[{{maxY,Black}},Partition[Riffle[maxY Exp[-vecerror[[All,7]]/2],gridcolor],2]];
cygrids=Join[{{0,Black}},Partition[Riffle[vecerror[[All,7]],gridcolor],2]];

Lgrids={xgrids,Lygrids};
cgrids={xgrids,cygrids};

{xminPlot,xmaxPlot}={Min[vecerror[[All,4]]],Max[vecerror[[All,5]]]};
,
vecerror={};
Lgrids=None;
Ltgrids=None;
cgrids=None;
{xminPlot,xmaxPlot}={xMin,xMax};
];


plot1d[chi2Tabtemp_,parname_,intord_]:=
(
chi2Tab=chi2Tabtemp;
If[boost>0,chi2Tab=boostres[chi2Tab,bpoints,boost]];
minchi2ma=Min[chi2Tab[[All,2]]];
chi2Tabx=chi2Tab;chi2Tabx[[All,2]]=chi2Tabx[[All,2]]-minchi2ma;
pchisq=ListPlot[chi2Tabx,Frame->True,Axes->False,Joined->True,PlotStyle->{color1,Thick},InterpolationOrder->intord,FrameStyle->15,FrameLabel->{parname,"-2Log L/\!\(\*SubscriptBox[\(L\), \(max\)]\)"},GridLines->{None,gausigmas[[;;3]]},PlotRange->{Full,{-.05,11}}];
LTabx=chi2Tabx;LTabx[[All,2]]=Exp[-chi2Tabx[[All,2]]/2];
plike=ListPlot[LTabx,Frame->True,Axes->False,Joined->True,PlotStyle->{color1,Thick},InterpolationOrder->intord,FrameStyle->15,FrameLabel->{parname,"L/\!\(\*SubscriptBox[\(L\), \(max\)]\)"},PlotRange->{All,{-.05,1.05}}];
);


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
