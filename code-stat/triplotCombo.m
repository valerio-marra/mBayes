(* ::Package:: *)

TriPlotCombo[tlabvec_]:=(

If[
NumericQ[dimF]
,
dcombo=dimF;
,
dcombo=Dimensions[chi2TabPar][[-1]]-1;
];

With[
{
	ca=ConstantArray[Null,{dcombo,dcombo}]
},
(*Graphics*)Grid[ReplacePart[ca,
	{
	{i_,i_}:>
		(
		unionrangex=MinMax@Flatten[AbsoluteOptions[potrix[#,i,i],PlotRange][[1,2,1]]&/@tlabvec];
		(*Print[{parnames[[i]],unionrangex}];*)
		Show[Sequence[potrix[#,i,i]&/@tlabvec],PlotRange->{unionrangex,All}]
		),
	{i_,j_}/;i>j:>
	(
		unionrangex=MinMax@Flatten[AbsoluteOptions[potrix[#,i,j],PlotRange][[1,2,1]]&/@tlabvec];
		unionrangey=MinMax@Flatten[AbsoluteOptions[potrix[#,i,j],PlotRange][[1,2,2]]&/@tlabvec];
		(*Print[{{parnames[[i]],parnames[[j]]},{unionrangex,unionrangey}}];*)
		Show[Sequence[potrix[#,i,j]&/@tlabvec],PlotRange->{unionrangex,unionrangey}]
	)
}],
Spacings->{0,0}(*,Dividers\[Rule]All*)(*,ImageSize\[Rule]1000*)]
]

);


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
