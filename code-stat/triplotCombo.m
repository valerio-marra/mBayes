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
		unionrangex=MinMax@@IntervalUnion@@(Interval@AbsoluteOptions[potrix[#,i,i],PlotRange][[1,2,1]]&/@tlabvec);
		unionrangey=MinMax@@IntervalUnion@@(Interval@AbsoluteOptions[potrix[#,i,i],PlotRange][[1,2,2]]&/@tlabvec);
		Show[Sequence[potrix[#,i,i]&/@tlabvec],PlotRange->{unionrangex,unionrangey}]
		),
	{i_,j_}/;i>j:>
	(
		unionrangex=MinMax@@IntervalUnion@@(Interval@AbsoluteOptions[potrix[#,i,j],PlotRange][[1,2,1]]&/@tlabvec);
		unionrangey=MinMax@@IntervalUnion@@(Interval@AbsoluteOptions[potrix[#,i,j],PlotRange][[1,2,2]]&/@tlabvec);
		Show[Sequence[potrix[#,i,j]&/@tlabvec],PlotRange->{unionrangex,unionrangey}]
	)
}],
Spacings->{0,0}(*,Dividers\[Rule]All*)(*,ImageSize\[Rule]1000*)]
]

);


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
