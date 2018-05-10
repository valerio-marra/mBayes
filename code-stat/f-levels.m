(* ::Package:: *)

(* summary table *)
tablex=TableForm[vecerror,TableHeadings->{{"1\[Sigma]","2\[Sigma]","3\[Sigma]","4\[Sigma]","5\[Sigma]"},{parnames[[plotname]],"\!\(\*SuperscriptBox[\(\[Sigma]\), \(-\)]\)","\!\(\*SuperscriptBox[\(\[Sigma]\), \(+\)]\)","\!\(\*SubscriptBox[\(x\), \(left\)]\)","\!\(\*SubscriptBox[\(x\), \(right\)]\)","gau \!\(\*SuperscriptBox[\(\[CapitalDelta]\[Chi]\), \(2\)]\)","actual \!\(\*SuperscriptBox[\(\[CapitalDelta]\[Chi]\), \(2\)]\)","check"}}];
outvec=vecerror[[All,;;3]];

(* summary plot *)
plikex=Plot[Likel[x],{x,xminPlot,xmaxPlot},Frame->True,Axes->False,PlotStyle->{color1,Thick},FrameStyle->18,FrameLabel->{parnames[[plotname]],"L"},PlotRange->All,ImageSize->450,GridLines->Lgrids,GridLinesStyle->Directive[Black,Dashed]];
pchi2x=Plot[chi2int[x],{x,xminPlot,xmaxPlot},Frame->True,Axes->False,PlotStyle->{color1,Thick},FrameStyle->18,FrameLabel->{parnames[[plotname]],"\[CapitalDelta]\!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\)=-2Log[L/\!\(\*SubscriptBox[\(L\), \(max\)]\)]"},PlotRange->All,ImageSize->450,GridLines->cgrids,GridLinesStyle->Directive[Black,Dashed]];
plotx=GraphicsRow[{pchi2x,plikex},Spacings->0];


(* warning *)
If[Min[vecerror[[All,4]]]==xMin||Max[vecerror[[All,5]]]==xMax,redege=1;wtext="!! WARNING !! The boundary of the parameter space has been reached !!";Print[wtext];(*Speak[wtext];*),redege=0;];


If[expout==1,
Export[ToFileName["results-analysis",rname<>"-run"<>ToString[run]<>"-sigmatab-par"<>ToString[plotname]<>".txt"],N[outvec],"Table","TableHeadings"->{"max","s-","s+             -             rows are 1, 2, 3, 4, 5 sigmas"}];
Export[ToFileName["results-analysis",rname<>"-run"<>ToString[run]<>"-plot1d-par"<>ToString[plotname]<>If[redege==0,".pdf","-ToBeRedone.pdf"]],plotx];
Quiet[DeleteFile[ToFileName["results-analysis",rname<>"-run"<>ToString[run]<>"-interp_fun-par"<>ToString[plotname]<>".txt"]]];
Save[ToFileName["results-analysis",rname<>"-run"<>ToString[run]<>"-interp_fun-par"<>ToString[plotname]<>".txt"],{Likel,EVI}];
];

If[summary==1,Print[tablex];];


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
