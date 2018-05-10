(* ::Package:: *)

TriPlotP[tlabel_,tab_,parlabels_,nlevelx_,style1d_,style2d_]:=(

pardimtri=Length[parlabels];
pSetri[tlabel]=CoordinateBounds[chi2TabPar][[;;-2]];

intord=1; (* interpolation order *)

(* triplot options *)
textsize=16; (* labels *)
textsize2=10; (* axes *)
custTicksX=Table[Automatic,pardimtri];
(*custTicksX={{.28,.29,.3},{0,.02,.04,.06},{.79,.8,.81}};*)
custTicksY=custTicksX[[2;;]];

(* 1d level options *)
isig=0; (* -n for n-sigma gaussian levels, n for n-sigma integrated levels; max n=5; 0 to skip levels *)
chi2IO=2; (* interpolation order for chi2; 2 should be used unless the resolution is low and spourious features appear; in this case use 1 *)
method=2; (* method for calculating the sigmas; 2 should be better than 1 *)
noisy=0; (* only for method 2 *)
testlev=0; (* 1 to troubleshoot the likelihood integration, only for method 2 *)

(* 2d level options *)
nlevel=nlevelx; (* -n for n-sigma gaussian levels, n for n-sigma integrated levels; max n=5 *)
testlev=0;(* 1 to troubleshoot the likelihood integration *)
summary=0; (* 1 to print a summary table *)

arato=1;
framesize=180;

padlabelx=50;
padlabely=62;
padnolabelx=25; (* controls x-gap size *)
padnolabely=25; (* controls y-gap size *)
padnonumbers=8;

nada=Text[Style[" ",FontSize->textsize]];
parnamesx=(Text[Style[#,FontSize->textsize]]&/@(parlabels));

seq:=Sequence;

With[
{
	dt=tab,
	ca=ConstantArray[Null,{pardimtri,pardimtri}],
	opts=Sequence[AspectRatio->arato,Frame->True(*,PlotRange->Full*),FrameStyle->textsize2,ImageMargins->0]
},
(*Graphics*)Grid[ReplacePart[ca,
	{
	{i_,i_}:>
		({xminp,xmaxp}={pSetri[tlabel][[i,1]],pSetri[tlabel][[i,2]]};
		plotname=i;
		chi2Tab=marginalizeTab[{plotname},dt];
		plot1d[chi2Tab,parnamesx[[plotname]],1];
		Get["code-stat/c-levels.m"];
		clev;
		potrix[tlabel,i,i]=Plot[Likel[x],{x,xminp,xmaxp},opts,Axes->False,Filling->style1d[[1]],PlotStyle->style1d[[2]],PlotRange->{{xminp,xmaxp},All},PlotRangePadding->None,
		FrameTicks->{{None,None},{custTicksX[[i]],None}},
			Evaluate@Which[
			i==1,seq[FrameLabel->{{nada,None},{None,None}},ImagePadding->{{padlabely,padnonumbers},{padnolabelx,padnonumbers}},ImageSize->{framesize+padlabely+padnonumbers,framesize+padnolabelx+padnonumbers}],
			i==pardimtri,seq[FrameLabel->{{None,None},{parnamesx[[i]],None}},ImagePadding->{{padnolabely,padnonumbers},{padlabelx,padnonumbers}},ImageSize->{framesize+padnolabely+padnonumbers,framesize+padlabelx+padnonumbers}],
			1<i<pardimtri,seq[FrameLabel->{None,None},ImagePadding->{{padnolabely,padnonumbers},{padnolabelx,padnonumbers}},ImageSize->{framesize+padnolabely+padnonumbers,framesize+padnolabelx+padnonumbers}]],
			GridLines->Ltgrids,GridLinesStyle->Directive[Black,Dashed]];
		potrix[tlabel,i,i]
		),
	{i_,j_}/;i>j:>
	(
		{xminp,xmaxp}={pSetri[tlabel][[j,1]],pSetri[tlabel][[j,2]]};
		{yminp,ymaxp}={pSetri[tlabel][[i,1]],pSetri[tlabel][[i,2]]};
		If[pardimtri>2,chi2Tab=marginalizeTab[{j,i},dt];,chi2Tab=dt;];
		Get["code-stat/a-levels.m"];
		plotnames={j,i};
		blev;
		potrix[tlabel,i,j]=ListContourPlot[dchi2TabF,Contours->levels,RotateLabel->True,ContourShading->style2d[[1]],ContourStyle->style2d[[2]],opts,InterpolationOrder->intord(*,Epilog->Inset[Graphics[{Black,Rectangle[]}],posc,Center,Scaled[.03]]*),Axes->False,
		PlotRange->{{xminp,xmaxp},{yminp,ymaxp}},FrameTicks->{{custTicksY[[i-1]],None},{custTicksX[[j]],None}},PlotRangePadding->None,
		Evaluate@Which[
			i==pardimtri&&j>1,seq[FrameLabel->{{None,None},{parnamesx[[j]],None}},ImagePadding->{{padnolabely,padnonumbers},{padlabelx,padnonumbers}},ImageSize->{framesize+padnolabely+padnonumbers,framesize+padlabelx+padnonumbers}],
			i==pardimtri&&j==1,seq[FrameLabel->{{parnamesx[[i]],None},{parnamesx[[j]],None}},ImagePadding->{{padlabely,padnonumbers},{padlabelx,padnonumbers}},ImageSize->{framesize+padlabely+padnonumbers,framesize+padlabelx+padnonumbers}],
			i>1&&j==1,seq[FrameLabel->{{parnamesx[[i]],None},{None,None}},ImagePadding->{{padlabely,padnonumbers},{padnolabelx,padnonumbers}},ImageSize->{framesize+padlabely+padnonumbers,framesize+padnolabelx+padnonumbers}],
			i>j,seq[FrameLabel->{None,None},ImagePadding->{{padnolabely,padnonumbers},{padnolabelx,padnonumbers}},ImageSize->{framesize+padnolabely+padnonumbers,framesize+padnolabelx+padnonumbers}]
			]];
		potrix[tlabel,i,j]
	)
}],
Spacings->{0,0}(*,Dividers\[Rule]All*)(*,ImageSize\[Rule]1000*)]
]

);


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
