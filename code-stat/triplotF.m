(* ::Package:: *)

(* to check the new Ellipsoid features *)
(*Needs["MultivariateStatistics`"];
style[opts__,x_Ellipsoid]:=Graphics[x]/.Line[y_]\[RuleDelayed]{opts,Polygon[y]}//First;
ellipsn[i_,j_,n_]:=Graphics[style[Opacity@0,EdgeForm[Directive[(*Thick,*)colorF]],Ellipsoid[vecmean[[{i,j}]],gaulevels[[n]]^.5 Sqrt[Eigenvalues[covmat[[{i,j},{i,j}]]]], Eigenvectors[covmat[[{i,j},{i,j}]]]]]];*)


(*eva=Sqrt[Eigenvalues[covmat]];eve=Eigenvectors[covmat];
angle=ArcTan[eve[[1,2]]/eve[[1,1]]];
eva[[1]]Cos[angle]
ax1=vecmean[[1]]-Intlevel eva[[1]]Cos[angle];
ax2=vecmean[[1]]+Intlevel eva[[1]]Cos[angle];
ay1=vecmean[[2]]-Intlevel eva[[1]]Sin[angle];ay2=vecmean[[2]]+Intlevel eva[[1]]Sin[angle];*)


TriPlotF[tlabel_,type_,mat_,vecmean_,parlabels_,nlevel_,style_]:=(
If[type==0,covmat=mat,covmat=Inverse[mat]];

dimF=Length[vecmean];
textsize=16; (* labels *)
textsize2=10; (* axes *)
custTicksX=Table[Automatic,dimF];
(*custTicksX={{.28,.29,.3},{0,.02,.04,.06},{.79,.8,.81}};*)
custTicksY=custTicksX[[2;;]];

arato=1;
framesize=180;

padlabelx=50;
padlabely=62;
padnolabelx=25; (* controls x-gap size *)
padnolabely=25; (* controls y-gap size *)
padnonumbers=8;

nlevelR=1.1 gaulevels[[nlevel]]^.5;

nada=Text[Style[" ",FontSize->textsize]];
parlabelsx=(Text[Style[#,FontSize->textsize]]&/@(parlabels));

seq:=Sequence;
ellipsn[i_,j_,n_]:=Graphics[{EdgeForm[style],Transparent,Ellipsoid[vecmean[[{i,j}]],gaulevels[[n]] Normal@Symmetrize[covmat[[{i,j},{i,j}]]]]}];

With[
{
	ca=ConstantArray[Null,{dimF,dimF}],
	opts=Sequence[AspectRatio->arato,Frame->True(*,PlotRange->Full*),FrameStyle->textsize2,ImageMargins->0]
},
(*Graphics*)Grid[ReplacePart[ca,
	{
	{i_,i_}:>
		(
		potrix[tlabel,i,i]=Plot[PDF[NormalDistribution[vecmean[[i]],covmat[[i,i]]^.5],x],{x,vecmean[[i]]-nlevelR covmat[[i,i]]^.5,vecmean[[i]]+nlevelR covmat[[i,i]]^.5},opts,Axes->False,(*Filling->Axis,*)PlotStyle->style,PlotRange->{{vecmean[[i]]-nlevelR covmat[[i,i]]^.5,vecmean[[i]]+nlevelR covmat[[i,i]]^.5},All},PlotRangePadding->None,
		FrameTicks->{{None,None},{custTicksX[[i]],None}},
			Evaluate@Which[
			i==1,seq[FrameLabel->{{nada,None},{None,None}},ImagePadding->{{padlabely,padnonumbers},{padnolabelx,padnonumbers}},ImageSize->{framesize+padlabely+padnonumbers,framesize+padnolabelx+padnonumbers}],
			i==dimF,seq[FrameLabel->{{None,None},{parlabelsx[[i]],None}},ImagePadding->{{padnolabely,padnonumbers},{padlabelx,padnonumbers}},ImageSize->{framesize+padnolabely+padnonumbers,framesize+padlabelx+padnonumbers}],
			1<i<dimF,seq[FrameLabel->{None,None},ImagePadding->{{padnolabely,padnonumbers},{padnolabelx,padnonumbers}},ImageSize->{framesize+padnolabely+padnonumbers,framesize+padnolabelx+padnonumbers}]]];
		potrix[tlabel,i,i]
		),
	{i_,j_}/;i>j:>
	(
		potrix[tlabel,i,j]=Show[Table[ellipsn[j,i,n],{n,nlevel}],RotateLabel->True,opts,Axes->False,PlotRange->{{vecmean[[j]]-nlevelR covmat[[j,j]]^.5,vecmean[[j]]+nlevelR covmat[[j,j]]^.5},{vecmean[[i]]-nlevelR covmat[[i,i]]^.5,vecmean[[i]]+nlevelR covmat[[i,i]]^.5}},PlotRangePadding->None,
		Evaluate@Which[
			i==dimF&&j>1,seq[FrameLabel->{{None,None},{parlabelsx[[j]],None}},ImagePadding->{{padnolabely,padnonumbers},{padlabelx,padnonumbers}},ImageSize->{framesize+padnolabely+padnonumbers,framesize+padlabelx+padnonumbers}],
			i==dimF&&j==1,seq[FrameLabel->{{parlabelsx[[i]],None},{parlabelsx[[j]],None}},ImagePadding->{{padlabely,padnonumbers},{padlabelx,padnonumbers}},ImageSize->{framesize+padlabely+padnonumbers,framesize+padlabelx+padnonumbers}],
			i>1&&j==1,seq[FrameLabel->{{parlabelsx[[i]],None},{None,None}},ImagePadding->{{padlabely,padnonumbers},{padnolabelx,padnonumbers}},ImageSize->{framesize+padlabely+padnonumbers,framesize+padnolabelx+padnonumbers}],
			i>j,seq[FrameLabel->{None,None},ImagePadding->{{padnolabely,padnonumbers},{padnolabelx,padnonumbers}},ImageSize->{framesize+padnolabely+padnonumbers,framesize+padnolabelx+padnonumbers}]
			]];
		potrix[tlabel,i,j]
	)
}],
Spacings->{0,0}(*,Dividers\[Rule]All*)(*,ImageSize\[Rule]1000*)]
]
);


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
