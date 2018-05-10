(* ::Package:: *)

Which[
cs==1,
color1=RGBColor[0/255,109/255,44/255];
color2=RGBColor[49/255,163/255,84/255];
color3=RGBColor[116/255,196/255,118/255];
color4=RGBColor[186/255,228/255,179/255];
color5=RGBColor[237/255,248/255,233/255];
color6=Transparent;
colors={color1,color2,color3,color4,color5,color6};
,
cs==2,
color1=RGBColor[61/255,82/255,161/255];
color2=RGBColor[58/255,137/255,201/255];
color3=RGBColor[119/255,183/255,229/255];
color4=RGBColor[180/255,221/255,247/255];
color5=RGBColor[230/255,245/255,254/255];
color6=Transparent;
colors={color1,color2,color3,color4,color5,color6};
,
cs==3,
color1=RGBColor[179/255,0/255,0/255];
color2=RGBColor[227/255,74/255,51/255];
color3=RGBColor[252/255,141/255,89/255];
color4=RGBColor[253/255,204/255,138/255];
color5=RGBColor[254/255,240/255,217/255];
color6=Transparent;
colors={color1,color2,color3,color4,color5,color6};
,
cs==4,
color1=RGBColor[129/255,15/255,124/255];
color2=RGBColor[136/255,86/255,167/255];
color3=RGBColor[140/255,150/255,198/255];
color4=RGBColor[179/255,205/255,227/255];
color5=RGBColor[237/255,248/255,251/255];
color6=Transparent;
colors={color1,color2,color3,color4,color5,color6};
,
cs==5,
color1=RGBColor[37/255,37/255,37/255];
color2=RGBColor[99/255,99/255,99/255];
color3=RGBColor[150/255,150/255,150/255];
color4=RGBColor[204/255,204/255,204/255];
color5=RGBColor[247/255,247/255,247/255];
color6=Transparent;
colors={color1,color2,color3,color4,color5,color6};
];


colorsx[n_]:=Join[colors[[;;Abs[n]]],{colors[[-1]]}];


palette=GraphicsRow[Table[Graphics[{colors[[i]],Disk[]},ImageSize->100],{i,1,6}]];


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
