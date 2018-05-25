(* ::Package:: *)

(* importing Pantheon binned data *)

(* data vector *)
datajlaBin=Import["cats/Pantheon/hlsp_ps1cosmo_panstarrs_gpc1_all_model_v1_lcparam.txt","Table","HeaderLines"->1];
ztabBin=datajlaBin[[All,2]];
{zminBin,zmaxBin}=MinMax[ztabBin];(*Row[{"z range:",{zminBin,zmaxBin}}];*)
mutabBin=datajlaBin[[All,5]];
ErrtabStat=datajlaBin[[All,6]];

(* covariance matrix *)
CovImp=Flatten[Import["cats/Pantheon/hlsp_ps1cosmo_panstarrs_gpc1_all_model_v1_sys.txt","Table"]];
NjlaBin=CovImp[[1]];
Dimensions[covmuBin=Partition[CovImp[[2;;]],NjlaBin]];
covmuBin=covmuBin+DiagonalMatrix[ErrtabStat^2];
inVcovmuBin=Inverse[covmuBin];
Errtab=Diagonal[covmuBin]^.5;

(* likelihood defs *)
offiv=Total[2Log[Errtab Sqrt[2Pi]]];
Sum0=Total[1/Errtab^2];
offi=Log[Sum0/(2Pi)]+ offiv;

offivf=Log[Det[2 Pi covmuBin]];
ones=ConstantArray[1,NjlaBin];
Sum00=ones.inVcovmuBin.ones; (* = Total[inVcovmuBin,-1] *)
offif=Log[Sum00/(2Pi)]+ offivf;


full=1; (* 1 for full covariance, 0 to only use the diagonal part *)

chi2test[om_?NumericQ,w0_?NumericQ,wa_?NumericQ]:=Module[
{a}
,
(*wz[z_]=w0+wa z/(1+z);*)
Ez[z_]=Sqrt[om (1+z)^3+(1-om) (1+z)^(3 (1+w0+wa))Exp[-3 wa z/(1+z)]];
dC[z_]=First[dCf[z]/.NDSolve[{dCf'[z]== 1/Ez[z],dCf[0]==0},dCf,{z,0,zmaxBin},MaxStepFraction->1/1000]];
dL[z_]= dC[z](1+z);
mo[z_]=5 Log[10,dL[z]];

Which[
full==1
,
vecmoi=mutabBin-mo[ztabBin];
Sum11=vecmoi.inVcovmuBin.vecmoi; 
Sum10=vecmoi.inVcovmuBin.ones; 
Sum11-Sum10^2/Sum00 (* this is \[Chi]2 *)
(*Sum11-Sum10^2/Sum00+offif*) (* this is -2Log[L] - it is normalized *)
,
full==0
,
vecmoi=mutabBin-mo[ztabBin];
Sum1=Total[vecmoi/Errtab^2];
Sum2=Total[vecmoi^2/Errtab^2];
Sum2-Sum1^2/Sum0 (* this is \[Chi]2 *)
(*Sum2-Sum1^2/Sum0+offi*) (* this is -2Log[L] - it is normalized *)
]

];


vectest = {.3, -1., 0.};

Print["The reference vector is 'vectest'; the \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) function is 'chi2test[om,w0,wa]'"];


vecPtest={.3, -1.1, -.1};

corrm={{1,.6,-.9},{.6,1,-.8},{-.9,-.8,1}};corrm//MatrixForm
errm=DiagonalMatrix[{.1,.15,.3}];
CPtest=(errm.corrm).errm;CPtest//MatrixForm
Ptest=Inverse[CPtest];

Print["The prior vector is 'vecPtest'; the prior Fisher matrix is 'Ptest'"];


Print[Row[{"Dimension of parameter space: ",Length[vectest]}]];


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
