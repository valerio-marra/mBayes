(* ::Package:: *)

(* for sanity checks *)


(*parnamez={"x","y","z"};*)

(*(* p1 *)
AppendTo[pSet,{vecsolz[[1]].5,vecsolz[[1]]1.5,50}];
(* p2 *)
AppendTo[pSet,{vecsolz[[2]].5,vecsolz[[2]]1.5,50}];
(* p3 *)
AppendTo[pSet,{vecsolz[[3]].5,vecsolz[[3]]1.5,50}];*)


vectest = {10., 20., 30.};

covtest={{2, 1/2, -3^(-1)}, {1/2, 1, 0}, {-3^(-1), 0, 2/3}};

chi2test[x_, y_, z_] := -2Log[PDF[MultinormalDistribution[vectest,covtest], {x, y, z}]];


Print["The reference vector is 'vectest'; the \!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\) function is 'chi2test[x,y,z]'"];


vecPtest={11., 18., 31.};

CPtest={{3., 1/5, -4^(-1)}, {1/5, 1, 0}, {-4^(-1), 0, 2/3}};
Ptest=Inverse[CPtest];


Print["The prior vector is 'vecPtest'; the prior Fisher matrix is 'Ptest'"];


Print[Row[{"Dimension of parameter space: ",Length[vectest]}]];


(* This code is released under the GPL license. Copyright 2018 by Valerio Marra (valerio.marra@me.com) *)
