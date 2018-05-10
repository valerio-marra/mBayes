(* ::Package:: *)

(* from https://gist.githubusercontent.com/lshifr/56c6fcfe7cafcd73bdf8/raw/LazyTuples.m *)


BeginPackage["LazyTuples`"]

LazyTuples::usage = "LazyTuples[lists, opts] create a LazyList of the specified chunk size (\"ChunkSize\" option), containing tuples of the original lists";

Begin["`Private`"]

Needs["Streaming`"];

ClearAll[next];
next[{left_, _}, dim_] := 
    {left - dim*(# - 1), #} &[IntegerPart[(left - 1)/dim] + 1];

ClearAll[multiDims];
multiDims[dims_] := Rest @ Reverse @ FoldList[Times, 1, Reverse @ dims];

ClearAll[multiIndex];
multiIndex[pos_, dims : {__Integer}] :=
    Rest@FoldList[next, {pos, 0}, multiDims@dims][[All, 2]]

ClearAll[take];
take[lists : {__List}, {start_, end_}] :=
    With[{rend = Min[end, Times @@ Map[Length, lists]]},
        Transpose @ MapThread[
            Part, 
            {lists, multiIndex[Range[start, rend], Length /@ lists]}
	    ]
    ];
    

ClearAll[$iteratorChunkSize];    
  
ClearAll[lazyTuplesVirtual];
lazyTuplesVirtual[lists:{__List}, chsize:_Integer?Positive:$iteratorChunkSize]:=
    With[{
        takeFun = 
            Function[index, 
                Function[
                    take[lists, {(index-1) * chsize+1, index * chsize}]
                ]
            ]
        },
        LazyListCreate[
            Streaming`PackageScope`VirtualChunkGeneratorMake[takeFun,{}],
            Function[index, If[# === 0, -1, #] & @ Length @ takeFun[index][]],
            "FiniteList" -> True
        ]
    ];
	
ClearAll[makeTupleIterator];
makeTupleIterator[lists:{__List}, chunkSize_Integer?Positive]:=
    With[{len = Times @@ Map[Length, lists]},
        Module[{ctr = 0, active = False},
            IteratorCreate[
                ListIterator,
                (active = True) &,
                Function[
                    If[ctr >= len, 
                        {}, 
                        (* else *)
                        With[{taken = take[lists, {ctr+1, Min[ctr + chunkSize, len]}]},
                            ctr += Length[taken];
                            taken
                        ]
                    ]
                ],
                TrueQ[active] &,
                Remove[active, ctr]&
            ]
        ]
    ];	
	
ClearAll[lazyTuplesIterative];
lazyTuplesIterative[lists:{__List}, chunkSize:_Integer?Positive:$iteratorChunkSize]:=
    LazyListCreate[makeTupleIterator[lists, chunkSize], chunkSize];
	
$iteratorChunkSize = 100000;	
	
ClearAll[lazyTuples];	
lazyTuples["Iterator"] = lazyTuplesIterative;
lazyTuples["Virtual"] = lazyTuplesVirtual;
lazyTuples[_][args___]:= $Failed;


ClearAll[LazyTuples];
Options[LazyTuples] = {
    "Method" -> "Iterator",
    "ChunkSize" :>  $iteratorChunkSize
};

LazyTuples[lists_, opts:OptionsPattern[]]:=
    lazyTuples[OptionValue["Method"]][lists, OptionValue["ChunkSize"]]; 
    

End[]

EndPackage[]
