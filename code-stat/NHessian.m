(* ::Package:: *)

NHessian::usage = "NHessian[f, x] computes a numerical approximation \
to the Hessian matrix evaluated at f[x]. NHessian take the option \
Scale, which can be a scalar or a vector (matching the length of the \
vector x). The default value is Scale -> \!\(\*SuperscriptBox[\(10\), \
\(-3\)]\)."

Options[NHessian] = {Scale -> 10^-3}

NHessian[f_, x_?(VectorQ[#, NumericQ] &), opts___?OptionQ] :=
Module[{n, h, norm, z, mat, f0},
n = Length[x];
h = Scale /. {opts} /. Options[NHessian];
norm = If[VectorQ[h], Outer[Times, 2 h, 2 h], 4 h^2];
z = If[VectorQ[h], DiagonalMatrix[h], h*IdentityMatrix[n]];
mat = ConstantArray[0., {n, n}];
f0 = f[x];
Do[
    mat[[i, j]] =
        If[i == j,
        (* then *)
            .5 (f[x + 2 * z[[i]]] - 2 f0 + f[x - 2 * z[[i]]]),
        (* else *)
            f[x + z[[i]] + z[[j]]] - f[x + z[[i]] - z[[j]]] - 
                f[x - z[[i]] + z[[j]]] + f[x - z[[i]] - z[[j]]]
        ],
{i, n}, {j, i, n}
];
(mat + Transpose[mat])/norm
]


(* credit: mef - see http://mathematica.stackexchange.com/questions/28948/numeric-calculation-of-hessian *)
