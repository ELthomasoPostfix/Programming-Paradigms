/* Example goal statements:

    Exp = "λx.((x x) x)", phrase(expression(D), Exp), concat_atom(Exp, ExpAtom).
    between(0, 4, D),     phrase(expression(D), Exp), concat_atom(Exp, ExpAtom).
    phrase(expression(D), Exp), concat_atom(Exp, ExpAtom).
    phrase(expression(Depth), Exp), concat_atom(Exp, ExpAtom), phrase(free(Free), Exp), length(Free, 1).

*/


% Allows strings to be used as input to phrase/2
:- set_prolog_flag(double_quotes, chars).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% UNTYPED LAMBDA CALC DCG CODE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% <exp> --> <var> | <app> | <fun>
% The chosen order of three expression derivatives is deliberate.
% Variables are the sole leaf term, and functions tend to recurse on themselves.
% The chosen order provides an optimal lambda expression generation capability.
expression(ExpDepth) --> variable(_), {
                            % GENERATION; if ExpDepth is uninitialized/anonymous, then it defaults to 0.
                            % This ensures the depth variable is sufficiently instantiated when generating expressions.
                            var(ExpDepth), ExpDepth = 0;
                            % VERIFICATION; variables may only be generated until the specified, recursive expression depth.
                            % This ensures a variable can be matched when verifying expressions.
                            nonvar(ExpDepth), ExpDepth >= 0
                         } |
                         application_limited(ExpDepth) |
                         function_limited(ExpDepth).


% <fun> --> λ<var>.<exp>
% The actual function pattern.
function(ExpDepth) --> "λ", variable(_), ".", expression(ExpDepth).
% Logic to facilitate BOTH function expression generation AND verification.
% Note: The succ/2 predicate fails when the second argument evaluates to 0.
function_limited(ExpDepth) -->
    % VERIFICATION; function expressions may only be generated until the specified, recursive expression depth.
    { nonvar(ExpDepth), succ(Predecessor, ExpDepth) }, function(Predecessor) |
    % GENERATION; if ExpDepth is uninitialized/anonymous, then await an instantiation of Predecessor to be
    % propagated up before evaluating the succ/2 predicate.
    { var(ExpDepth) }, function(Predecessor), { succ(Predecessor, ExpDepth) }.


% <app> --> (<exp> <exp>)
% The actual application pattern.
% Note: The succ/2 predicate fails when the second argument evaluates to 0.
application(ExpDepth) --> "(", expression(ExpDepth), " ", expression(ExpDepth), ")".
% Logic to facilitate BOTH application expression generation AND verification.
application_limited(ExpDepth) -->
    % VERIFICATION; application expressions may only be generated until the specified, recursive expression depth.
    { nonvar(ExpDepth), succ(Predecessor, ExpDepth) }, application(Predecessor) |
    % GENERATION; if ExpDepth is uninitialized/anonymous, then await an instantiation of Predecessor to be
    % propagated up before evaluating the succ/2 predicate.
    { var(ExpDepth) }, application(Predecessor), { succ(Predecessor, ExpDepth) }.


% <var> --> x | y | z | ...
variable(x) --> [x].
variable(y) --> [y].
variable(z) --> [z].




%%%%%%%%%%%%%%%%%%%%%%%%
% β-REDUCTION DCG CODE %
%%%%%%%%%%%%%%%%%%%%%%%%


% To implement beta reductions, just replace all matches of Var in Body of a function
% --> can't just ignore free & bound vars???
reduce(Var, BodyAtom) --> "λ", [Var], ".", ...(Body), { concat_atom(Body, BodyAtom) }.

% Separate a string containing Var into a list of the substrings between each Var and the Vars.
% e.g. "aaybbycyyd" --> [[aa], y, [bb], y, [c], y, [], y]
stale(Var, [Prefix, Var | Recursive]) --> ...(Prefix), { \+ memberchk(Var, Prefix) }, [Var], stale(Var, Recursive).
stale(_, []) --> [].

prefixed_var(Var, [Prefix, Var]) --> ...(Prefix), { \+ memberchk(Var, Prefix) }, [Var].

split(Var, [Prefix, Var | Postfix]) -->
    prefixed_var(Var, [Prefix, Var]),
    (
        split(Var, Postfix) |
        ...(Tail), { Postfix = [Tail] }
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%
% FREE VARIABLE DCG CODE %
%%%%%%%%%%%%%%%%%%%%%%%%%%


% Suppose x variable and s, t lambda terms;
% free(x) = [x]
free([X]) --> variable(X).
% free(λx.t) = free(t) \ [x]
free(Free) --> "λ", variable(Var), ".", free(BodyFree), { delete(BodyFree, Var, Free) }.
% free((t s)) = free(t) U free(s)
free(Free) --> "(", free(LFree), " ", free(RFree), ")", { union(LFree, RFree, Free) }.

substitute(TargetVar, SubstTerm, SubstTerm) --> variable(TargetVar).
substitute(TargetVar, _, Var) --> variable(Var), { Var \= TargetVar }.
substitute(TargetVar, SubstTerm, ['(', LExp, ' ', RExp, ')']) -->
    "(", substitute(TargetVar, SubstTerm, LExp), " ", substitute(TargetVar, SubstTerm, RExp), ")".






%%%%%%%%%%%%%%%%%%%%%%%%%%
% TEST CODE %
%%%%%%%%%%%%%%%%%%%%%%%%%%


f(A, BB) --> "f", [A], ".", ...(B), { concat_atom(B, BB) }.


...([]) --> [].
...([X|Xs]) --> [X], ...(Xs).