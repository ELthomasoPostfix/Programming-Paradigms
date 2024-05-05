/* Example goal statements:

No constraints (depth & exp unspecified), so unstructured output:
    phrase(expression(D), Exp), concat_atom(Exp, ExpAtom).

Few constraints (depth XOR exp specified), so structured output:
    Exp = "λx.((x x) x)", phrase(expression(D), Exp), concat_atom(Exp, ExpAtom).
    between(0, 4, D),     phrase(expression(D), Exp), concat_atom(Exp, ExpAtom).

Separate constraints:
    phrase(expression(Depth), Exp), concat_atom(Exp, ExpAtom), phrase(free(Free), Exp), length(Free, 1).

Reductions:
    reduce('x', "λz.z", "(x λx.y)", Reduced), !.

*/



%%%%%%%%%%%%%%%%%%%%%%%%
% CONFIG & HELPER CODE %
%%%%%%%%%%%%%%%%%%%%%%%%


% Allows strings to be used as input to phrase/2
:- set_prolog_flag(double_quotes, chars).


% max(+V1, +V2, -Max)
% Compute the Max of V1 and V2. V1 and V2 must be instantiated.
max(V1, V2, Max) :- V1 > V2, Max is V1; V1 =< V2, Max is V2.


% A DCG that represents any list at all.
% Useful for describing arbitrary sequences and getting access to them,
% so that you can apply constraints to the through plain Prolog code.
% e.g.      triple(Seq) --> ...(Seq), { length(Seq, 3) }.
...([]) --> [].
...([X|Xs]) --> [X], ...(Xs).



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
%       This is forces the depth limitation constraint.
function_limited(ExpDepth) -->
    % VERIFICATION; function expressions may only be generated until the specified, recursive expression depth.
    { nonvar(ExpDepth), succ(Predecessor, ExpDepth) }, function(Predecessor) |
    % GENERATION; if ExpDepth is uninitialized/anonymous, then await an instantiation of Predecessor to be
    % propagated up before evaluating the succ/2 predicate.
    { var(ExpDepth) }, function(Predecessor), { succ(Predecessor, ExpDepth) }.


% <app> --> (<exp> <exp>)
% The actual application pattern.
% Note: The succ/2 predicate fails when the second argument evaluates to 0.
%       This is forces the depth limitation constraint.
application(LExpDepth, RExpDepth) --> "(", expression(LExpDepth), " ", expression(RExpDepth), ")".
% Logic to facilitate BOTH application expression generation AND verification.
application_limited(ExpDepth) -->
    % VERIFICATION; application expressions may only be generated until the specified, recursive expression depth.
    { nonvar(ExpDepth), succ(Predecessor, ExpDepth) }, application(Predecessor, Predecessor) |
    % GENERATION; if ExpDepth is uninitialized/anonymous, then await an instantiation of Predecessor to be
    % propagated up before evaluating the succ/2 predicate. The max/3 predicate allows the two LR-expressions
    % of the application to have a differing depth; i.e. LExpDepth \= RExpDepth is allowed.
    { var(ExpDepth) }, application(LExpDepth, RExpDepth), { max(LExpDepth, RExpDepth, Predecessor), succ(Predecessor, ExpDepth) }.


% <var> --> x | y | z | ...
variable(X) --> [X], { var_(X) }.

% Use a variable predicate allow generalization of the variable DC rule.
var_(x).
var_(y).
var_(z).



%%%%%%%%%%%%%%%%%%%%%%%%%%
% β-REDUCTION DCG CODE %
%%%%%%%%%%%%%%%%%%%%%%%%%%


% Perform a β-reduction.
% Enforces pre-conditions for the proper handling of the substitute//3 DCG.
% reduce(+TargetVar, +SubstitutionTerm, +Exp, -RedAt)
% Params:
%   1) The variable to replace
%   2) The lambda expression to replace the variable by
%   3) The lambda expression to apply the replacement to
%   4) The result after replacement
reduce(TargetVar, SubstitutionTerm, Exp, RedAt) :-
    % Require the substitution value to be a list (containing characters).
    is_list(SubstitutionTerm) -> (
        % Check that the Exp is a valid lambda expression, for completeness' sake
        phrase(expression(_), Exp) -> (
            % Do the substitution.
            phrase(substitute(TargetVar, SubstitutionTerm, Reduced), Exp),
            write(Reduced),
            concat_atom(Reduced, RedAt)
        ); concat_atom(Exp, ExpAtom),
           format('The expression MUST be a valid lambda expression. Did you use only valid variables? Got:\n    ~w\n', [ExpAtom])
    ); format('The substitution term MUST be a list, got:\n    ~w\n', [SubstitutionTerm]).


% Extract the free terms of a lambda expression.
% Suppose x is a variable and s, t are lambda terms;
% free(x) = [x]
free([X]) --> variable(X).
% free(λx.t) = free(t) \ [x]
free(Free) --> "λ", variable(Var), ".", free(BodyFree), { delete(BodyFree, Var, Free) }.
% free((t s)) = free(t) U free(s)
free(Free) --> "(", free(LFree), " ", free(RFree), ")", { union(LFree, RFree, Free) }.


% x[x := r] = r
substitute(TargetVar, SubstTerm, SubstTerm) --> variable(TargetVar).
% y[x := r] = y      given x \= y
substitute(TargetVar, _, [Var]) --> variable(Var), { Var \= TargetVar }.
% (t s)[x := r] = (t[x := r] s[x := r])
substitute(TargetVar, SubstTerm, SubstResult) -->
    "(", substitute(TargetVar, SubstTerm, LExp), " ", substitute(TargetVar, SubstTerm, RExp), ")",
    { append(['(' | LExp], [' '], LSubst), append(RExp, [')'], RSubst), append(LSubst, RSubst, SubstResult) }.
% (λx.t)[x := r] = λx.t      var x is bound by an abstraction
substitute(TargetVar, _, ['λ', TargetVar, '.' | Rest]) -->
    "λ", variable(TargetVar), ".", ...(Rest), { phrase(expression(_), Rest) }.
% (λy.t)[x := r] = λy.(t[x := r])      given x\= y AND y does not appear in the free vars of r (y is fresh for r)
substitute(TargetVar, SubstTerm, ['λ', Var, '.' | BodyExp]) -->
    "λ", variable(Var), ".", { Var \= TargetVar, phrase(free(Free), SubstTerm), \+ memberchk(Var, Free) }, substitute(TargetVar, SubstTerm, BodyExp).



%%%%%%%%%%%%%
% TEST CODE %
%%%%%%%%%%%%%


test_untyped_lambda() :-
    %
    % Unspecified depth; specified expression
    %

    % Test basic lambda expression syntax
    % A variable
    phrase(expression(_), "x"),
    % An application expression & embedded expressions
    phrase(expression(_), "(x x)"),
    phrase(expression(_), "(λx.x x)"),
    phrase(expression(_), "(x λx.x)"),
    phrase(expression(_), "(λx.x λx.x)"),
    phrase(expression(_), "(x (x x))"),
    phrase(expression(_), "((x x) x)"),
    phrase(expression(_), "((x x) (x x))"),
    % A function expression & embedded expressions
    phrase(expression(_), "λx.x"),
    phrase(expression(_), "λx.(x x)"),
    phrase(expression(_), "λx.(x λx.x)"),
    phrase(expression(_), "λx.(λx.x x)"),
    phrase(expression(_), "λx.(λx.x λx.x)"),
    phrase(expression(_), "λx.((x x) (x x))"),
    phrase(expression(_), "λx.λx.x"),
    phrase(expression(_), "λx.λx.(x x)"),
    phrase(expression(_), "λx.λx.λx.x"),
    

    % Test verification of balanced vs unbalanced applications.
    % i.e. applications where the L/R expressions have different depth.
    phrase(expression(_), "λx.((x x) λx.x)"),       % same depth,  3-3
    phrase(expression(_), "λx.((x x) x)"),          % left heavy,  3-2
    phrase(expression(_), "λx.(x (x x))"),          % right heavy, 2-3
    phrase(expression(_), "λx.((x (x x)) (x x))"),  % diff depth,  (3-4)-(3-3)
    phrase(expression(_), "λx.(((x x) x) (x x))"),  % diff depth,  (4-3)-(3-3)
    phrase(expression(_), "λx.((x x) ((x x) x))"),  % diff depth,  (3-3)-(4-3)
    phrase(expression(_), "λx.((x x) (x (x x)))"),  % diff depth,  (3-3)-(3-4)

    % Test application vs function depth equivalence.
    % i.e. recursively embedding an application or a function both increase depth by one
    % and are result in equivalent depths in the context of expression evaluation/generation.
    phrase(expression(DEq), "λx.x"),
    succ(DEq, DEqSucc),
    phrase(expression(DEqSucc), "λx.λx.x"),
    phrase(expression(DEq), "(x x)"),
    phrase(expression(DEqSucc), "((x x) x)").
