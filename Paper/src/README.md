# Paper

This source code serves as an appendix to the Programming Paradigms paper assignment. It serves to illustrate the use of DCGs, by showing their applications through Prolog.

# Examples

This section details the usage, read calling conventions, of the examples provided here.

## DCG syntax

Generate sequences that showcase the basic syntax of DCGs in Prolog. Optional, positional parameters can be specified:
1) The $X$ variable of the $head(X)$ definite clause; MUST _some_ or _a_

```sh
# Specify X
./dcg_syntax.sh some

# Allow default X assignment
./dcg_syntax.sh
```

At the time of writing, three alternative calls of the $phrase$ predicate are done.
1) Leave $X$ of $head(X)$ as a free variable
2) Fill in $X$ of $head(X)$ with the script parameter
3) Make use of an unnamed definite clause in the $phrase$ predicate call to add a second punctuation mark

## abc

Generate a sequence of the form $a^n b^n c^n$. Optional, positional parameters can be specified:
1) The $n$ value; MUST be a positive integer $\geq 0$

```sh
# Specify depth
./abc.sh 3

# Allow default depth
./abc.sh
```

## Sierpinski

Generate an SVG image for a sierpinski triangle L-system grammar. Optional, positional parameters can be specified:
1) The recursion depth of the replacements; MUST be a positive integer $\geq 0$
2) The sierpinski pattern implementation method; if $true$ then call the pure DCG implementation, if $false$ call the mixed DCG + predicate implementation

```sh
# Specify depth
./sierpinski.sh 3

# Allow default depth
./sierpinski.sh
```

