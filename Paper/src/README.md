# Paper

This source code serves as an appendix to the Programming Paradigms paper assignment. It serves to illustrate the use of DCGs, by showing their applications through Prolog.

# Examples

This section details the usage, read calling conventions, of the examples provided here.

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

