# Paper

This source code serves as an appendix to the Programming Paradigms paper assignment. It serves to illustrate the use of DCGs, by showing their applications through Prolog.

# Examples

This section details the usage, read calling conventions, of the examples provided here.

## abc

Generate a sequence of the form $a^n b^n c^n$. The $n$ value can be optionally specified.

```sh
# Specify depth
./abc.sh 3

# Allow default depth
./abc.sh
```

## Sierpinski

Generate an SVG image for a sierpinski triangle L-system grammar. The recursion depth of the replacements can be optionally specified.

```sh
# Specify depth
./sierpinski.sh 3

# Allow default depth
./sierpinski.sh
```

