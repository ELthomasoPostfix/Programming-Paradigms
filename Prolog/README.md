# Prolog

For this part of the assignment, **3 questions** out of 25 unique questions of the AoC 2023 questions should be solved in Prolog. The questions solved in Prolog may not be submitted for the Haskell assignment as well.

# Solutions

This section details the solutions to the chosen AoC questions for Prolog. However, additional information should only be expected if the solution is non-trivial to verify or understand.

## AoC Day 8

### General

[Day 8 of AoC 2023](https://adventofcode.com/2023/day/8) is about graph traversal. We start off by explaining why Prolog was chosen over Haskell.

Both Prolog and Haskell are very useful when making use of tail-recursion. However, Haskell has the drawback of fairly complicated data structure creation and or management. In the case of day 8, the solution requires the ability traverse the constructed graph in a fixed movement pattern of left/right moves from root to a specific terminal, zzz. This necessitates lookup functionality (a dictionary) or list indexation, both of which are fairly difficult or complicated in Haskell. In contrast, the query resolution strategy of Prolog is inherently based on trees and backtracking. This makes Prolog uniquely suited to solving this question.

### Bash

We must ensure that the generated Prolog solution is easily runnable in the [SWISH Prolog web app](https://swish.swi-prolog.org/). A problem presents itself immediately: file access. The web app, understandably, limits the Prolog functionality related to accessing different files. This means it would be hard to provide the graph structure as a separate "database" of Prolog facts. Our strategy will thus be to generate a single, large solution file.

To simplify the Prolog code, we will simply generate the final code files, [day8_1.pl](/Prolog/day8_1.pl), using a [bash script](/Prolog/day8.sh). This script makes use of the AoC question input, [day8.txt](/Prolog/day8.txt), and two template files, [a code template](/Prolog/day8_1_template.pl) and [a data template](/Prolog/day8_data_template.pl), to create a file that contains both the facts (data) and rules (recursive code) to solve the problem.

To actually generate the [day8_1.pl](/Prolog/day8_1.pl) file, run the [day8.sh](/Prolog/day8.sh) script from the `/Prolog/` directory:

```sh
.../Prolog$ chmod +x day8.sh
.../Prolog$ ./day8.sh
```

The resulting `.pl` script contains the code, followed by a long list of all the relevant facts, extracted from the input file.

The code template contains the recursive logic of the traversal and a helper rule to check if a given character dictates that the next move in the graph should be a left or right move.

The data template contains a helper rule, to ensure the L-R movement pattern wraps around (continues infinitely) when the recursion exhausts it. This rule lives in the data template because part of the input, the L-R movement pattern, must be injected into its code via string replacement.


### Part 1: Query

To extract the answer to the question, a query of the following form should be ran in the SWI Prolog web app when using the [day8_1.pl](/Prolog/day8_1.pl) code file:

```Prolog
?- follow_path(aaa, [], StepCount)
```

where `StepCount` resolves to the amount of steps to reach `zzz` from `aaa` using the required movement pattern. The code has hardcoded into it that `zzz` is the final destination.
