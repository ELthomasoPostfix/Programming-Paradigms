# Prolog

For this part of the assignment, **3 questions** out of 25 unique questions of the AoC 2023 questions should be solved in Prolog. The questions solved in Prolog may not be submitted for the Haskell assignment as well.

# Installation

The verification of the solutions is to be performed using a local installation of the SWI Prolog compiler.

The [SWI Prolog compiler](https://www.swi-prolog.org/build/PPA.html) may need to be installed:

```sh
sudo apt-add-repository ppa:swi-prolog/stable
sudo apt-get update
sudo apt-get install swi-prolog
```

# Useful Links

This section lists links to useful resources:

* [Prolog basics](https://www.youtube.com/playlist?list=PLEvH6T-1oh75UagVp5BqeOVageBljdmaC)
* [Memory & Choice points](https://www.youtube.com/watch?v=xABsTV8kuGE)
* ["The Power of Prolog"](https://www.metalevel.at/prolog/introduction)


# Solutions

This section details the solutions to the chosen AoC questions for Prolog. However, additional information should only be expected if the solution is non-trivial to verify or understand.


## AoC Day 8

### General

[Day 8 of AoC 2023](https://adventofcode.com/2023/day/8) is about graph traversal. We start off by explaining why Prolog was chosen over Haskell.

Both Prolog and Haskell are very useful when making use of tail-recursion. However, Haskell has the drawback of fairly complicated data structure creation and or management. In the case of day 8, the solution requires the ability traverse the constructed graph in a fixed movement pattern of left/right moves from root to a specific terminal, `zzz` for part 1 and `??z` for part 2 where the `?` represents any character. This necessitates lookup functionality (a dictionary) or list indexation, both of which are fairly difficult or complicated in Haskell, to find the left or right children of any node. In contrast, the query resolution strategy of Prolog is inherently based on trees and backtracking. This makes Prolog uniquely suited to solving this problem.

### Code Structure

The Prolog code file, [day8.pl](/Prolog/day8.pl), contains three major components:
* **A parsing method**, to convert the input file into facts and rules dynamically
* **A simple DCG** to model the input's structure
* **A recursive reachability predicate** to check if some terminal node is reachable from a starting node in the graph

The reachability predicate alone solves for part 1 of the question. The solution for part 2 builds upon this predicate.

### Parts 1 & 2: Queries

The following code block shows how to solve for the solutions of parts 1 and 2 respectively.


```sh
# Load the code file into the swipl environment
?- [day8].
true.

# Generate facts and rules from the input file dynamically
?- generate_facts("./day8.txt").
false.

# Solve for part 1, the first result is the desired answer
?- reachable(aaa, is_terminal_1, [], RecDepth).
RecDepth = 12643 ;
RecDepth = 25286 ;
RecDepth = 37929 .

# Solve for part 2, the sole result is the desired answer
?- recdepths_lcm(RecDepth).
RecDepth = 13133452426987.
```

where `RecDepth` resolves to the amount of steps needed to reacg some terminal node from the given start node, using the required movement pattern.

For start nodes and terminal nodes, there are multiple options.
For part 1, only the node `aaa` is a start node. For part 2, any node `??a` is a start node, where `?` denotes any character.
For part 1, only `zzz` is a terminal node. This is expressed through predicate `is_terminal_1`. For part 2, any node `??z` is a terminal node. This is expressed through predicate `is_terminal_2`.
