INPUT_FILE_PATH="day8.txt"
swipl -s day8 -g "
\+ generate_facts(\"$INPUT_FILE_PATH\"),

reachable(aaa, is_terminal_1, [], RecDepth_P1) -> !,
format('part 1: ~w\n', [RecDepth_P1]),

recdepths_lcm(RecDepth_P2),
format('part 2: ~w\n', [RecDepth_P2]),

halt.
"
