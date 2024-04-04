INPUT_FILE_PATH="day3.txt"
swipl -s day3 -g "
\+ generate_facts(\"$INPUT_FILE_PATH\"),

marked_sum(Sum),
format('part 1: ~w\n', [Sum]),

gear_ration_sum(Sum2),
format('part 2: ~w\n', [Sum2]),

halt.
"
