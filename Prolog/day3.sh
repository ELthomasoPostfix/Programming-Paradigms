INPUT_FILE_PATH="day3.txt"
swipl -s day3 -g "
\+ generate_facts(\"$INPUT_FILE_PATH\"),

marked_sum(Sum),
format('part 1: ~w\n', [Sum]),

format('please wait +/- 2 min, part 2 is coming ...\n', []),
gear_ratio_sum(Sum2),
format('part 2: ~w\n', [Sum2]),

halt.
"
