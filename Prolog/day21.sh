INPUT_FILE_PATH="day21.txt"
swipl -s day21 -g "
generate_facts(\"$INPUT_FILE_PATH\"),

start(StartCoord),
bfs([StartCoord], 64, FinalFringe),
length(FinalFringe, Len),
format('part 1: ~w\n', [Len]),

solution3(26501365, Surface),
format('part 2: ~w\n', [Surface]),

halt.
"
