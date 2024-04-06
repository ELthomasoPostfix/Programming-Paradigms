INPUT_FILE_PATH="day21.txt"
swipl -s day21 -g "
generate_facts(\"$INPUT_FILE_PATH\"),

start(StartCoord),
bfs([StartCoord], 64, FinalFringe),
length(FinalFringe, Len),
format('part 1: ~w\n', [Len]),

%recdepths_lcm(RecDepth_P2),
%format('part 2: ~w\n', [RecDepth_P2]),

halt.
"
