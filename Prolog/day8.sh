#!/usr/bin/env bash

# The following links were used as inspiration; bash is hard :(
# use grep?               https://stackoverflow.com/a/1892107
# sed unix specifics      https://stackoverflow.com/a/17145789

# The true AoC input file path
FILE_INPUT_PATH_DATA="day8.txt"
# The path of the template file for generating the data(base) file
FILE_INPUT_PATH_TEMPLATE_DATA="day8_data_template.pl"
# The path of the template file for generating the final Prolog file
FILE_INPUT_PATH_TEMPLATE_CODE="day8_1_template.pl"
# The data(base) output file path
FILE_OUTPUT_PATH_P1="day8_1.pl"
# The regex pattern of the R-L movement pattern to adhere to
REGEX_MOVE_PATTERN="^([RL]+)$"
# The regex pattern of individual movement graph nodes
REGEX_NODE_CONNECT="^(\w+) = \((\w+), (\w+)\)$"
# The regex pattern of the target Prolog rules of individual movement graph nodes
# Enforces lowercase so that all elements are Prolog atoms
REGEX_NODE_TARGET="left\(\L\1, \L\2\).\nright\(\L\1, \L\3\)."


cat $FILE_INPUT_PATH_TEMPLATE_CODE > $FILE_OUTPUT_PATH_P1

# Extract the singular movement pattern string from the input
MOVE_PATTERN=$(grep -oE "${REGEX_MOVE_PATTERN}" $FILE_INPUT_PATH_DATA)
# Initialize the data output data file with a move pattern Prolog rule
sed -E "s/<MOVE_PATTERN_TARGET>/\`$MOVE_PATTERN\`/" $FILE_INPUT_PATH_TEMPLATE_DATA \
    >> $FILE_OUTPUT_PATH_P1

# Append the parsed movement graph to the output data file, as a sequence of Prolog facts
grep -oE "${REGEX_NODE_CONNECT}" $FILE_INPUT_PATH_DATA \
    | sed -nE "s/${REGEX_NODE_CONNECT}/${REGEX_NODE_TARGET}/p" \
    >> $FILE_OUTPUT_PATH_P1

