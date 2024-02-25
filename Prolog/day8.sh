#!/usr/bin/env bash

# The following links were used as inspiration; bash is hard :(
# use grep?               https://stackoverflow.com/a/1892107
# sed unix specifics      https://stackoverflow.com/a/17145789

# The true AoC input file path
FILE_INPUT_PATH_DATA="day8.txt"
# The path of the template file for generating the data(base) part of each output file
FILE_INPUT_PATH_TEMPLATE_DATA="day8_data_template.pl"
# The path of the day 8 part 1 template file for generating the final Prolog file
FILE_INPUT_PATH_TEMPLATE_CODE_P1="day8_1_template.pl"
# The path of the day 8 part 2 template file for generating the final Prolog file
FILE_INPUT_PATH_TEMPLATE_CODE_P2="day8_2_template.pl"
# The final day 8 part 1 Prolog output file path
FILE_OUTPUT_PATH_P1="day8_1.pl"
# The final day 8 part 2 Prolog output file path
FILE_OUTPUT_PATH_P2="day8_2.pl"
# The regex pattern of the R-L movement pattern to adhere to
REGEX_MOVE_PATTERN="^([RL]+)$"
# The regex pattern of individual movement graph nodes
REGEX_NODE_CONNECT="^(\w+) = \((\w+), (\w+)\)$"
# The regex pattern of the target Prolog rules of individual movement graph nodes
# Enforces lowercase so that all elements are Prolog atoms
REGEX_NODE_TARGET="left\(\L\1, \L\2\).\nright\(\L\1, \L\3\)."


#########################
# GENERATE DAY 8 PART 1 #
#########################

# Initialize part 1 final output by prepending it with the part 1 code
cat $FILE_INPUT_PATH_TEMPLATE_CODE_P1 > $FILE_OUTPUT_PATH_P1

# Extract the singular movement pattern string from the input
MOVE_PATTERN=$(grep -oE "${REGEX_MOVE_PATTERN}" $FILE_INPUT_PATH_DATA)
# Define the L-R movement pattern Prolog rules
sed -E "s/<MOVE_PATTERN_TARGET>/\`$MOVE_PATTERN\`/" $FILE_INPUT_PATH_TEMPLATE_DATA \
    >> $FILE_OUTPUT_PATH_P1

# Append the parsed movement graph, as a sequence of Prolog facts
grep -oE "${REGEX_NODE_CONNECT}" $FILE_INPUT_PATH_DATA \
    | sed -nE "s/${REGEX_NODE_CONNECT}/${REGEX_NODE_TARGET}/p" \
    >> $FILE_OUTPUT_PATH_P1


#########################
# GENERATE DAY 8 PART 2 #
#########################


# Initialize part 2 final output by prepending it with the part 2 code
cat $FILE_INPUT_PATH_TEMPLATE_CODE_P2 > $FILE_OUTPUT_PATH_P2

# Define the L-R movement pattern Prolog rules
sed -E "s/<MOVE_PATTERN_TARGET>/\`$MOVE_PATTERN\`/" $FILE_INPUT_PATH_TEMPLATE_DATA \
    >> $FILE_OUTPUT_PATH_P2

# Append the parsed movement graph, as a sequence of Prolog facts
grep -oE "${REGEX_NODE_CONNECT}" $FILE_INPUT_PATH_DATA \
    | sed -nE "s/${REGEX_NODE_CONNECT}/${REGEX_NODE_TARGET}/p" \
    >> $FILE_OUTPUT_PATH_P2
