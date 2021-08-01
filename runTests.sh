#!/usr/bin/bash

GREEN='\033[1;32m'
RED='\033[1;31m'
NC='\033[0m' # No Color

testBase="$(realpath ./test/lang)"
tmpRunResult="$(mktemp /tmp/flow_test_lang_run.XXXXXX)"

# Write to result
exec 3>$tmpRunResult
# Read from result
exec 4<$tmpRunResult

correct=0
wrong=0

printf "[pre :: build]: start\n"
stack build 
printf "[pre :: build]: done\n"


for f in $(ls "${testBase}/out")
do
    base="$(basename -- $f)"
    base="${base%.*}"
    input="${testBase}/in/${base}.in"
    output="$(cat ${testBase}/out/${base}.out)"

    echo "[start]: $f"
    [[ -f $input ]] && echo "[$f :: input]: $(cat $input)"
    echo "[$f :: expected]:"
    echo $output

    if [ -f "$input" ]; 
        then 
            cat $input | stack run -- "${testBase}/src/${base}.hf" >&3
        else 
            stack run -- "${testBase}/src/${base}.hf" >&3
    fi

    result="$(cat <&4)"
    echo "[$f :: returned]:"
    echo $result
    
    if [[ "$result" == "$output" ]];
        then 
            echo -e "[${GREEN}correct${NC}]: $f"
            ((correct++))
        else 
            echo -e "[${RED}wrong${NC}]: $f"
            ((wrong++))
    fi
    echo -e "\n"
done

echo "errors: $wrong; correct: $correct; total: $((wrong + correct))"