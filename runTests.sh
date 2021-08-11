#!/usr/bin/bash

GREEN='\033[1;32m'
RED='\033[1;31m'
NC='\033[0m' # No Color

testBase="$(realpath ./test/lang)"
tmpRunResult="$(mktemp /tmp/flow_test_lang_run.XXXXXX)"
caseSeparator="----------<>----------"

# Write to result
exec 3>$tmpRunResult
# Read from result
exec 4<$tmpRunResult

correct=0
wrong=0

printf "[pre :: build]: start\n"

stack build 
sleep "0.4s"

printf "[pre :: build]: done\n"


function execTest () {
    local f=$1
    local input="$2"
    local output="$(cat $3)"
    local src=$4
    local optLvl=$5

    echo "[start]: $f"

    [[ -f $input ]] && echo "[$f :: input]: $(cat $input)"
    echo "[$f :: expected]:"
    echo $output

    echo -e $caseSeparator
    for i in 0 1
    do
        local name="${f}/ FOPT=${i}"
        export FOPT=$i

        if [ -f "$input" ]; 
            then 
                cat $input | stack run -- $src >&3
            else 
                stack run -- $src >&3
        fi

        result="$(cat <&4)"
        echo "[$name :: returned]:"
        echo $result
        
        if [[ "$result" == "$output" ]];
            then 
                echo -e "[${GREEN}correct${NC}]: $name"
                ((correct++))
            else 
                echo -e "[${RED}wrong${NC}]: $name"
                ((wrong++))
        fi
        echo -e $caseSeparator 
    done   

    echo -e "[done]: $f\n\n"
}

for f in $(ls "${testBase}/out")
do
    base="$(basename -- $f)"
    base="${base%.*}"
    input="${testBase}/in/${base}.in"
    output="${testBase}/out/${base}.out"
    src="${testBase}/src/${base}.hf"

    execTest $f $input $output $src 
done

echo "errors: $wrong; correct: $correct; total: $((wrong + correct))"