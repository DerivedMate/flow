#!/usr/bin/bash

testBase="$(realpath ./test/lang)"
tmpRunResult="$(mktemp /tmp/flow_test_lang_run.XXXXXX)"

# Write to result
exec 3>$tmpRunResult
# Read from result
exec 4<$tmpRunResult

correct=0
wrong=0

stack build

for f in $(ls "${testBase}/out")
do
    base="$(basename -- $f)"
    base="${base%.*}"
    input="${testBase}/in/${base}.in"
    output="$(cat ${testBase}/out/${base}.out)"

    echo "[start]: $f"
    echo "[$f :: input]: $(cat $input)"
    echo "[$f :: expect]: $output"

    if [ -f $input ]; 
        then 
            cat $input | stack run -- "${testBase}/src/${base}.hf" >&3
        else 
            stack run -- "${testBase}/src/${base}.hf" >&3
    fi

    result="$(cat <&4)"
    echo "[$f :: returned]: $result"
    
    if [[ "$result" == "$output" ]];
        then 
            echo "[correct]: $f"
            ((correct++))
        else 
            echo "[wrong]: $f"
            ((wrong++))
    fi
    echo ""
done

echo "errors: $wrong; correct: $correct; total: $((wrong + correct))"