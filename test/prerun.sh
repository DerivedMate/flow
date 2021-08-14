#!/bin/bash

# Generate cyclicDep.out path
echo "Error: cyclic dependency tree. Root path: $(realpath ./lang/src/cyclicDep.hf)" > ./lang/out/cyclicDep.out