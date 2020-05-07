#!/bin/bash
# Create MakeFile Header

obj=$(ls "../scheme/compiled" | grep -o "^.*\." | tr -d "." | awk "{print \$0\".o\"}" | paste -sd " ")
c=$(ls "../scheme/compiled" | grep -o "^.*\." | tr -d "." | awk "{print \$0\".c\"}" | paste -sd " ")
plus=$(ls "../scheme/compiled" | grep -o "^.*\." | tr -d "." | awk "{print \$0\".o \"}" | paste -sd "+")
comma=$(ls "../scheme/compiled" | grep -o "^.*\." | tr -d "." | awk "{print \$0\".o\"}" | paste -sd ",")

echo "MIMOSA_OBJ = $obj"
echo "MIMOSA_OBJ_PLUS =  +$plus"
echo "MIMOSA_OBJ_COMMA = ,$comma"
echo "MIMOSA_C = $c"


pobj=$(ls "../scheme/preload-compiled" | grep -o "^.*\." | tr -d "." | awk "{print \$0\".o\"}" | paste -sd " ")
pc=$(ls "../scheme/preload-compiled" | grep -o "^.*\." | tr -d "." | awk "{print \$0\".c\"}" | paste -sd " ")
pplus=$(ls "../scheme/preload-compiled" | grep -o "^.*\." | tr -d "." | awk "{print \$0\".o \"}" | paste -sd "+")
pcomma=$(ls "../scheme/preload-compiled" | grep -o "^.*\." | tr -d "." | awk "{print \$0\".o\"}" | paste -sd ",")

echo "PRE_MIMOSA_OBJ = $pobj"
echo "PRE_MIMOSA_OBJ_PLUS =  +$pplus"
echo "PRE_MIMOSA_OBJ_COMMA = ,$pcomma"
echo "PRE_MIMOSA_C = $pc"
