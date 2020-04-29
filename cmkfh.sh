#!/bin/bash
# Create MakeFile Header
obj=$(ls "../scheme/compiled" | grep -o "^[[:alpha:]]*\." | tr -d "." | awk "{print \$0\".o\"}" | paste -sd " ")
c=$(ls "../scheme/compiled" | grep -o "^[[:alpha:]]*\." | tr -d "." | awk "{print \$0\".c\"}" | paste -sd " ")
plus=$(ls "../scheme/compiled" | grep -o "^[[:alpha:]]*\." | tr -d "." | awk "{print \$0\".o \"}" | paste -sd "+")
comma=$(ls "../scheme/compiled" | grep -o "^[[:alpha:]]*\." | tr -d "." | awk "{print \$0\".o\"}" | paste -sd ",")
echo "MIMOSA_OBJ = $obj"
echo "MIMOSA_OBJ_PLUS =  +$plus"
echo "MIMOSA_OBJ_COMMA = ,$comma"
echo "MIMOSA_C = $c"
