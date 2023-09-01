# COMPILER AND TYPE CHECKER WORKS: 
# files=(queens fun fact fact2 91 list list_foldr fact fact2 kfact lazy0)

# COMPILER WORKS:
# files=(nators)

# issue with mallocing inner function env in function implementation?
# files=(nested_let)

# some naming issue with lazy evaluation
files=(lazy)

for file in ${files[@]}
do 
    echo "/**********************/"
    echo TEST ${file}.dats:
    echo Interpreting:
    ./project ./TEST/${file}.dats ./RUNTIME/${file}
    gcc ./RUNTIME/${file}.c -O2 -o ./RUNTIME/${file}.out
    echo
    echo Compilation:
    ./RUNTIME/${file}.out
    # rm -f ./RUNTIME/${file}.c
    rm -f ./RUNTIME/${file}.out
    echo
done;
