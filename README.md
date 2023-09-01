# Final
## Updates
`project.sats`: I updated datatypes `t1ype`, `t1val`, and `t2cmp` to handle streams and lazy evaluation. I also edited `t2ins` to handle closure environments.\

## Interpreter
I edited the interpreter so that it can handle streams and lazy evaluation. I also fixed the mistakes I made in `T1Mseq` and the list operations from the midterm. 

## Type Checking and Type Inference
I followed the structure of the type checker in assignment 4 and extended the relevant functions (`tpVar_occurs_t1ype`, `t1ype_unify`, `t1erm_oftype1`, `t1erm_oftype1_opr`) to handle the new term constructors and operations. 

## A-Normal Form
I added some functions to extend the closure environment (`t3env_extend`, `t3env_extend1`). I also added two utility functions to hold a counter for lazy functions and environment declarations (`t2lzy_new`, `t2env_new`). 

## Emitting `C` Code
I added a function to implement lazy functions: `emit2_lzy`. 

I also added functions for declaring environments. `emit2_env_dec`. Then before I call a function, I make sure that the environment is initialized using `emit2_env_imp` and `handle_env`. It is possible for a function to call another function multiple times using the same environment. To handle this situation, I use the `find_env` and `reset_env_list` functions. If I find that the environment is already implemented in the function, then I don't initialize the environment. When implementing a new function, I call `reset_env_list` to reset the list of implemented environments. 

The structure of my `C` emitter is still similar to the midterm. I just extended most of the functions to contain a closure environment as part of the argument. 

I started the implementation of `t2env_emit0` by including the `runtime.h`. Then, I execute `emit2_fun_dec` for declaring all the functions. Next, I do the implementation of the functions in `emit2_fun_imp`. Finally, I implement everything else wrapped around a `main` function in `emit2_main`.\
For the function implementations (including `main`), I declare all the registers at the beginning in `emit2_reg_dec`.\
Then I created functions for emitting code for each datatype, with an additional one for `t2bnds` in the `T2Iif0` case (`emit2_bnds_for_if0`) to obtain the register value that's returned. 

### Runtime
I extended `runtime.h` so that every function has a closure environment. 

## Testing
I did my testing on the files in the directory `./TEST`. Everything can be tested by using `script.sh`.
