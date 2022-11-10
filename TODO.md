## What's to figure out
1. Write gc
2. Remember to reset pages after gc
3. Allocation strategy for the old space

## To move
1. GC
2. Old space would be nice, not necessary yet.
3. Class universe aka vm runtime 
    - We need more classes, like int, bool, bytes and so on.
    - Also we somehow need to be able to search for classes.
    - And some classes won't be in the runtime. Nor in the permament heap
4. Test cases
5. Vm logic
    - Evaluation, dispatch loop and so on
6. Parser and lexer