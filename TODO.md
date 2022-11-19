## What's to figure out
1. Allocation strategy for the old space

## To move
1. Old space would be nice, not necessary yet.
2. Class universe aka vm runtime 
    - We need more classes, like int, bool, bytes and so on.
    - Also we somehow need to be able to search for classes.
    - And some classes won't be in the runtime. Nor in the permament heap
3. Test cases
4. Vm logic
    - Evaluation, dispatch loop and so on
5. Parser and lexer