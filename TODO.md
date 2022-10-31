## What's to figure out

1. When to allocate a new page and when to gc
2. Write gc
3. Semispaces when using pages.
    - When one allocates a new page for the "from" space, 
      how can we be sure that after gc there will be space left
      in the "to" space.
4. Allocation strategy for the old space