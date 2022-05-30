# Transfinite Lists

An "efficient" implementation of transfinite lists. Implemented in Racket for aesthetic reasons.

Explanation: https://medium.com/@thealexvarga/efficient-transfinite-lists-b27323941419

### Usage

After running `main.rkt`, execute `neato out/*.dot -n -Tpng -O` to draw the output files.

### Known issues

- `put` assumes its argument is `w^a` for some `a`
- `get` assumes its argument is in the list
