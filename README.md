# racket

PPL Racket Assignment 2015
- Sudoku solver. Open matrix.rkt and enter (solve matrix) to run.

### NOTE this is a **partial** solution. Explanation below
-  The second step in the algorithm is not working properly when integrated into the main program *"Find a number in a set that does not occur in any other set in the same row (or column, or box).Reduce that set to a singleton containing that one number.* 
- See **test-matrix.rkt** and * WORK IN PROGRESS* section at the bottom of the page for this missing part of the program.  These functions compare an element against a given list and check if any elements are not contained in the elements in the list. **This works correctly** but when merging it into the main solve function it is giving odd results.  Try running it (solve matrix) and you will get one error in an otherwise solved sudoku.  You will see the principle is correct.
- The part of the solution is also quite verbose and needs refactoring / streamlining so I felt it was best to omit this from the official solution but show my working.
