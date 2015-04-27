# racket

PPL Racket Assignment 2015
- Sudoku solver. Open matrix.rkt and enter (solve matrix) to run.

### NOTE this is a **partial** solution. Explanation below
-  The second step in the algorithm isnot working properly when integrated into the main program *"Find a number in a set that does not occur in any other set in the same row (or column, or box).Reduce that set to a singleton containing that one number.* 
- See **test-matrix.rkt** and * WORK IN PROGRESS* section at the bottom of the page.  These functions compare an element against a list and checks if any elements are not contained in the elements in the list. **This works correctly** but when merging it into the main solve function it is giving odd results.  
- The part of the solution is also quite verbose and needs refactoring and streamlining so I felt it was best to omit this from the official solution but show my working. 
