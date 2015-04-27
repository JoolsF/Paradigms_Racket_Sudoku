# racket

PPL Racket Assignment 2015
- Sudoku solver. Open matrix.rkt and enter (solve matrix) to run.

### NOTE this is a **partial** solution. Explanation below
-  The second step in the algorithm is not working properly when integrated into the main program *"Find a number in a set that does not occur in any other set in the same row (or column, or box).Reduce that set to a singleton containing that one number.* The solution has therefore been ommited from matrix.rkt.
- See **test-matrix.rkt** and * WORK IN PROGRESS* section at the bottom of the page for this missing part of the program.  These functions compare an element against a given list and check if any elements are not contained in the elements in the list. **This works correctly** but when merging it into the main solve function it is giving odd results.  
- Try running (solve matrix) in test-matrix.rkt and you will get one error in an otherwise solved sudoku.  It will give you an idea of where I was going with it.  Obviously I'd also need to run this on rows and grid squares, this part of the program just runs on columns in the test version for illustration purposes.
