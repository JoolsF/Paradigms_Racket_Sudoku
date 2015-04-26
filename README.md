# racket

PPL Racket Assignment 2015
- Sudoku solver. Enter (solve matrix) to run.

### NOTE this is a **partial** solution. Explanation below
-  The second step in the algorithm isnot working properly when integrated into the main program *"Find a number in a set that does not occur in any other set in the same row (or column, or box).Reduce that set to a singleton containing that one number.* 
- See ;;;;;NOT USED;;;;; comment at the bottom for *(only-possible-no lsts lstcompare)* function which compares an element against a list and checks if any elements are not contained in the elements in the list. **This works correctly** but when merging it into main program it is giving odd results.
