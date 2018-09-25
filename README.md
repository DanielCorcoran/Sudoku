# Sudoku
This program is a sudoku solver, implemented in Haskell, that takes in a list of lists as a sudoku board and returns a list of lists as a solved board.  Each list represents a box of the puzzle and the list fills in a box left to right, top to bottom.  0s are used for blank spaces.  For instance,

[[0,0,0,1,0,2,0,0,5],[0,0,7,0,0,5,0,4,0],[5,0,8,4,6,3,0,0,0],  
[0,8,0,9,0,0,6,7,3],[0,4,0,0,0,0,0,2,0],[2,6,3,0,0,7,0,1,0],  
[0,0,0,7,4,6,3,0,1],[0,3,0,2,0,0,4,0,0],[6,0,0,3,0,8,0,0,0]]

is a valid input for a board.  Any board with a unique solution that is correctly entered into the program can be solved with the solve function.  There are 4 boards that are stored in the program: easy, medium, hard, and evil.  They can be solved with

solve easy  
solve medium  
solve hard  
solve evil  

There is a main function that can be called that will print out all 4 boards and their solutions.
