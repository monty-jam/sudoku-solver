## Sudoku solver

Napište sudoku solver. Na vstupu dostanete hrací desku s libovolným počtem předvyplněných buněk. Vaším úkolem je vyplnit Sudoku, pokud řešení existuje.

Vaše aplikace musí umět alespoň:

* Načíst a interně zareprezentovat sudoku.
* Najít alespoň jedno řešení (existuje-li) libovolně zadaného sudoku.
* ***Velikost hrací desky není omezená***

## Example usage

Pro spuštění solveru se stromovou rekurzí použijte funkci, která vratí seznam všech možných řešení:

`> (solve-sudoku-tree mtx1)`

Solver s koncovou rekurzí (pouze jedno řešení):

`> (solve-sudoku-tail mtx1)`

V programu připraveno 8 různých sudoku pro testování:

*  *mtx0* - empty 9x9 sudoku
*  *mtx1* - 9x9 sudoku
*  *mtx1-1* - 9x9 sudoku with many empty cells
*  *mtx2* - 4x4 sudoku
*  *mtx3* - 1x1 sudoku
*  *mtx4* - 1x1 sudoku without empty cells
*  *mtx5* - 16x16 sudoku
*  *mtx6* - 9x9 sudoku without a solution
