# Fill-In Puzzle Solver (Prolog)

This project provides a Prolog-based solver for Fill-In puzzles. 
Fill-In puzzles are logic word puzzles similar to crosswords, where words must be placed into a grid given a list of candidates and constraints like blocked cells or pre-filled letters. 
It uses constraint logic programming over finite domains (CLP(FD)) via SWI-Prolog's `clpfd` library.
This was written in 2021 as part of an assignment for a declarative programming subject.

## Features

- Supports solving puzzles with:
  - Solid (`#`) cells that cannot be filled
  - Pre-filled letters in the grid
  - Left-to-right and top-to-bottom word placement
- Uses domain labeling (`ffc`) to optimise the solution search
- Ensures each word from the wordlist is used exactly once

## How It Works

1. **Character-to-integer mapping** is used to allow efficient constraint solving via `clpfd`.
2. **Puzzle gaps** (horizontal and vertical slots between `#`s) are extracted and matched with the wordlist.
3. **Constraint satisfaction** is applied using `tuples_in/2` and `labeling/2`.
4. The puzzle is filled by unifying the matched solution with the input puzzle.

## Usage

You must have [SWI-Prolog](https://www.swi-prolog.org/) installed.

### Load the solver

```bash
swipl
?- [puzzle_solver].
```

### Solve a Puzzle

```prolog
?- Puzzle = [
       [_, _, _, _],
       [_, _, _, '#'],
       [_, _, _, _]
   ],
   WordList = [[b,o,a,t], [a,r,t], [n,e,e,d], [b,a,n], [o,r,e], [a,t,e]],
   puzzle_solution(Puzzle, WordList),
   maplist(writeln, Puzzle).
```

