#!/usr/bin/env swipl
% Author: Han Perry
% Purpose: Solves Fill-in puzzles
%
% A Fill-in puzzle consists of a grid where each square can initially be blank, 
% filled in solid, or pre-filled with a letter, and a list of words which are 
% to be placed into the grid. Each word in the wordlist is placed exactly once 
% in either left-to-right or top-to-bottom direction. This file provides the 
% predicate puzzle_solution/2 which can be used to solve a Fill-in puzzle, 
% given a solution exists. It is implemented using clpfd library to search the 
% solution space in the order of labeling the variable with smallest domain 
% which appears in the most constraints first (ffc) (when a unique solution 
% isn't already found).

:- ensure_loaded(library(clpfd)).
:- ensure_loaded(library(lists)).

%% puzzle_solution(+Puzzle, +Wordlist)
%
% Solves the Fill-in puzzle inputed according to the given words in Wordlist 
% and puzzle structure represented by Puzzle. Puzzle is a list of same-lengthed 
% lists containing `'#'` to represent a solid, unfillable square, a single lower
% case letter to denote that the square is filled, or an underscore variable for
% a fillable square. Wordlist is a ground list of lists containing letters or 
% characters that spell out the words to appear in the solved puzzle. After 
% calling puzzle_solution, Puzzle will be bound to a completed solution if one 
% exists. In implementation, all non-hash characters are mapped to integer 
% values to allow compatibility with prolog's built-in clpfd library, and
% mapped back to original characters when being unified with the input Puzzle.
% The labeling is done using the ffc setting, so when a solution is not already 
% determined, the variable chosen to be unified first is the one with the 
% smallest domain (most restricted) that appears in the most constraints.

puzzle_solution(Puzzle, WordList) :-
    % translate the letters into integers
    make_mapping(WordList, Mapping),
    maplist(translate(Mapping), WordList, WordListT),
    % split the puzzle into lists of 'gaps' (lists of variables and prefilled
    % letters) of a similar format to WordList
    make_gaps(Puzzle, WordGaps0),
    % duplicate list of lists of variables to unify as integers
    copy_term(WordGaps0, WordGaps1),
    % translate any letters in the list of gaps to integers
    maplist(translate(Mapping), WordGaps1, WordGaps),
    % match the words and gaps
    tuples_in(WordGaps, WordListT),
    maplist(labeling([ffc]), WordGaps),
    % ensure all words are matched up with a single gap
    msort(WordListT, Sorted),
    msort(WordGaps, Sorted),
    % translate back to letters
    transpose_pairs(Mapping, MappingR),
    maplist(translate(MappingR), WordGaps, WordGaps0).

%% make_mapping(+WordList, -Mapping)
%
% Makes Mapping, a list of pairs of the form character-integer to be used to 
% translate the characters to integers. WordList is a list of lists of 
% characters

make_mapping(WordList, Mapping) :-
    append(WordList, Letters0),
    list_to_set(Letters0, Letters),
    enumerate(Letters, Mapping).

%% make_gaps(+Puzzle, -WordGaps)
%
% Splits the puzzle into lists of 'gaps' (lists of variables and prefilled
% letters) which will be a similar format to WordList. Includes both horizontal 
% and vertical gaps/slots in the puzzle

make_gaps(Puzzle, WordGaps) :-
    maplist(split_at_hash, Puzzle, Horizontals0),
    append(Horizontals0, Horizontals),
    transpose(Puzzle, PuzzleT),
    maplist(split_at_hash, PuzzleT, Verticals0),
    append(Verticals0, Verticals),
    append(Horizontals, Verticals, WordGaps).

%% enumerate(+Ls, -Enum)
%
% enumerates the elements from 1 to the length of Ls. Ls should contain unique 
% letters if using this as a way to map letters to integers. Enum is a pairs 
% list of element and integer.

enumerate(Ls, Enum) :-
    enumerate(Ls, Enum, 0, []).

%% enumerate(+Ls, -Enum, +N, +Part)
%
% Helper predicate for enumerate/2

enumerate([], EnumR, _, Enum) :-
    reverse(Enum, EnumR).
enumerate([L|Ls], Enum, N, Part) :-
    N1 is N + 1,
    enumerate(Ls, Enum, N1, [L-N1|Part]).

%% translate(+Pairs, +Original, -Translation)
%
% Replaces each element in Original list with corresponding value in pair, 
% Translation is the translated list.

translate(Pairs, Original, Translation) :-
    translate(Original, Translation, Pairs, []).

%% translate(+Original, -Translation, +Pairs, +Part)
%
% Helper predicate for translate/3

translate([], TranslationR, _, Translation) :-
    reverse(Translation, TranslationR).
translate([L|Original], Translation, Pairs, Part) :-
    (   var(L) ->
        translate(Original, Translation, Pairs, [L|Part])
    ;   member(L-T, Pairs),
        translate(Original, Translation, Pairs, [T|Part])
    ).

%% split_at_hash(+List, -Result)
%
% Split List into list of sub-lists (Result) between each hash symbol `'#'`
% and removes the hashes, ignores sub-lists of single letters, single 
% underscores or empty.

split_at_hash(List, Result) :-
    split_at_hash(List, [], Result, []).

%% split_at_hash(+List, +Result0, -Result, +Word)
%
% Helper predicate for split_at_hash/2

split_at_hash([], Result0, Result, Word) :-
    (   (length(Word, L), L > 1) ->
        reverse(Word, Word0),
        append(Result0, [Word0], Result)
    ;   Result0 = Result
    ).
split_at_hash([C|List], Result0, Result, Word) :-
    (   (var(C) ; C \= '#') ->
        split_at_hash(List, Result0, Result, [C|Word])
    ;   (length(Word, L), L > 1) ->
        reverse(Word, Word0),
        append(Result0, [Word0], Result1),
        split_at_hash(List, Result1, Result, [])
    ;   split_at_hash(List, Result0, Result, [])
    ).

