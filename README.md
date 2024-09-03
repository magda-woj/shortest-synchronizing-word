# DFA Synchronizing Word Finder

## Description

This program checks if a given deterministic finite automaton (DFA) has a synchronizing word, and if so, it outputs the shortest such word. A synchronizing word is a sequence of input symbols that, when applied to any state of the DFA, leads to a specific state. If the DFA is not synchronizing, the program outputs the string `"automaton isn't synchronizing"`.

## Input File Format

The input file must describe a valid DFA and follow this format:

1. **First Line**: An integer `n` representing the number of states in the DFA.
2. **Second Line**: A string of `k` symbols representing the alphabet, with all symbols listed consecutively without spaces.
3. **Third Line**: A description of the transition function in the following format:
   - `[list0, list1, ... listn-1]`, where each `listi` is a `k`-element list of tuples `(c, m)`.
   - `c` is a `Char` representing a symbol from the alphabet, and `m` is an integer representing the target state (0 to `n-1`).
   - This means that the transition function sends state `i` to state `m` using symbol `c` (i.e., `transitionFunction(i, c) = m`).

## Example Input

3 ab [[('a', 1), ('b',1)], [('a', 1), ('b', 2)], [('a', 0), ('b', 1)]]


### Explanation:
- The DFA has 3 states (`0`, `1`, `2`).
- The alphabet consists of two symbols: `a` and `b`.
- The transition function is defined as:
  - From state `0`: `a` leads to `1`, `b` leads to `1`
  - From state `1`: `a` leads to `1`, `b` leads to `2`
  - From state `2`: `a` leads to `0`, `b` leads to `1`

## Output

- If the DFA has a synchronizing word, the program prints the shortest synchronizing word.
- If the DFA does not have a synchronizing word, the program prints `"automaton isn't synchronizing"`.

## Notes

- The `main` function and file handling assume that states are numbered from `0` to `n-1`.
- However, the functions implemented to find the shortest synchronizing word only require that the type representing states is comparable.


