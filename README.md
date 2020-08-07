# WFF Validation Methods in Haskell

Module that has methods for validating propositional logic formulas.

## How to use

To store a formula you should use the function `readWFF`<sup>1</sup>.

This function uses the following convention to read logical operations:
- "~" for The "not" operation
- "^" for The "and" operation
- "V" for The "or" operation
- ">" for The "imply" operation
- "=" for "The if and only if" operation

See the exemple storing the morgan law (` ¬(ϕ ∨ ψ) ↔ (¬ϕ ∧ ¬ψ) `):

```
*DataWFF.PropositionalCalculus> morganLaw = readWFF "( ~ (p V q) ) = ( ( ~ (p) ) ^ ( ~ (q) ) ) "
```

## Proofs examples

### Proving the law of distributivity

This code shows how the semantic tree method works (note that all interpretations have a value of 1, so it is a tautology) :

```

*SemanticTree.SemanticTree> distributivityLaw = readWFF "(((p) ^ (q)) V (r)) = (((p) V (r)) ^ ((q) V (r))) "

*SemanticTree.SemanticTree> (putStrLn . viewSemanticTreeProcess . readWFF) distributivityLaw

(((p) ^ (q)) V (r)) = (((p) V (r)) ^ ((q) V (r)))
    (((1) ^ (q)) V (r)) = (((1) V (r)) ^ ((q) V (r)))
        (((1) ^ (1)) V (r)) = (((1) V (r)) ^ ((1) V (r))), interpretation: 1
        (((1) ^ (0)) V (r)) = (((1) V (r)) ^ ((0) V (r)))
            (((1) ^ (0)) V (1)) = (((1) V (1)) ^ ((0) V (1))), interpretation: 1
            (((1) ^ (0)) V (0)) = (((1) V (0)) ^ ((0) V (0))), interpretation: 1
    (((0) ^ (q)) V (r)) = (((0) V (r)) ^ ((q) V (r)))
        (((0) ^ (1)) V (r)) = (((0) V (r)) ^ ((1) V (r)))
            (((0) ^ (1)) V (1)) = (((0) V (1)) ^ ((1) V (1))), interpretation: 1
            (((0) ^ (1)) V (0)) = (((0) V (0)) ^ ((1) V (0))), interpretation: 1
        (((0) ^ (0)) V (r)) = (((0) V (r)) ^ ((0) V (r)))
            (((0) ^ (0)) V (1)) = (((0) V (1)) ^ ((0) V (1))), interpretation: 1
            (((0) ^ (0)) V (0)) = (((0) V (0)) ^ ((0) V (0))), interpretation: 1


*SemanticTree.SemanticTree> isTautology distributivityLaw

Tautology
```

***

[1]: I know that instead of creating `readWFF` to read formulas I should create an instance of the Read class and then use the `read` function. but I didn't quite understand the methods of this class, so feel free to fix it.
