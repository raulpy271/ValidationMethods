# WFF Validation Methods in Haskell

This Module uses the **Semantic Tree** and **Reduction to Absurd** methods to test the validity of propositional logic formulas and shows how these methods work.

## How to use

To store a formula you should use the function `readWFF`<sup>1</sup>.

This function uses the following convention to read logical operations:
- "~" for The "not" operation
- "^" for The "and" operation
- "V" for The "or" operation
- ">" for The "imply" operation
- "=" for The "if and only if" operation

See the example storing the morgan law (` ¬(ϕ ∨ ψ) ↔ (¬ϕ ∧ ¬ψ) `):

```
*DataWFF.PropositionalCalculus> morganLaw = readWFF "( ~ (p V q) ) = ( ( ~ (p) ) ^ ( ~ (q) ) ) "
```

## Proofs examples

### Using Semantic Tree Method

This method places the formula in a tree to test its different interpretations (see the diagram). Each leaf represents the value of an interpretation. When all leaves have true value, the tested formula is a tautology, or when all leaves have false value then the formula is a contradiction.

![Diagram of semantic tree method](/SemanticTree/contraposition-law.png)

see other proof using this module:

#### Proving the law of distributivity

This code shows how the semantic tree method works (note that all interpretations have a value of 1, so it is a tautology) :

```

*SemanticTree.SemanticTree> distributivityLaw = readWFF "(((p) ^ (q)) V (r)) = (((p) V (r)) ^ ((q) V (r))) "

*SemanticTree.SemanticTree> (putStrLn . viewSemanticTreeProcess) distributivityLaw

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

```

### Proof by contradiction

The proof by contradiction work assuming that the value of a formula is false and checking for any logic contradiction. If so, then the formula is a tautology.

To check any occurrence of contradiction, this algorithm uses some rules of inference. if you want to see her, print the value of function `laws`.

#### Proving the *Modus Ponens* Rule

Using contradiction proof, this code proves the inference rule called modus ponens:

```

*ReductioAdAbsurdum.ReductioAdAbsurdum> modusPonens = readWFF  "(p ^ (p > q)) > q"
*ReductioAdAbsurdum.ReductioAdAbsurdum> (putStrLn . viewReductioAdAbsurdumProcess) modusPonens

Assuming that the formula is a contradiction we have:
((p) ^ ((p) > (q))) > (q)
  T  T      T       F  F 
Applying the result in the formula we have:
((1) ^ ((1) > (0))) > (0)
  T  T      T  T    F  F 
Two values were found for the preposition '0'.
Therefore, the initial formula is a tautology.

```

---

[1]: I know that instead of creating `readWFF` to read formulas I should create an instance of the Read class and then use the `read` function. But I didn't quite understand the methods of this class, so feel free to fix it.
