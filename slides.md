% Type-Targeted Testing
% Eric Seidel

# Refinement Types

- `{v:T | p}`
- traditionally used for program verification
- we show that refinement types can also be viewed as exhaustive test-suite
- allows for *gradual verification*
    1. write high-level spec as refinement type
    2. immediate gratification from comprehensive test-suite
    3. once design has settled, add hints / inductive invariants to allow verification

# Target

- generates tests from refinement types via *query-decode-check* loop

    1. translate input types into SMT **query**
    2. **decode** SMT model into concrete values
    3. run function and **check** that result inhabits output type

- exhaustively checks all inputs up to a given depth-bound

# Primitive Types

```haskell
rescale :: r1:Nat -> r2:Nat -> s:Rng r1 -> Rng r2
rescale r1 r2 s = s * (r2 `div` r1)
```

- input constraint

$C_0 = 0 \leq r1 \wedge 0 \leq r2 \wedge 0 \leq s < r1$

- model

$[r1 \mapsto 1, r2 \mapsto 0, s \mapsto 0]$

- concrete input:

`rescale 1 0 0 == 0`

- fails postcondition check

$0 < 0$ ??

```haskell
rescale :: r1:Pos -> r2:Pos -> s:Rng r1 -> Rng r2
rescale r1 r2 s = s * (r2 `div` r1)
```

# Containers

```haskell
type Score = Rng 100

average :: [(Pos, Score)] -> Score
average []  = 0
average wxs = total `div` n
  where
    total   = sum [w * x | (w, x) <- wxs ]
    n       = sum [w     | (w, _) <- wxs ]
```

# Ordered Containers

```haskell
insert :: Ord a => a -> Sorted a -> Sorted a 

data Sorted a = [] 
              | (:) { h :: a 
                    , t :: Sorted {v:a | h < v}
                    }
```

# Structured Containers

```haskell
best :: k:Nat -> {v:[Score] | k <= len v} 
     -> {v:[Score] | k = len v}
best k xs = take k $ reverse $ sort xs

measure len :: [a] -> Nat
len []      = 0
len (x:xs)  = 1 + len xs
```

# Evaluation

- compared Target against QuickCheck, SmallCheck, Lazy SmallCheck
    - `Data.Map`, `RBTree`, `XMonad.StackSet`
    - no custom generators

- \<insert graphs\>

- takeaway
    - QuickCheck **requires** custom generators for functions with complex preconditions
    - Target can explore larger inputs than Lazy SmallCheck
    - furthermore, Target specs are amenable to future formal verification
