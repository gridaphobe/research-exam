% Type-Targeted Testing
% Eric Seidel
% eseidel@cs.ucsd.edu

\newcommand{\ltup}[2]{\mathrm{({#1},{#2})}}
\newcommand{\lcons}[2]{\mathrm{{#1}:{#2}}}
\newcommand{\lnil}{\mathrm{[]}}
\newcommand{\imp}{\Rightarrow}
\newcommand{\xor}{\oplus}
\newcommand{\defeq}{\ \doteq\ }
\newcommand{\wedge}{\ \land\ }

\newcommand\val[1]{\sigma(x)}
\newcommand\cvar[1]{\mathit{{#1}}}
\newcommand\clen[1]{\cstr{len}\ {#1}}
\newcommand\cstr[1]{\mathsf{{#1}}}
\newcommand\ttrue{\cvar{true}}
\newcommand\tfalse{\cvar{false}}

\newcommand\meta[1]{[\![#1]\!]}
\newcommand\reft[3]{\{{#1}:{#2}\ |\ {#3}\}}

# Refinement Types

- `{v:t | p}`
    - the set of values `v` of type `t` satisfying a predicate `p`

```haskell
type Nat   = {v:Int | v >= 0}
type Pos   = {v:Int | v >  0}
type Rng N = {v:Int | v >= 0 && v < N}
```

- construct refinement types for containers and functions by refining type parameters

```haskell
f :: x:Nat -> {v:Nat | v = x + 1}
```

- type of functions that take a natural number and increment it by one

# Refinement Types

- traditionally used for program verification
- we show that refinement types *can also be viewed as exhaustive test-suite*
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

$\cstr{C_0} \defeq 0 \leq \cvar{r1} \wedge 0 \leq \cvar{r2} \wedge 0 \leq s < \cvar{r1}$

- model

$[\cvar{r1} \mapsto 1, \cvar{r2} \mapsto 1, \cvar{s} \mapsto 0]$

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

$\begin{aligned}
\cstr{C_{list}} & \defeq & (\cvar{c}_{00} \Rightarrow \cvar{xs}_0 = \lnil) \wedge 
                           (\cvar{c}_{01} \Rightarrow \cvar{xs}_0 = \lcons{\cvar{x}_1}{\cvar{xs}_1}) & \wedge &
                           (\cvar{c}_{00} \oplus \cvar{c}_{01}) \\
                & \wedge & (\cvar{c}_{10} \Rightarrow \cvar{xs}_1 = \lnil) \wedge
                           (\cvar{c}_{11} \Rightarrow \cvar{xs}_1 = \lcons{\cvar{x}_2}{\cvar{xs}_2}) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{c}_{10} \oplus \cvar{c}_{11}) \\
                & \wedge & (\cvar{c}_{20} \Rightarrow \cvar{xs}_2 = \lnil) \wedge 
                           (\cvar{c}_{21} \Rightarrow \cvar{xs}_2 = \lcons{\cvar{x}_3}{\cvar{xs}_3}) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{c}_{20} \oplus \cvar{c}_{21}) \\
                & \wedge & (\cvar{c}_{30} \Rightarrow \cvar{xs}_3 = \lnil) & \wedge &
                           (\cvar{c}_{21} \Rightarrow \cvar{c}_{30})
\end{aligned}$

$\begin{aligned}
\cstr{C_{data}} & \defeq & (\cvar{c}_{01} \Rightarrow \cvar{x}_1 = \ltup{\cvar{w}_1}{\cvar{s}_1} \ \wedge\ 0 < \cvar{w}_1 \ \wedge\ 0 \leq \cvar{s}_1 < 100) \\
                & \wedge & (\cvar{c}_{11} \Rightarrow \cvar{x}_2 = \ltup{\cvar{w}_2}{\cvar{s}_2} \ \wedge\ 0 < \cvar{w}_2 \ \wedge\ 0 \leq \cvar{s}_2 < 100) \\
                & \wedge & (\cvar{c}_{21} \Rightarrow \cvar{x}_3 = \ltup{\cvar{w}_3}{\cvar{s}_3} \ \wedge\ 0 < \cvar{w}_3 \ \wedge\ 0 \leq \cvar{s}_3 < 100)
\end{aligned}$

# Ordered Containers

```haskell
insert :: Ord a => a -> Sorted a -> Sorted a 

data Sorted a = [] 
              | (:) { h :: a 
                    , t :: Sorted {v:a | h < v}
                    }
```

$\begin{aligned}
\cstr{C_{ord}}   & \defeq & (\cvar{c}_{11} \Rightarrow \cvar{x}_1 < \cvar{x}_2)
                   \wedge   (\cvar{c}_{21} \Rightarrow \cvar{x}_2 < \cvar{x}_3\ \wedge\ \cvar{x}_1 < \cvar{x}_3)
\end{aligned}$

# Structured Containers

```haskell
best :: k:Nat -> {v:[Score] | k <= len v} 
     -> {v:[Score] | k = len v}
best k xs = take k $ reverse $ sort xs

measure len :: [a] -> Nat
len []      = 0
len (x:xs)  = 1 + len xs
```

$\begin{aligned}
\cstr{C_{size}} & \defeq & (\cvar{c}_{00} \Rightarrow \clen{\cvar{xs}_{0}} = 0) \wedge 
                           (\cvar{c}_{01} \Rightarrow \clen{\cvar{xs}_{0}} = 1 + \clen{\cvar{xs}_1}) \\
                & \wedge & (\cvar{c}_{10} \Rightarrow \clen{\cvar{xs}_{1}} = 0) \wedge 
                           (\cvar{c}_{11} \Rightarrow \clen{\cvar{xs}_{1}} = 1 + \clen{\cvar{xs}_2}) \\
                & \wedge & (\cvar{c}_{20} \Rightarrow \clen{\cvar{xs}_{2}} = 0) \wedge 
                           (\cvar{c}_{21} \Rightarrow \clen{\cvar{xs}_{2}} = 1 + \clen{\cvar{xs}_3}) \\
                & \wedge & (\cvar{c}_{30} \Rightarrow \clen{\cvar{xs}_{3}} = 0)
\end{aligned}$

# Evaluation

- compared Target against QuickCheck, SmallCheck, Lazy SmallCheck
    - `Data.Map`, `RBTree`, `XMonad.StackSet`
    - no custom generators

- `Data.Map`
    - checked balancing and ordering invariants

- `RBTree`
    - checked red-black and ordering invariants

- `XMonad.StackSet`
    - checked uniqueness of windows

# Evaluation
<img height=500px src="benchmarks.png">

# Takeaway
- Target can explore larger inputs than Lazy SmallCheck
- Target specs are amenable to future formal verification
- QuickCheck **requires** custom generators for functions with complex preconditions
