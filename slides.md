% Automated Specification-Based Testing
% Eric Seidel
% eseidel@cs.ucsd.edu

\newcommand{\lnode}[3]{\mathrm{Node\ {#1}\ {#2}\ {#3}}}
\newcommand{\lleaf}{\mathrm{Leaf}}
\newcommand{\ltup}[2]{\mathrm{({#1},{#2})}}
\newcommand{\lcons}[2]{\mathrm{{#1}:{#2}}}
\newcommand{\lnil}{\mathrm{[]}}
\newcommand{\imp}{\Rightarrow}
\newcommand{\xor}{\oplus}
\newcommand{\defeq}{\ \doteq\ }
\newcommand{\wedge}{\ \land\ }

\newcommand\val[1]{\sigma(x)}
\newcommand\cvar[1]{\mathrm{{#1}}}
\newcommand\clen[1]{\cstr{len}\ {#1}}
\newcommand\cstr[1]{\mathsf{{#1}}}
\newcommand\ttrue{\cstr{true}}
\newcommand\tfalse{\cstr{false}}

\newcommand\meta[1]{[\![#1]\!]}
\newcommand\reft[3]{\{{#1}:{#2}\ |\ {#3}\}}

# A Binary Search Tree Library

```haskell
data Tree
  = Leaf
  | Node Int Tree Tree
```

```haskell
insert :: Int -> Tree -> Tree
delete :: Int -> Tree -> Tree
```

. . .

> Did I get it "right"?

# Testing

Two key questions to answer when testing:

1. How to **provide** inputs?
2. How to **check** outputs?

<!-- # Outline -->

<!-- 1. **Human-generated tests** -->
<!-- 2. Machine-enumerated inputs -->
<!-- 3. Dynamic-Symbolic Execution -->
<!-- 4. Type-targeted testing -->

# Standard Practice: Unit-Testing

Programmer specifies inputs *and* outputs

. . .

```haskell
assertEquals (insert 1 Leaf) (Node 1 Leaf Leaf)

assertEquals (insert 1 (Node 2 Leaf Leaf)) (Node 1 Leaf (Node 2 Leaf Leaf))
```

. . .

A lot of effort to produce a "complete" test-suite!

<!-- But this is tiresome... -->

<!-- . . . -->

<!-- > hope that these tests generalize! -->

# This Talk

#### Techniques for Automatic Unit-Testing

Existing Techniques

1. **Black-box testing**
2. White-box testing

Our contribution

3. Type-targeted testing

# Black-box testing

Given a **specification** of expected behavior, but no knowledge of internals

Generate many inputs and validate against spec

. . .

1. How to **provide** inputs?
    - Machine enumerates based on **specification**
2. How to **check** outputs?
    - Programmer supplies **oracle**

<!-- . . . -->

<!-- 1 is harder, start with 2 -->

<!-- - Machine enumerates many inputs -->
<!-- - Programmer specifies oracle to check outputs -->

<!-- . . . -->

<!-- MOVE THIS LATER, SHOW iSBST -->
<!-- ```haskell -->
<!-- prop_insert_elem x t = x `elem` insert x t -->
<!-- prop_insert_bst  x t = isBST (insert x t) -->
<!-- ``` -->

# Checking Outputs with Oracles

```haskell
isBST t = case t of
  Leaf -> True
  Node y l r -> abs (height l - height r) <= 1
             && all (< y) l && all (> y) r
             && isBST l     && isBST r
```

`isBST` **checks** whether a tree satisfies the balancing and ordering invariants

# Providing Inputs by Enumeration

**Small-scope hypothesis**: if a bug exists, a "small" input will probably trigger it

- TestEra (2001), Korat (2004), SmallCheck (2008)

# SmallCheck

<!-- ```haskell -->
<!-- data Tree -->
<!--   = Leaf -->
<!--   | Node Int Tree Tree -->

<!-- instance Serial Tree where -->
<!--   series = cons0 Leaf \/ cons3 Node -->
<!-- ``` -->

<!-- . . . -->

<!-- ENUMERAUTE ALL SMALL INPUTS -->

Given a **property**, SmallCheck enumerates all "small" inputs and runs the property

<!-- WHAT ARE INPUTS AND OUTPUTS -->
. . .

```haskell
prop_insert_bst :: Int -> Tree -> Bool
prop_insert_bst x t = isBST (insert x t)
```

`prop_insert_bst` states that `insert` should return a **valid** tree

. . .

<!-- HOW TO RUN IT -->
```haskell
>>> smallCheck 3 prop_insert_bst
```

Test all integers in range `[-3,3]` and all trees of depth 3

. . .

<!-- WHAT IS OUTPUT -->
```haskell
Failed test no. 4.
there exist 0, Node 0 Leaf (Node 0 Leaf Leaf) such that
  condition is false
```

4th test produces **invalid** tree as output

. . .

Property does not hold for **all** trees!

# SmallCheck: Adding Preconditions

Given a **property**, SmallCheck enumerates all "small" inputs and runs the property

```haskell
prop_insert_bst :: Int -> Tree -> Bool
prop_insert_bst x t = isBST t ==> isBST (insert x t)
```

**If** the input tree is valid, **then** the output tree should be valid

<!-- HOW TO RUN IT -->
```haskell
>>> smallCheck 3 prop_insert_bst
```

Test all integers in range `[-3,3]` and all trees of depth 3

. . .

```haskell
Completed 567 tests without failure.
But 434 did not meet ==> condition.
```

Only 133 input trees were valid!

# SmallCheck: How small?

Given a **property**, SmallCheck enumerates all "small" inputs and runs the property

```haskell
prop_insert_bst :: Int -> Tree -> Bool
prop_insert_bst x t = isBST t ==> isBST (insert x t)
```

**If** the input tree is valid, **then** the output tree should be valid

<!-- HOW TO RUN IT -->
```haskell
>>> smallCheck 4 prop_insert_bst
```

Test all integers in range `[-4,4]` and all trees of depth 4

<!-- # SmallCheck: How small? -->

. . .

```haskell
..........................................................
```

. . .

Exponential blowup in input space confines search to **very small** inputs!

<!-- (Again, custom generators are a standard solution to increase feasible search depth) -->

. . .

- Heuristics to prune "equivalent" inputs (Lazy SmallCheck, Korat)
- Can be brittle in practice
    <!-- - but must be careful how you structure precondition -->
    <!-- - e.g. should binary-search tree check ordering or balancing first? -->

<!-- POP BACK TO BLACK-BOX, STRIKEOUT ALL, REPLACE WITH RANDOM -->

# Black-box testing

Given a **specification** of expected behavior, but no knowledge of internals

Generate many inputs and validate against spec

1. How to **provide** inputs?
    - Machine ~~enumerates~~ **randomly samples** based on **specification**
2. How to **check** outputs?
    - Programmer supplies **oracle**

# Providing Inputs by Random Sampling

- Choose **random** inputs from **entire domain**
- Enables checking larger inputs
- No guarantee of minimal counterexample
- QuickCheck (2000), JCrasher (2004), Randoop (2007)

<!-- # QuickCheck -->

<!-- - provides DSL for writing random value generators -->

<!-- <\!-- ```haskell -\-> -->
<!-- <\!-- instance Arbitrary Tree where -\-> -->
<!-- <\!--   arbitrary = oneof [ leaf, node ] -\-> -->
<!-- <\!--     where -\-> -->
<!-- <\!--     leaf = return Leaf -\-> -->
<!-- <\!--     node = do x <- arbitrary -\-> -->
<!-- <\!--               l <- arbitrary -\-> -->
<!-- <\!--               r <- arbitrary -\-> -->
<!-- <\!--               return (Node x l r) -\-> -->
<!-- <\!-- ``` -\-> -->

<!-- - properties specified as with SmallCheck -->

# QuickCheck

Given a property, QuickCheck samples random inputs and runs the property

```haskell
prop_insert_bst x t
  = isBST t ==> isBST (insert x t)
```

If the input tree is valid, then the output tree should be valid

. . .

```haskell
>>> quickCheck prop_insert_bst
```

Test `prop_insert_bst` on 100 random, **valid** inputs

. . .

```haskell
+++ OK, passed 100 tests.
```

How is this possible? SmallCheck showed that input domain is *very* sparse!

# QuickCheck: With Statistics

Given a property, QuickCheck samples random inputs and runs the property

```haskell
prop_insert_bst x t
  = isBST t ==> isBST (insert x t)
```

If the input tree is valid, then the output tree should be valid

```haskell
>>> quickCheck prop_insert_bst
```

Test `prop_insert_bst` on 100 random, valid inputs, while collecting statistics

<!-- # QuickCheck: Testing `insert` -->

<!-- ```haskell -->
<!-- prop_insert_bst x t -->
<!--   = isBST t ==> collect (size t) (isBST (insert x t)) -->
<!-- ``` -->

. . .

```haskell
+++ OK, passed 100 tests:
73% 0
21% 1
 6% 2
```

73% of the trees were **empty** and 21% had only one element

# QuickCheck: With Non-Trivial Inputs

Given a property, QuickCheck samples random inputs and runs the property

```haskell
prop_insert_bst x t
  = isBST t && size t > 1 ==> isBST (insert x t)
```

If the input tree is valid **and** has more than one element, then the output tree should be valid

```haskell
>>> quickCheck prop_insert_bst
```

Test `prop_insert_bst` on 100 random, valid inputs

. . .

```haskell
*** Gave up! Passed only 37 tests.
```

Less than 1/10 generated trees were valid

. . .

Input domain is too sparse, QuickCheck cannot generate trees with more than 2 elements!

# QuickCheck: Custom Generators

```haskell
newtype BST = Tree

instance Arbitrary BST where
  arbitrary = ...

prop_insert_bst x (BST xs)
  = isBST (insert x xs)
```

# QuickCheck: Custom Generators

```haskell
newtype BST = Tree

instance Arbitrary BST where
  arbitrary = ...

prop_insert_bst x (BST xs)
  = isBST (insert x xs)
```

**Problem**

1. How to generate random **valid** trees?
2. Are we sampling from uniform distribution?
3. Must define a new type/generator for **each** precondition!

# Black-box testing

Given a **specification** of expected behavior, but no knowledge of internals

Generate many inputs and validate against spec

1. How to **provide** inputs?
    - Machine enumerates (or samples) based on **specification**
2. How to **check** outputs?
    - Programmer supplies **oracle**


# Black-box testing

Given a **specification** of expected behavior, but no knowledge of internals

Generate many inputs and validate against spec

1. How to **provide** inputs?
    - Machine enumerates (or samples) based on **specification**
2. How to **check** outputs?
    - Programmer supplies **oracle**

## Problems

- Brute-force enumeration of inputs suffers from input explosion
<!-- - Random generation enables testing larger inputs -->
<!-- - Sampling from a **uniform** distribution provides better case for generalizing outcome -->
- Random sampling requires custom generators for preconditions

# This Talk

#### Techniques for Automatic Unit-Testing

Existing Techniques

1. Black-box testing
2. **White-box testing**

Our contribution

3. Type-targeted testing

# White-Box Testing

- Given program **implementation**, make it **crash**
- Enumerating program paths instead of inputs
- **Symbolic execution** groups equivalent inputs
<!-- - aim for 100% coverage as quickly as possible -->

<!-- # Dynamic-Symbolic Testing -->

<!-- - introduced by Godefroid et al and Cadar et al in 2005 -->

<!-- - combines symbolic execution to enumerate code paths with concrete execution to trigger bugs -->

<!-- - search for inputs that make the program crash -->

# White-Box Testing Via Symbolic Execution

- Originally envisioned as static-analysis technique
- Program memory $M$ maps variables to symbolic expressions
- Construct **path condition** $P$ describing constraints to trigger current path

```haskell
f x y
  = let z = y + 1
    in if z > 0
       then assert (z!=0)
       else True
```

# White-Box Testing Via Symbolic Execution

- Originally envisioned as static-analysis technique
- Program memory $M$ maps variables to symbolic expressions
- Construct **path condition** $P$ describing constraints to trigger current path

```haskell
f x y                     -- 0
  = let z = y + 1
    in if z > 0
       then assert (z!=0)
       else True
```

$M_0 = \{x \mapsto \alpha_1, y \mapsto \alpha_2\}$

$P_0 = \langle \rangle$

# White-Box Testing Via Symbolic Execution

- Originally envisioned as static-analysis technique
- Program memory $M$ maps variables to symbolic expressions
- Construct **path condition** $P$ describing constraints to trigger current path

```haskell
f x y                     -- 0
  = let z = y + 1         -- 1
    in if z > 0
       then assert (z!=0)
       else True
```

$M_1 = \{x \mapsto \alpha_1, y \mapsto \alpha_2, z \mapsto (\alpha_2 + 1)\}$

$P_1 = \langle \rangle$

# White-Box Testing Via Symbolic Execution

- Originally envisioned as static-analysis technique
- Program memory $M$ maps variables to symbolic expressions
- Construct **path condition** $P$ describing constraints to trigger current path

```haskell
f x y                     -- 0
  = let z = y + 1         -- 1
    in if z > 0           -- 2
       then assert (z!=0)
       else True
```

$M_2 = \{x \mapsto \alpha_1, y \mapsto \alpha_2, z \mapsto (\alpha_2 + 1)\}$

$P_2 = \langle \rangle$

# White-Box Testing Via Symbolic Execution

- Originally envisioned as static-analysis technique
- Program memory $M$ maps variables to symbolic expressions
- Construct **path condition** $P$ describing constraints to trigger current path

```haskell
f x y                     -- 0
  = let z = y + 1         -- 1
    in if z > 0           -- 2
       then assert (z!=0) -- 3
       else True
```

$M_3 = \{x \mapsto \alpha_1, y \mapsto \alpha_2, z \mapsto (\alpha_2 + 1)\}$

$P_3 = \langle z > 0 \rangle$

. . .

**Implicit** branch condition `z==0` would trigger assertion failure

. . .

Check satisfiability of:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;$M_3 \land P_3 \land z = 0$

# White-Box Testing Via Symbolic Execution

- Originally envisioned as static-analysis technique
- Program memory $M$ maps variables to symbolic expressions
- Construct **path condition** $P$ describing constraints to trigger current path

```haskell
f x y                     -- 0
  = let z = y + 1         -- 1
    in if z > 0           -- 2
       then assert (z!=0) -- 3
       else True
```

$M_3 = \{x \mapsto \alpha_1, y \mapsto \alpha_2, z \mapsto (\alpha_2 + 1)\}$

$P_3 = \langle z > 0 \rangle$

**Implicit** branch condition `z==0` would trigger assertion failure

Check satisfiability of:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;$M_3 \land P_3 \land z = 0$&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**UNSAT**

. . .

Assertion cannot fail!

# The Problem With Symbolic Execution

1. Relies on constraint solver to reason about path feasibility
    - Many programs are difficult to express in solver's logic
2. Path-explosion on real-world programs

# Dynamic-Symbolic Execution

- Combine symbolic and concrete execution
- Fall back on **concrete** value when symbolic execution fails
- DART (2005), CUTE (2006), EXE (2006), PEX (2008), KLEE (2008)

# Dynamic-Symbolic Execution

- Combine symbolic and concrete execution
- Fall back on **concrete** value when symbolic execution fails
- DART (2005), CUTE (2006), EXE (2006), PEX (2008), KLEE (2008)
- Start with random inputs, e.g. $\{x = 1, t = \cstr{Node}\ 2\ \cstr{Leaf}\ \cstr{Leaf}\}$

```haskell
insert x t = case t of
  Leaf       -> singleton x
  Node y l r
    | x <  y -> bal y (insert x l) r
    | x >  y -> bal y l (insert x r)
    | x == y -> t
```

> - At `LT` branch, we have $P_{LT} = \langle t = \cstr{Node}\ y\ l\ r, x < y \rangle$
> - Choose new path by negating path condition and solving for new inputs, e.g. $t = \cstr{Node}\ y\ l\ r \land \lnot (x < y)$
> - Many more sophisticated search techniques have been explored

# Dynamic-Symbolic Execution: Specifications

`insert` will never crash on its own, need to check specification

```haskell
prop_insert_bst x t =
  if isBST t
  then assert (isBST (insert x t))
  else True
```

. . .

**Problem**: paths must pass through `isBST` before reaching `insert`!

<!-- # Dynamic-Symbolic Execution: Preconditions -->

```haskell
isBST t = case t of
  Leaf -> True
  Node y l r -> abs (height l - height r) <= 1
             && all (< y) l && all (> y) r
             && isBST l     && isBST r
```

# Dynamic-Symbolic Execution: Preconditions

```haskell
isBST t = case t of
  Leaf -> True
  Node y l r ->
    | not (abs (height l - height r) <= 1) -> False
    | not (all (< y) l)                    -> False
    | not (all (> y) r)                    -> False
    | not (isBST l)                        -> False
    | not (isBST r)                        -> False
    | otherwise                            -> True
```

. . .

- 5 possible paths for *invalid* node, only 1 for *valid* node
- Compounds as execution unfolds recursive datatype

. . .

> Solver enumerates paths through **precondition** instead of function

# White-Box Testing

- Given program **implementation**, make it **crash**
- Enumerating program paths instead of inputs
- **Symbolic execution** groups equivalent inputs

# White-Box Testing

- Given program **implementation**, make it **crash**
- Enumerating program paths instead of inputs
- **Symbolic execution** groups equivalent inputs

## Problems

- Symbolic execution suffers from inexpressive logics and path explosion
- Dynamic-symbolic execution only addresses first issue
- Path explosion particularly problematic when faced with recursive preconditions

# This Talk

#### Techniques for Automatic Unit-Testing

<!-- 1. Human-generated tests -->
Existing Techniques

1. Black-box testing
2. White-box testing

Our contribution

3. **Type-targeted testing**

# What We Want

<!-- > Write a single generator per type, that can generate values satisfying different predicates. -->

Systematically generate **valid inputs** that are guaranteed to **pass the precondition**

. . .

1. How to **provide** inputs?
2. How to **check** outputs?

<!-- # Type-Targeted Testing -->
. . .

<!-- SUBPLAN -->

<!-- 1. refinement types -->
<!-- 2. generate inputs -->
<!-- 3. run function -->
<!-- 4. check outputs -->

**Approach**: Use **refinement types** to provide and check

# Target
Generates tests from **refinement types** via query-decode-check loop

1. Translate input types into SMT **query**
2. **Decode** SMT model into concrete values
3. Run function and **check** that result inhabits output type

. . .

Exhaustively checks all inputs up to a given depth-bound

> Like SmallCheck with a smarter generator


# Refinement Types

### `{v:t | p}`

> The set of values `v` of type `t` satisfying a logical predicate `p`

# Refinement Types

### `{v:t | p}`

> The set of values `v` of type `t` satisfying a logical predicate `p`

### Simple Refinement Types

```haskell
type Nat   = {v:Int | 0 <= v}
type Pos   = {v:Int | 0 <  v}
type Rng N = {v:Int | 0 <= v && v < N}
```

The natural numbers, positive integers, and integers in the range $[0,N)$

# Refinement Types

### `{v:t | p}`

> The set of values `v` of type `t` satisfying a logical predicate `p`

### Simple Refinement Types

```haskell
type Nat   = {v:Int | 0 <= v}
type Pos   = {v:Int | 0 <  v}
type Rng N = {v:Int | 0 <= v && v < N}
```

The natural numbers, positive integers, and integers in the range $[0,N)$

### Compound Refinement Types

Describe properties of containers and function contracts by refining component types

<!-- ```haskell -->
<!-- [{v:Int | v /= 0}] -->
<!-- ``` -->

<!-- Lists that contain no zeros -->

```haskell
x:Nat -> {v:Nat | v = x + 1}
```
Functions that take a natural number and increment it by one

# Target
Generates tests from refinement types via query-decode-check loop

1. Translate input types into SMT **query**
2. **Decode** SMT model into concrete values
3. Run function and **check** that result inhabits output type

# Step 1: Query

```haskell
type Nat   = {v:Int | 0 <= v}
type Rng N = {v:Int | 0 <= v && v < N}

rescale :: r1:Nat -> r2:Nat -> s:Rng r1 -> Rng r2
```

. . .

Represent preconditions directly in logic

$\cstr{C_0} \defeq 0 \leq \cvar{r_1} \wedge 0 \leq \cvar{r_2} \wedge 0 \leq s < \cvar{r_1}$

# Step 2: Decode

```haskell
type Nat   = {v:Int | 0 <= v}
type Rng N = {v:Int | 0 <= v && v < N}

rescale :: r1:Nat -> r2:Nat -> s:Rng r1 -> Rng r2
```

Represent preconditions directly in logic

$\cstr{C_0} \defeq 0 \leq \cvar{r_1} \wedge 0 \leq \cvar{r_2} \wedge 0 \leq s < \cvar{r_1}$

A model $[\cvar{r_1} \mapsto 1, \cvar{r_2} \mapsto 1, \cvar{s} \mapsto 0]$
maps to a concrete test case

```haskell
>>> rescale 1 1 0
```

# Step 3: Check

```haskell
type Nat   = {v:Int | 0 <= v}
type Rng N = {v:Int | 0 <= v && v < N}

rescale :: r1:Nat -> r2:Nat -> s:Rng r1 -> Rng r2
```

Represent preconditions directly in logic

$\cstr{C_0} \defeq 0 \leq \cvar{r_1} \wedge 0 \leq \cvar{r_2} \wedge 0 \leq s < \cvar{r_1}$

A model $[\cvar{r_1} \mapsto 1, \cvar{r_2} \mapsto 1, \cvar{s} \mapsto 0]$
maps to a concrete test case

```haskell
>>> rescale 1 1 0
0
```

Postcondition is:&nbsp;&nbsp;&nbsp;`{v:Int | 0 <= v && v < r2}`

# Step 3: Check

```haskell
type Nat   = {v:Int | 0 <= v}
type Rng N = {v:Int | 0 <= v && v < N}

rescale :: r1:Nat -> r2:Nat -> s:Rng r1 -> Rng r2
```

Represent preconditions directly in logic

$\cstr{C_0} \defeq 0 \leq \cvar{r_1} \wedge 0 \leq \cvar{r_2} \wedge 0 \leq s < \cvar{r_1}$

A model $[\cvar{r_1} \mapsto 1, \cvar{r_2} \mapsto 1, \cvar{s} \mapsto 0]$
maps to a concrete test case

```haskell
>>> rescale 1 1 0
0
```

Postcondition is:&nbsp;&nbsp;&nbsp;`{v:Int | 0 <= v && v < r2}`

After substituting `v` and `r2`:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;$0 \leq 0\quad\ \wedge\quad 0 < 1$

# Step 3: Check

```haskell
type Nat   = {v:Int | 0 <= v}
type Rng N = {v:Int | 0 <= v && v < N}

rescale :: r1:Nat -> r2:Nat -> s:Rng r1 -> Rng r2
```

Represent preconditions directly in logic

$\cstr{C_0} \defeq 0 \leq \cvar{r_1} \wedge 0 \leq \cvar{r_2} \wedge 0 \leq s < \cvar{r_1}$

A model $[\cvar{r_1} \mapsto 1, \cvar{r_2} \mapsto 1, \cvar{s} \mapsto 0]$
maps to a concrete test case

```haskell
>>> rescale 1 1 0
0
```

Postcondition is:&nbsp;&nbsp;&nbsp;`{v:Int | 0 <= v && v < r2}`

After substituting `v` and `r2`:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;$0 \leq 0\quad\ \wedge\quad 0 < 1$&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**VALID**

. . .

Force new test by adding refutation constraint $\lnot (\cvar{r_1} = 1 \land \cvar{r_2} = 1 \land \cvar{s} = 0)$

# Repeat With New Test

```haskell
type Nat   = {v:Int | 0 <= v}
type Rng N = {v:Int | 0 <= v && v < N}

rescale :: r1:Nat -> r2:Nat -> s:Rng r1 -> Rng r2
```

Represent preconditions directly in logic, excluding 1st test

$\cstr{C_1} \defeq 0 \leq \cvar{r_1} \wedge 0 \leq \cvar{r_2} \wedge 0 \leq s < \cvar{r_1} \wedge \lnot (\cvar{r_1} = 1 \land \cvar{r_2} = 1 \land \cvar{s} = 0)$

. . .

A model $[\cvar{r_1} \mapsto 1, \cvar{r_2} \mapsto 0, \cvar{s} \mapsto 0]$
maps to a concrete test case

```haskell
>>> rescale 1 0 0
0
```

. . .

Postcondition is:&nbsp;&nbsp;&nbsp;`{v:Int | 0 <= v && v < r2}`

After substituting `v` and `r2`:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;$0 \leq 0\quad\ \wedge\quad 0 < 0$

# Repeat With New Test

```haskell
type Nat   = {v:Int | 0 <= v}
type Rng N = {v:Int | 0 <= v && v < N}

rescale :: r1:Nat -> r2:Nat -> s:Rng r1 -> Rng r2
```

Represent preconditions directly in logic, excluding 1st test

$\cstr{C_1} \defeq 0 \leq \cvar{r_1} \wedge 0 \leq \cvar{r_2} \wedge 0 \leq s < \cvar{r_1} \wedge \lnot (\cvar{r_1} = 1 \land \cvar{r_2} = 1 \land \cvar{s} = 0)$

A model $[\cvar{r_1} \mapsto 1, \cvar{r_2} \mapsto 0, \cvar{s} \mapsto 0]$
maps to a concrete test case

```haskell
>>> rescale 1 0 0
0
```

Postcondition is:&nbsp;&nbsp;&nbsp;`{v:Int | 0 <= v && v < r2}`

After substituting `v` and `r2`:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;$0 \leq 0\quad\ \wedge\quad 0 < 0$&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;**INVALID**

`rescale 1 0 0` fails the postcondition check!

<!-- ```haskell -->
<!-- rescale :: r1:Pos -> r2:Pos -> s:Rng r1 -> Rng r2 -->
<!-- rescale r1 r2 s = s * (r2 `div` r1) -->
<!-- ``` -->

# Target
Generates tests from refinement types via query-decode-check loop

1. Translate input types into SMT query
2. Decode SMT model into concrete values
3. Run function and check that result inhabits output type

. . .

How should we handle **structured data**?

# Containers

```haskell
type Weight = Pos
type Score  = Rng 100

average :: [(Weight, Score)] -> Score
```

. . .

How to generate lists via SMT solver?

# Containers: Query

A **single** set of constraints describes **all possible** inputs

<div class="skel">
<img src="dot/skeleton-1.png">
</div>

# Containers: Query

A **single** set of constraints describes **all possible** inputs

<div class="skel">
<img src="dot/skeleton-2.png">
</div>

# Containers: Query

A **single** set of constraints describes **all possible** inputs

<div class="skel">
<img src="dot/skeleton-3.png">
</div>

# Containers: Query

A **single** set of constraints describes **all possible** inputs

<div class="skel">
<img src="dot/skeleton-4.png">
</div>

# Containers: Query

A **single** set of constraints describes **all possible** inputs

<div class="skel">
<img src="dot/skeleton-5.png">
</div>

# Containers: Query

A **single** set of constraints describes **all possible** inputs

<div class="skel-query">
<img src="dot/skeleton-1-choice.png">
<div>
Choice variables $\cvar{c}$ **guard** other constraints

$\begin{aligned}
\cstr{C_{list}} & \defeq & \\
\end{aligned}$
</div>
</div>

# Containers: Query

A **single** set of constraints describes **all possible** inputs

<div class="skel-query">
<img src="dot/skeleton-2-choice.png">
<div>
Choice variables $\cvar{c}$ **guard** other constraints

$\begin{aligned}
\cstr{C_{list}} & \defeq & (\cvar{c}_{00} \Rightarrow \cvar{xs}_0 = \lnil) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{xs}_0 = \lcons{\cvar{x}_1}{\cvar{xs}_1}) & \wedge &
                           (\cvar{c}_{00} \oplus \cvar{c}_{01}) \\
\end{aligned}$
</div>
</div>

# Containers: Query

A **single** set of constraints describes **all possible** inputs

<div class="skel-query">
<img src="dot/skeleton-3-choice.png">
<div>
Choice variables $\cvar{c}$ **guard** other constraints

$\begin{aligned}
\cstr{C_{list}} & \defeq & (\cvar{c}_{00} \Rightarrow \cvar{xs}_0 = \lnil) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{xs}_0 = \lcons{\cvar{x}_1}{\cvar{xs}_1}) & \wedge &
                           (\cvar{c}_{00} \oplus \cvar{c}_{01}) \\
                & \wedge & (\cvar{c}_{10} \Rightarrow \cvar{xs}_1 = \lnil) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{xs}_1 = \lcons{\cvar{x}_2}{\cvar{xs}_2}) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{c}_{10} \oplus \cvar{c}_{11}) \\
\end{aligned}$
</div>
</div>

# Containers: Query

A **single** set of constraints describes **all possible** inputs

<div class="skel-query">
<img src="dot/skeleton-4-choice.png">
<div>
Choice variables $\cvar{c}$ **guard** other constraints

$\begin{aligned}
\cstr{C_{list}} & \defeq & (\cvar{c}_{00} \Rightarrow \cvar{xs}_0 = \lnil) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{xs}_0 = \lcons{\cvar{x}_1}{\cvar{xs}_1}) & \wedge &
                           (\cvar{c}_{00} \oplus \cvar{c}_{01}) \\
                & \wedge & (\cvar{c}_{10} \Rightarrow \cvar{xs}_1 = \lnil) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{xs}_1 = \lcons{\cvar{x}_2}{\cvar{xs}_2}) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{c}_{10} \oplus \cvar{c}_{11}) \\
                & \wedge & (\cvar{c}_{20} \Rightarrow \cvar{xs}_2 = \lnil) & \wedge &
                           (\cvar{c}_{21} \Rightarrow \cvar{xs}_2 = \lcons{\cvar{x}_3}{\cvar{xs}_3}) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{c}_{20} \oplus \cvar{c}_{21}) \\
\end{aligned}$
</div>
</div>

# Containers: Query

A **single** set of constraints describes **all possible** inputs

<div class="skel-query">
<img src="dot/skeleton-5-choice.png">
<div>
Choice variables $\cvar{c}$ **guard** other constraints

$\begin{aligned}
\cstr{C_{list}} & \defeq & (\cvar{c}_{00} \Rightarrow \cvar{xs}_0 = \lnil) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{xs}_0 = \lcons{\cvar{x}_1}{\cvar{xs}_1}) & \wedge &
                           (\cvar{c}_{00} \oplus \cvar{c}_{01}) \\
                & \wedge & (\cvar{c}_{10} \Rightarrow \cvar{xs}_1 = \lnil) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{xs}_1 = \lcons{\cvar{x}_2}{\cvar{xs}_2}) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{c}_{10} \oplus \cvar{c}_{11}) \\
                & \wedge & (\cvar{c}_{20} \Rightarrow \cvar{xs}_2 = \lnil) & \wedge &
                           (\cvar{c}_{21} \Rightarrow \cvar{xs}_2 = \lcons{\cvar{x}_3}{\cvar{xs}_3}) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{c}_{20} \oplus \cvar{c}_{21}) \\
                & \wedge & (\cvar{c}_{30} \Rightarrow \cvar{xs}_3 = \lnil) & & & \wedge &
                           (\cvar{c}_{21} \Rightarrow \cvar{c}_{30})\\
\end{aligned}$
</div>
</div>

# Containers: Query

A **single** set of constraints describes **all possible** inputs

<div class="skel-query">
<img src="dot/skeleton-5-choice.png">
<div>
<div>
Choice variables $\cvar{c}$ **guard** other constraints
</div>
<div>
$\begin{aligned}
\cstr{C_{list}} & \defeq & (\cvar{c}_{00} \Rightarrow \cvar{xs}_0 = \lnil) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{xs}_0 = \lcons{\cvar{x}_1}{\cvar{xs}_1}) & \wedge &
                           (\cvar{c}_{00} \oplus \cvar{c}_{01}) \\
                & \wedge & (\cvar{c}_{10} \Rightarrow \cvar{xs}_1 = \lnil) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{xs}_1 = \lcons{\cvar{x}_2}{\cvar{xs}_2}) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{c}_{10} \oplus \cvar{c}_{11}) \\
                & \wedge & (\cvar{c}_{20} \Rightarrow \cvar{xs}_2 = \lnil) & \wedge &
                           (\cvar{c}_{21} \Rightarrow \cvar{xs}_2 = \lcons{\cvar{x}_3}{\cvar{xs}_3}) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{c}_{20} \oplus \cvar{c}_{21}) \\
                & \wedge & (\cvar{c}_{30} \Rightarrow \cvar{xs}_3 = \lnil) & & & \wedge &
                           (\cvar{c}_{21} \Rightarrow \cvar{c}_{30})\\
\end{aligned}$
</div>
<div>
$\begin{aligned}
\cstr{C_{data}} & \defeq & (\cvar{c}_{01} \Rightarrow \cvar{x}_1 = \ltup{\cvar{w}_1}{\cvar{s}_1} \ \wedge\ 0 < \cvar{w}_1 \ \wedge\ 0 \leq \cvar{s}_1 < 100) \\
                & \wedge & (\cvar{c}_{11} \Rightarrow \cvar{x}_2 = \ltup{\cvar{w}_2}{\cvar{s}_2} \ \wedge\ 0 < \cvar{w}_2 \ \wedge\ 0 \leq \cvar{s}_2 < 100) \\
                & \wedge & (\cvar{c}_{21} \Rightarrow \cvar{x}_3 = \ltup{\cvar{w}_3}{\cvar{s}_3} \ \wedge\ 0 < \cvar{w}_3 \ \wedge\ 0 \leq \cvar{s}_3 < 100)
\end{aligned}$
</div>
<div>
Full constraint $\cstr{C} \defeq \cstr{C_{list}} \land \cstr{C_{data}}$
</div>
</div>
</div>

# Containers: Decode

A **single** set of constraints describes **all possible** inputs

<div class="skel-query">
<img src="dot/skeleton-1-decode.png">
<div>
Follow the choice variables to reconstruct the list

$\begin{aligned}
[&\cvar{c_{00}} \mapsto\ \tfalse,\ \cvar{c_{01}} \mapsto\ \ttrue,\ \cvar{x_1} \mapsto \ltup{\cvar{w_1}}{\cvar{s_1}},\ \cvar{w_1} \mapsto 1,\ \cvar{s_1} \mapsto 2, &\\
&\cvar{c_{10}} \mapsto\ \ttrue,\ \cvar{c_{11}} \mapsto\ \tfalse,\ \cvar{x_2} \mapsto \ltup{\cvar{w_2}}{\cvar{s_2}},\ \cvar{w_2} \mapsto 3,\ \cvar{s_2} \mapsto 4, \ldots &]\\
\end{aligned}$
</div>
</div>

# Containers: Decode

A **single** set of constraints describes **all possible** inputs

<div class="skel-query">
<img src="dot/skeleton-2-decode.png">
<div>
Follow the choice variables to reconstruct the list

$\begin{aligned}
[&\cvar{c_{00}} \mapsto\ \tfalse,\ \cvar{c_{01}} \mapsto\ \ttrue,\ \cvar{x_1} \mapsto \ltup{\cvar{w_1}}{\cvar{s_1}},\ \cvar{w_1} \mapsto 1,\ \cvar{s_1} \mapsto 2, &\\
&\cvar{c_{10}} \mapsto\ \ttrue,\ \cvar{c_{11}} \mapsto\ \tfalse,\ \cvar{x_2} \mapsto \ltup{\cvar{w_2}}{\cvar{s_2}},\ \cvar{w_2} \mapsto 3,\ \cvar{s_2} \mapsto 4, \ldots &]\\
\end{aligned}$

- $\cvar{c_{01}} \mapsto\ \ttrue \Rightarrow \cvar{xs_0} = \lcons{\cvar{x_1}}{\cvar{xs_1}}$
</div>
</div>

# Containers: Decode

A **single** set of constraints describes **all possible** inputs

<div class="skel-query">
<img src="dot/skeleton-3-decode.png">
<div>
Follow the choice variables to reconstruct the list

$\begin{aligned}
[&\cvar{c_{00}} \mapsto\ \tfalse,\ \cvar{c_{01}} \mapsto\ \ttrue,\ \cvar{x_1} \mapsto \ltup{\cvar{w_1}}{\cvar{s_1}},\ \cvar{w_1} \mapsto 1,\ \cvar{s_1} \mapsto 2, &\\
&\cvar{c_{10}} \mapsto\ \ttrue,\ \cvar{c_{11}} \mapsto\ \tfalse,\ \cvar{x_2} \mapsto \ltup{\cvar{w_2}}{\cvar{s_2}},\ \cvar{w_2} \mapsto 3,\ \cvar{s_2} \mapsto 4, \ldots &]\\
\end{aligned}$

- $\cvar{c_{01}} \mapsto\ \ttrue \Rightarrow \cvar{xs_0} = \lcons{\cvar{x_1}}{\cvar{xs_1}}$
- $\cvar{c_{10}} \mapsto\ \ttrue \Rightarrow \cvar{xs_1} = \lnil$
</div>
</div>

# Containers: Decode

A **single** set of constraints describes **all possible** inputs

<div class="skel-query">
<img src="dot/skeleton-3-decode.png">
<div>
Follow the choice variables to reconstruct the list

$\begin{aligned}
[&\cvar{c_{00}} \mapsto\ \tfalse,\ \cvar{c_{01}} \mapsto\ \ttrue,\ \cvar{x_1} \mapsto \ltup{\cvar{w_1}}{\cvar{s_1}},\ \cvar{w_1} \mapsto 1,\ \cvar{s_1} \mapsto 2, &\\
&\cvar{c_{10}} \mapsto\ \ttrue,\ \cvar{c_{11}} \mapsto\ \tfalse,\ \cvar{x_2} \mapsto \ltup{\cvar{w_2}}{\cvar{s_2}},\ \cvar{w_2} \mapsto 3,\ \cvar{s_2} \mapsto 4, \ldots &]\\
\end{aligned}$

- $\cvar{c_{01}} \mapsto\ \ttrue \Rightarrow \cvar{xs_0} = \lcons{\cvar{x_1}}{\cvar{xs_1}}$
- $\cvar{c_{10}} \mapsto\ \ttrue \Rightarrow \cvar{xs_1} = \lnil$

Realized value: `[(1,2)]`
</div>
</div>

# Containers: Refuting

A **single** set of constraints describes **all possible** inputs

<div class="skel-query">
<img src="dot/skeleton-3-decode.png">
<div>
Follow the choice variables to reconstruct the list

$\begin{aligned}
[&\cvar{c_{00}} \mapsto\ \tfalse,\ \cvar{c_{01}} \mapsto\ \ttrue,\ \cvar{x_1} \mapsto \ltup{\cvar{w_1}}{\cvar{s_1}},\ \cvar{w_1} \mapsto 1,\ \cvar{s_1} \mapsto 2, &\\
&\cvar{c_{10}} \mapsto\ \ttrue,\ \cvar{c_{11}} \mapsto\ \tfalse,\ \cvar{x_2} \mapsto \ltup{\cvar{w_2}}{\cvar{s_2}},\ \cvar{w_2} \mapsto 3,\ \cvar{s_2} \mapsto 4, \ldots &]\\
\end{aligned}$

- $\cvar{c_{01}} \mapsto\ \ttrue \Rightarrow \cvar{xs_0} = \lcons{\cvar{x_1}}{\cvar{xs_1}}$
- $\cvar{c_{10}} \mapsto\ \ttrue \Rightarrow \cvar{xs_1} = \lnil$

Realized value: `[(1,2)]`

**Only** refute constraints that contribute to **realized** value

$\lnot (\cvar{c_{00}} = \tfalse \land \cvar{c_{01}} = \ttrue \land \cvar{x_1} = \ltup{\cvar{w_1}}{\cvar{s_1}} \land \cvar{w_1} = 1 \land \cvar{s_1} = 2 \land \cvar{c_{10}} = \ttrue)$
</div>
</div>


# Structured Containers

```haskell
best :: k:Nat -> {xs:[Score] | k <= len xs} -> {v:[Score] | k = len v}
```

`best` takes a list of **at least** `k` scores, and returns a list with **exactly** `k` scores.

# Structured Containers

```haskell
best :: k:Nat -> {xs:[Score] | k <= len xs} -> {v:[Score] | k = len v}
```

`best` takes a list of **at least** `k` scores, and returns a list with **exactly** `k` scores.

```haskell
measure len :: [a] -> Nat
len []      = 0
len (x:xs)  = 1 + len xs
```

`len` is a **logical function** that describes the length of a list.

<!-- best k xs = take k $ reverse $ sort xs -->

# Structured Containers: Query

```haskell
best :: k:Nat -> {xs:[Score] | k <= len xs} -> {v:[Score] | k = len v}
```

`best` takes a list of **at least** `k` scores, and returns a list with **exactly** `k` scores.

```haskell
measure len :: [a] -> Nat
len []      = 0
len (x:xs)  = 1 + len xs
```

`len` is a **logical function** that describes the length of a list.

Instantiate measure definition each time we unfold `[]` or `(:)`

# Structured Containers: Query

```haskell
best :: k:Nat -> {xs:[Score] | k <= len xs} -> {v:[Score] | k = len v}
```

<div class="skel-query">
<img src="dot/skeleton-5-choice.png">
<div>
<div>
$\begin{aligned}
\cstr{C_{list}} & \defeq & (\cvar{c}_{00} \Rightarrow \cvar{xs}_0 = \lnil) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{xs}_0 = \lcons{\cvar{x}_1}{\cvar{xs}_1}) & \wedge &
                           (\cvar{c}_{00} \oplus \cvar{c}_{01}) \\
                & \wedge & (\cvar{c}_{10} \Rightarrow \cvar{xs}_1 = \lnil) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{xs}_1 = \lcons{\cvar{x}_2}{\cvar{xs}_2}) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{c}_{10} \oplus \cvar{c}_{11}) \\
                & \wedge & (\cvar{c}_{20} \Rightarrow \cvar{xs}_2 = \lnil) & \wedge &
                           (\cvar{c}_{21} \Rightarrow \cvar{xs}_2 = \lcons{\cvar{x}_3}{\cvar{xs}_3}) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{c}_{20} \oplus \cvar{c}_{21}) \\
                & \wedge & (\cvar{c}_{30} \Rightarrow \cvar{xs}_3 = \lnil) & & & \wedge &
                           (\cvar{c}_{21} \Rightarrow \cvar{c}_{30})\\
\end{aligned}$
</div>
</div>
</div>

# Structured Containers: Query

```haskell
best :: k:Nat -> {xs:[Score] | k <= len xs} -> {v:[Score] | k = len v}
```

<div class="skel-query">
<img src="dot/skeleton-5-choice.png">
<div>
<div>
$\begin{aligned}
\cstr{C_{list}} & \defeq & (\cvar{c}_{00} \Rightarrow \cvar{xs}_0 = \lnil) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{xs}_0 = \lcons{\cvar{x}_1}{\cvar{xs}_1}) & \wedge &
                           (\cvar{c}_{00} \oplus \cvar{c}_{01}) \\
                & \wedge & (\cvar{c}_{10} \Rightarrow \cvar{xs}_1 = \lnil) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{xs}_1 = \lcons{\cvar{x}_2}{\cvar{xs}_2}) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{c}_{10} \oplus \cvar{c}_{11}) \\
                & \wedge & (\cvar{c}_{20} \Rightarrow \cvar{xs}_2 = \lnil) & \wedge &
                           (\cvar{c}_{21} \Rightarrow \cvar{xs}_2 = \lcons{\cvar{x}_3}{\cvar{xs}_3}) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{c}_{20} \oplus \cvar{c}_{21}) \\
                & \wedge & (\cvar{c}_{30} \Rightarrow \cvar{xs}_3 = \lnil) & & & \wedge &
                           (\cvar{c}_{21} \Rightarrow \cvar{c}_{30})\\
\end{aligned}$
</div>
<div>
$\begin{aligned}
\cstr{C_{size}} & \defeq & (\cvar{c}_{00} \Rightarrow \clen{\cvar{xs}_{0}} = 0) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \clen{\cvar{xs}_{0}} = 1 + \clen{\cvar{xs}_1}) \\
                & \wedge & (\cvar{c}_{10} \Rightarrow \clen{\cvar{xs}_{1}} = 0) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \clen{\cvar{xs}_{1}} = 1 + \clen{\cvar{xs}_2}) \\
                & \wedge & (\cvar{c}_{20} \Rightarrow \clen{\cvar{xs}_{2}} = 0) & \wedge &
                           (\cvar{c}_{21} \Rightarrow \clen{\cvar{xs}_{2}} = 1 + \clen{\cvar{xs}_3}) \\
                & \wedge & (\cvar{c}_{30} \Rightarrow \clen{\cvar{xs}_{3}} = 0) &        &
\end{aligned}$
</div>
</div>
</div>

# Structured Containers: Query

```haskell
best :: k:Nat -> {xs:[Score] | k <= len xs} -> {v:[Score] | k = len v}
```

<div class="skel-query">
<img src="dot/skeleton-5-choice.png">
<div>
<div>
$\begin{aligned}
\cstr{C_{list}} & \defeq & (\cvar{c}_{00} \Rightarrow \cvar{xs}_0 = \lnil) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{xs}_0 = \lcons{\cvar{x}_1}{\cvar{xs}_1}) & \wedge &
                           (\cvar{c}_{00} \oplus \cvar{c}_{01}) \\
                & \wedge & (\cvar{c}_{10} \Rightarrow \cvar{xs}_1 = \lnil) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{xs}_1 = \lcons{\cvar{x}_2}{\cvar{xs}_2}) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \cvar{c}_{10} \oplus \cvar{c}_{11}) \\
                & \wedge & (\cvar{c}_{20} \Rightarrow \cvar{xs}_2 = \lnil) & \wedge &
                           (\cvar{c}_{21} \Rightarrow \cvar{xs}_2 = \lcons{\cvar{x}_3}{\cvar{xs}_3}) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \cvar{c}_{20} \oplus \cvar{c}_{21}) \\
                & \wedge & (\cvar{c}_{30} \Rightarrow \cvar{xs}_3 = \lnil) & & & \wedge &
                           (\cvar{c}_{21} \Rightarrow \cvar{c}_{30})\\
\end{aligned}$
</div>
<div>
$\begin{aligned}
\cstr{C_{size}} & \defeq & (\cvar{c}_{00} \Rightarrow \clen{\cvar{xs}_{0}} = 0) & \wedge &
                           (\cvar{c}_{01} \Rightarrow \clen{\cvar{xs}_{0}} = 1 + \clen{\cvar{xs}_1}) \\
                & \wedge & (\cvar{c}_{10} \Rightarrow \clen{\cvar{xs}_{1}} = 0) & \wedge &
                           (\cvar{c}_{11} \Rightarrow \clen{\cvar{xs}_{1}} = 1 + \clen{\cvar{xs}_2}) \\
                & \wedge & (\cvar{c}_{20} \Rightarrow \clen{\cvar{xs}_{2}} = 0) & \wedge &
                           (\cvar{c}_{21} \Rightarrow \clen{\cvar{xs}_{2}} = 1 + \clen{\cvar{xs}_3}) \\
                & \wedge & (\cvar{c}_{30} \Rightarrow \clen{\cvar{xs}_{3}} = 0) &        &
\end{aligned}$
</div>
<div>
Enforce relation between `k` and `xs` by adding constraint $k \leq \clen{\cvar{xs}_0}$

$\cstr{C} \defeq \cstr{C_{list}} \land \cstr{C_{data}} \land \cstr{C_{size}} \land 0 \leq \cvar{k} \leq \clen{\cvar{xs}_0}$
</div>
</div>
</div>

# DEMO: Targeting Binary Search Trees

# Evaluation

### Our Claims
1. Target handles highly structured inputs automatically
2. Target generates tests that provide high code coverage

### Benchmarks
1. `Data.Map`: checked balancing and ordering invariants
2. `RBTree`: checked red-black and ordering invariants
3. `XMonad.StackSet`: checked uniqueness of windows

Compare Target against QuickCheck and SmallCheck
    
<!-- 3. RESULT -->

# Evaluation: Results

### Target checks larger inputs than brute-force

<img id="benchmarks-fig" src="benchmarks.png">

# Evaluation: Results

### Target provides high coverage with low investment

<img id="coverage-fig" src="coverage.png">

# Takeaway

**Target** - a new approach for automatically testing functions with preconditions

> - vs **enumeration**: Target defers the onset of input explosion
> - vs **sampling**: Target does not require custom generators
> - vs **symbolic execution**: Target guarantees that inputs pass the precondition

<!-- > - Target can explore larger input spaces than explicit enumeration -->
<!-- > - Target does not require custom generators -->
<!-- > - Target  -->
<!-- > - Target specs are amenable to future formal verification -->

# Backup Slides

# Dynamic-Symbolic Testing: Why Concrete + Symbolic?

```c
struct foo { int i; char c; }
bar (struct foo *a) {
  if (a->c == 0) {
    *((char *)a + sizeof(int)) = 1;
    if (a->c != 0)
      abort();
  }
}
```

- Symbolic executors cannot report with **certainty** that `abort` is reachable
<!-- - pointer arithmetic confuses alias analysis -->
- Dynamic-Symbolic testing need only solve `a->c == 0` to produce **concrete** input that will blow up!
<!-- - fill gaps in symbolic reasoning with **concrete** value -->

<!-- # Encoding Trees of Depth 2 -->

<!-- $\begin{aligned} -->
<!-- \cstr{C_{tree}} & \defeq & (\cvar{c}_{t0} \Rightarrow \cvar{t} = \lleaf) & \wedge & -->
<!--                            (\cvar{c}_{t1} \Rightarrow \cvar{t} = \lnode{\cvar{x}_t}{\cvar{l}_t}{\cvar{r}_t}) & \wedge & -->
<!--                            & & (\cvar{c}_{t0} & \oplus & \cvar{c}_{t1}) \\ -->
<!--                 & \wedge & (\cvar{c}_{l_t0} \Rightarrow \cvar{l}_t = \lleaf) & \wedge & -->
<!--                            (\cvar{c}_{l_t1} \Rightarrow \cvar{l}_t = \lnode{\cvar{x}_l}{\cvar{l}_l}{\cvar{r}_l}) & \wedge & -->
<!--                            (\cvar{c}_{t1} & \Rightarrow & \cvar{c}_{l_t0} & \oplus & \cvar{c}_{l_t1}) \\ -->
<!--                 & \wedge & (\cvar{c}_{r_t0} \Rightarrow \cvar{r}_t = \lleaf) & \wedge & -->
<!--                            (\cvar{c}_{r_t1} \Rightarrow \cvar{r}_t = \lnode{\cvar{x}_r}{\cvar{l}_r}{\cvar{r}_r}) & \wedge & -->
<!--                            (\cvar{c}_{t1} & \Rightarrow & \cvar{c}_{r_t0} & \oplus & \cvar{c}_{r_t1}) \\ -->
<!--                 & \wedge & (\cvar{c}_{l_l0} \Rightarrow \cvar{l}_l = \lleaf) &  & -->
<!--                             & \wedge & -->
<!--                            (\cvar{c}_{l_t1} & \Rightarrow & \cvar{c}_{l_l0}) &  & \\ -->
<!--                 & \wedge & (\cvar{c}_{r_l0} \Rightarrow \cvar{r}_l = \lleaf) &  & -->
<!--                             & \wedge & -->
<!--                            (\cvar{c}_{l_t1} & \Rightarrow & \cvar{c}_{r_l0}) &  & \\ -->
<!--                 & \wedge & (\cvar{c}_{l_r0} \Rightarrow \cvar{l}_r = \lleaf) &  & -->
<!--                             & \wedge & -->
<!--                            (\cvar{c}_{r_t1} & \Rightarrow & \cvar{c}_{l_r0}) &  & \\ -->
<!--                 & \wedge & (\cvar{c}_{r_r0} \Rightarrow \cvar{r}_r = \lleaf) &  & -->
<!--                             & \wedge & -->
<!--                            (\cvar{c}_{r_t1} & \Rightarrow & \cvar{c}_{r_r0}) &  & \\ -->
<!-- \end{aligned}$ -->


<!-- # NOTES -->

<!-- - [ ] perhaps start with demo -->
<!-- - [ ] intro is abrupt -->
<!-- - [X] monomorphic tree -->
<!-- - [ ] don't show class isntances -->
<!-- - [X] fix smallcheck/quickcheck examples -->
<!-- - [ ] preface symbolic execution better (why?) -->
<!-- - [X] keep rescale def around -->
<!-- - [X] kill questions slide -->
<!-- - [ ] maybe start with `average` -->
<!-- - [ ] more comparisons!! -->
<!-- - [X] un-demorgan refutations -->
<!-- - [X] clarify that we use a single set of constraints to represent all possible inputs -->
<!-- - [X] no sub-bullets or just bold -->
<!-- - [ ] more consistent slide titles -->
<!-- - [ ] explain all code with english -->
<!-- - [ ] RECAP: re-use black-box slides with PROBLEM section -->


<!-- # Questions -->
<!-- - do we need base types in refinements (i.e. why not assertions?) -->
<!--     - we use base types to implicitly quantify over elements of containers -->
<!--     - avoid recursive assertions, which are difficult to reason about -->
<!-- - is theory of inductive datatypes decidable? -->
<!--     - theory is decidable, but NP-complete -->
<!--     - Arrays: QF is decidable (NP-complete), w/ quals undecidable -->
<!--     - Integers: QF is NP-complete, w/ quals undecidable -->
<!--     - contrast to QF_EUF, which is polynomial -->
<!-- - studies validating small-scope hypothesis? -->
<!--     - 2003 (unpublished) study of java collections framework claims validity -->
