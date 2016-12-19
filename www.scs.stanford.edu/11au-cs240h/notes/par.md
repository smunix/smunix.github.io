% Folds and parallelism


# A Monday recap

Someone asked a thoughtful question on Monday about folds,
specifically about `foldr` vs `foldl` and `foldl'`.

I wasn't very happy with the answers I came up with, because I was in
a bit of a rush.

* Always be suspicious of teachers who get into a muddle if you spring
  an unexpected question!
  
* Do they really know what they're talking about, or are they just
  good at reading slides?

Today, let's slow down and think about folds a little more.


# Folding from the right

Here's the definition of `foldr`:

~~~~ {.haskell}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f = go
  where
    go (x:xs) = f x (go xs)
    go z      = z
~~~~

It associates to the right, so here's what a use of `foldr` looks
like, expanded:

~~~~ {.haskell}
foldr f z [a,b,c,d] = f a (f b (f c (f d z)))
~~~~


# foldr and strictness: 0 of n

Suppose we want to sum the elements of a list using `foldr`:

~~~~ {.haskell}
foldr (+) 0 [3..6]
~~~~

In order for the `(+)` operator to add two numbers, it has to know
what each one is, so at least for the usual types we work with, it's
*strict* in both of its arguments.

(Note: `(+)` is not required to be strict.  That behaviour depends on
the instance of `Num` in use.)


# foldr and strictness: 1 of n

Let's do a stepwise expansion of the `foldr`-based sum.

~~~~ {.haskell}
foldr (+) 0 (3:{-...-}) = 3 + {-...-}
~~~~

We can't produce an answer after consuming the first element of the
list, because `+` is strict in its arguments.


# foldr and strictness: 2 of n

~~~~ {.haskell}
foldr (+) 0 (3:4:{-...-}) = 3 + (4 + {-...-})
~~~~

Neither can we produce an answer after the second element.

In fact, we're building up an expression that we can't reduce to a
smaller form, because we're associating to the right and we haven't
seen the rest of the list yet (see the definition of `foldr`).


# foldr and strictness: n of n

In fact, not until we reach the end of the list do we have a number on
the right hand side of a `+`:

~~~~ {.haskell}
foldr (+) 0 (3:4:5:6:[]) = 3 + (4 + (5 + (6 + 0)))
~~~~

What we've done is build up a big expression that we can't evaluate
until we've seen the entire list.

This is due in equal part to two factors:

* `foldr` is right associative.

* The function we're folding with is strict in both arguments.


# So how about foldl?

If `foldr` was getting us all screwed up due to the associativity,
maybe `foldl` will do better.

~~~~ {.haskell}
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f = go
  where
    go z (x:xs) = go (f z x) xs
    go z _      = z
~~~~

Let's see an expansion.

~~~~ {.haskell}
foldl f z [a,b,c,d] = f (f (f (f z a) b) c) d
~~~~

Ouch! Maybe we can make that clearer with infix syntax?

~~~~ {.haskell}
foldl (+) z [a,b,c,d] = (((z + a) + b) + c) + d
~~~~


# foldl and strictness

For summing elements, this looks better than `foldr` on the face of
things, because at every step through the list, the function we're
folding with can see both of its arguments.

But there's a catch.

None of the intermediate results are visible to the caller of `foldl`,
so the caller cannot force those results to be evaluated.

In other words, we're still building up a big thunk!


# A strict left fold

This function is defined for us in `Data.List`, and it has the same
type as `foldl`:

~~~~ {.haskell}
{-# LANGUAGE BangPatterns #-}

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f = go
  where
    go !z (x:xs) = go (f z x) xs
    go !z  _     = z
~~~~

The crucial difference lies with the strictness annotation, which
forces each intermediate result to be evaluated to WHNF.


# foldl' and strictness: 1 of n

That same sum again.

~~~~ {.haskell}
foldl' (+) 0 [1,2,3]
~~~~

First step:

~~~~ {.haskell}
foldl' (+) 0 (1:{-...-}) = foldl' (0+1) {-...-}
                         = foldl' 1     {-...-}
~~~~


# foldl' and strictness: 2 of n

Second step:

~~~~ {.haskell}
foldl' (+) 1 (2:{-...-}) = foldl' (1+2) {-...-}
                         = foldl' 2     {-...-}
~~~~

The structure of `foldl'` causes us to replace the accumulator `z`
with a new value at each step.

The strictness annotation (we could have used `seq` too) causes that
accumulator to be evaluated to WHNF before we continue.


# foldl' and strictness: n of n

Last step:

~~~~ {.haskell}
foldl' (+) 3 [] = 3
~~~~

I hope this makes the difference between `foldl'` and `foldr` more
apparent!


# Don't use foldl

It should be clear by now that plain old `foldl` is very rarely
useful.  

In fact, I've *never* personally found a situation where it was the
right kind of fold to use.

If you find yourself thinking "I need a left fold", it's safe to
assume that `foldl'` is what you'll need.


# foldr and laziness

We've discussed the fact that using `foldr` with a strict function is
a bad idea, because it generates a chain of thunks.

What if we used a function that was *lazy* in its second argument?

Hmm.  Conveniently enough, we have an everyday example of just such a
function: the list constructor!

What does this function do?

~~~~ {.haskell}
foldr (:) []
~~~~


# A handy way to think about foldr

Let's do another expansion, but this time we'll align the input list
and result expression lexically:

~~~~ {.haskell}
foldr (<+>) z 
  (a  :  (b  :  (c  :  (d  :  [])))) =
  (a <+> (b <+> (c <+> (d <+> z ))))
~~~~

Notice that each `(:)` in the input list is "replaced" by an
application of `(<+>)` in the result, and the `[]` at the end is
replaced with `z`.

(It doesn't matter what `(<+>)` actually does here.  We're purely
interested in the structures of the two expressions.)


# Quick exercise 1: map

Write a version of `map` that does not pattern match on its input
list, but instead uses `foldr`:

~~~~ {.haskell}
map :: (a -> b) -> [a] -> [b]

map (+1) [1,2,3] == [2,3,4]
~~~~

# Quick exercise 1: my implementation


~~~~ {.haskell}
myMap f = foldr go []
  where go x xs = f x : xs
~~~~


# Quick exercise 2: filter

Write a version of `filter` that again uses `foldr`:

~~~~ {.haskell}
filter :: (a -> Bool) -> [a] -> [a]

filter odd [0..5] == [1,3,5]
~~~~


# Quick exercise 2: my version

~~~~ {.haskell}
myFilter p = foldr go []
  where
    go x xs
      | p x       = x : xs
      | otherwise = xs
~~~~


# Quick exercise 3: append

Write an `append` function that has the same behaviour as the `(++)`
operator:

~~~~ {.haskell}
(++) :: [a] -> [a] -> [a]

[1,2,3] ++ [4,5,6] == [1,2,3,4,5,6]
~~~~

For bonus points, make your implementation as short as possible.


# Quick exercise 3: mine

~~~~ {.haskell}
append = flip (foldr (:))
~~~~


# Parallelism vs concurrency

A handy rule of thumb for distinguishing the two:

* Concurrency: "I have a number of potentially unrelated tasks to deal
  with."
  
* Parallelism: "I want my single question to be answered sooner if I
  can throw more resources at the problem."

Many languages blur these notions together, by providing a "one size
fits all" toolkit for concurrency and parallelism.


# What does a concurrent system need?

We do not need multiple CPU cores to achieve concurrent processing,
nor do we even need threads.

However, a good language runtime and programming environment *will*
offer improved throughput and latency on concurrent problems if it can
use multiple cores.


# What about a parallel system?

The essence of parallel programming is to have multiple computing
elements process parts of a problem simultaneously.

GHC provides a "baked in" way of using multiple CPU cores to do this.

To take advantage of this, we *must* build our executables with
`-threaded`.

This links a Haskell program against the "threaded runtime", which can
schedule lightweight Haskell threads across multiple CPU cores, and
which can also run parallel programs on those cores.


# Low-level parallel programming

In the `Control.Parallel` module lives an intriguing function.

~~~~ {.haskell}
par :: a -> b -> b
~~~~

This is the parallel cousin to `seq`.  It tells the threaded runtime
that it may be beneficial to evaluate the first argument in parallel
with the second.

Like `seq`, `par` returns the value of its second argument.


# pseq

As it happens, we need another oddball function to work with `par`.

~~~~ {.haskell}
pseq :: a -> b -> b
~~~~

This is semantically identical to `seq`, but with a subtle operational
difference.

`seq` is strict in *both* arguments, so the compiler can take an
expression like this:

~~~~ {.haskell}
a `seq` b 
~~~~

And transform it into this:

~~~~ {.haskell}
b `seq` a `seq` b
~~~~

This can be a problem when annotating code for parallelism, when we
need more control over the order of evaluation.

In contrast to `seq`, `pseq` is only strict in its *first* argument.

This restricts the transformations that the compiler can perform, and
lets us retain control of the evaluation order.


# When to use par

We use `par` when two conditions are true:

* The first argument is somewhat expensive to compute.

* We don't have an immediate need for the result of the first
  computation.

If the first argument is too cheap, then the book-keeping overhead to
evaluate it in parallel will diminish or erase any performance gain.


# Futures

The `par` operator makes use of the overlap between lazy evaluation
and futures.

To implement lazy evaluation, we need *thunks*: a representation for
expressions which are not yet evaluated but whose value may later be
demanded.

A *future* is a computation whose value is being evaluated in parallel
and which we may wait for.

`par` offers a way to annotate a lazy computation as being potentially
profitable to evaluate in parallel.

In other words, it turns a lazy computation into a future.


# O rly?

In practice, `par` and `pseq` are a pain in the neck to use.

To use `par` effectively, we have to get some surprisingly subtle
things right:

1. Pass an unevaluated computation to `par`

2. The unevaluated computation must be somewhat expensive

3. Ensure that its value will not be required by the enclosing
   computation for a while

4. Ensure that the result is shared by the rest of the program

If we miss any of 1 through 3, then we achieve little or no speedup.

If we miss 4, then the GC may get rid of the computed result before it
can be used.

* In practice, even experts fall afoul of these requirements on a
  regular basis.


# The Par monad

The `monad-par` package provides a library that makes parallelism
easier to achieve and reason about.

~~~~
cabal install monad-par
~~~~

It gives us a type named `Par`:

~~~~ {.haskell}
newtype Par a

instance Functor Par
instance Applicative Par
instance Monad Par
~~~~

To evaluate a `Par` computation, we use `runPar`:

~~~~ {.haskell}
runPar :: Par a -> a
~~~~

Notice that this has no side effects, so it will run
deterministically.


# Building blocks

To start a parallel computation, we use the `fork` action:

~~~~ {.haskell}
fork :: Par () -> Par ()
~~~~

Forked tasks need a way to communicate with each other, for which
we use the `IVar` type:

~~~~ {.haskell}
data IVar a
    deriving (Eq)

new :: Par (IVar a)
get :: IVar a -> Par a
put :: (NFData a) => IVar a -> a -> Par ()
~~~~

The `IVar` type is a *write-once* mutable reference.  The `get`
function blocks until a `put` has been performed.


# The NFData class

We know that `seq` evaluates an expression to WHNF, but sometimes we
want to completely evaluate an expression to normal form.

Enter the `deeqseq` package:

~~~~ {.haskell}
import Control.DeepSeq

class NFData a where
    rnf :: a -> ()
~~~~

We can write an `NFData` instance for any custom type.  Our `rnf`
implementation must reduce an expression to normal form.

~~~~ {.haskell}
data Foo a = Foo Int a

instance (NFData a) => NFData (Foo a) where
    rnf (Foo x y) = rnf x `seq` rnf y
~~~~


# Higher level operations

An extremely common pattern is for a thread to `fork` several children
and wait for them to finish.

We can easily capture this idea with a suitable combinator.

~~~~ {.haskell}
spawn :: (NFData a) => Par a -> Par (IVar a)
spawn act = do
  i <- new
  fork (put i =<< act)
  return i
~~~~

In fact, usually all we want is to simply wait for all children and
return a list of their results.

~~~~ {.haskell}
parMapM :: (NFData b) => (a -> Par b) -> [a] -> Par [b]
parMapM f acts = do 
  ivars <- mapM (spawn . f) acts
  mapM get ivars
~~~~


# Computing the mean of a sample

Suppose we need the mean of a list of numbers.  The naive way to do so
would be `sum xs / length xs`, but this has numerical stability problems.

Here's a more stable method:

~~~~ {.haskell}
import Data.List

data T a = T !a !Int

mean :: (RealFrac a) => [a] -> a
mean = fini . foldl' go (T 0 0)
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
      where m' = m + (x - m) / fromIntegral n'
            n' = n + 1
~~~~


# Questionable numbers

Supposing we have a set of sample data that we know little about, in
particular its precision and variance.

This is exactly the kind of problem that the criterion benchmarking
library faces: we have performance data, but it's dirty and doesn't
follow any particular statistical distribution.

A technique called the
[jackknife](http://en.wikipedia.org/wiki/Resampling_(statistics)) lets
us estimate these parameters.

We successively recompute a function (such as `mean`) over subsets of
a sample, each time with a sliding window cut out.

For a $w$-width window and $k$ samples, the jackknife has a cost of
$O((k-w)^2)$.

~~~~ {.haskell}
jackknife :: ([a] -> b) -> [a] -> [b]
jackknife f = map f . resamples 500
~~~~

# Resampling

It's easy to write a resampling function using familiar building
blocks.

~~~~ {.haskell}
resamples :: Int -> [a] -> [[a]]
resamples k xs =
    take (length xs - k) $
    zipWith (++) (inits xs) (map (drop k) (tails xs))
~~~~

Our function resamples a list with a window size of `k`.

~~~~ {.haskell}
>> resamples 2 [0..5]
[[    2,3,4,5],
 [0,    3,4,5],
 [0,1,    4,5],
 [0,1,2,    5]]
~~~~


# Speeding up the jackknife

The nice thing about the jackknife is that each element of the result
list is independent, so it's an "embarrassingly parallel" problem.

The `monad-par` package is very helpful in making this trivial to
parallelize.

~~~~ {.haskell}
import Control.Monad.Par (runPar, parMap)

jackknifeP f = runPar . parMap f . resamples 500
~~~~

Let's try it out!


# Suitably noisy data

![Noise!](foo.png)


# A test program

~~~~ {.haskell}
import System.Random.Mersenne

crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

main = do
  (xs,ys) <- splitAt 1500 . take 6000 <$> (randoms =<< getStdGen)
  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknifeP mean rs
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)
~~~~


# Compiling

We have to remember to build using *both* `-threaded` and `-rtsopts`.

~~~~
ghc -O --make -rtsopts -threaded Jackknife
~~~~

* `-threaded` gives us the threaded runtime.

* We'll need `-rtsopts` to tell the RTS how many cores to use.  *It
  will default to just one if we do not specify otherwise*.

To use e.g. 3 cores:

~~~~
./Jackknife +RTS -N3
~~~~

If we want to use all cores:

~~~~
./Jackknife +RTS -N
~~~~


# Other approaches to parallelism

Meanwhile, `par` and the `monad-par` packages are not the only ways to
achieve parallel speedups in Haskell.

There are other more "researchy" projects under way:

* For multicore machines: Data Parallel Haskell

* For GPU computing: the accelerate package


# Data Parallel Haskell

[DPH](http://www.haskell.org/haskellwiki/GHC/Data_Parallel_Haskell) is
the name of a research project to support
[nested data parallelism](http://www.cs.cmu.edu/~scandal/cacm/cacm2.html)
under GHC on multicore CPUs.

Nested data parallelism extends the programming model of *flat* data
parallelism, as known from modern Fortran dialects, to:

* Irregular parallel computations (such as divide-and-conquer
  algorithms) and

* Irregular data structures (such as sparse matrices and tree
  structures).

This project is still a work in progress, and is quite technically
intricate.


# Accelerate

The
[accelerate library](http://hackage.haskell.org/package/accelerate)
defines an embedded language for regular, multi-dimensional array
computations with multiple backends to facilitate high-performance
implementations.

Currently, there are two backends:

* An interpreter that serves as a reference implementation of the
  intended semantics of the language.
  
* A [CUDA](http://www.nvidia.com/object/cuda_home_new.html) backend
  generating code for CUDA-capable NVIDIA GPUs.


# Busting a myth

You'll sometimes hear uninformed people claim that functional
programming is some kind of magic pixie dust for parallel programming.

This is not really true.

Certainly, aspects of the programming model make incidental parts of
the problem easier.

However, tricky considerations like data dependencies, memory
placement, and thread scheduling have every bit as insidious an effect
on parallel performance in Haskell as in C.

Parallel programming is a *subtle art* no matter which language you do
it in.
