% Phantoms and mutants

# Language hacking

Let's create a very small fragment of a programming language:

~~~~ {.haskell}
data Expr = Num Int             -- atom
          | Str String          -- atom
          | Op BinOp Expr Expr  -- compound
            deriving (Show)

data BinOp = Add | Concat
             deriving (Show)
~~~~

And an interpreter for it:

~~~~ {.haskell}
interp x@(Num _)                     = x
interp x@(Str _)                     = x
interp (Op Add a b)                  = Num (i a + i b)
  where i x = case interp x of Num a -> a
interp (Op Concat (Str a) (Str b))   = Str (a ++ b)
~~~~


# Does it work?

Our very quick round of prototyping gave us a tiny interpreter that
actually seems to work:

~~~~ {.haskell}
>> interp (Op Add (Num 2) (Num 3))
Num 5
~~~~

Please help me to spot some problems with my interpreter!


# Two sides of the same problem

1. We can construct ill-formed expressions ("add a `Num` to a `Str`").

1. Our interpreter crashes on these expressions, because we (quite
   reasonably) didn't take their possible existence into account.


# Watch your language!

Here's a slightly modified version of our language:

~~~~ {.haskell}
data Expr a = Num Int
            | Str String
            | Op BinOp (Expr a) (Expr a)
              deriving (Show)

-- This is unchanged.
data BinOp = Add | Concat
             deriving (Show)
~~~~

We've introduced a type parameter here...

...But we never actually use it to represent a *value* of whatever
type `a` is.


Let's see where that takes us.


# Some modifications to our interpreter

Here is our modified interpreter.

~~~~ {.haskell}
interp x@(Num _)       = x
interp x@(Str _)       = x
interp (Op Add a b)    = Num (i a + i b)
  where i x = case interp x of Num a -> a
interp (Op Concat a b) = Str (i a ++ i b)
  where i x = case interp x of Str y -> y
~~~~

Our only change is to apply `interp` recursively if we're asked to
perform a `Concat`.  

We could have done this in our original interpreter, so that can't be
the real fix.  But what *is*?

What's the type of the rewritten `interp`?


# Our new type

The interpreter function now has this type:

~~~~ {.haskell}
interp :: Expr a -> Expr a
~~~~

But we know from the definitions of `Expr` and `BinOp` that we never
use a value of type `a`.  Then what purpose does this type parameter
serve?

Recall the type of `Expr`:

~~~~ {.haskell}
data Expr a = ...
            | Op BinOp (Expr a) (Expr a)
~~~~

# Some context

Let's think of that `a` parameter as expressing our *intent* that:

* The operands of an `Op` expression should have the same types.

* The resulting `Expr` value should *also* have this type.

~~~~ {.haskell}
data Expr a = ...
            | Op BinOp (Expr a) (Expr a)
~~~~

In fact, the type system will enforce these constraints for us.


# Building blocks

The first step in making all of this machinery work is to define some
functions with the right types.

These two functions will construct atoms (values that can't be reduced
any further) in our language:

~~~~ {.haskell}
num :: Int -> Expr Int
num = Num

str :: String -> Expr String
str = Str
~~~~


# Applying operators safely

These two functions construct compound expressions:

~~~~ {.haskell}
add :: Expr Int -> Expr Int -> Expr Int
add = Op Add

cat :: Expr String -> Expr String -> Expr String
cat = Op Concat
~~~~

Notice that each one enforces the restriction that its parameters must
be compatible.


# A trusted computing base

One we have our functions defined, the last step is to lock our world
down.

Here's what the beginning of my module looks like:

~~~~ {.haskell}
module Interp
    (
      Expr,       -- type constructor
      interp,     -- interpreter
      num, str,   -- atom constructors
      add, cat,   -- expression constructors
    ) where
~~~~

Notice that we've exercised *careful control* over what we're
exporting.

* We export the `Expr` type constructor, but *none* of its value
  constructors.
  
* Users of our module don't need `BinOp`, so we don't export that at
  all.
  

# More about our type and export choices

Consequences of exporting only the type constructor for `Expr`:

* Clients cannot use the value constructors to create new values.  

* The *only* way for a client to construct expressions is using our
  handwritten "smart constructor" functions with their carefully
  chosen types.

* Clients cannot pattern-match on an `Expr` value.  Our internals are
  opaque; we could change our implementation without clients being
  able to tell.

These are in fact the completely standard techniques for creating
abstract data types in Haskell.  So where does the type parameter come
in?


# Consequences of that type parameter

Due to our judicious use of both abstraction and that type parameter:

* Clients cannot construct ill-formed expressions. Any attempts will
  be rejected by the type checker.
  
This additional safety comes "for free": 

* We don't need runtime checks for ill-formed expressions, because
  they cannot occur.

* Our added type parameter never represents data at runtime, so it has
  zero cost when the program runs.
  

# Phantom types

When we refer to a type parameter on the left of a type definition,
without ever using *values* of that type on the right, we call it a
*phantom type*.

We're essentially encoding *compile-time data* using types, and the
compiler computes with this data before our program is ever run.


# Mutable variables

We've already seen the very handy `MVar` type, which represents a
"blocking mutable box": we can put a value in or take one out, but
we'll block if we put when full or take when empty.

Even though `MVar`s are the fastest blocking concurrent structure in
the industry (they made the the Kessel Run in less than twelve
parsecs!), we don't always want blocking semantics.

For cases where we want *non-*blocking updates, there's the `IORef`
type, which gives us mutable references.

~~~~ {.haskell}
import Data.IORef

newIORef    :: a -> IO (IORef a)

readIORef   :: IORef a -> IO a
writeIORef  :: IORef a -> a -> IO ()

modifyIORef :: IORef a -> (a -> a) -> IO ()
~~~~


# Managing mutation

Application writers are often faced with a question like this:

* I have a big app, and parts of it need their behaviour tweaked by an
  administrator at runtime.
  
There are of course many ways to address this sort of problem.

Let's consider one where we use a reference to a piece of config data.

Any code that's executing in the `IO` monad can, if it knows the name of
the config reference, retrieve the current config:

~~~~ {.haskell}
curCfg <- readIORef cfgRef
~~~~

The trouble is, ill-behaved code could clearly also *modify* the
current configuration, and leave us with a debugging nightmare.


# Phantom types to the rescue!

Let's create a new type of mutable reference.

We use a phantom type `t` to statically track whether a piece of code
is allowed to modify the reference or not.

~~~~ {.haskell}
import Data.IORef

newtype Ref t a = Ref (IORef a)
~~~~

Remember, our use of `newtype` here means that the `Ref` type only
exists at compile time: it imposes *no* runtime cost.

Since we are using a phantom type, we don't even need values of our
access control types:

~~~~ {.haskell}
data ReadOnly
data ReadWrite
~~~~

We're already in a good spot!  Not only are we creating
compiler-enforced access control, but it will have *zero* runtime
cost.


# Creating a mutable reference

To create a new reference, we just have to ensure that it has the
right type.

~~~~ {.haskell}
newRef :: a -> IO (Ref ReadWrite a)
newRef a = Ref `fmap` newIORef a
~~~~


# Reading and writing a mutable reference

Since we want to be able to read both read-only and read-write
references, we don't need to mention the access mode when writing a
type signature for `readRef`.

~~~~ {.haskell}
readRef :: Ref t a -> IO a
readRef (Ref ref) = readIORef ref
~~~~

Of course, code can only write to a reference if the compiler can
statically prove (via the type system) that it has write access.

~~~~ {.haskell}
writeRef :: Ref ReadWrite a -> a -> IO ()
writeRef (Ref ref) v = writeIORef ref v
~~~~


# Converting a reference to read-only

This function allows us to convert any kind of reference into a
read-only reference:

~~~~ {.haskell}
readOnly :: Ref t a -> Ref ReadOnly a
readOnly (Ref ref) = Ref ref
~~~~

In order to prevent clients from promoting a reference from read-only
to read-write, we do *not* provide a function that goes in the
opposite direction.

We also use the familiar technique of constructor hiding at the top of
our source file:

~~~~ {.haskell}
module Ref
    (
      Ref, -- export type ctor, but not value ctor
      newRef, readOnly,
      readRef, writeRef
    ) where
~~~~


# Meaning: that slippery thing

What does this type signature mean?

~~~~ {.haskell}
something :: a -> a
~~~~

What are all of the possible behaviours of a code with this type?

What about this signature?

~~~~ {.haskell}
another :: [a]
~~~~


# Being more explicit

What does this type signature mean?

~~~~ {.haskell}
something :: a -> a
~~~~

We know that for *all possible types* `a`, this function accepts a
value of that type, and returns a value of that type.

We *clearly* cannot enumerate all possible types, so we equally
clearly cannot create all (or indeed *any*) values of these types.

Therefore, if we exclude crashes and infinite loops, the only possible
behaviour for this function is to return its input.


# Being even more explicit: quantifiers

In fact, Haskell provides a keyword, `forall`, to make this
quantification over type parameters more explicit:

~~~~ {.haskell}
something :: forall a. a -> a
~~~~

The same "universal quantification" syntax works with typeclass
constraints:

~~~~ {.haskell}
something :: forall a. (Show a) -> String
~~~~

Here, our quantifier is "for all types `a`, where the *only thing we
know about* `a` is what the `Show` typeclass tells us we can do".

These `forall` keywords are implied if they're not explicitly written.


# Building blocks

Love 'em or hate 'em, everybody has to deal with databases.

Here are some typical functions that a low-level database library will
provide, for clients that have to modify data concurrently:

~~~~ {.haskell}
begin    :: Connection -> IO Transaction
commit   :: Transaction -> IO ()
rollback :: Transaction -> IO ()
~~~~

We can create a new transaction with `begin`, finish an existing
one with `commit`, or cancel one with `rollback`.

Typically, once a transaction has been committed or rolled back,
accessing it afterwards will result in an exception.


# Shaky foundations build a shaky house

Clearly, these constructs make it easy to inadvertantly write bad
code.

~~~~ {.haskell}
oops conn = do
  txn <- begin conn
  throwIO (AssertionFailed "forgot to roll back!")
  -- also forgot to commit!
~~~~

We can avoid `rollback` and `commit` forgetfulness with a suitable
combinator:

~~~~ {.haskell}
withTxn :: Connection -> IO a -> IO a
withTxn conn act = do
  txn <- begin conn
  r <- act `onException` rollback txn
  commit txn
  return r
~~~~

All right!  The code running in `act` never sees a `Transaction`
value, so it can't leak a committed or rolled back transaction.


# But still...

We're not out of the woods yet!

High-performance web apps typically use a dynamically managed pool of
database connections.

~~~~ {.haskell}
getConn :: Pool -> IO Connection
returnConn :: Pool -> Connection -> IO ()
~~~~ {.haskell}

It's a major bug if a database connection is not returned to the pool
at the end of a handler.

So we write a combinator to handle this for us:

~~~~ {.haskell}
withConn :: Pool -> (Connection -> IO a) -> IO a
withConn pool act =
  bracket (getConn pool) (returnConn pool) act
~~~~

Nice and elegant. But correct? Read on!


# Connections vs transactions

In a typical database API, once we enter a transaction, we don't need
to refer to the handle we got until we either commit or roll back the
transaction.

So it was fine for us to write a transaction wrapper like this:

~~~~ {.haskell}
withTxn :: Connection -> IO a -> IO a
~~~~

On other other hand, if we're talking to a database, we definitely
need a connection handle.

~~~~ {.haskell}
query :: Connection -> String -> IO [String]
~~~~

So we have to pass that handle into our combinator:

~~~~ {.haskell}
withConn :: Pool -> (Connection -> IO a) -> IO a
~~~~


# "Ouch, sorry about that!"

Unfortunately, since `withConn` gives us a connection handle, we can defeat the
intention of the combinator (sometimes accidentally).

What is the type of this function?

~~~~ {.haskell}
evil pool = withConn pool return
~~~~


# Phantom types! They'll save us again!

Here, we are using the `newtype` keyword to associate a phantom type
with the `IO` monad.

~~~~ {.haskell}
newtype DB c a = DB {
      fromDB :: IO a
    }
~~~~

We're going to run some code in the `IO` monad, and pass around a
little extra bit of type information at compile time.

Let's create a phantom-typed wrapper for our earlier `Connection`
type:

~~~~ {.haskell}
newtype SafeConn c = Safe Connection
~~~~

Where are these phantom types taking us?


# Safe querying

The easiest place to start to understand with a little use of our new
code, in the form of a function we'll export to clients.

This is just a wrapper around the `query` function we saw earlier,
making sure that our `newtype` machinery is in the right places to
keep the type checker happy.

~~~~ {.haskell}
safeQuery :: SafeConn c -> String -> DB c [String]
safeQuery (Safe conn) str = DB (query conn str)
~~~~

Notice that our phantom type `c` is mentioned in both our uses of
`SafeConn c` and `DB c`: we're treating it as a token that we have to
pass around.

Our library will *not* be exporting the value constructors for
`SafeConn` or `DB` to clients.  Once again, this `newtype` machinery
is internal to us!


# Giving a client a connection from a pool

Here, we'll use our earlier exception-safe `withConn` combinator.
Recall its type:

~~~~ {.haskell}
withConn :: Pool -> (Connection -> IO a) -> IO a
~~~~

To make it useful in our new setting, we have to wrap the
`Connection`, and unwrap the `DB c` that is our `act` to get an action
in the `IO` monad.

~~~~ {.haskell}
withSafeConn pool act =
  withConn pool $ \conn ->
    fromDB (act (Safe conn))
~~~~

It's not at all obvious what this is doing for us until we see the
type of `withSafeConn`.


# Scariness

Here's a burly type for you:

~~~~ {.haskell}
{-# LANGUAGE Rank2Types #-}

withConnection :: Pool
               -> (forall c. SafeConn c -> DB c a) 
               -> IO a
~~~~

We've introduced a universal quantifier (that `forall`) into our type
signature.  And we've added a `LANGUAGE` pragma!  Whoa. Duuude.

Relax!  Let's not worry about those details just yet.  What does our
signature seem to want to tell us?

* We accept a `Pool`.

* And an "I have a connection, so I can talk to the database now"
  action that accepts a `SafeConn c`, returning a value `a` embedded
  in the type `DB c`.

Not so scary after all.  Well, except for the details we're ignoring.


# Universal quantification to the rescue!

Let's start with the obviously bothersome part of the type signature.

~~~~ {.haskell}
(forall c. SafeConn c -> DB c a)
~~~~

This is the same universal quantification we've seen before, meaning:

* Our "I can haz connection" action must work *over all types* `c`.

* The *scope* of `c` extends only to the rightmost parenthesis here.

Putting it back into context:

~~~~ {.haskell}
withConnection :: Pool
               -> (forall c. SafeConn c -> DB c a) 
               -> IO a
~~~~

The type variable `c` can't escape from its scope, so `a` *cannot be
related* to `c`.


# Wait, wait. What, exactly, got rescued?

~~~~ {.haskell}
withConnection :: Pool
               -> (forall c. SafeConn c -> DB c a) 
               -> IO a
~~~~

Because `SafeConn c` shares the same phantom type as `DB c`, and the
quantified `c` type cannot escape to the outer `IO`, there is no way
for a `SafeConn c` *value* to escape, either!

In other words, we have ensured that a user of `withConnection` cannot
either accidentally allow or force a connection to escape from the
place where we've deemed them legal to use.


# Rank-2 types

Standard Haskell types and functions have just one scope for
universal quantification.

~~~~ {.haskell}
foo :: forall a b. a -> b -> a
~~~~

When an extra level of scoping for universal quantification is
introduced, this is called a rank-2 type.

~~~~ {.haskell}
fnord :: forall b. (forall a. a -> a) -> b
~~~~

(Normal types are thus called rank-1 types.)

Although widely used, rank-2 types are not yet a part of the Haskell
standard, hence our use of a pragma earlier:

~~~~ {.haskell}
{-# LANGUAGE Rank2Types #-}
~~~~


# Bonus question 1

What expressions can we write that have this type?

~~~~ {.haskell}
[forall a. a]
~~~~

What about this one?

~~~~ {.haskell}
[forall a. (Enum a) => a]
~~~~

Or this?

~~~~ {.haskell}
[forall a. (Num a) => a]
~~~~

# Bonus question 2

Do we have time to talk about how to write a `Monad` instance for `DB`?


# Purity in the face of change

We've now seen several cases where phantom types and rank-2 types let
us use the compiler to automatically prevent ourselves from writing
bad code.

We can also use them to introduce safe, controlled mutation into our
programs.


# Sad face

A typical lament of a functional programmer:

* "Alas! Woe is me! Etc., etc.! There is no known purely functional
  algorithm for my problem that performs as well as this seductive
  imperative code!"

:-(

:-(

Of course, in the worst case, we can emulate a flat, mutable memory
with a purely functional map, thus incurring only $O(\log\ n)$ of
additional overhead.


# Cake: having and eating

Enter the `ST` monad!

~~~~ {.haskell}
import Control.Monad.ST
~~~~

This defines for us a function with a glorious rank-2 type:

~~~~ {.haskell}
>> :t runST
runST :: (forall s. ST s a) -> a
~~~~

Since we've only just been introduced to rank-2 types, we know exactly
what this implies:

* What happens in the `ST` monad *stays* in the `ST` monad.

* Nevertheles, we can obtain a pure result when we run an action in
  this monad.  That's an exciting prospect!


# Mutable references, ST style

The `STRef` type gives us the same mutable references as `IORef`, but
in the `ST` monad.

~~~~ {.haskell}
import Control.Monad.ST
import Data.STRef

whee :: ST s Int
whee z = do
  r <- newSTRef z
  modifySTRef r (+1)
  readSTRef r
~~~~

Let's try this in `ghci`:

~~~~ {.haskell}
>> runST (whee 1)
2
~~~~

Thanks to chaining of the universally quantified `s`, there is no way
for an `STRef` to escape from the `ST` monad, save by the approved
route of reading its current value with `readSTRef`.

~~~~ {.haskell}
newSTRef  :: a -> ST s (STRef s a)
readSTRef :: STRef s a -> ST s a
~~~~


# Arrays and vectors

For working with large collections of uniform data, the usual
representation in most languages is an array.

The longtime standard for working with arrays in Haskell is the
`Array` type, from the `array` package, but I don't like it: it has an
API that is simultaneously bizarre, too general, and puny.

I much prefer its modern cousin, the `vector` package:

* `vector` provides a *vastly* richer API than `array`.

* A `Vector` is one-dimensional and indexed by `Int`s counting from
  zero, so it's easy to reason about.
  
* An `Array` is indexed by an instance of the `Ix` class, can have
  arbitrary bounds, and makes my brain hurt.


# Families and flavours of vectors

The `vector` package provides two "flavours" of vector type:

* `Vector` types are immutable.

* `MVector` types can be modified in either the `ST` or `IO` monad,
  and cannot be read by purely functional code.
  
Within these flavours, there are two "families" of vector type:

* Unboxed vectors are tightly packed in contiguous memory.  They are
  very fast, but it is only possible to create unboxed vectors of
  certain types, and an unboxed vector can't store thunks.
  
* Normal vectors are boxed, just like ordinary Haskell values. Any
  value can be stored in a plain old vector, at the cost of an
  additional level of indirection.
  
We can thus have an immutable unboxed vector, a mutable boxed vector,
and so on.


# Mutable vectors in action

The classic Haskell implementation of a "quicksort":

~~~~ {.haskell}
import Data.List (partition)

qsort (p:xs) = qsort lt ++ [p] ++ qsort ge
  where (lt,ge) = partition (<p) xs
qsort _      = []
~~~~

This isn't *really* a quicksort, because it doesn't operate in-place.

We can apply our newfound knowledge to this problem:

~~~~ {.haskell}
import qualified Data.Vector.Unboxed.Mutable as V
import Control.Monad.ST (ST)

quicksort :: V.MVector s Int -> ST s ()
quicksort vec = go 0 (V.length vec)
  where
    {- ... -}
~~~~


# The recursive step

~~~~ {.haskell}
    recur left right
      | left >= right = return ()
      | otherwise     = do
      idx <- partition left right
             (left + (right-left) `div` 2)
      recur left (idx-1)
      recur (idx+1) right
~~~~


# Partitioning the vector

(Remember, `vec` is in scope here.)

~~~~ {.haskell}
    partition left right pivotIdx = do
      pivot <- V.read vec pivotIdx
      V.swap vec pivotIdx right
      let loop i k
            | i == right = V.swap vec k right >>
                           return k
            | otherwise = do
            v <- V.read vec i
            if v < pivot
              then V.swap vec i k >>
                   loop (i+1) (k+1)
              else loop (i+1) k
      loop left left
~~~~


# From immutable to mutable, and back

We can even use this in-place sort to efficiently perform an in-place
sort of an immutable array!

Our building blocks:

~~~~ {.haskell}
thaw   :: Vector a -> ST s (MVector s a)
create :: (forall s. ST s (MVector s a)) -> Vector a
~~~~

* `thaw` creates a new mutable vector, and copies the contents of the
  immutable vector into it.

* `create` runs an `ST` action that returns a mutable vector, and
  "freezes" its result to be immutable, and hence usable in pure code.

~~~~ {.haskell}
import qualified Data.Vector.Unboxed as U

vsort :: U.Vector Int -> U.Vector Int
vsort v = U.create $ do
            vec <- U.thaw v
            quicksort vec
            return vec
~~~~


# Mutability, purity, and determinism

The big advantage of the `ST` monad is that it gives us the ability to
efficiently run computations that require mutability, while both the
inputs to and results of our computations remain pure.

In order to achieve this, we sacrifice some power:

* We can't run arbitrary `IO` actions.  No database accesses, no
  filesystem, etc.

* Other potential sources of nondeterminism (e.g. threads) are thus
  also off limits.


# Laziness

Originally, this lecture was supposed to be all about the joys of lazy
evaluation, but we hijacked much of our time to serve other purposes.

I'm going to talk a little bit about it anyway.

In a minute.


# A digression

How can we use random numbers to approximate the value of $\pi$?


# A digression

How can we use random numbers to approximate the value of pi?

* Take two random numbers, $x$ and $y$, on the interval $[0,1]$

* Add their squares: $r = x^2 + y^2$

* We have a $\pi/4$ probability of $r \le 1$

What can we do with this knowledge?


# Purely functional random numbers

Haskell supplies a `random` package that we can use in a purely
functional setting.

~~~~ {.haskell}
class Random a where
    random :: RandomGen g => g -> (a, g)

class RandomGen g where
    next   :: g -> (Int, g)
    split  :: g -> (g, g)
~~~~


# RandomGen

The `RandomGen` class is a building block: it specifies an interface
for a generator that can generate uniformly distributed pseudo-random
`Int`s.

There is one default instance of this class:

~~~~ {.haskell}
data StdGen {- opaque -}

instance RandomGen StdGen
~~~~


# Random

The `Random` class specifies how to generate a pseudo-random value of
some type, given the random numbers generated by a `Gen` instance.

Quite a few common types have `Random` instances.

* For `Int`, the instance will generate any representable value.

* For `Double`, the instance will generate a value in the range
  $[0,1]$.
  

# Generators are pure

Since we want to use a PRNG in pure code, we obviously can't modify
the state of a PRNG when we generate a new value.

This is why `next` and `random` return a *new* state for the PRNG
every time we generate a new pseudo-random value.


# Throwing darts at the board

Here's how we can generate a guess at $x^2 + y^2$:

~~~~ {.haskell}
guess :: (RandomGen g) => (Double,g) -> (Double,g)
guess (_,g) = (z, g'')
    where z        = x^2 + y^2
          (x, g')  = random g
          (y, g'') = random g'
~~~~

Note that we have to hand back the *final* state of the PRNG along
with our result! 

If we handed back `g` or `g'` instead, our numbers would either be all
identical or disastrously correlated (every `x` would just be a repeat
of the previous `y`).


# Global state

We can use the `getStdGen` function to get a handy global PRNG state:

~~~~ {.haskell}
getStdGen :: IO StdGen
~~~~

This does *not* modify the state, though. If we use `getStdGen` twice
in succession, we'll get the same result each time.

To be safe, we should update the global PRNG state with the final PRNG
state returned by our pure code:

~~~~ {.haskell}
setStdGen :: StdGen -> IO ()
~~~~


# Ugh - let's split!

Calling `getStdGen` and `setStdGen` from `ghci` is a pain, so let's
write a combinator to help us.

Remember that `split` method from earlier?

~~~~ {.haskell}
class RandomGen g where
    split  :: g -> (g, g)
~~~~

This "forks" the PRNG, creating two children with different states.

The hope is that the states will be different enough that
pseudo-random values generated from each will not be obviously
correlated.

~~~~ {.haskell}
withGen :: (StdGen -> a) -> IO a
withGen f = do
  g <- getStdGen
  let (g',g'') = split g
  setStdGen g'
  return (f g'')
~~~~


# Living in ghci

Now we can use our `guess` function reasonably easily.

~~~~ {.haskell}
>> let f = fst `fmap` withGen (guess . ((,) 0))
>> f
1.2397265526054513
>> f
0.9506331164887969
~~~~


# Let's iterate

Here's a useful function from the `Prelude`:

~~~~ {.haskell}
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
~~~~

Obviously that list is infinite.

Let's use `iterate` and `guess`, and as much other `Prelude` machinery
as we can think of, to write a function that can approximate $\pi$.

By the way, in case you don't recognize this technique, it's a famous
example of the family of
[Monte Carlo methods](http://en.wikipedia.org/wiki/Monte_Carlo_method).


# Where's the connection to laziness?

What aspects of laziness were important in developing our solution?
