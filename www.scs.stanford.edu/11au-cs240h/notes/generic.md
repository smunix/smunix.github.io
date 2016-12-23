
# Coverting tuples to lists

* Let's say you wanted to convert pairs to lists of `Strings`

    ~~~~ {.haskell}
    pairToStringList :: (Show a, Show b) => (a, b) -> [String]
    pairToStringList (a, b) = [show a, show b]
    ~~~~

    ~~~~
    *Main> pairToStringList (True, Just 3)
    ["True","Just 3"]
    ~~~~

* Now say you want to convert a pair of `Enum`s to a list of `Int`s

    ~~~~ {.haskell}
    pairToIntList :: (Enum a, Enum b) => (a, b) -> [Int]
    pairToIntList (a, b) = [fromEnum a, fromEnum b]
    ~~~~

* Can we generalize this function?  Would like to say:

    ~~~~ {.haskell}
    pairToList conv (a, b) = [conv a, conv b]
    pairToList show (True, Just 3)   -- error
    ~~~~

    * Unfortunately, can't pass *methods* as arguments, only
      *functions*

        ~~~~ {.haskell}
        pairToList :: (a -> b) -> (a, a) -> [b]
        ~~~~

# Polymorphism with fundeps

* Let's represent ad hoc polymorphic methods with a *class*

    ~~~~ {.haskell}
    {-# LANGUAGE MultiParamTypeClasses #-}
    {-# LANGUAGE FunctionalDependencies #-}
    {-# LANGUAGE FlexibleInstances #-}

    class Function f a b | f a -> b where
        funcall :: f -> a -> b
    instance Function (a -> b) a b where
        funcall = id

    pairToList :: (Function f a c, Function f b c) =>
                  f -> (a, b) -> [c]
    pairToList f (a, b) = [funcall f a, funcall f b]
    ~~~~

* Use placeholder singleton types to represent particular methods

    ~~~~ {.haskell}
    data ShowF = ShowF
    instance (Show a) => Function ShowF a [Char] where
        funcall _ = show

    data FromEnumF = FromEnumF
    instance (Enum a) => Function FromEnumF a Int where
        funcall _ = fromEnum
    ~~~~

# `Function` in action

* Now singleton types act like method arguments:

    ~~~~ {.haskell}
    *Main> pairToList ShowF (True, 3)
    ["True","3"]
    *Main> pairToList FromEnumF (False, 7)
    [0,7]
    ~~~~

* Now, what if you wanted `tupleToList` for arbitrary $n$-tuples?
    * Can auto-generate instances for a generic tuple fold, e.g.:

    ~~~~ {.haskell}
    class TupleFoldr f z t r | f z t -> r where
        tupleFoldr :: f -> z -> t -> r
    ~~~~

    * Works okay for small tuples, craps out around 10-tuple without
      larger `-fcontext-stack` argument

* Unfortunately, I'm temporarily out of compile-time tricks
    * An alternative is to use run-time type information (RTTI)
    * RTTI easier to reason about, but adds runtime overhead and
      errors
    * We will come back to static tricks at end of lecture

# [`DeriveDataTypeable`][] extension

* Haskell allows six classes to be automatically derived
    * `Show`, `Read`, `Eq`, `Ord`, `Bounded`, `Enum`
* [`DeriveDataTypeable`] extension adds two more: `Typeable`, `Data`

    ~~~~ {.haskell}
    data MyType = Con1 Int | Con2 String deriving (Typeable, Data)
    ~~~~

    * These types encode run-time type information in various ways
    * Require that inner types (`Int`, `String` above) also have
      instances
    * Okay to use parameterized types, though

    ~~~~ {.haskell}
    data MyTyCon a = MyTyCon a deriving (Typeable, Data)
    ~~~~

    * Most standard library types have `Typeable` and `Data` instances
* Provide programming approach known as "scrap your boilerplate"
    * GHC's support described by two papers: [[Boilerplate1]],
      [[Boilerplate2]]

# The [`Typeable`][] class

* `import Data.Typeable` to get `Typeable` class:

    ~~~~ {.haskell}
    class Typeable a where
        typeOf :: a -> TypeRep -- Note: never evaluates argument

    data TypeRep -- Opaque, but instance of Eq, Ord, Show, Typeable
    ~~~~

* This allows us to compare types for equality

    ~~~~ {.haskell}
    rtTypeEq :: (Typeable a, Typeable b) => a -> b -> Bool
    rtTypeEq a b = typeOf a == typeOf b
    ~~~~

    ~~~~
    *Main> rtTypeEq True False
    True
    *Main> rtTypeEq True 5
    False
    ~~~~

* Big Whoop!
    * Couldn't we already do this at compile time with
      `OverlappingInstances`?
    * Doing it dynamically is less exciting, but different
    * And allows one very important function...

# Type Casting

* GHC has a function
  [`unsafeCoerce`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Unsafe-Coerce.html#v:unsafeCoerce)

    ~~~~ {.haskell}
    unsafeCoerce :: a -> b
    ~~~~

    * And note: it doesn't just return $\bot$
    * If the name doesn't scare you, the type signature should

* Let's use `Typeable` to make a safe `cast` function

    ~~~~ {.haskell}
    cast :: (Typeable a, Typeable b) => a -> Maybe b
    cast a = fix $ \ ~(Just b) -> if typeOf a == typeOf b
                                  then Just $ unsafeCoerce a
                                  else Nothing
    ~~~~

    ~~~~
    *Main> cast "hello" :: Maybe String
    Just "hello"
    *Main> cast "hello" :: Maybe Int
    Nothing
    ~~~~

    * Safe if `typeOf` on two different types always returns different
      `TypeRep`s
    * Guaranteed by `deriving (Typeable)`; SafeHaskell disallows
      manual instances

# Generalized casting

* To cast monadic computations, etc., use generalized cast, `gcast`:

    ~~~~ {.haskell}
    import Data.Maybe (fromJust)

    gcast :: (Typeable a, Typeable b) => c a -> Maybe (c b)
    gcast ca = mcr
      where mcr = if typeOf (unc ca) == typeOf (unc $ fromJust mcr)
                  then Just $ unsafeCoerce ca
                  else Nothing
            unc :: c x -> x
            unc = undefined
    ~~~~

    ~~~~
    *Main> fromJust $ gcast (readFile "/etc/issue") :: IO String
    "\nArch Linux \\r  (\\n) (\\l)\n\n"
    *Main> fromJust $ gcast (readFile "/etc/issue") :: IO Int
    *** Exception: Maybe.fromJust: Nothing
    ~~~~

* Note undefined function `unc` in definition of `gcast`
    * Common idiom--poses no problem because `typeOf` is not strict
    * Recall context `Typeable b =>` is like a hidden argument; often
      use undefined functions with type signatures to unpack types and
      get dictionaries

# Using `Typeable`: `mkT` [[Boilerplate1]]

* Write a function that behaves like `id` except on one type

    ~~~~ {.haskell}
    mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
    mkT f = case cast f of
              Just g -> g
              Nothing -> id
    ~~~~

    * `mkT` stands for "make transformation"

* Example:

    ~~~~ {.haskell}
    newtype Salary = Salary Double deriving (Show, Data, Typeable)

    raiseSalary :: (Typeable a) => a -> a
    raiseSalary = mkT $ \(Salary s) -> Salary (s * 1.04)
    ~~~~

    ~~~~
    *Main> raiseSalary ()
    ()
    *Main> raiseSalary 7
    7
    *Main> raiseSalary (Salary 7)
    Salary 7.28
    ~~~~

# Using `Typeable`: `mkQ` [[Boilerplate1]]

* Function that computes over one type or returns default val:

    ~~~~ {.haskell}
    mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
    mkQ defaultVal fn a = case cast a of
                            Just b -> fn b
                            Nothing -> defaultVal
    ~~~~

    * `mkQ` stands for "make query"
* Example

    ~~~~ {.haskell}
    salaryVal :: Typeable a => a -> Double
    salaryVal = mkQ 0 $ \(Salary s) -> s
    ~~~~

    ~~~~
    *Main> salaryVal ()
    0.0
    *Main> salaryVal 7
    0.0
    *Main> salaryVal (Salary 7)
    7.0
    ~~~~

# Functions on multiple types: `extQ`

* `mkQ` only works for one type
    * Let's extend `mkQ`'s output to work on another type [[Boilerplate1]]

    ~~~~ {.haskell}
    extQ :: (Typeable a, Typeable b) =>
            (a -> r) -> (b -> r) -> a -> r
    extQ q f a = case cast a of
                   Just b -> f b
                   Nothing -> q a
    ~~~~

* Now can cascade multiple one-type query functions

    ~~~~ {.haskell}
    myShow :: Typeable a => a -> Maybe String
    myShow = mkQ Nothing (Just . show :: Int -> Maybe String)
             `extQ` (Just . show :: Bool -> Maybe String)
             `extQ` (Just . show :: Integer -> Maybe String)
             `extQ` (Just . show :: Double -> Maybe String)
    ~~~~

    * Kind of tedious, but could approximate goal of `tupleToList` at
      beginning of lecture if tuples contain limited number of types

# [`ExistentialQuantification`][] extension

* Lets you introduce type variables on right side of `data`
  declaration

    ~~~~ {.haskell}
    {-# LANGUAGE ExistentialQuantification #-}
    data Step s a = Done | Skip !s | Yield !a !s
    data Stream a = forall s. Stream (s -> Step s a) !s                
    ~~~~

    * Given a value of type `Stream a`, there exists a type `s` such
      that...<br> But syntax uses `forall`, not `exists`, to avoid
      introducing new keyword
    * Very safe extension (`Control.Exception` relies on it)
* Don't confuse with [`Rank2Types`], where `forall` means for all
  types `s`:

    ~~~~ {.haskell}
    data Stream a = Stream (forall s. s -> Step s a)
    ~~~~

* Contexts on existential variables like hidden dictionary fields

    ~~~~ {.haskell}
    data Showable = forall a. (Show a) => Showable a
    instance Show Showable where
        show (Showable a) = "Showable " ++ show a
    ~~~~

    * A `Showable` value has both a value of type `a`, and a
      dictionary for `Show`

# Example: Dynamic type

* [`Data.Dynamic`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Dynamic.html)
  has type `Dynamic`, which can hold anything `Typeable`

    ~~~~ {.haskell}
    data Dynamic  -- opaque type
    toDyn :: Typeable a => a -> Dynamic
    fromDynamic :: Typeable a => Dynamic -> Maybe a
    ~~~~

* Actual implementation slightly gross
    * Uses `unsafeCoerce` to coerce everything to a placeholder `Obj`
      type
* But easy to implement safely with `ExistentialQuantification`:

    ~~~~ {.haskell}
    data Dynamic = forall a. Typeable a => Dynamic a

    toDyn :: Typeable a => a -> Dynamic
    toDyn = Dynamic

    fromDynamic :: Typeable a => Dynamic -> Maybe a
    fromDynamic (Dynamic a) = cast a
    ~~~~

# Example: Extensible exceptions [[Marlow]]

* GHC runtime implements primitive, unsafe exceptions

    ~~~~ {.haskell}
    raise# :: a -> b
    catch# :: IO a -> (b -> IO a) -> IO a  -- slight simplification
    ~~~~

    * Must ensure that, as used, `b` is always same type, otherwise
      get unsafe coercion
* In reality, want many exception types, organized into a hierarchy
* [`Control.Exception`][] implements safe, hierarchical exceptions
    * `raise#` and `catch#` only ever called with one type:
      `SomeException`

    ~~~~ {.haskell}
    class (Typeable e, Show e) => Exception e where
        toException :: e -> SomeException
        toException = SomeException                 -- default impl
        fromException :: SomeException -> Maybe e
        fromException (SomeException e) = cast e    -- default impl

    data SomeException = forall e. Exception e => SomeException e
        deriving Typeable  -- note use of ExistentialQuantification
    instance Show SomeException where
        show (SomeException e) = show e
    ~~~~

# Throwing and catching exceptions

~~~~ {.haskell}
class (Typeable e, Show e) => Exception e where
    toException :: e -> SomeException
    fromException :: SomeException -> Maybe e
~~~~

* To throw an exception, first convert it to type `SomeException`

    ~~~~ {.haskell}
    throw :: Exception e => e -> a
    throw e = raise# (toException e)
    ~~~~

* To catch an exception, must ensure it matches desired type

    ~~~~ {.haskell}
    -- Define catchX because catch#'s real type more complicated
    catchX :: IO a -> (b -> IO a) -> IO a
    catchX (IO a) handler = IO $ catch# a (unIO . handler)

    catch :: (Exception e) => IO a -> (e -> IO a) -> IO a
    catch action handler = catchX action handler'
        where handler' se | Just e <- fromException se = handler e
                          | otherwise                  = throwIO se
    ~~~~

    * Note calling `handler e` makes `fromException se` uses `e`'s
      `Exception` dictionary, because `Just e` is `fromException`'s
      return value

# Making hierarchical exceptions

* Easy to add your own top-level exception type

    ~~~~ {.haskell}
    data MyException = MyException deriving (Show, Typeable)
    instance Exception MyException -- use default methods
    ~~~~

* But you can also create a hierarchy of exception types

    ~~~~ {.haskell}
    data AppError = forall e. Exception e => AppError e
                    deriving (Typeable)
    instance Show AppError where show (AppError e) = show e
    instance Exception AppError

    data Error1 = Error1 deriving (Show, Typeable)
    instance Exception Error1 where
        toException = toException . AppError
        fromException se = do  -- using Maybe as a Monad here
          AppError e <- fromException se
          cast e

    -- Now can do the same for Error2, and catch both as AppError
    ~~~~

    * Let's you catch just `Error1`, or any `AppError`

# The [`Data`] class

~~~~ {.haskell}
class Typeable a => Data a where ...
~~~~

* In addition to `Typeable`, can also derive `Data`
    * Allows generic traversal and construction of data structures
    * We will build it up one method at a time, using the following example

    ~~~~ {.haskell}
    import Data.Data

    data T a b = C1 a b | C2 deriving (Typeable, Data)
    ~~~~

* `deriving Data` will cause this `gfoldl` method to be defined

    ~~~~ {.haskell}
    gfoldl k z (C1 a b) = z C1 `k` a `k` b
    gfoldl k z C2       = z C2
    ~~~~

    * This allows us to implement functions working over all sized tuples
* Two limitations:
    1. Once you introduce types, things get uglier [cosmetic]
    2. The only dictionaries available are `Data` and `Typeable` [fundamental]

# `gfoldl` traversals

* The actual type of `gfoldl`:

    ~~~~ {.haskell}
    -- Recall:  gfoldl k z (C1 a b) = ((z C1) `k` a) `k` b

    gfoldl  :: (forall d b. Data d => c (d -> b) -> d -> c b)  -- k
            -> (forall g. g -> c g)                            -- z
            -> a
            -> c a
    ~~~~

* If you ignore the `c` parameter, looks like re-applying constructor
    * E.g., call `gfoldl ($) id x`, where `b` type of partially
       applied constructor
    * Can wrap `Identity` monad (applicative functor) around values to
      ignore `c`

    ~~~~ {.haskell}
    raiseSalaries :: (Data x) => x -> x
    raiseSalaries x = runIdentity $ gfoldl step return (raiseSalary x)
        where step cdb d = cdb <*> (pure $ raiseSalaries d)
    ~~~~

    * Function only bumps salaries, leaves other data fields alone

    ~~~~
    *Main> raiseSalaries $ Just (1, Salary 4, True, (Salary 7, ()))
    Just (1,Salary 4.16,True,(Salary 7.28,()))
    ~~~~

# `gfoldl` queries

* Can use a different type `c` to ignore constructor/arg types

    ~~~~ {.haskell}
    newtype Q r a = Q { unQ :: r }

    qappend :: (Monoid r) => Q r a -> Q r b -> Q r c
    qappend (Q r1) (Q r2) = Q $ mappend r1 r2
    ~~~~

    * Notice we completely ignore second type argument (`a`)
* Now say we want to sum all salaries in a structure

    ~~~~ {.haskell}
    sumSalaries :: (Data x) => x -> Double
    sumSalaries x = getSum $ unQ $ gfoldl step (\_ -> toQ x) x
        where step tot d = tot `qappend` (Q $ Sum $ sumSalaries d)
              toQ = mkQ (Q $ Sum 0) $ \(Salary s) -> Q $ Sum s
    ~~~~

    ~~~~
    *Main> sumSalaries (Salary 7, Salary 9, True, Just (Salary 4))
    20.0
    ~~~~

# Unfolding [[Boilerplate2]]

* We've seen how to traverse, modify, and reduce data structures
    * Could, for instance, use `gfoldl` to serialize a data structure
    * What about unserializing a data structure?
* `Data` contains two more useful methods

    * Again, assume example type

    ~~~~ {.haskell}
    data T a b = C1 a b | C2 deriving (Typeable, Data)
    ~~~~

    * And `Data` will contain the following methods for `T`:

    ~~~~ {.haskell}
    toConstr (C1 _ _) = ...     -- encodes constructor number
    toConstr C2       = ...

    gunfold k z c = case constrIndex c of
                        1 -> k (k (z C1))
                        2 -> z C2
    ~~~~

    * This is the dual of `gfoldl`--instead of supplying the values to
      `k`, now `k` has a chance to feed values to the constructor

# Type of `gunfold`

~~~~ {.haskell}
class (Typeable a) => Data a where
    dataTypeOf :: a -> DataType -- non-strict, return has [Constr]
    toConstr :: a -> Constr
    gunfold :: (forall b r. Data b => c (b -> r) -> c r)
            -> (forall r. r -> c r)
            -> Constr
            -> c a

dataTypeConstrs :: DataType -> [Constr]
indexConstr :: DataType -> Int -> Constr
maxConstrIndex :: DataType -> Int
~~~~

* Now you can use `cast` to produce values to feed into constructor
* Can use to implement generic read/unmarshal functions
    * See examples in [[Boilerplate2]] paper

# The [`generic-deriving`](http://hackage.haskell.org/package/generic-deriving) package

* All this boilerplate stuff happens at runtime
    * Slows your program down, doesn't catch errors at compile time
* Another approach is to do it all statically
  [[Magalh&#xe3;es]](http://dreixel.net/research/pdf/gdmh.pdf)

    * Define a single `Generic` class that converts any datatype to a
      `Rep` that can be computed over generically:

    ~~~~ {.haskell}
    {-# LANGUAGE TypeFamilies #-}

    class Generic a where
      type Rep a :: * -> *
      from :: a -> Rep a x
      to :: Rep a x -> a
    ~~~~

    * `type Rep` is an extension called `TypeFamilies`.  Can read
      above as:

        ~~~~ {.haskell}
        class Generic a rep | a -> rep where
	    from :: a -> rep x
	    to :: rep x -> a
        ~~~~

* But what is a generic representation?

# `generic-deriving` classes

* Need to be able to deconstruct/query `Rep`; let's use classes for
  that

    ~~~~ {.haskell}
    {-# LANGUAGE TypeFamilies, KindSignatures #-}

    class Datatype d where
      datatypeName :: t d (f :: * -> *) a -> String
      moduleName   :: t d (f :: * -> *) a -> String
    class Selector s where
      selName :: t s (f :: * -> *) a -> String
    class Constructor c where
      conName :: t c (f :: * -> *) a -> String
    ~~~~

    * For example:

    ~~~~ {.haskell}
    {-# LANGUAGE TemplateHaskell #-}
    import Generics.Deriving
    import Generics.Deriving.TH

    data T a b = C1 a b | C2 deriving (Show)
    deriveAll ''T -- creates a Generic instance for T
    ~~~~

    ~~~~
    *Main> datatypeName $ from (C1 () ())
    "T"
    ~~~~

# `generic-deriving` types

~~~~ {.haskell}
-- Nullary constructor (e.g., C2 in data T = ... | C2)
data U1 p = U1

-- Constructor with multiple arguments
data (:*:) f g p = f p :*: g p
infixr 6 :*:

-- Type with multiple constructors
data (:+:) f g p = L1 { unL1 :: f p } | R1 { unR1 :: g p }
infixr 5 :+:

newtype K1 i c p = K1 { unK1 :: c }
type Rec0 = K1 R

newtype M1 i c f p = M1 { unM1 :: f p }
data D; type D1 = M1 D -- c instance of Datatype, f is C1 or :+:
data C; type C1 = M1 C -- c instance of Constructor, f is S1 or :*:
data S; type S1 = M1 S -- c instance of Selector, f is Rec0 or U1
~~~~

* Ignore parameter `p` (reserved to support type parameters of kind
  &#x2217; &#x2192; &#x2217;)
* `M1` exists so a single traversal method can skip over `D1`, `C1`, and `S1`
* Could say `newtype Rec0 c p = K1 c`, but some instances use `K1 P`

# Example `deriveAll` output

~~~~ {.haskell}
data T a b = C1 a b | C2 deriving (Show)

-- deriveAll ''T spit out:
data T_
instance Datatype T_ where
    datatypeName _ = "T"
    moduleName _ = "Main"

data T_C1_
data T_C2_
instance Constructor T_C1_ where conName _ = "C1"
instance Constructor T_C2_ where conName _ = "C2"

type Rep0T_ a_0 b_1 = D1 T_
  (C1 T_C1_ (S1 NoSelector (Rec0 a_0) :*: S1 NoSelector (Rec0 b_1))
   :+: (C1 T_C2_ (S1 NoSelector U1)))

instance Generic (T a_0 b_1) where
    type Rep (T a_0 b_1) = Rep0T_ a_0 b_1
    from (C1 f0 f1) = M1 (L1 (M1 (M1 (K1 f0) :*: M1 (K1 f1))))
    from (C2)       = M1 (R1 (M1 (M1 U1)))
    to (M1 (L1 (M1 (M1 (K1 f0) :*: M1 (K1 f1))))) = C1 f0 f1
    to (M1 (R1 (M1 (M1 (U1)))))                   = C2
~~~~

# How can we use this?

* Say we are defining our own `Show`-like class

    ~~~~ {.haskell}
    class MyShow a where myShow :: a -> String
    instance MyShow [Char] where myShow = show
    instance MyShow Int where myShow = show
    ~~~~

* Want it to work with all user-defined data types
    * Let's define a class `Show1` to deal with annoying `p`
      parameters

    ~~~~ {.haskell}
    {-# LANGUAGE FlexibleInstances, UndecidableInstances,
      OverlappingInstances, TypeSynonymInstances, TypeOperators,
      TypeFamilies, TemplateHaskell, FlexibleContexts #-}

    class MyShow1 f where myShow1 :: f p -> String
    ~~~~

    * And let's define generic traversal methods

    ~~~~ {.haskell}
    instance (MyShow1 f) => MyShow1 (M1 i c f) where  -- for D1, S1
        myShow1 m1 = myShow1 (unM1 m1)
    instance (MyShow1 f, MyShow1 g) => MyShow1 (f :+: g) where
        myShow1 (L1 a) = myShow1 a
        myShow1 (R1 a) = myShow1 a
    ~~~~

# Non-generic instances of `MyShow1`

* When we hit a constructor, want to print the name

    ~~~~ {.haskell}
    instance (Constructor c, MyShow1 f) => MyShow1 (C1 c f) where
        myShow1 m1 = conName m1 ++ myShow1 (unM1 m1)
    ~~~~

    * We're using OverlappingInstances, since already have `M1` instance

* When we have no constructor args, don't show anything

    ~~~~ {.haskell}
    instance MyShow1 U1 where myShow1 _ = ""
    ~~~~

* When we have multiple constructor args, show them all

    ~~~~ {.haskell}
    instance (MyShow1 f, MyShow1 g) => MyShow1 (f :*: g) where
        myShow1 (fp :*: gp) = myShow1 fp ++ myShow1 gp
    ~~~~

* When you hit the actual value, show it

    ~~~~ {.haskell}
    instance (MyShow c) => MyShow1 (K1 i c) where
        myShow1 k1 = ' ' : myShow (unK1 k1)
    ~~~~

    * Now we're calling `myShow`, which we haven't yet defined for
      many types

# Implementing a generic `MyShow`

* Now can define generic `MyShow` in terms of `MyShow1`

    ~~~~ {.haskell}
    instance (Generic a, MyShow1 (Rep a)) => MyShow a where
        myShow a = myShow1 $ from a
    ~~~~

* Can we avoid `OverlappingInstances`?
    * Could have defined separate `D1`, `S1` instances of `Show1` (easy)
    * Could have avoided completely generic instance<br> Recommended
      use is just to define a *function* `myShowDefault`, then

    ~~~~ {.haskell}
    myShowDefault :: (Generic a, MyShow1 (Rep a)) => a -> String
    myShowDefault a = myShow1 $ from a

    instance MyShow T1 where myShow = myShowDefault
    instance MyShow T2 where myShow = myShowDefault
    instance MyShow T3 where myShow = myShowDefault
    ...
    ~~~~

    * There's still the problem of different behavior for `[Char]`
      vs. `[a]`...<br> I don't see how to fix this one, but might be
      possible


# GHC support for generic deriving

* GHC 7.2 supports Generic deriving through two new language extensions
* [`DeriveGenerics`][] extension
    * Just add `deriving (Generic)` to the end of declarations
* [`DefaultSignatures`][] extension
    * Allows default methods that don't work for all instances

    ~~~~ {.haskell}
    class MyShow a where
        myShow :: a -> String
        default myShow :: (Generic a, MyShow1 (Rep a)) => a -> String
        myShow = myShowDefault
    ~~~~

    * Makes it easier to declare instances

    ~~~~ {.haskell}
    instance MyShow T    -- no need for a where clause
    ~~~~


[`Data`]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Data.html#t:Data
[`Control.Exception`]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Exception.html
[`DeriveDataTypeable`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/deriving.html#deriving-typeable
[[Boilerplate1]]: http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/hmap.ps
[[Boilerplate2]]: http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/gmap2.ps
[`Typeable`]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/Data-Typeable.html#t:Typeable
[`ExistentialQuantification`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#existential-quantification
[`Rank2Types`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#universal-quantification
[[Marlow]]: http://community.haskell.org/~simonmar/papers/ext-exceptions.pdf
[`DefaultSignatures`]: http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#class-default-signatures
[`DeriveGenerics`]: http://www.haskell.org/haskellwiki/Generics
