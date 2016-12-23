% Parsing and continuations

# Let's talk about parsing

A whole lot of programming involves interacting with external sources
of data:

* Files

* Network peers

Sometimes the data from these sources is wrapped up behind a nice API,
but often we need to interpret it ourselves.


# Example: HTTP 1.1

Here's the beginning of one of the most famous network protocols
around, the BNF definition of a HTTP 1.1 request:

~~~~
Request = Request-Line              ; Section 5.1
          *(( general-header        ; Section 4.5
           | request-header         ; Section 5.3
           | entity-header ) CRLF)  ; Section 7.1
          CRLF
          [ message-body ]          ; Section 4.3
~~~~

(The BNF grammar used above is typical of the IETF, and is defined in
[RFC 2616 section 2](http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2)
if you're interested.)


# The two approaches to parsing

Long ago, the ancients wrote all of their parsers by hand, because
that was all they could do.

After a long while, techniques arrived to automatically generate
parsers from machine-readable language descriptions.

Each approach has its advantages.  Parser generators remain popular
because they are expressive and can catch certain classes of error.

Why does expressiveness matter? Let's run a quick survey.


# Parsing by machine: Ragel fragment

~~~~ {.rl}
%%{
  machine http_parser_common;

  scheme = ( alpha | digit | "+" | "-" | "." )* ;
  absolute_uri = (scheme ":" (uchar | reserved )*);

  path = ( pchar+ ( "/" pchar* )* ) ;
  query = ( uchar | reserved )* %query_string ;
  param = ( pchar | "/" )* ;
  params = ( param ( ";" param )* ) ;
  rel_path = ( path? %request_path (";" params)? ) ("?" %start_query query)?;
  absolute_path = ( "/"+ rel_path );

  Request_URI = ( "*" | absolute_uri | absolute_path ) >mark %request_uri;
  Fragment = ( uchar | reserved )* >mark %fragment;
  Method = ( upper | digit | safe ){1,20} >mark %request_method;

  http_number = ( digit+ "." digit+ ) ;
  HTTP_Version = ( "HTTP/" http_number ) >mark %http_version ;
  Request_Line = ( Method " " Request_URI ("#" Fragment){0,1} " " HTTP_Version CRLF ) ;

  field_name = ( token -- ":" )+ >start_field %write_field;

  field_value = any* >start_value %write_value;

  message_header = field_name ":" " "* field_value :> CRLF;

  Request = Request_Line ( message_header )* ( CRLF @done );

  main := Request;
}%%
~~~~


# Parsing by hand: C++ parser fragment

~~~~ {.cpp}
void Response::ProcessStatusLine( std::string const& line )
{
    const char* p = line.c_str();

    while( *p && *p == ' ' )
        ++p;

    while( *p && *p != ' ' )
        m_VersionString += *p++;
    while( *p && *p == ' ' )
        ++p;

    std::string status;
    while( *p && *p != ' ' )
        status += *p++;
    while( *p && *p == ' ' )
        ++p;

    while( *p )
        m_Reason += *p++;

    m_Status = atoi( status.c_str() );
    if( m_Status < 100 || m_Status > 999 )
        throw Wobbly( "BadStatusLine (%s)", line.c_str() );

    if( m_VersionString == "HTTP:/1.0" )
        m_Version = 10;
    else if( 0==m_VersionString.compare( 0,7,"HTTP/1." ) )
        m_Version = 11;
    else
        throw Wobbly( "UnknownProtocol (%s)", m_VersionString.c_str() );

    m_State = HEADERS;
    m_HeaderAccum.clear();
}
~~~~

# Parsing by hand: Java parser fragment, page 1

~~~~ {.java}
public class HttpResponseParser extends AbstractMessageParser {

    private final HttpResponseFactory responseFactory;
    private final CharArrayBuffer lineBuf;

    public HttpResponseParser(
            final SessionInputBuffer buffer,
            final LineParser parser,
            final HttpResponseFactory responseFactory,
            final HttpParams params) {
        super(buffer, parser, params);
        if (responseFactory == null) {
            throw new IllegalArgumentException("Response factory may not be null");
        }
        this.responseFactory = responseFactory;
        this.lineBuf = new CharArrayBuffer(128);
    }
~~~~


# Parsing by hand: Java parser fragment, page 2

~~~~ {.java}
    protected HttpMessage parseHead(
            final SessionInputBuffer sessionBuffer)
        throws IOException, HttpException, ParseException {

        this.lineBuf.clear();
        int i = sessionBuffer.readLine(this.lineBuf);
        if (i == -1) {
            throw new NoHttpResponseException("The target server failed to respond");
        }
        //create the status line from the status string
        ParserCursor cursor = new ParserCursor(0, this.lineBuf.length());
        StatusLine statusline = lineParser.parseStatusLine(this.lineBuf, cursor);
        return this.responseFactory.newHttpResponse(statusline, null);
    }
}
~~~~


# A retrospective

With the Ragel fragment, we had most of a parser squeezed into a
single page.  That's impressive!

The obvious downsides? 

* It's a completely unfamiliar language that we'd have to learn.

* The boundary between Ragel and "real" programming language is not
  pretty.

The C++ code is very low-level.  Pointer arithmetic?  It's quite hard
to tell what is really going on.

And the Java?  Alas, I cannot find anything very polite to say.


# Can manual parsing be liberated from detail?

Surely we can achieve a way of parsing that's almost as concise and
close to the original BNF as Ragel, but without escaping into a
different and unfamiliar language.

Ragel is a domain-specific language, but we want one that's *embedded*
in Haskell.


# What's in a grammar?

A modren BNF grammar bears many features that are familiar to
functional programmers.

* A grammar is built from a combination of smaller rules, just as a
  function is constructed from smaller operations.

* Abstract operators such as repetition and choice *combine* existing
  rules to create new ones, in the same way that we have combinators
  to operate on functions.

The resemblance extends to data:

* A sequence is similar to a recursive type.

* An alternative resembles a sum type.

* A fixed series of tokens looks like a product type.


# Enter Parsec

There's a long history of functional programmers writing
parsing libraries.

Parsec
([Leijen 2001](http://legacy.cs.uu.nl/daan/download/papers/parsec-paper.pdf))
was the first to be practical for real world use, and is now the most
famous.


# Parsec and attoparsec

Inspired by Parsec, I wrote a specialized derivative named
[attoparsec](http://hackage.haskell.org/package/attoparsec).

How do the two differ?

* It's optimized for fast stream and file parsing, so it works with
  the `ByteString` type.  Parsec is more general: it can parse
  `String` or `Text`, or other more exotic token types.

* attoparsec does not attempt to give friendly error messages (no file
  names or line numbers).  Parsec might be a better choice for
  e.g. parsing source files, where the friendliness/performance
  tradeoff is a little different.

In this class, we'll talk about attoparsec, because (a) it's my baby
(let's be honest!)  and (b) we're interested in systems-style
applications.

# Getting started with attoparsec

If you don't have it installed on your system, fetch it:

~~~~
cabal install attoparsec
~~~~

All of our examples will assume this language extension:

~~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}
~~~~

This extension generalizes the type of quoted string constants:

~~~~ {.haskell}
>> :set -XOverloadedStrings
>> import Data.String
>> :type "foo"
"foo" :: IsString a => a
~~~~

With `OverloadedStrings` enabled, we can write a string literal of
any of the common string-like types:

~~~~ {.haskell}
>> import Data.ByteString.Char8
>> :info IsString
class IsString a where
    fromString :: String -> a
instance IsString [Char]
instance IsString ByteString
~~~~


# Parsing a request line

What does an HTTP request line consist of?

~~~~
GET /foo HTTP/1.1
~~~~

As a data type:

~~~~ {.haskell}
import Data.ByteString (ByteString)

data Request = Request {
      requestMethod  :: ByteString
    , requestUri     :: ByteString
    , requestVersion :: ByteString
    } deriving (Eq, Ord, Show)
~~~~


# A word about imports

With functions that deal in `ByteString` values, you'll often see
imports like these:

~~~~ {.haskell}
import qualified Data.Attoparsec as P
import qualified Data.Attoparsec.Char8 as P8

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
~~~~

The reason for this is that we'll often have two versions of a
function, one specialized for `Word8` (a byte) and another for "let's
cheat, and pretend this `Word8` is really a `Char`":

~~~~ {.haskell}
B.count  :: Word8 -> ByteString -> Int
B8.count :: Char  -> ByteString -> Int
~~~~

Of course cheating is unsafe.  Let's see when we can get away with it.


# Bytes, characters, and cheating

If you're using the `ByteString` type, you *must* know when it's
reasonably tolerable to cheat and pretend that `Word8` is `Char`.

* For a protocol such as HTTP 1.1 where the headers are pretty much
  always 7-bit ASCII text, it's generally okay.
  
* Even so, many of these protocols (including HTTP 1.1) have
  exceptions, so you *still* have to be careful.

* Otherwise, you should either always work with `Word8`, or use a more
  appropriate type such as
  [`Text`](http://hackage.haskell.org/package/text) or `String`.


# Land of many imports

Here are the modules we'll want in order to write our HTTP parser:

~~~~ {.haskell}
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8

import Data.Attoparsec as P
import Data.Attoparsec.Char8 (char8, endOfLine)
import qualified Data.Attoparsec.Char8 as P8

import Control.Applicative hiding (many)
import Data.Word (Word8)
import Data.Char
~~~~


# Actually parsing a request line

And here's a complete parser.

~~~~ {.haskell}
requestLine :: Parser Request
requestLine = do
  method <- P.takeWhile1 isToken <* char8 ' '
  cheesyUri <- P8.takeWhile1 (/=' ') <* char8 ' '
  ver <- version <* endOfLine
  return (Request method cheesyUri ver)

version :: Parser ByteString
version = string "HTTP/" >>
          P8.takeWhile (\c -> isDigit c || c == '.')
~~~~


# Starting small

Our first observation is that attoparsec's `Parser` type is monadic,
as implied by our use of the `>>` operator here.

~~~~ {.haskell}
version :: Parser ByteString
version = string "HTTP/" >>
          P8.takeWhile (\c -> isDigit c || c == '.')
~~~~

The `string` combinator succeeds only if the beginning of the current
input matches the given string.

~~~~ {.haskell}
string :: ByteString -> Parser ByteString
~~~~

The `P8.takeWhile` combinator always succeeds, and consumes input as
long as the given predicate returns `True` on each successive byte
(treated as a `Char`).

~~~~ {.haskell}
P8.takeWhile :: (Char -> Bool) -> Parser ByteString
~~~~


# Success and consumption

An attoparsec parser can do two things.

* It may succeed or fail.

* It may consume input, or leave the input unchanged.

What does it mean to consume input?

* If the current input is `"foo"`, and we consume one byte, the input
  will then become `"oo"`.

As the previous slide implies, it's possible for a parser to succeed
without consuming input.

A parser may also fail, but consume input while doing so.


# What's wrong with our version parser?

Let's identify, and fix, the problems with that version parsing code.

* This is a mini-lab. It's your job to make it work better, right here
  in class!  You have 5 minutes.

Along the way, you can take advantage of the fact that `Parser` is an
applicative functor.

You can also use the very handy "pointy sequencing" combinators:

~~~~ {.haskell}
(*>) :: Applicative f => f a -> f b -> f b
(<*) :: Applicative f => f a -> f b -> f a
~~~~

Please explore what they actually do in `ghci`, and let us know what
you figure out!


# While you work

When you want to test your parser, use `parseOnly` or `parseTest`:

~~~~ {.haskell}
import qualified Data.Attoparsec as P

P.parseOnly :: Parser a -> ByteString -> Either String a
P.parseTest :: Show a => Parser a -> ByteString -> IO ()
~~~~

Feel free to ask us questions, and for actual help if you're stuck.

It's totally reasonable to ask "isn't there surely some pre-existing
function that will solve problem *x* for me already?"

* Remember: if you are stuck or have a question, someone else is
  probably in the same situation.

We're not grading your effort here!  So don't stress out; we just want
to make sure you're understanding how the pieces fit together.


# My solution

This code is completely applicative: no monadic operations in sight.
Hooray!

~~~~ {.haskell}
betterVersion = string "HTTP/" *>
                ((,) <$> (P8.decimal <* char8 '.') <*> P8.decimal)
~~~~


# Next up

You have 5 minutes to write a parser for the first line of an HTTP
response:

~~~~
Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
~~~~

Where:

* `SP` is a space character

* `CRLF` is a carriage return followed by a line feed

* `Status-Code` is a 3-digit number

* `Reason-Phrase` is any text *excluding* `CR` and `LF`


# My solution

~~~~ {.haskell}
responseLine :: Parser ((Int,Int,Int), ByteString)
responseLine =
    (,) <$> (betterVersion *> sp *> xxx <* sp) 
        <*> P.takeTill P8.isEndOfLine <* P8.endOfLine
  where xxx = (,,) <$> x <*> x <*> x
        x = digitToInt <$> P8.digit
        sp = char8 ' '
~~~~

Let's try it in `ghci`:

~~~~ {.haskell}
>> :set -XOverloadedStrings
>> P.parseOnly responseLine "HTTP/1.1 200 OK\r\n"
Right ((2,0,0),"OK")
~~~~


# Alternatives

It often happens that there are several possible valid parses in one
spot.

The `Alternative` class lets us express this idea: it's like a
`Monoid`, only expressed via applicative functors.  (The related class
for monads is named `MonadPlus`.)

~~~~ {.haskell}
class Applicative f => Alternative f where
    -- the identity
    -- usually means "failure"
    empty :: f a

    -- an associative binary operation
    -- usually means "if a fails, then b"
    (<|>) :: f a -> f a -> f a
~~~~


# Alternatives in action

For example:

~~~~ {.haskell}
data Foo = Bar
         | Quux
           deriving (Show)

foo   =   (Bar <$ string "bar")
     <|>  (Quux <$ string "quux")
~~~~

(I bet you wonder what that `<$` operator is.  Figure it out!)

If you have many alternatives to string together, there are some
useful combinators:

~~~~ {.haskell}
-- one or more
some :: Alternative f => f a -> f [a]

-- zero or more
many :: Alternative f => f a -> f [a]
~~~~

Other types for which their `Alternative` instances are very handy
include `STM`, `[]`, and `Maybe`.



# Last exercise - whew!

Here's the BNF for an HTTP `Accept` header.

~~~~
Accept         = "Accept" ":"
		 #( media-range [ accept-params ] )
media-range    = ( "*/*"
		 | ( type "/" "*" )
		 | ( type "/" subtype )
		 ) *( ";" parameter )
type           = token
subtype        = token
accept-params  = ";" "q" "=" qvalue *( accept-extension )
accept-extension = ";" token [ "=" ( token | quoted-string ) ]
~~~~

And here are a couple of simple and more complex examples:

~~~~
Accept: audio/*; q=0.2, audio/basic

Accept: text/plain; q=0.5, text/html,
	text/x-dvi; q=0.8, text/x-c
~~~~

Go forth and write as much of an `Accept` parser as you can in 10
minutes.


# Yo dawg

I heard you like continuations.

So I put a continuation in your continuation.

So you can continue while you continue.


# What is this I don't even

Let's play around in `ghci` again.

~~~~ {.haskell}
>> :type P.parse
P.parse :: Parser a -> ByteString -> Result a
>> P.parse responseLine "HTTP/1.1"
Partial _
~~~~

What's that `Result` type?


# It's all about results

There are many applications where we are fed partial pieces of input,
and can produce a final parse only when we've been given enough input.

* Common case: a TCP connection where we're receiving small segments
  of input at a time, fragmented on unknown boundaries.

~~~~ {.haskell}
data Result r = Fail ByteString [String] String
              | Partial (ByteString -> Result r)
              | Done ByteString r

instance Functor Result
instance Show r => Show (Result r)
~~~~

The `Partial` constructor captures that behaviour.

* It contains a *function* that indicates that we cannot give an
  answer until the function is fed more input.
  
* We can start feeding the parser input as soon as we have some, and
  if it doesn't return `Fail` or `Done`, we can "refill" it once more
  input arrives.

* Completely separates the concern of parsing from those of connection
  management, buffering (not needed at all), timeouts, and the like.


# Behind the scenes

attoparsec achieves this magical-seeming behaviour by being built
entirely (and invisibly!) using *continuations*.

At any time, there are two continuations in play:

~~~~ {.haskell}
-- What to do if the current parse fails.
type Failure   r = Input -> Added -> More -> [String] -> String 
                -> Result r
-- The Strings above are for reporting an error message.

-- What to do if the current parse succeeds.
type Success a r = Input -> Added -> More -> a 
                -> Result r
~~~~

What are those other types that they refer to?

~~~~ {.haskell}
-- The current input.
newtype Input = I {unI :: B.ByteString}

-- Input that was fed to us when we returned a Partial result.
newtype Added = A {unA :: B.ByteString}

-- Have we reached the end of all input?
data More = Complete | Incomplete
            deriving (Eq, Show)
~~~~


# What's in a parser

The scoped type parameter `r` represents "the rest of the parse".

Remember that a scoped type variable effectively lets the callee of a
function decide what the concrete type to be used is.

Here, we've scoped `r` because we want attoparsec (and not its
callers) to decide what the concrete type of the continuation is.

~~~~ {.haskell}
{-# LANGUAGE Rank2Types #-}

newtype Parser a = Parser {
      runParser :: forall r. Input -> Added -> More
                -> Failure   r
                -> Success a r
                -> Result r
    }
~~~~


# Reducing the headache factor

That is undeniably a beefy definition.  

A parser is a function of five arguments, of which two are
continuations. And we're using rank-2 types, too.  Yow!

Here's an easier way to think about it.

The first three arguments are really just the state of the input.
Here is a more traditional way to package up that information:

~~~~ {.haskell}
data InputState = {
      stInput :: Input
    , stAdded :: Added
    , stMore  :: More
    }
~~~~

So really, all we have is input state, what to do on failure, and what
to do on success.  Looked at this way, it doesn't seem so
scary... right?


# Games coders play

The implementation of `fmap` is simple, and representative of the
low-level internals of attoparsec.

~~~~ {.haskell}
fmap :: (a -> b) -> Parser a -> Parser b
fmap f m = Parser $ \i0 a0 m0 kf ks ->
           let ks' i1 a1 m1 a = ks i1 a1 m1 (f a)
           in  runParser m i0 a0 m0 kf ks'
~~~~

Throughout the library, much of the code simply replaces either the
failure or the success continuation with a different one.

The case above looks a little daunting, but all we've done is replace
the success continuation.  The rest is just plumbing.


# Asking for more input

Supposing the internal plumbing needs to ask for more input.  This is
pretty easy to do.

~~~~ {.haskell}
prompt i0 a0 _m0 kf ks = Partial $ \s ->
    if B.null s
    then kf i0 a0 Complete
    else ks (I (unI i0 +++ s))
            (A (unA a0 +++ s)) 
	    Incomplete
~~~~


# Running and ending a parse

As is usually the case with monads, the user-visible "run this monad"
function is quite simple.

~~~~ {.haskell}
parse :: Parser a -> B.ByteString -> Result a
parse m s = runParser m (I s) (A B.empty) Incomplete failK successK
~~~~

The only slight complication is that we need to create "terminal
continuations", i.e. continuations that do not chain up yet another
continuation, but instead return us to our usual mode of computation.

~~~~ {.haskell}
failK :: Failure a
failK i0 _a0 _m0 stack msg = Fail (unI i0) stack msg

successK :: Success a a
successK i0 _a0 _m0 a = Done (unI i0) a
~~~~
