Streaming Paradigms
-------------------

There are two dual paradigms for stream processing in Haskell. In the first
paradigm we represent a stream as a data type and use functions to work on it.
In the second paradigm we represent *stream processors* as data types and
provide them individual data elements to process, there is no explicit
representation of the stream as a data type. In the first paradigm we work with
data representation and in the second paradigm we work with function
representations. Both of these paradigms have equal expressive power. The
latter uses the monadic composition for data flow whereas the former does not
need monadic composition for straight line stream processing and therefore can
use it for higher level composition e.g.  to compose streams in a product
style.

To see an example of the first paradigm, let us use the ``vector`` package to
represent a monadic stream of integers as ``Stream IO Int``. This data
representation of stream is passed explicitly to the stream processing
functions like ``filter`` and ``drop`` to manipulate it::

  import qualified Data.Vector.Fusion.Stream.Monadic as S

  stream :: S.Stream IO Int
  stream = S.fromList [1..100]

  main =  do
    let str = (S.filter even . S.drop 10) stream
    toList str >>= putStrLn . show

Pure lists and vectors are the most basic examples of streams in this paradigm.
The streaming IO libraries just extend the same paradigm to monadic streaming.
The API of these libraries is very much similar to lists with a monad parameter
added.

The second paradigm is direct opposite of the first one, there is no stream
representation in this paradigm, instead we represent *stream processors* as
data types. A stream processor represents a particular process rather than
data, and we compose them together to create composite processors. We can call
them stream transducers or simply pipes. Using the ``machines`` package::

  import qualified Data.Machine as S

  producer :: S.SourceT IO Int
  producer = S.enumerateFromTo 1 100

  main =  do
    let processor = producer S.~> S.dropping 10 S.~> S.filtered even
    S.runT processor >>= putStrLn . show

Both of these paradigms look almost the same, right? To see the difference
let's take a look at some types. In the first paradigm we have an explicit
stream type and the processing functions take the stream as input and produce
the transformed stream::

  stream :: S.Stream IO Int
  filter :: Monad m => (a -> Bool) -> Stream m a -> Stream m a

In the second paradigm, there is no stream data type, there are stream
processors, let's call them boxes that represent a process.  We have a
*SourceT* box that represents a singled ended producer and a *Process* box or a
pipe that has two ends, an input end and an output end, a ``MachineT``
represents any kind of box. We put these boxes together using the ``~>``
operator and then run the resulting machine using ``runT``::

  producer :: S.SourceT IO Int
  filtered :: (a -> Bool) -> Process a a
  dropping :: Int -> Process a a
  (~>) :: Monad m => MachineT m k b -> ProcessT m b c -> MachineT m k c

Custom pipes can be created using a Monadic composition and primitives to
receive and send data usually called ``await`` and ``yield``.

.. |str| replace:: `streamly <https://github.com/composewell/streamly>`__

+-----------------------------------------------------------------------------+
| Streaming libraries using the direct paradigm.                              |
+------------------------+----------------------------------------------------+
| Library                | Remarks                                            |
+========================+====================================================+
| vector                 | The simplest in this category, provides            |
|                        | transformation and combining of monadic            |
|                        | streams but no monadic composition of streams.     |
|                        | Provides a very simple list like API.              |
+------------------------+----------------------------------------------------+
| streaming              | * Encodes a return value to be supplied when the   |
|                        |   stream ends. The monad instance passes on the    |
|                        |   streams and combines the return values.          |
|                        | * Functor general                                  |
|                        | * The API is more complicated than vector because  |
|                        |   of the return value and the functor layer.       |
+------------------------+----------------------------------------------------+
| list-t                 | Provides straight line composition of streams      |
|                        | as well as a list like monadic composition.        |
|                        | The API is simple, just like ``vector``.           |
+------------------------+----------------------------------------------------+
|                        | Like list-t, in addition to straight line          |
|                        | composition it provides a list like monadic        |
|                        | composition of streams, supports combining streams |
|                        | concurrently supports concurrent applicative and   |
|                        | monadic composition.                               |
| |str|                  | The basic API is very much like lists and          |
|                        | almost identical to ``vector`` streams.            |
+------------------------+----------------------------------------------------+

+-----------------------------------------------------------------------------+
| Streaming libraries using the pipes paradigm.                               |
+------------------------+----------------------------------------------------+
| Library                | Remarks                                            |
+========================+====================================================+
| conduit                | ``await`` and ``yield`` data to upstream or        |
|                        | downstream pipes; supports pushing leftovers back. |
+------------------------+----------------------------------------------------+
| pipes                  | ``await`` and ``yield`` data to upstream or        |
|                        | downstream pipes                                   |
+------------------------+----------------------------------------------------+
| machines               | Can await from two sources, left and right.        |
+------------------------+----------------------------------------------------+
