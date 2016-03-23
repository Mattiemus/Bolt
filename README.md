# The Bolt Programming Language

Bolt is a object oriented and functional programming language - built as a hobby project
to learn the inner workings of more complex compilers.

# Basic Syntax

Bolt has a simple syntax based upon Haskell, Rust, and Pony.

Functions are written as follows

    fun fibs(n: Int32): Int32 {
        if (n == 0 || n == 1) {
            return 1;
        } else {
            return fibs(n - 1) + fibs(n - 2);
        }
    }

Type annotations are applied using the colon ':' symbol. Bolt includes several built-in types
including: Boolean, ISize, ILong, Int8, Int16, Int32, Int64, UISize, UILong, UInt8, UInt16, UInt32,
UInt64, Float32,  and Float64. New types can be defined like so:

    type SignedInteger = ISize | ILong | Int8 | Int16 | Int32 | Int64

This also demonstrates the use of union types. Union types are much like unions in C, where the
value contained can be any of the specified types, however only one of them at a given time.
Intersection types are simmilar, however mean the value is all of the specified types.

    type Person = HasName & HasDOB & HasFavColor

Bolt allows custom primitive types which interestingly also define their own type, so can be combined
with type unions to create enumerations, like so.

    primitive Red
    primitive Blue
    primitive Green
    type Color = Red | Blue | Green

# Installation and Usage

Bolt includes a cabal build script, to build the interpreter run the following from a terminal:

    $ cabal configure
    $ cabal build
