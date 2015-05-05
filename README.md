<<<<<<< HEAD
# NamedTuples

Syntax

    @NT( a, b )                 -> Defines a tuple with a and b as members
    @NT( a::Int64, b::Float64 ) -> Defines a tuple with the specific arg types as members
    @NT( a => 1, b => "hello")  -> Defines and constructs a tuple with the specifed members and values
    @NT( a, b )( 1, "hello")    -> Is equivalent to the above definition
    @NT( a::Int64 )( 2.0 )      -> Calls `convert( Int64, 2.0 )` on construction and sets `a`

NamedTuples may be used anywhere you would use a regular Tuple, this includes method definition and return values.

    module Test
    using NamedTuples

    function foo( y )
        a = 1
        x = 3
        return  @NT( a => 1, b => "world", c => "hello", d=>a/x, y => a/y  )
    end
    function bar( nt::@NT( a::Int64, c::ASCIIString ))
        return repeat( nt.c, nt.a )
    end

    end

    Test.foo( 1 ) # Returns a NamedTuple of 5 elements
    Test.bar( @NT( a=> 2, c=>"hello")) # Returns `hellohello`

There is at most one instance of a NamedTuple type with a given set of Members and Types, hence

    typeof( @NT( a::Int64, b::Float64 )(1, 3.0) ) == typeof( @NT( a => 1, b => 2.0 ))

NamedTuple definitions are shared across all modules. The underlying imutable types are constructed at first use.

NamedTuples support iteration, indexing and behave as immutable associative containers.

    @NT( a => 1 ).a == 1
    @NT( a => 1 )[1] == 1
    @NT( a => 1 )[:a] == 1
    length( @NT( a => 1)) == 1
    length( @NT( a => 1, b => 2.0)) == 2
    first( @NT( a => 1, b => 2.0 )) == ( :a, 1)
    last( @NT( a => 1, b => 2.0 )) == ( :b, 2.0)
    for( (k,v) in @NT( a => 1, b => 1 ))
        prinln( "$k => $v")
    end

[![Build Status](https://travis-ci.org/mdcfrancis/NamedTuples.jl.svg?branch=master)](https://travis-ci.org/mdcfrancis/NamedTuples.jl)
=======
# NamedTuples.jl
NamedTuples.jl
>>>>>>> 56566d368c8b4ad7d02605b71b7d8438250c84cd
