using NamedTuples
using Base.Test

@test @NT( a => 1 ).a == 1
@test @NT( a => 1 )[1] == 1
@test @NT( a => 1 )[:a] == 1

@test @NT( :a => 1 ).a == 1
@test @NT( :a => 1 )[1] == 1
@test @NT( :a => 1 )[:a] == 1

@test length( @NT( a => 1)) == 1
@test length( @NT( a => 1, b => 2.0)) == 2

@test first( @NT( a => 1, b => 2.0 )) == 1
@test last( @NT( a => 1, b => "hello", c => 2.0 )) == 2.0
@test [ v for v in @NT( a => 1.0, b => 2.0 ) ] == [ 1.0, 2.0 ]

@test ( x = @NT( a::Int64, b::Float64 )( 1, 2.0 ) ; typeof(x.a) == Int64 && typeof(x.b) == Float64 )
@test @NT( a => 1, b => "hello")  ==  @NT( a, b )( 1, "hello")
@test @NT( a => 1) != @NT( b => 1 )

@test hash( @NT( a => 1, b => "hello"))  ==  hash( @NT( a, b )( 1, "hello") )
@test hash( @NT( a => 1, b => "hello")) != hash( @NT( a => 1, b => 2.0 ))

@test @NT( a ) ==  @NT( a )
@test @NT( a ) !=  @NT( b )

@test typeof( @NT( a::Int64, b::Float64 )(1, 3.0) ) == typeof( @NT( a => 1, b => 2.0 ))

# Syntax tests, including anon named tuples
@test @NT( a, b ) <: NamedTuple
@test @NT( ::Int64, ::Float64 ) <: NamedTuple
@test typeof( @NT( 1, 2, "3" )) <: NamedTuple
@test typeof( @NT( 1 + 2, "hello")) <: NamedTuple
@test length( @NT( 1, 2, 3, "hello")[ 1:2 ]) == 2

@test isbits( @NT( ::Int64, ::Float64)) == true

nt = @NT( a=>1, b=>2, c=>3 )
@test nt.a == 1
@test nt.b == 2
@test nt.c == 3
@test haskey( nt, :x ) == false
x = setindex( nt, :x, 123 )
@test x.x == 123
@test x.a == 1
@test x.b == 2
@test x.c == 3
@test nt[[:c,:b]] == @NT( c=>3, b=>2 )

y = delete( x, :a)
@test x != y
@test haskey( nt, :a ) == true
@test haskey( y, :a ) == false
@test x.x == 123
@test x.a == 1
@test x.b == 2
@test x.c == 3

@test merge( nt, @NT( d => "hello", e => "world"))  == @NT( a=>1,b=>2,c=>3,d=>"hello",e=>"world")
