using NamedTuples
using Base.Test

@test @NT( a = 1 ).a == 1
@test @NT( a = 1 )[1] == 1
@test @NT( a = 1 )[:a] == 1

@test @NT( :a = 1 ).a == 1
@test @NT( :a = 1 )[1] == 1
@test @NT( :a = 1 )[:a] == 1

@test length( @NT( a = 1)) == 1
@test length( @NT( a = 1, b = 2.0)) == 2

@test first( @NT( a = 1, b = 2.0 )) == 1
@test last( @NT( a = 1, b = "hello", c = 2.0 )) == 2.0
@test [ v for v in @NT( a = 1.0, b = 2.0 ) ] == [ 1.0, 2.0 ]

@test ( x = @NT( a::Int64, b::Float64 )( 1, 2.0 ) ; typeof(x.a) == Int64 && typeof(x.b) == Float64 )
@test @NT( a = 1, b = "hello")  ==  @NT( a, b )( 1, "hello")
@test @NT( a = 1) != @NT( b = 1 )
@test @NT(a=1) == @NT(a=1.)

@test hash( @NT( a = 1, b = "hello"))  ==  hash( @NT( a, b )( 1, "hello") )
@test hash( @NT( a = 1, b = "hello")) != hash( @NT( a = 1, b = 2.0 ))

@test @NT( a ) ==  @NT( a )
@test @NT( a ) !=  @NT( b )

@test @NT(a::Int, b) == @NT(a::Int, b::Any)

@test typeof( @NT( a::Int, b::Float64 )(1, 3.0) ) == typeof( @NT( a = 1, b = 2.0 ))

# Syntax tests, including anon named tuples
@test @NT( a, b ) <: NamedTuple
@test @NT( ::Int64, ::Float64 ) <: NamedTuple
@test typeof( @NT( 1, 2, "3" )) <: NamedTuple
@test typeof( @NT( 1 + 2, "hello")) <: NamedTuple
@test length( @NT( 1, 2, 3, "hello")[ 1:2 ]) == 2

@test isbits( @NT( ::Int64, ::Float64)) == true

nt = @NT( a=1, b=2, c=3 )
@test nt.a == 1
@test nt.b == 2
@test nt.c == 3
@test haskey( nt, :x ) == false
x = setindex( nt, :x, 123 )
@test x.x == 123
@test x.a == 1
@test x.b == 2
@test x.c == 3
@test nt[[:c,:b]] == @NT( c=3, b=2 )

y = delete( x, :a)
@test x != y
@test haskey( nt, :a ) == true
@test haskey( y, :a ) == false
@test x.x == 123
@test x.a == 1
@test x.b == 2
@test x.c == 3

@test map(-, @NT(x=1, y=2)) == @NT(x=-1, y=-2)
@test map(+, @NT(x=1, y=2), @NT(x=1, y=2)) == @NT(x=2, y=4)
@test_throws ArgumentError map(+, @NT(x=1, y=2), @NT(y=1, x=2))
@test map(string, @NT(x=1, y=2)) == @NT(x="1", y="2")
@test map(round, @NT(x=1//3, y=Int), @NT(x=3, y=2//3)) == @NT(x=0.333, y=1)

@test @NT(x=1, y=2) <  @NT(x=1, y=2.5)
@test @NT(x=1, y=2) >= @NT(x=1, y=2.0)

@test merge( nt, @NT( d = "hello", e = "world"))  == @NT( a=1,b=2,c=3,d="hello",e="world")

@test get.(@NT( a = Nullable(3), b = Nullable("world") )) == @NT( a = 3, b = "world")
@test_throws MethodError @NT( a = 3) .+ [4]
@test_throws MethodError [4] .+ @NT( a = 3)

# serialize and deserialize
addprocs(1)
@everywhere using NamedTuples
x = @NT(a=1, b=2)
y = @fetchfrom 2 identity(x)
@test isa(y,NamedTuple)
@test y.a == 1
@test y.b == 2

io = IOBuffer()
serialize(io, Union{})
@test deserialize(seekstart(io)) === Union{}

# allow custom types
struct Empty end
nt = @NT(a::Empty, b::Int)
@test nt.parameters[1] == Empty

# type reuse
module A
    using NamedTuples
    const NT = @NT(x)
end

module B
    using NamedTuples
    const NT = @NT(x)
end

@test A.NT === B.NT
