using NamedTuples
using Base.Test

@test @NT( a => 1 ).a == 1
@test @NT( a => 1 )[1] == 1
@test @NT( a => 1 )[:a] == 1

@test length( @NT( a => 1)) == 1
@test length( @NT( a => 1, b => 2.0)) == 2

@test first( @NT( a => 1, b => 2.0 )) == ( :a, 1)
@test last( @NT( a => 1, b => "hello", c => 2.0 )) == ( :c, 2.0)
@test [ v for( (k,v) in @NT( a => 1.0, b => 2.0 ) ) ] == [ 1.0, 2.0 ]

@test ( x = @NT( a::Int64, b::Float64 )( 1, 2.0 ) ; fieldtype( x, :a ) == Int64 && fieldtype( x, :b) == Float64 )
@test @NT( a => 1, b => "hello")  ==  @NT( a, b )( 1, "hello")
@test @NT( a => 1) != @NT( b => 1 )

@test hash( @NT( a => 1, b => "hello"))  ==  hash( @NT( a, b )( 1, "hello") )
@test hash( @NT( a => 1, b => "hello")) != hash( @NT( a => 1, b => 2.0 ))

@test @NT( a ) ==  @NT( a )
@test @NT( a ) !=  @NT( b )

@test typeof( @NT( a::Int64, b::Float64 )(1, 3.0) ) == typeof( @NT( a => 1, b => 2.0 ))

