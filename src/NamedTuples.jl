__precompile__()
module NamedTuples

abstract NamedTuple <: Associative

Base.keys( t::NamedTuple ) = fieldnames( t )
Base.values( t::NamedTuple ) = [ getfield( t, i ) for i in fieldnames( t )]
Base.length( t::NamedTuple ) = length( fieldnames( t ))
# Iteration
Base.start( t::NamedTuple ) = 1
Base.done( t::NamedTuple, iter ) = iter<length( fieldnames( NamedTuple ))
Base.next( t::NamedTuple, iter ) = ( ( fieldnames(t)[iter], getfield( t, iter )), iter += 1)
Base.endof( t::NamedTuple ) = length( t )
Base.last( t::NamedTuple ) = (fieldnames(t)[end], t[end] )
Base.writemime(io::IO, ::MIME"text/plain", t::NamedTuple) = show( io, t )
Base.show( io::IO,t::NamedTuple) = print( io, "(", join([ "$k => $v" for (k,v) in t ], ", ") ,")")
# Make this indexable so that it works like a Tuple
Base.getindex( t::NamedTuple, i::Int ) = getfield( t, i )
Base.getindex( t::NamedTuple, i::UnitRange{Int64}) = slice( t, i )
# We also support indexing by symbol
Base.getindex( t::NamedTuple, i::Symbol ) = getfield( t, i )
Base.getindex( t::NamedTuple, i::Symbol, default ) = get( t, i, default)
# This is a linear lookup...
Base.get( t::NamedTuple, i::Symbol, default ) = i in keys(t) ? t[i] : default
# Deep compare

function Base.(:(==))( lhs::NamedTuple, rhs::NamedTuple)
    ( lhs === rhs ) && return true
    ( typeof( lhs ) != typeof( rhs )) && return false
    for( i in 1:length( lhs ))
        if( lhs[i] != rhs[i])
            return false
        end
    end
    return true
end
# Deep hash

function Base.hash( nt::NamedTuple, hs::UInt64)
    h = 17
    for( v in values(nt) )
        h = h * 23 + hash( v, hs )
    end
    return h
end


# Helper type, for transforming parse tree to NameTuple definition
immutable ParseNode{T} end

function trans( ::Type{ParseNode{:Symbol}}, expr::Expr)
    (expr.args[1],nothing,nothing)
end

# Only escape items that need to be escaped.
escape( e::Expr ) = esc( e )
escape( e::Symbol ) = esc( e )
escape( e ) = e

function trans( ::Type{ParseNode{:(=>)}}, expr::Expr)
    (sym, typ ) = trans( expr.args[1])
    return (sym, typ, escape( expr.args[2] ))
end

# Allow unary
function trans( ::Type{ParseNode{:(::)}}, expr::Expr)
    if( length( expr.args ) > 1 )
        return ( expr.args[1], expr.args[2], nothing)
    else
        return ( nothing, expr.args[1], nothing)
    end
end

function trans( expr::Expr )
    trans( ParseNode{expr.head}, expr)
end

function trans( sym::Symbol )
    return (sym, nothing, nothing)
end

function trans( ::Type{ParseNode{:quote}}, expr::Expr )
    return trans( expr.args[1] )
end

# Literal nodes
function trans{T}( lit::T )
    return (nothing, nothing, escape(lit) )
end

function trans{T}( ::Type{ParseNode{T}}, expr::Expr)
    return (nothing, nothing, escape(expr) )
end

function runme( mod, builder )
  eval( mod, builder )
end

#
# Create a NameTuple in the context of this module
# this is only done if the tuple has not already been
# constructed.
function create_tuple( fields::Vector{Symbol})
    len = length( fields )
    name = symbol( string( "_NT_", join( fields)) )
    types = [symbol("T$n") for n in 1:len]
    tfields = [ Expr(:(::), symbol( fields[n] ), symbol( "T$n") ) for n in 1:len ]
    def = Expr(:type, false, Expr( :(<:), Expr( :curly, name, types... ), :NamedTuple ), Expr(:block, tfields...) )
    ifdef = Expr(:call, :isdefined, QuoteNode(name))
    builder = ( :(!( $ifdef ) && eval( $def ) ) )
    eval( current_module(), builder )
    return name
end

#
# Given a symbol list create the NamedTuple
#
@doc doc"Given a symbol vector create the `NamedTuple`" ->
function make_tuple( syms::Vector{Symbol} )
    name  = create_tuple( syms )
    return esc( name )
end

#
# Given an expression vector create the NamedTuple
#
@doc doc"Given an expression vector create the `NamedTuple`" ->
function make_tuple( exprs::Vector)
    len    = length( exprs )
    fields = Array(Symbol, len)
    values = Array(Any, len)
    typs   = Array(Any, len)

    # Are we directly constructing the type, if so all values must be
    # supplied by the caller, we use this state to ensure this
    construct = false
    # handle the case where this is defining a datatype
    for( i in 1:len )
        expr = exprs[i]
        ( sym, typ, val ) = trans( expr )
        if( construct == true && val == nothing || ( i > 1 && construct == false && val != nothing ))
            error( "Invalid tuple, all values must be specified during construction @ ($expr)")
        end
        construct  = val != nothing
        fields[i]  = sym != nothing?sym:symbol( "_$(i)_")
        typs[i] = typ
        # On construction ensure that the types are consitent with the declared types, if applicable
        values[i]  = ( typ != nothing && construct)? Expr( :call, :convert, typ, val ) : val
    end

    name = create_tuple( fields )

    # Either call the constructor with the supplied values or return the type
    if( !construct )
        return Expr( :curly, esc( name ), typs... )
    else
        return Expr( :call, esc( name ), values ... )
    end
end

function nt_eval( expr::Vector )
  return eval( make_tuple( expr ) )
end

@doc doc"""
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
""" ->
macro NT( expr::Expr... )
    return make_tuple( collect( expr ))
end

@doc doc"""
Create a slice of an existing NamedTuple using a UnitRange. Construct a new NamedTuple with
the result.
This copies the underlying data.
""" ->
function Base.slice( t::NamedTuple, rng::UnitRange{Int64})
    name = create_tuple( fieldnames(t)[rng] )
    # FIXME - shoudl handle the type only case
    return eval( current_module(), Expr( :call, name, [ getfield( t, i ) for i in rng ] ... ) )
end

@doc doc"""
Merge two NamedTuples favoring the lhs
Order is preserved lhs names come first.
This copies the underlying data.
""" ->
function Base.merge( lhs::NamedTuple, rhs::NamedTuple )
    nms = unique( vcat( fieldnames( lhs ), fieldnames( rhs )) )
    name = create_tuple( nms )
    # FIXME should handle the type only case
    vals = [ haskey( lhs, nm ) ? lhs[nm] : rhs[nm] for nm in nms ]
    return eval( current_module(), Expr( :call, name, vals... ) )
end

@doc doc"""
Create a new NamedTuple with the new value set on it, either overwriting
the old value or appending a new value.
This copies the underlying data.
""" ->
function setindex{V}( t::NamedTuple, key::Symbol, val::V)
    nt = eval( current_module(),create_tuple( [key] ))( val )
    return merge( t, nt )
end

@doc doc"""
Create a new NamedTuple with the secifed element removed.
This copies the underlying data.
""" ->
function delete( t::NamedTuple, key::Symbol )
    nms = filter( x->x!=key, fieldnames( t ) )
    name = create_tuple( nms )
    vals = [ getindex( t, nm ) for nm in nms ]
    return eval(current_module(), Expr( :call, name, vals... ) )
end

export @NT, NamedTuple, setindex, delete

end # module
