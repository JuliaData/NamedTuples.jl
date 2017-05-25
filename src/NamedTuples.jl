__precompile__()
module NamedTuples

abstract type NamedTuple end

Base.keys( t::NamedTuple ) = fieldnames( t )
Base.values( t::NamedTuple ) = [ getfield( t, i ) for i in 1:nfields( t ) ]
Base.haskey( t::NamedTuple, k ) = k in keys(t)
Base.length( t::NamedTuple ) = nfields( t )
# Iteration
Base.start( t::NamedTuple ) = 1
Base.done( t::NamedTuple, iter ) = iter > nfields( t )
Base.next( t::NamedTuple, iter ) = ( getfield( t, iter ), iter + 1 )
Base.endof( t::NamedTuple ) = length( t )
Base.last( t::NamedTuple ) = t[end]
function Base.show( io::IO, t::NamedTuple )
    print(io, "(")
    first = true
    for (k,v) in zip(keys(t),values(t))
        !first && print(io, ", ")
        print(io, k, " = "); show(io, v)
        first = false
    end
    print(io, ")")
end
# Make this indexable so that it works like a Tuple
Base.getindex( t::NamedTuple, i::Int ) = getfield( t, i )
# We also support indexing by symbol
Base.getindex( t::NamedTuple, i::Symbol ) = getfield( t, i )
Base.getindex( t::NamedTuple, i::Symbol, default ) = get( t, i, default )
# This is a linear lookup...
Base.get( t::NamedTuple, i::Symbol, default ) = i in keys(t) ? t[i] : default
# Deep compare

import Base: ==

function ==( lhs::NamedTuple, rhs::NamedTuple)
    ( lhs === rhs ) && return true
    ( typeof( lhs ) != typeof( rhs )) && return false
    for i in 1:length( lhs )
        if ( lhs[i] != rhs[i])
            return false
        end
    end
    return true
end
# Deep hash

function Base.hash( nt::NamedTuple, hs::UInt64)
    h = 17
    for v in values(nt)
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

function trans( ::Union{Type{ParseNode{:(=)}},Type{ParseNode{:kw}}}, expr::Expr)
    (sym, typ ) = trans( expr.args[1])
    return (sym, typ, escape( expr.args[2] ))
end

function trans( ::Type{ParseNode{:call}}, expr::Expr)
    if expr.args[1] == :(=>)
        Base.depwarn("\"=>\" syntax for NamedTuple construction is deprecated, use \"=\" instead.", Symbol("@NT"))
        (sym, typ ) = trans( expr.args[1])
        return (sym, typ, escape( expr.args[2] ))
    end
    return (nothing, nothing, escape(expr) )
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

function gen_namedtuple_ctor_body(n::Int, args)
    types = [ :(typeof($x)) for x in args ]
    cnvt = [ :(convert(fieldtype(TT,$n),$(args[n]))) for n = 1:n ]
    if n == 0
        texpr = :T
    else
        texpr = :(NT{$(types...)})
    end
    if isless(Base.VERSION, v"0.6.0-")
        tcond = :(NT === NT.name.primary)
    else
        tcond = :(isa(NT,UnionAll))
    end
    quote
        if $tcond
            TT = $texpr
        else
            TT = NT
        end
        if nfields(TT) !== $n
            throw(ArgumentError("wrong number of arguments to named tuple constructor"))
        end
        $(Expr(:new, :TT, cnvt...))
    end
end

# constructor for all NamedTuples
@generated function (::Type{NT}){NT<:NamedTuple}(args...)
    n = length(args)
    aexprs = [ :(args[$i]) for i = 1:n ]
    return gen_namedtuple_ctor_body(n, aexprs)
end

# specialized for certain argument counts
for n = 0:5
    args = [ Symbol("x$n") for n = 1:n ]
    @eval function (::Type{NT}){NT<:NamedTuple}($(args...))
        $(gen_namedtuple_ctor_body(n, args))
    end
end

#
# Create a NameTuple in the context of this module
# this is only done if the tuple has not already been
# constructed.
function create_tuple( fields::Vector{Symbol})
    escaped_fieldnames = [replace(string(i), "_", "__") for i in fields]
    name = Symbol( string( "_NT_", join( escaped_fieldnames, "_")) )
    if !isdefined(NamedTuples, name)
        len = length( fields )
        types = [Symbol("T$n") for n in 1:len]
        tfields = [ Expr(:(::), Symbol( fields[n] ), Symbol( "T$n") ) for n in 1:len ]
        def = Expr(:type, false, Expr( :(<:), Expr( :curly, name, types... ), :NamedTuple ),
                   Expr(:block, tfields...,
                        Expr(:tuple)))  # suppress default constructors
        eval(NamedTuples, def)
    end
    return name
end

#
# Given a symbol list create the NamedTuple
#
@doc doc"Given a symbol vector create the `NamedTuple`" ->
function make_tuple( syms::Vector{Symbol} )
    return create_tuple( syms )
end

#
# Given an expression vector create the NamedTuple
#
@doc doc"Given an expression vector create the `NamedTuple`" ->
function make_tuple( exprs::Vector)
    len    = length( exprs )
    fields = Array{Symbol}(len)
    values = Array{Any}(len)
    typs   = Array{Any}(len)

    # Are we directly constructing the type, if so all values must be
    # supplied by the caller, we use this state to ensure this
    construct = false
    # handle the case where this is defining a datatype
    for i in 1:len
        expr = exprs[i]
        ( sym, typ, val ) = trans( expr )
        if( construct == true && val == nothing || ( i > 1 && construct == false && val != nothing ))
            error( "Invalid tuple, all values must be specified during construction @ ($expr)")
        end
        construct  = val != nothing
        fields[i]  = sym != nothing?sym:Symbol( "_$(i)_")
        typs[i] = typ
        # On construction ensure that the types are consitent with the declared types, if applicable
        values[i]  = ( typ != nothing && construct)? Expr( :call, :convert, typ, val ) : val
    end

    name = create_tuple( fields )

    # Either call the constructor with the supplied values or return the type
    if( !construct )
        if len == 0
            return name
        end
        return Expr( :curly, name, typs... )
    else
        return Expr( :call, name, values ... )
    end
end

@doc doc"""
Syntax

    @NT( a, b )                 -> Defines a tuple with a and b as members
    @NT( a::Int64, b::Float64 ) -> Defines a tuple with the specific arg types as members
    @NT( a = 1, b = "hello")  -> Defines and constructs a tuple with the specifed members and values
    @NT( a, b )( 1, "hello")    -> Is equivalent to the above definition
    @NT( a::Int64 )( 2.0 )      -> Calls `convert( Int64, 2.0 )` on construction and sets `a`

NamedTuples may be used anywhere you would use a regular Tuple, this includes method definition and return values.

    module Test
    using NamedTuples

    function foo( y )
        a = 1
        x = 3
        return  @NT( a = 1, b = "world", c = "hello", d=a/x, y = a/y  )
    end
    function bar( nt::@NT( a::Int64, c::ASCIIString ))
        return repeat( nt.c, nt.a )
    end

    end

    Test.foo( 1 ) # Returns a NamedTuple of 5 elements
    Test.bar( @NT( a= 2, c="hello")) # Returns `hellohello`
""" ->
macro NT( expr... )
    return make_tuple( collect( expr ))
end

# Helper function for 0.4 compat
if VERSION < v"0.5.0" 
    getfieldname( t, i ) = fieldnames(t)[i]
else
    getfieldname( t, i ) = fieldname( t, i )
end

@inline function Base.map(f, nt::NamedTuple, nts::NamedTuple...)
    # this method makes sure we don't define a map(f) method
    _map(f, nt, nts...)
end

@generated function _map(f, nts::NamedTuple...)
    fields = fieldnames(nts[1])
    for x in nts[2:end]
        if !isequal(fieldnames(x), fields)
            throw(ArgumentError("All NamedTuple inputs to map must have the same fields in the same order"))
        end
    end
    N = nfields(nts[1])
    M = length(nts)

    NT = create_tuple(fields) # This type will already exist if this function may be called
    args = Expr[:(f($(Expr[:(getfield(nts[$i], $j)) for i = 1:M]...))) for j = 1:N]
    quote
        NamedTuples.$NT($(args...))
    end
end

@generated function Base.isless(t1::NamedTuple, t2::NamedTuple)
    if !isequal(fieldnames(t1), fieldnames(t2))
        throw(ArgumentError("NamedTuple inputs to isless must have the same fields in the same order"))
    end
    quote
        Base.@nexprs $(nfields(t1)) i -> begin
            a_i = getfield(t1, i)
            b_i = getfield(t2, i)
            if !isequal(a_i, b_i)
                return isless(a_i, b_i)
            end
        end
        return false
    end
end

function Base.getindex( t::NamedTuple, rng::AbstractVector )
    names = unique( Symbol[ isa(i,Symbol) ? i : getfieldname(typeof(t),i) for i in rng ] )
    name = create_tuple( names )
    getfield(NamedTuples,name)([ getfield( t, i ) for i in names ]...)
end

@doc doc"""
Merge two NamedTuples favoring the lhs
Order is preserved lhs names come first.
This copies the underlying data.
""" ->
@generated function Base.merge( lhs::NamedTuple, rhs::NamedTuple )
    field_assignments = Expr[]
    names_from_left = Set{Symbol}()
    for i in fieldnames(lhs)
        push!(field_assignments, Expr(:(=), i, Expr(:., :lhs, QuoteNode(i))))
        push!(names_from_left, i)
    end

    for i in fieldnames(rhs)
        if !(i in names_from_left)
            push!(field_assignments, Expr(:(=), i, Expr(:., :rhs, QuoteNode(i))))
        end
    end

    constructor_call = Expr(:macrocall, Symbol("@NT"), field_assignments...)

    return constructor_call
end

@doc doc"""
Create a new NamedTuple with the new value set on it, either overwriting
the old value or appending a new value.
This copies the underlying data.
""" ->
function setindex{V}( t::NamedTuple, key::Symbol, val::V)
    nt = getfield( NamedTuples, create_tuple( [key] ))( val )
    return merge( t, nt )
end

@doc doc"""
Create a new NamedTuple with the specified element removed.
""" ->
function delete( t::NamedTuple, key::Symbol )
    nms = filter( x->x!=key, fieldnames( t ) )
    name = create_tuple( nms )
    vals = [ getindex( t, nm ) for nm in nms ]
    return getfield(NamedTuples, name)(vals...)
end

Base.Broadcast._containertype(::Type{<:NamedTuple}) = NamedTuple
Base.Broadcast.promote_containertype(::Type{NamedTuple}, ::Type{NamedTuple}) = NamedTuple
Base.Broadcast.promote_containertype(::Type{NamedTuple}, _) = error()
Base.Broadcast.promote_containertype(_, ::Type{NamedTuple}) = error()

@inline function Base.Broadcast.broadcast_c(f, ::Type{NamedTuple}, nts...)
    _map(f, nts...)
end

export @NT, NamedTuple, setindex, delete

end # module
