using Base: depwarn
import Base.getindex

export setindex, delete

function Base.getindex( t::NamedTuple, rng::AbstractVector )
    depwarn("`getindex(t::NamedTuple, rng::AbstractVector)` is deprecated", :getindex)
    names = unique( Symbol[ isa(i,Symbol) ? i : getfieldname(typeof(t),i) for i in rng ] )
    ty = create_namedtuple_type( names )
    ty([ getfield( t, i ) for i in names ]...)
end

@deprecate getindex( t::NamedTuple, i::Symbol, default ) get( t, i, default )

"""
Create a new NamedTuple with the new value set on it, either overwriting
the old value or appending a new value.
This copies the underlying data.
"""
function setindex( t::NamedTuple, key::Symbol, val::V) where V
    depwarn("`setindex(t::NamedTuple,  key::Symbol, val::V)` is deprecated", :setindex)
    nt = create_namedtuple_type( [key] )( val )
    return merge( t, nt )
end

"""
Create a new NamedTuple with the specified element removed.
"""
function delete( t::NamedTuple, key::Symbol )
    depwarn("`delete(t::NamedTuple,  key::Symbol)` is deprecated", :delete)
    nms = filter( x->x!=key, fieldnames( t ) )
    ty = create_namedtuple_type( nms )
    vals = [ getindex( t, nm ) for nm in nms ]
    return ty(vals...)
end
