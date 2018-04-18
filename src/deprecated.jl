# BEGIN NamedTuples 4.1.0 deprecations

if VERSION < v"0.7.0-DEV.2738"
    # Deprecate `@NT( a::Int64, b::Float64 )( 1, 2.0 )` syntax. Note that we're avoiding
    # creating these functions on Julia 0.7 as NamedTuples.jl previously did not work and
    # doesn't require the deprecation.

    @generated function (::Type{NT})(args...) where {NT <: NamedTuple}
        Base.depwarn("`$NT(args...)` is deprecated, use `$NT((args...,))` instead.",
                     Symbol("NamedTuple(args...)"))
        n = length(args)
        aexprs = [ :(args[$i]) for i = 1:n ]
        return gen_namedtuple_ctor_body(n, aexprs)
    end

    # specialized for certain argument counts
    for n = 0:5
        args = [ Symbol("x$n") for n = 1:n ]
        @eval function (::Type{NT})($(args...)) where {NT <: NamedTuple}
            Base.depwarn("`$NT(args...)` is deprecated, use `$NT((args...,))` instead.",
                         Symbol("NamedTuple(args...)"))
            $(gen_namedtuple_ctor_body(n, args))
        end
    end
end

# END NamedTuples 4.1.0 deprecations
