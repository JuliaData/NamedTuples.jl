# BEGIN NamedTuples 5.0.0 deprecations

if VERSION < v"0.7.0-DEV.2738"
    # Create a nicer error message for the breaking change to the
    # `@NT( a::Int64, b::Float64 )( 1, 2.0 )` syntax. Note that we will avoid creating
    # this error on Julia 0.7 as NamedTuples.jl previously did not work and won't require
    # the deprecation.

    function (::Type{NT})(args...) where {NT <: NamedTuple}
        error(string(
            "`$NT(args...)` is no longer supported as it is ambiguous with the new " *
            "`$NT((args...,))` constructor."
        ))
    end
end

# END NamedTuples 5.0.0 deprecations
