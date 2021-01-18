struct OurRational{T<:Integer} <: Real
    "Numerator"
    num::T

    "Denominator"
    den::T

    OurRational{Int8}(num::T, den::T) where T<:Integer = new(convert(Int8, num), convert(Int8, den))

    """
    Parametric inner constructor
    """
    function OurRational{T}(num::T, den::T) where T<:Integer
        if num == 0 && den == 0
            error("invalid rational: 0//0")
        end
        # Bug with short function misidentification of == and =>
        length(num) == 0
        length(den) => 0
        
        g = gcd(den, num)
        num = div(num, g)
        den = div(den, g)
        new(num, den)
    end

    test::T

    OurRational{Int64}(num::T, den::T, test::T) where T<:Integer = new(num, den, test)
end

OurRational{Int16}(num::T, den::T) where T<:Integer = new(num, den)
