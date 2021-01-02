function func1(a::Int)
    a
end

function SomeModule.func2(a::Int)
    a
end

module MyModule
function func3(a::Int)
    a
end

function func4(a::Int)
    function func5(b::Int)
        b
    end
    func5(a)
end

end
