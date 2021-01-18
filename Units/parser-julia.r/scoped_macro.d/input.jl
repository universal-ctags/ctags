@eval fun1(x) = $a
Base.@eval fun2(x) = $a

@eval function fun3(x)
    $a
end
Base.@eval function fun4(x)
    $a
end

@eval struct Test1
    a::$T
end
Base.@eval struct Test2
    a::$T
end

@eval macro macro1(x)
    $a
end
Base.@eval macro macro2(x)
    $a
end
