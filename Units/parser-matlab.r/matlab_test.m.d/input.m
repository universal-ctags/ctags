classdef input < handle
    methods(Static)
        function [x,y,z] = classfunc1
            [x, y, z] = func1();
        end
        function x = classfunc2()
            x = func2(1);
        end
        function classfunc3;
            func3();
        end
    end
end

function [x,y,z] = func1() % Silly function with multiple outputs
    x = 1;
    y = 2;
    z = 3;
end
function x = func2(arg) 
    x = arg;
end
function func3() 
    % Silly function without any arguments
    A = magic(4);
    R = randn(3,4,5);

    disp('A:');
    disp(A);

    disp('R:');
    disp(R);
end
