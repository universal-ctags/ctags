function [x,y,z] = func1 
end
function x = func2 
end
function func3 
end
function y = func4(a, b, c)
end
function func5   % this comment should be ignored --> X = FAIL5
end
functionality = FAIL6; % this is not a function and should not be parsed
