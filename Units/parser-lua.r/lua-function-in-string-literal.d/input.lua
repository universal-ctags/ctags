-- Taken from issue #2439 opened by @andrejlevkovitch
local function caller(str, foo)
  foo()
  print(str)
end

caller("foo called", function()
end)

caller("functional", function()
end)

caller("function foo", function()
end)

function a()
end

function ab()
end

function abc ()
end

function e ()
end

function ef ()
end

x = function()
end

y = function ()
end

z = function  ()
end
