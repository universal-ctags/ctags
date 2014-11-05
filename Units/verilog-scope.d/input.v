module mod

function f1()
begin
end
endfunction

function f2()
begin
  // no idea if this is valid but whatever
  function f2sub()
  begin
    function f2deeper()
      // here, f2sub used to have scope=function:mod.mod.f2.f2sub
      // notice the duplicate "mod"
    endfunction
  end
  endfunction
end
endfunction

endmodule
