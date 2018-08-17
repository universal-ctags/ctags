function(tag_this1)
endfunction(tag_this1)
	function(	Bfunctiontag_this_2 ${BAR})
	endfunction()

function(cxx_shared_library#
         name cxx_flags)
  cxx_library_with_type(${name} SHARED "${cxx_flags}" ${ARGN})
endfunction()

    FUNCTION		(
        d_ALSO_Tag_this#[[ set(foo) ]]
    true)
    ENDFUNCTION()

Function(
eLastlyTagThis_ "hello")
Endfunction()

function(a)
  set(var1_in_func_a TRUE)
  function(b)
	set(var2_in_func_a_b TRUE)
    function(c)
    	set(var3_in_func_a_b_c TRUE)
    endfunction(c)
  endfunction(b)
endfunction(a)

set(var4_in_no_func TRUE)

function(${not_this} foo)
function(not-this foo)
function(not.this foo)
function(1notthis foo)
