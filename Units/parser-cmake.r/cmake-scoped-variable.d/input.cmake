function(func1)
	set(var1 TRUE)
	set(var2 "hello")
endfunction()

	function(	func2 ${BAR})
		set ( var_3_unscoped "hello" PARENT_SCOPE)
		set ( var_4_unscoped "hello" "world" PARENT_SCOPE)
		set ( var_5_unscoped "hello \"" ${ARGN} "\" world" PARENT_SCOPE)
	endfunction()

function(cxx_shared_library name cxx_flags)
  cxx_library_with_type(${name} SHARED "${cxx_flags}" ${ARGN})

  # yes cmake files reall have these types of things:
  set (_var6 "-x ${_xLanguage_${_language}} -c \"${_prefixFile}\" -o \"${_pchFile}\"")
  set (_var7_unscoped "-x ${_xLanguage_${_language}} -c \"${_prefixFile}\" -o \"${_pchFile}\"" PARENT_SCOPE)
  set (
  	_var8_unscoped
  	"-x ${_xLanguage_${_language}} -c \"${_prefixFile}\" -o \"${_pchFile}\""
  	PARENT_SCOPE)

  set (_var9 "/FI\"${_prefixFileNative}\"")
  set (_var10 "/FI\"${_prefixFileNative}\"" "/FI\"${_prefixFileNative}\"")
  set (_var11_unscoped "/FI\"${_prefixFileNative}\"" "/FI\"${_prefixFileNative}\"" PARENT_SCOPE)
endfunction()

    FUNCTION		(
        func4
    true)
		SET ( var_12 "hello" foo ${bar})

	ENDFUNCTION()

macro(macro1)
	set(var13_unscoped TRUE)
endmacro()

function(a)
	function(b)
		macro(macro2)
			set(var14_a_b TRUE)
		endmacro()

		function(c ...)
			set(var15_a_b_c TRUE)
			set(var_16_unscoped "hello \"" ${ARGN} "\" world" PARENT_SCOPE)
		endfunction()
	endfunction()
endfunction()


