indexing

	description: "Compiler-dependent external addressing of objects"
	revision: "$Revision$"
	date: "$Date$"
	copyright: "Copyright 2002 Darren Hiebert and others"
	license: "Eiffel Forum License, version 1"

class EXTERNAL_ADDRESSING

feature -- Access

	string_from_c (p: POINTER): STRING is
			-- A new string created from an external C string
		do
			if (p /= default_pointer) then
				create Result.make (0)
				Result.from_c (p)
			end
		end

	string_to_c (str: STRING): POINTER is
			-- The address of the first character of the string, or
			-- default_pointer if `str' is void. This in an inherently
			-- dangerous function, since the object referred to by the pointer
			-- could be moved by the garbage collector before the pointer is
			-- used. Caveat emptor!
		local
			a: ANY
		do
			if str = Void then
				a := ("").to_c
			else
				a := str.to_c
			end
			Result := c_address ($a)
		end

	c_address (p: POINTER): POINTER is
			-- The address passed in
		do
			Result := p
		end

end
