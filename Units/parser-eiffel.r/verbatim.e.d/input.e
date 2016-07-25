note

	description: "[
		This is a verbatim string.
		]"

class VERBATIM

feature -- Sample

	constant: STRING = "[
			Vertbatim constant
			]"

feature {NONE} -- Implementation

	cpp_query (obj: FX_OBJECT; app: POINTER): POINTER
			-- Header comment
		external "[
			C++ [ClassName %"EFXVisual.h%"]
			(EIF_OBJECT, FXApp*): void*
			]"
		end

	tail_attribute: INTEGER

note

	library: "[
			EiffelBase: Library of reusable components for Eiffel.
			]"


end
