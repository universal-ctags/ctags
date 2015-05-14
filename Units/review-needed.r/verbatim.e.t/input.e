indexing

	description: "[
		This is a verbatim string.
		]"

class VERBATIM

feature -- Sample

	constant: STRING is "[
			Vertbatim constant
			]"

feature {NONE} -- Implementation

	cpp_query (obj: FX_OBJECT; app: POINTER): POINTER is
			-- Header comment
		external "[
			C++ [ClassName %"EFXVisual.h%"]
			(EIF_OBJECT, FXApp*): void*
			]"
		end

	tail_attribute: INTEGER

indexing

	library: "[
			EiffelBase: Library of reusable components for Eiffel.
			]"


end
