indexing

	description: "Variable extension fields for tag file entry."
	revision: "$Revision$"
	date: "$Date$"
	copyright: "Copyright 2002 Darren Hiebert and others"
	license: "Eiffel Forum License, version 1"

class TAG_EXTENSION_FIELDS

inherit

	ARRAY [TUPLE [STRING, STRING]]
		rename
			item as array_item
		export
			{NONE} all
		redefine
			out
		end

creation

	make_from_c

feature -- Initialization

	make_from_c (buffer: STRING) is
			-- Create from buffer containing C tagEntry structure
		require
			buffer_exists: buffer /= Void
		local
			i: INTEGER
			field_count: INTEGER
			p, list: POINTER
			key: STRING
			value: STRING
		do
			from
				field_count := c_field_count (addressing.string_to_c (buffer))
				make (1, field_count)
				list := c_extension_fields (addressing.string_to_c (buffer))
				i := 0
			until
				i >= field_count
			loop
				p := list + (i * tag_extension_field_size)
				key := addressing.string_from_c (c_key (p))
				value := addressing.string_from_c (c_value (p))
				put ([value, key], i + 1)
				i := i + 1
			end
		end

feature -- Access

	item (key: STRING): STRING is
			-- Value associated with `key'
		require
			non_void_key: key /= Void
		local
			i: INTEGER
			key_for_iteration: STRING
		do
			from
				i := lower
			until
				i > upper or Result /= Void
			loop
				key_for_iteration ?= array_item (i).item (2)
				check not_void: key_for_iteration /= Void end
				if key.is_equal (key_for_iteration) then
					Result ?= array_item (i).item (1)
				end
			end
		end

feature -- Output

	out: STRING is
			-- Printable representation
		local
			key, value: STRING
			i: INTEGER
		do
			create Result.make (30)
			from
				i := lower
			until
				i > upper
			loop
				Result.extend (Tab)
				key ?= array_item (i).item (2)
				check key_exists: key /= Void end
				Result.append (key)
				Result.extend (Colon)
				value ?= array_item (i).item (1)
				if value /= Void then
					Result.append (value)
				end
				i := i + 1
			end
		end

feature {NONE} -- Implementation

	Colon: CHARACTER is ':'

	Tab: CHARACTER is '%T'

	addressing: EXTERNAL_ADDRESSING is
			-- Access to external addressing features
		once
			create Result
		end

feature {NONE} -- Externals

	tag_extension_field_size: INTEGER is
		external
			"C [macro %"readtags.h%"] (): long"
		alias
			"sizeof (tagExtensionField)"
		end
	
	c_field_count (p: POINTER): INTEGER is
		external
			"C [struct %"readtags.h%"] (tagEntry): unsigned short"
		alias
			"fields.count"
		end

	c_extension_fields (p: POINTER): POINTER is
		external
			"C [struct %"readtags.h%"] (tagEntry): tagExtensionField"
		alias
			"fields.list"
		end

	c_key (p: POINTER): POINTER is
		external
			"C [struct %"readtags.h%"] (tagExtensionField): const char*"
		alias
			"key"
		end

	c_value (p: POINTER): POINTER is
		external
			"C [struct %"readtags.h%"] (tagExtensionField): const char*"
		alias
			"value"
		end

end
