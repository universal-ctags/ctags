indexing

	description: "Describes a single entry in a tag file."
	revision: "$Revision$"
	date: "$Date$"
	copyright: "Copyright 2002 Darren Hiebert and others"
	license: "Eiffel Forum License, version 1"

class TAG_ENTRY

inherit

	ANY
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
			p: POINTER
		do
			p := addressing.string_to_c (buffer)

			name        := addressing.string_from_c (c_name (p))
			file        := addressing.string_from_c (c_file (p))
			pattern     := addressing.string_from_c (c_pattern (p))
			line_number := c_line_number (p)
			kind        := addressing.string_from_c (c_kind (p))
			file_scoped := c_file_scope (p)

			if c_field_count (p) > 0 then
				create extension_fields.make_from_c (buffer)
			end
		end

feature -- Access

	name: STRING
			-- Name of tag

	file: STRING
			-- Path of source file containing definition of tag

	pattern: STRING
		    -- Pattern for locating source line (may be void)

	line_number: INTEGER
		    -- Line number in source file of tag definition
			-- (may be zero if not known)

	kind: STRING
			-- Kind of tag (may by name, character, or void if not known)

	file_scoped: BOOLEAN
			-- Is tag of file-limited scope?

	extension_fields: TAG_EXTENSION_FIELDS
			-- Extension fields (may be void if not available)

feature -- Output

	out: STRING is
			-- Printable representation of tag entry
		do
			create Result.make (name.count + 1 + file.count + 1 + pattern.count + 40)
			Result.append (name)
			Result.extend (Tab)
			Result.append (file)
			Result.extend (Tab)
			if pattern /= Void then
				Result.append (pattern)
			else
				Result.append_integer (line_number)
			end
		end

	out_full: STRING is
			-- Printable representation of tag entry with extension fields
		local
			sep: STRING
		do
			sep := ";%""
			Result := out
			Result.copy (out)
			if kind /= Void then
				Result.append (sep); sep := Empty_string
				Result.extend (Tab)
				Result.append (Kind_key)
				Result.extend (Colon)
				Result.append (kind)
			end
			if file_scoped then
				Result.append (sep); sep := Empty_string
				Result.extend (Tab)
				Result.append (File_scope_key)
				Result.extend (Colon)
			end
			if extension_fields /= Void then
				Result.append (sep); sep := Empty_string
				Result.append (extension_fields.out)
			end
		end

feature {NONE} -- Implementation

	Kind_key: STRING is "kind"
			-- Key used for "kind" of tag

	File_scope_key: STRING is "file"
			-- Key used for file scoping of tag

	Colon: CHARACTER is ':'
			-- Key-value separator

	Tab: CHARACTER is '%T'
			-- Field separator

	Empty_string: STRING is ""
			-- Empty string

	addressing: EXTERNAL_ADDRESSING is
			-- Access to external addressing features
		once
			create Result
		end

feature {NONE} -- Externals

	c_name (p: POINTER): POINTER is
		external
			"C [struct %"readtags.h%"] (tagEntry): const char*"
		alias
			"name"
		end

	c_file (p: POINTER): POINTER is
		external
			"C [struct %"readtags.h%"] (tagEntry): const char*"
		alias
			"file"
		end

	c_pattern (p: POINTER): POINTER is
		external
			"C [struct %"readtags.h%"] (tagEntry): const char*"
		alias
			"address.pattern"
		end

	c_line_number (p: POINTER): INTEGER is
		external
			"C [struct %"readtags.h%"] (tagEntry): unsigned long"
		alias
			"address.lineNumber"
		end

	c_kind (p: POINTER): POINTER is
		external
			"C [struct %"readtags.h%"] (tagEntry): const char*"
		alias
			"kind"
		end

	c_file_scope (p: POINTER): BOOLEAN is
		external
			"C [struct %"readtags.h%"] (tagEntry): short"
		alias
			"fileScope"
		end

	c_field_count (p: POINTER): INTEGER is
		external
			"C [struct %"readtags.h%"] (tagEntry): unsigned short"
		alias
			"fields.count"
		end

end
