indexing

	description: "Demo program for TAG_FILE functionality."
	revision: "$Revision$"
	date: "$Date$"
	copyright: "Copyright 2002 Darren Hiebert and others"
	license: "Eiffel Forum License, version 1"

class TAG_FILE_READER

inherit

	TAG_FILE_SORT_TYPES
		export
			{NONE} all
		end

creation

	make

feature -- Initialization

	make (args: ARRAY [STRING]) is
			-- Root creation
		local
			i, j: INTEGER
			arg: STRING
		do
			from
				set_program_name (args.item (0))
				if args.upper = 0 then
					io.error.put_string (usage)
					exceptions.die (1)
				end
				set_default_options
				i := 1
			until
				i > args.upper
			loop
				arg := args.item (i)
				if arg.item (1) /= '-' then
					find_tag (arg)
				else
					from j := 2 until j > arg.count loop
						inspect arg.item (j)
						when 'e' then showing_extension_fields := True
						when 'i' then ignoring_case := True
						when 'p' then partial_matching := True
						when 'l' then list_tags
						when 't' then
							if j < arg.count then
								tag_file_name := arg.substring (j+1, arg.count)
								j := arg.count
							elseif i < args.upper then
								i := i + 1
								tag_file_name := args.item (i)
							else
								io.error.put_string (usage)
								exceptions.die (1)
							end
						when 's' then
							sort_override := True
							j := j + 1
							if j > arg.count then
								sort_type := Sort_case_sensitive
							elseif arg.item (j).is_digit then
								sort_type := arg.item (j).code - ('0').code
							else
								io.error.put_string (usage)
								exceptions.die (1)
							end
						else
							report_unknown_option (arg.item (j))
						end
						j := j + 1
					end
				end
				i := i + 1
			end
		end

feature {NONE} -- Implmentation

	program_name: STRING
			-- Name by which program is executed

	tag_file_name: STRING
			-- Name of tag file

	sort_type: INTEGER
			-- Sort file override

	sort_override: BOOLEAN
			-- Did the user override the sort method?

	showing_extension_fields: BOOLEAN
			-- Did the user request extension fields in output?

	ignoring_case: BOOLEAN
			-- Did the user request ignoring of case in searches?

	partial_matching: BOOLEAN
			-- Did the user request partial matching?

	file: TAG_FILE
			-- Tag file under consideration

	usage: STRING is
		require
			program_name_assigned: program_name /= Void
		once
			Result :=
				"Find tag file entries matching specified names.%N%NUsage: "
			Result.append (program_name)
			Result.append (
				" [-ilp] [-s[0|1]] [-t file] [name(s)]%N%N%
				%Options:%N%
				%    -e           Include extension fields in output.%N%
				%    -i           Perform case-insensitive matching.%N%
				%    -l           List all tags.%N%
				%    -p           Perform partial matching.%N%
				%    -s[0|1|2]    Override sort detection of tag file.%N%
				%    -t file      Use specified tag file (default: %"tags%").%N%
				%Note that options are acted upon as encountered, so order is significant.%N")
		end

	set_program_name (name: STRING) is
			-- Set the program name from path
		require
			name_supplied: name /= Void
		local
			slash: INTEGER
		do
			slash := name.last_index_of ('/', name.count)
			if slash = 0 then
				slash := name.last_index_of ('\', name.count)
			end
			program_name := clone (name)
			if slash > 0 then
				program_name.keep_tail (program_name.count - slash)
			end
		end

	set_default_options is
			-- Set default options
		do
			tag_file_name := "tags"
		end

	report_unknown_option (switch: CHARACTER) is
			-- Report `switch' as unknown option
		do
			io.error.put_string (program_name)
			io.error.put_string (": unknown option: ")
			io.error.put_character (switch)
			io.error.put_character ('%N')
			exceptions.die (1)
		end

	find_tag (name: STRING) is
			-- Find tag matching `name'
		do
			from
				open_tag_file
				if ignoring_case then file.ignore_case end
				if partial_matching then file.match_partial end
				file.search (name)
			until
				file.off
			loop
				print_tag (file.item)
				file.continue_search
			end
			file.close
		end

	list_tags is
			-- List all tags
		do
			open_tag_file
			from file.start until file.off loop
				print_tag (file.item)
				file.forth
			end
			file.close
		end

	open_tag_file is
			-- Open tag file and check for errors
		do
			create file.make (tag_file_name)
			if not file.open then
				io.error.put_string (program_name)
				io.error.put_string (": cannot open tag file: ")
				io.error.put_string (tag_file_name)
				io.error.put_character ('%N')
				exceptions.die (1)
			end
			if sort_override then file.set_sort_type (sort_type) end
		end

	print_tag (tag: TAG_ENTRY) is
			-- Print tag contents to standard output
		require
			tag_supplied: tag /= Void
		do
			if showing_extension_fields then
				io.put_string (tag.out_full)
			else
				io.put_string (tag.out)
			end
			io.put_character ('%N')
		end

	exceptions: EXCEPTIONS is
		once
			create Result
		end

end
