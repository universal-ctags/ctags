indexing

	description: "Abstraction of file containing tag records"
	revision: "$Revision$"
	date: "$Date$"
	copyright: "Copyright 2002 Darren Hiebert and others"
	license: "Eiffel Forum License, version 1"

class TAG_FILE

inherit

	TAG_FILE_FORMATS
	TAG_FILE_SORT_TYPES
	EXTERNAL_ADDRESSING
		export
			{NONE} all
		end

creation

	make

feature -- Initialization

	make (nm: STRING) is
			-- Create from name of existing tag file
		require
			name_not_empty: nm /= Void and then not nm.is_empty
		do
			format := Format_extended
			sort_type := Sort_case_sensitive
			create tag_file_info_struct.make_filled ('%U', tag_file_info_size)
			create tag_entry_struct.make_filled ('%U', tag_entry_size)
			handle := c_tags_open (
				string_to_c (nm), string_to_c (tag_file_info_struct))
			open := handle /= default_pointer
			if open then
				path := nm
				format := c_file_format (string_to_c (tag_file_info_struct))
				sort_type := c_file_sort (string_to_c (tag_file_info_struct))
				program_author := string_from_c (
					c_program_author (string_to_c (tag_file_info_struct)))
				program_name := string_from_c (
					c_program_name (string_to_c (tag_file_info_struct)))
				program_url := string_from_c (
					c_program_url (string_to_c (tag_file_info_struct)))
				program_version := string_from_c (
					c_program_version (string_to_c (tag_file_info_struct)))
			else
				error_number := c_file_error_number (
					string_to_c (tag_file_info_struct))
				error_description := string_from_c (c_strerror (error_number))
			end
		ensure
			opened: open or else (error_number > 0 and error_description /= Void)
		end

feature -- Access

	path: STRING
			-- Path of tag file

	program_author: STRING
		    -- Name of author of generating program (may be null)

	program_name: STRING
		    -- Name of program (may be void)

	program_url: STRING
		    -- URL of distribution (may be void)

	program_version: STRING
		    -- Program version (may be void)

	item: TAG_ENTRY
			-- Current tag

feature -- Status report

	open: BOOLEAN
			-- Is the tag file open? If this is false, then the system "errno"
			-- variable may be examined to determine the cause.

	error_number: INTEGER
			-- Value of errno of O/S when creation of `Current' fails

	error_description: STRING
			-- Value of errno of O/S when creation of `Current' fails

	off: BOOLEAN
			-- Is there another tag after this one?

	format: INTEGER
			-- Format of tag file
			
	sort_type: INTEGER
		    -- Type of sorting used on file.

	partial_matching: BOOLEAN
			-- Should searches find tags whose whose leading characters match
			-- the supplied string?

	ignoring_case: BOOLEAN
			-- Should searches match without regard to case?

feature -- Status setting

	set_sort_type (type: INTEGER) is
			-- Force valud for sort type of file if automatic detection does
			-- not work.
		require
			open: open
			valid_sort_type: valid_sort_type (type)
		local
			return_code: INTEGER
		do
			return_code := c_tags_set_sort_type (handle, type)
			check no_error: return_code = Tag_success end
			sort_type := type
		ensure
			sort_type_overridden: sort_type = type
		end

	ignore_case is
			-- Disable recognition of case for searches
		require
			open: open
		do
			ignoring_case := True
		ensure
			searches_ignore_case: ignoring_case
		end

	observe_case is
			-- Enable recognition of case for searches
		require
			open: open
		do
			ignoring_case := False
		ensure
			searches_observe_case: not ignoring_case
		end

	match_partial is
			-- Enable partial matching for searches
		require
			open: open
		do
			partial_matching := True
		ensure
			searches_match_partial: partial_matching
		end

	match_full is
			-- Enable full matching for searches
		require
			open: open
		do
			partial_matching := False
		ensure
			searches_match_full: not partial_matching
		end

	start is
			-- Move to first tag in file
		do
			read_tag (c_tags_first (handle, string_to_c (tag_entry_struct)))
		end

	forth is
			-- Advance to next tag in file
		require
			exists: open
			another_available: not off
		do
			read_tag (c_tags_next (handle, string_to_c (tag_entry_struct)))
		ensure
			item_found: not off implies item /= Void
		end

	search (name: STRING) is
			-- Search from beginning of file for tag matching `name'.
			-- Search affected by values of `partial_matching' and
			-- `ignoring_case'.
		require
			open: open
			name_supplied: name /= Void and then not name.is_empty
		do
			search_name := clone (name)
			read_tag (c_tags_find (handle, string_to_c (tag_entry_struct),
					string_to_c (name), search_options))
		ensure
			match:
				not off implies (item /= Void and then not item.name.is_empty)
			non_partial_match:
				(not off and not partial_matching)
					implies item.name.count = name.count
			partial_match:
				(not off and partial_matching)
					implies item.name.count >= name.count
			case_match:
				(not off and not ignoring_case) implies
					name.is_equal (item.name.substring (1, name.count))
			case_ignore_match:
				(not off and not ignoring_case) implies
					name.as_upper.is_equal (
						item.name.substring (1, name.count).as_upper)
		end

	continue_search is
			-- Continue search initiated with `search' using same parameters
			-- and options.
		require
			open: open
			not off
		do
			read_tag (c_tags_find_next (handle, string_to_c (tag_entry_struct)))
		ensure
			match:
				not off implies (item /= Void and then not item.name.is_empty)
			non_partial_match:
				(not off and not partial_matching)
					implies item.name.count = search_name.count
			partial_match:
				(not off and partial_matching)
					implies item.name.count >= search_name.count
			case_match:
				(not off and not ignoring_case) implies
					search_name.is_equal (
						item.name.substring (1, search_name.count))
			case_ignore_match:
				(not off and not ignoring_case) implies
					search_name.as_upper.is_equal (
						item.name.substring (1, search_name.count).as_upper)
		end

	close is
			-- Close tag file
		require
			open: open
		local
			return_code: INTEGER
		do
			return_code := c_tags_close (handle)
			check no_failure: return_code = Tag_success end
			handle := default_pointer
		ensure
			handle_reset: handle = default_pointer
		end

feature {NONE} -- Implementation

	handle: POINTER
			-- Handle to tag file

	tag_file_info_struct: STRING
			-- Buffer for tagFileInfo C structure

	tag_entry_struct: STRING
			-- Buffer for tagEntry C structure

	search_name: STRING
			-- Name used in `search'

	search_options: INTEGER is
			-- Bit representation of search options
		do
			if partial_matching then
				Result := Tag_partial_match
			end
			if ignoring_case then
				Result := Result + Tag_ignore_case
			end
		end

	read_tag (return_code: INTEGER) is
		do
			if return_code = Tag_success then
				create item.make_from_c (tag_entry_struct)
				off := False
			else
				off := True
				item := Void
			end
		end

feature {NONE} -- Externals

	c_strerror (errnum: INTEGER): POINTER is
		external
			"C (int): char* | <errno.h>"
		alias
			"strerror"
		end

	tag_file_info_size: INTEGER is
		external
			"C [macro %"readtags.h%"] (): long"
		alias
			"sizeof (tagFileInfo)"
		end

	tag_entry_size: INTEGER is
		external
			"C [macro %"readtags.h%"] (): long"
		alias
			"sizeof (tagEntry)"
		end

	Tag_success: INTEGER is
		external
			"C [macro %"readtags.h%"] (): int"
		alias
			"TagSuccess"
		end

	Tag_failure: INTEGER is
		external
			"C [macro %"readtags.h%"] (): int"
		alias
			"TagFailure"
		end

	Tag_unsorted: INTEGER is
		external
			"C [macro %"readtags.h%"] (): int"
		alias
			"TAG_UNSORTED"
		end

	Tag_sorted: INTEGER is
		external
			"C [macro %"readtags.h%"] (): int"
		alias
			"TAG_SORTED"
		end

	Tag_fold_sorted: INTEGER is
		external
			"C [macro %"readtags.h%"] (): int"
		alias
			"TAG_FOLDSORTED"
		end

	Tag_partial_match: INTEGER is
		external
			"C [macro %"readtags.h%"] (): int"
		alias
			"TAG_PARTIALMATCH"
		end

	Tag_ignore_case: INTEGER is
		external
			"C [macro %"readtags.h%"] (): int"
		alias
			"TAG_IGNORECASE"
		end

	c_tags_open (file_path, info: POINTER): POINTER is
			-- extern tagFile *tagsOpen (const char *filePath,
			--   tagFileInfo *info);
		external
			"C (const char*, tagFileInfo*): tagFile* | %"readtags.h%""
		alias
			"tagsOpen"
		end

	c_tags_set_sort_type (h: POINTER; type: INTEGER): INTEGER is
			-- extern tagResult tagsSetSortType (tagFile *file, sortType type);
		external
			"C (tagFile*, sortType): tagResult | %"readtags.h%""
		alias
			"tagsSetSortType"
		end

	c_tags_first (h, entry: POINTER): INTEGER is
		external
			"C (tagFile*, tagEntry*): tagResult | %"readtags.h%""
		alias
			"tagsFirst"
		end

	c_tags_next (h, entry: POINTER): INTEGER is
		external
			"C (tagFile*, tagEntry*): tagResult | %"readtags.h%""
		alias
			"tagsNext"
		end

	c_tags_find (h, entry, name: POINTER; options: INTEGER): INTEGER is
			-- extern tagResult tagsFind (tagFile *file, tagEntry *entry,
			--   const char *name, int options);
		external
			"C (tagFile*, tagEntry*, const char*, int): tagResult | %"readtags.h%""
		alias
			"tagsFind"
		end

	c_tags_find_next (h, entry: POINTER): INTEGER is
			-- extern tagResult tagsFindNext (tagFile *file, tagEntry *entry);
		external
			"C (tagFile*, tagEntry*): tagResult | %"readtags.h%""
		alias
			"tagsFindNext"
		end

	c_tags_close (h: POINTER): INTEGER is
			-- extern tagResult tagsClose (tagFile *file);
		external
			"C (tagFile*): tagResult | %"readtags.h%""
		alias
			"tagsClose"
		end

	c_file_opened (p: POINTER): BOOLEAN is
		external
			"C [struct %"readtags.h%"] (tagFileInfo): int"
		alias
			"status.opened"
		end

	c_file_error_number (p: POINTER): INTEGER is
		external
			"C [struct %"readtags.h%"] (tagFileInfo): int"
		alias
			"status.error_number"
		end

	c_file_format (p: POINTER): INTEGER is
		external
			"C [struct %"readtags.h%"] (tagFileInfo): short"
		alias
			"file.format"
		end

	c_file_sort (p: POINTER): INTEGER is
		external
			"C [struct %"readtags.h%"] (tagFileInfo): short"
		alias
			"file.sort"
		end

	c_program_author (p: POINTER): POINTER is
		external
			"C [struct %"readtags.h%"] (tagFileInfo): const char*"
		alias
			"program.author"
		end

	c_program_name (p: POINTER): POINTER is
		external
			"C [struct %"readtags.h%"] (tagFileInfo): const char*"
		alias
			"program.name"
		end

	c_program_url (p: POINTER): POINTER is
		external
			"C [struct %"readtags.h%"] (tagFileInfo): const char*"
		alias
			"program.url"
		end

	c_program_version (p: POINTER): POINTER is
		external
			"C [struct %"readtags.h%"] (tagFileInfo): const char*"
		alias
			"program.version"
		end

invariant

	valid_format: valid_format (format)
	valid_sort_type: valid_sort_type (sort_type)

end
