note

	description: "Try everything"

deferred class UBER_CLASS [G -> CONSTRAINT] obsolete "message"

inherit

	ANY
		rename
			generator as my_generator,
			generating_type as my_generating_type,
			infix "/" as infix "//"
		export
			{NONE} my_generator, my_generating_type;
			{ANY} same_type
		undefine
			is_equal
		redefine
			conforms_to
		select
			consitent
		end

	LINKED_LIST [G]
		export {NONE}
			all
		end

create

	make, make2

create {SOMETHING}

	make3

feature -- Initialization

	make
		do
		end

	make2
		do
		end

	make3
		do
		end

feature {ANY, NONE} -- Feature clause comment

	procedure_full
		require else
		local
		do
		ensure then
		rescue
		end

	guard: INTEGER

invariant

	label: condition

note

	license: "May not be used for any purpose"

end
