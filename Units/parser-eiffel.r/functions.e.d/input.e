deferred class FUNCTIONS

feature -- One

	function0: INTEGER
		do
		end

feature -- Two

	function1 (one: STRING)
		once
		end

	function2 (one: INTEGER; two: STRING): INTEGER
		deferred
		end

	function_anchored: like function0
		do
		end

	function_full (one: STRING)
			-- header comment
		note
		obsolete
			"Obsolete message"
		require
		local
		do
			-- Implementation
		ensure
		rescue
		end

	infix_alias alias "<" (other: like Current): BOOLEAN
		do
		end

	prefix_alias alias "+": like Current
		do
		end

end
