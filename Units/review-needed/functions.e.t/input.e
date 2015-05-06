deferred class FUNCTIONS

feature -- One

	function0: INTEGER is
		do
		end

feature -- Two

	function1 (one: STRING) is
		once
		end

	function2 (one: INTEGER; two: STRING): INTEGER is
		deferred
		end

	function_anchored: like function0 is
		do
		end

	function_full (one: STRING) is
		require
		local
		do
			-- Implementation
		ensure
		rescue
		end

	infix "<" (other: like Current): BOOLEAN is
		do
		end

	prefix "+": like Current is
		do
		end

end
