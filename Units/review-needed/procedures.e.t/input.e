deferred class PROCEDURES

feature -- One

	procedure0 is
		do
		end

feature -- Two

	procedure1 (one: STRING) is
		once
		end

	procedure2 (one: INTEGER; two: STRING) is
		deferred
		end

	procedure_full (one: STRING) is
		require
		local
		do
			-- Implementation
		ensure
		rescue
		end

end
