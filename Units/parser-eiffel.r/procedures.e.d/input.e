deferred class PROCEDURES

feature -- One

	procedure0
		do
		end

feature -- Two

	procedure1 (one: STRING)
		once
		end

	procedure2 (one: INTEGER; two: STRING)
		deferred
		end

	procedure_full (one: STRING)
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

end
