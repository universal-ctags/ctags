class ALIASES

feature -- One

	attribute_a, attribute_b: INTEGER

	constant_a, constant_b: INTEGER = 1

feature -- Two

	function1a, function1b (one: DOUBLE): DOUBLE
		do
		end

feature -- Three

	procedure1a, procedure1b (one: DOUBLE)
		do
		end

	a alias "<", b (other: like Current): BOOLEAN
		do
		end

	yor alias "^", zor (other: like Current): INTEGER
		do
		end

end
