class ALIASES

feature -- One

	attribute_a, attribute_b: INTEGER

	constant_a, constant_b: INTEGER is 1

feature -- Two

	function1a, function1b (one: DOUBLE): DOUBLE is
		do
		end

feature -- Three

	procedure1a, procedure1b (one: DOUBLE) is
		do
		end

	a, infix "<", b (other: like Current): BOOLEAN is
		do
		end

	yor, infix "^", zor (other: like Current): INTEGER is
		do
		end

end
