indexing
	label1: "value"

class
	DEPRECATED_SYNTAX_TEST

inherit
	ANY
		rename
			infix "/" as infix "//"
		end
creation
	make

feature
	make is
		do
		end

	feature1 is
			-- Header comment
		indexing
		obsolete
			"Obsolete message"
		require
		local
		do
		end

	feature2: INTEGER is 42

	infix "<" (other: like Current): BOOLEAN
		do
		end

	prefix "+": like Current
		do
		end

indexing
	label2: "value"
end
