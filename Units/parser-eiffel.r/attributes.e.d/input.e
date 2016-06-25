expanded class ATTRIBUTES

feature

	one: INTEGER
			-- Header comment

	two: ARRAY [STRING]
			-- Header comment

	three: HASH_TABLE [ARRAY [DOUBLE], STRING]
			-- Header comment

	four: DOUBLE
			-- Header comment

	five, six: INTEGER is unique
			-- Header comment

	seven: INTEGER is 1
			-- Header comment

	eight: DOUBLE is 2.0
			-- Header comment

	nine: STRING is "abc"
			-- Header comment

	anchored: like one

end
