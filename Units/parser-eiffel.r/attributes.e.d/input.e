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

	five, six: INTEGER = unique
			-- Header comment

	seven: INTEGER = 1
			-- Header comment

	eight: DOUBLE = 2.0
			-- Header comment

	nine: STRING = "abc"
			-- Header comment

	anchored: like one
			-- Header comment

	attribute_short: STRING
			-- Header comment
		attribute
		end

	attribute_full: STRING
			-- Header comment
		note
		obsolete
			"Obsolete message"
		require
		local
		attribute
			-- Implementation
		ensure
		rescue
		end

	final_feature
		do
		end

end
