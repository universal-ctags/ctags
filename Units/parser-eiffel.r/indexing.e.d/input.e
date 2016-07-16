indexing
	name1: "value"

class
	INDEXING_KEYWORD_TEST

feature
	feature1
			-- header comment
		indexing
		obsolete
			"Obsolete message"
		require
		local
		do
		end

	feature2: INTEGER

indexing
	name2: "value"
end
