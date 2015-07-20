function test () {
	var one = {
		val: 1
	};

	var two = {
		val: 2,
		/* the trailing comma used to break recognizing as a class, and as local
		 * variables are not emitted `val` was orphaned */
	};
}
