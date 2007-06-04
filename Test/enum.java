public enum e {
	A,    // should be 'e', not 'f'
	B(),  // should be 'e', not 'm'
	C(1), // should be 'e', not missing
	D,    // should be 'e', not 'f'
	E(),  // should be 'e', not 'm'
	F(1), // should be 'e', not missing
	;

	public String string;
	public final Shape shape;
	public final boolean twoKeywordsInARow;

	public String getString() {
		return string;
	}
	public final Shape getShape() {
		return shape;
	}
}
