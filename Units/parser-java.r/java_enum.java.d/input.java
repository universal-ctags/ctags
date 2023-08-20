public class C {
	public enum TrivialEnum {
		FIRST_VALUE,
		SECOND_VALUE
	}
	public enum FancyEnum {
		A(1), B(2);
		private int i;
		FancyEnum(int i) {
			this.i = i;
		}
		void m() {
		}
	}
}

public enum StringEnum {

	X("X"),
	Y("Y"),
	Z("Z");

	private StringEnum(String _s) {
		s = _s;
	}

	private final String s;
}
