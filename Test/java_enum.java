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
