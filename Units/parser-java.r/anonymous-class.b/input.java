// See #1739
public class input {
    interface greeting {
	public void greet(String word);
    }

    public void hello () {
	greeting g = new greeting() {
		public void greet (String word) {
		}
	};
	g.greet("hello");
    }
    public void bye () {
	greeting h = new greeting() {
		public void greet (String word) {
		}
	};
	h.greet("bye");
    }

}
