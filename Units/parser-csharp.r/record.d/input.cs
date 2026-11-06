namespace Demo
{
	public class MyClass {
		public int Field;
		public void Method() {}
	}

	public record MyRecord {
		public int Field;
		public void Method() {}
	}

	public record Person(string First, string Last);

	public abstract record Shape : IShape {
		public int Id;
	}

	public sealed record Point(int X, int Y) : Shape
	{
		public double Length() { return 0; }
	}

	public record Empty;

	public class Holder {
		private Record record;
		public Record Other { get; set; }
	}
}
