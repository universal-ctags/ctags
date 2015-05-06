class ForEachTest {
	static void Main(string[] args) {
		int[] fibarray = new int[] { 0, 1, 2, 3, 5, 8, 13 };
		foreach (int i in fibarray) {
			System.Console.WriteLine(i);
		}
	}
}
