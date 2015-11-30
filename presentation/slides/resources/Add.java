public class Add {
    public int add_reference(int x, int y) {
	return x + y;
    }

    public int add_with_bug(int x, int y) {
	if (x == 7) return 42;
	return x + y;
    }
}
