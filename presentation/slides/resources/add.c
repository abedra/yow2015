int add_reference(int x, int y) {
  return x + y;
}

int add_with_bug(int x, int y) {
  if (x == 5) return 42;
  return x + y;
}
