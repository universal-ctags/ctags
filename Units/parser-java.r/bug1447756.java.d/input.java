// Java 1.5 generic interfaces appear to be tagged on the
// parameterized type name instead of the interface name,
// e.g.,

	      public interface Foo<T> {
		      public T bar();
	      }

// is tagged on 'T' instead of 'Foo'.
