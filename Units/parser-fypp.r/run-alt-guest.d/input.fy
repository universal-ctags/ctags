#! This test case is derived from #1810, a pull request submitted by @p-vitt
#! and parser-java.r/enum.java.d/input.java
#! Callable needs only string argument
#:def debug_code(code)
  #:if DEBUG > 0
    $:code
  #:endif
#:enddef debug_code
public enum e {
	A,    // should be 'e', not 'f'
	B(),  // should be 'e', not 'm'
	C(1), // should be 'e', not missing

#! Pass code block as first positional argument
#:call debug_code
  if (size(array) > 100) then
    print *, "DEBUG: spuriously large array"x
  end if
#:endcall debug_code
	D,    // should be 'e', not 'f'
	E(),  // should be 'e', not 'm'
	F(1), // should be 'e', not missing
	;
#! Callable needs also non-string argument types
#:def repeat_code(code, repeat)
  #:for ind in range(repeat)
    $:code
  #:endfor
#:enddef repeat_code

        e(int x) {}
        e() {}

	public String string;
	public final Shape shape;
	public final boolean twoKeywordsInARow;

#! fypp preprocessor comments  here, and
	public String getString() {
		return string;
	}

#! there
	public final Shape getShape() {
		return shape;
	}
#! Pass code block as positional argument and 3 as keyword argument "repeat"
#:call repeat_code(repeat=3)
this will be repeated 3 times
#:endcall repeat_code
}
