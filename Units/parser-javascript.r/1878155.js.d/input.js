// Tags should include:
//   functions
//       my_function
//   classes
//       RE
//   global variables
//       foo
//

// This should handle the escaped quote
var RE={"bar":/foo\"/};

// This should also handle the escaped quote
// A tag should be created for the global variable "foo".
var foo="foo \" some other stuff";

// A tag should be created for this function "my_function".
function my_function() {
}
