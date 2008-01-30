
// This should handle the escaped quote
var RE={/foo\"/: "bar"};

// This should also handle the escaped quote
// A tag should be created for the global variable "foo".
var foo="foo \" some other stuff";

// A tag should be created for this function "my_function".
function my_function() {
}
