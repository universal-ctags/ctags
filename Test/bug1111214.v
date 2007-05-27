// File example.v
//
// Below is an example of a comment that is mis-parsed by exuberant ctags.
// It uses the multi-line comment format, i.e. /* ... */ except that in
// this case, the character sequence immediately preceeding the closing
// delimiter is an asterisk. (Any even number of asterisks would have the
// same problem.
// The line immediately afterwards is used to demonstrate the problem.
// the module name 'wahoo' isn't recognised, because the parser mistakenly
// thinks we are still in a multi-line comment.
/*
 * I am a multi-line comment
 * I happen to end in a strange
 * (but legal) way: **/
module wahoo ()
begin
end
