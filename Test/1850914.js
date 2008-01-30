// This file should generate the following tags:
//
//   methods
//     objectLiteral.objLiteralMethod
//   properties
//     objectLiteral.objLiteralProperty


/**
* This is an object literal
*/
var objectLiteral = {
/**
* This is a literal object property
*/
objLiteralProperty : 1,
/**
* This is a literal object method
*/
objLiteralMethod : function(){}
}

// When I run ctags on this (ctags -f - test.js) I get no output. I expect it
// to give me something for both "objectLiteral" and "objLiteralMethod".

