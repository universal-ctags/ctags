/*
 * "Lowercase "object" isn't a keyword"
 * 
 * ctags -f - bug3036476.js should output:
 * 
 * variables:
 *    container
 * 
 * functions:
 *    container.object
 *    container.object.method1
 *    container.object.method2
 */

var container = {};

container.object = function() {}
container.object.method1 = function() {}
container.object.method2 = function() {}

