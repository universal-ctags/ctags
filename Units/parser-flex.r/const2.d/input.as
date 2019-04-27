// https://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/statements.html#const
const MIN_AGE:int = 21;

const product_array:Array = new Array("Studio", "Dreamweaver", "Flash", "ColdFusion", "Contribute", "Breeze"); 
product_array.push("Flex"); // array operations are allowed
product_array = ["Other"];  // assignment is an error
trace(product_array);
