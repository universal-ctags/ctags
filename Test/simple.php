// Examples from PHP Language Reference at
//   http://www.php.net/manual/en/langref.php

<?php
$var = "Bob";
$Var = "Joe";
echo "$var, $Var";      // outputs "Bob, Joe"

$4site = 'not yet';     // invalid; starts with a number
$_4site = 'not yet';    // valid; starts with an underscore
$täyte = 'mansikka';    // valid; 'ä' is ASCII 228.
?>

<?php
define("CONSTANT", "Hello world.");
echo CONSTANT; // outputs "Hello world."
echo Constant; // outputs "Constant" and issues a notice.
?>

function foo ($arg_1, $arg_2, ..., $arg_n)
{
    echo "Example function.\n";
    return $retval;
}

class Cart
{
    var $items;  // Items in our shopping cart
   
    // Add $num articles of $artnr to the cart
 
    function add_item ($artnr, $num)
    {
        $this->items[$artnr] += $num;
    }
   
    // Take $num articles of $artnr out of the cart
 
    function remove_item ($artnr, $num)
    {
        if ($this->items[$artnr] > $num) {
            $this->items[$artnr] -= $num;
            return true;
        } else {
            return false;
        }   
    }
}
