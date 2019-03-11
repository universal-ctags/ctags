// Taken from "Example #7 Nullable return type declaration (as of PHP 7.1.0)"
// in http://php.net/manual/en/functions.returning-values.php

<?php
function get_item(): ?string {
    if (isset($_GET['item'])) {
        return $_GET['item'];
    } else {
        return null;
    }
}
?>
