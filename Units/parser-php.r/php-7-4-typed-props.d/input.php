<?php
// Derived from https://www.php.net/manual/en/migration74.new-features.php
class Person
{
        public int $age;
}

class User extends Person
{
    public int $id;
    public ?string $name;
    public self $next;
    public parent $child;
    public function __construct($id, $name) {
        $this->id = $id;
        $this->name = $name;
        $this->next = $this;
        $this->child = $this;
    }
}
$u = new User (0, "A");
echo $u->next->name;
?>
