class one {
    file { "/tmp/multipleclassone": content => "one" }
}

class two {
    file { "/tmp/multipleclasstwo": content => "two" }
}

include one
