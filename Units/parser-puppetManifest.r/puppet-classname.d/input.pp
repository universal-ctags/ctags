class t {
    file { "/tmp/multipleclasst": content => "one" }
}

class one_ {
    file { "/tmp/multipleclassone_": content => "one" }
}

class on_e {
    file { "/tmp/multipleclasson_e": content => "one" }
}


class o_ne::tw_o {
    file { "/tmp/multipleclasso_netw_o": content => "two" }
}

class o_ne::two_ {
    file { "/tmp/multipleclasso_netwo_": content => "two" }
}

include t
include one_
include on_e
include o_ne::tw_o
include o_ne::two_
