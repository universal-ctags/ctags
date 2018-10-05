
define t {
    file { "/tmp/multipledefinet": content => "one" }
}

define one_ {
    file { "/tmp/multipledefineone_": content => "one" }
}

define on_e {
    file { "/tmp/multipledefineon_e": content => "one" }
}


define o_ne::tw_o {
    file { "/tmp/multipledefineo_netw_o": content => "two" }
}

define o_ne::two_ {
    file { "/tmp/multipledefineo_netwo_": content => "two" }
}

t { 't': }
one_ { 'one_': }
on_e { 'on_e': }
o_ne::tw_o { 'o_ne::tw_o': }
o_ne::two_ { 'o_ne::two_': }
