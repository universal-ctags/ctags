define one_one::two($ensure) {
    file { "/tmp/12": ensure => $ensure }
}

define three::four_four($ensure) {
    file { "/tmp/34": ensure => $ensure }
}

