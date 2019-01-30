class base {
   file { "/tmp/classheir1": ensure => file, mode => '0755' }
}

include base

