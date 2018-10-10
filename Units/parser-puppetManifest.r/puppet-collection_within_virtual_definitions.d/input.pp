define test($my_name) {
    file {"/tmp/collection_within_virtual_definitions1_$my_name.txt":
        content => "File name $my_name\n"
    }
    Test2 <||>
}

define test2() {
    file {"/tmp/collection_within_virtual_definitions2_$name.txt":
        content => "This is a test\n"
    }
}

node default {
    @test {"foo":
        my_name => "foo"
    }
    @test2 {"foo2": }
    Test <||>
}
