struct Bar {
    f int = 99
    g Foo = Foo{66}
    h string = "hi"
    i int @[required]
    j int @[required] = 99
    k string @[json: 'k'] = "j"
}
