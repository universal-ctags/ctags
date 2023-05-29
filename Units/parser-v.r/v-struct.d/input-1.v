struct Bar {
    f int = 99
    g Foo = Foo{66}
    h string = "hi"
    i int @[required]
    j int = 99 @[required]
    k string = "j" @[json: 'k']
}
