package unions

Value :: union {
    bool,
    i32,
    f32,
    string,
}

Non_Nil :: union #no_nil {
    int,
    string,
}

Result :: union($T: typeid) {
    T,
    string,
}
