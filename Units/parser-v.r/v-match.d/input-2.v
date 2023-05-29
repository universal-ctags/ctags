fn (c Colour) kind() {
    return match c {
        .red, .green, .blue { "primary" }
        .yellow, .cyan, .magenta { "secondary" }
        else { "tertiary" }
    }
}
