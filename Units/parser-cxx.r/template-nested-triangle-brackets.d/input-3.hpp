// Taken from the issue #2433 submitted by @andrejlevkovitch

template <typename T> struct TestStruct {};

template <typename T> struct TemplateParameterStruct {};

struct ParameterStruct {};

template <> struct TestStruct<ParameterStruct> {};

template <typename P> struct TestStruct<TemplateParameterStruct<P>> {};
