// Derived from #3948 submitted by @cpardotortosa
class a {
    static { class b {} }
    fld0;
    static { class c {} }
    static fld1;
    static { class d {} }
    static fld2 = 1;
}
a.b
