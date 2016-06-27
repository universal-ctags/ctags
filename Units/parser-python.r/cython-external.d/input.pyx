
cdef extern void order_spam1(int tons)

# we ignore those ATM
cdef extern from "spam.h":
    void order_spam2(int tons)

cdef import from "spam.h":
    void order_spam3(int tons)
