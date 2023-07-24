/* Taken from https://en.cppreference.com/w/c/language/_Alignas */
#include <stdalign.h>
#include <stdio.h>

// every object of type struct sse_t will be aligned to 16-byte boundary
// (note: needs support for DR 444)
struct sse_t
{
    alignas(16) float sse_data[4];
};

// every object of type struct data will be aligned to 128-byte boundary
struct data
{
    char x;
    alignas(128) char cacheline[128]; // over-aligned array of char,
                                      // not array of over-aligned chars
};

int main(void)
{
    printf("sizeof(data) = %zu (1 byte + 127 bytes padding + 128-byte array)\n",
           sizeof(struct data));

    printf("alignment of sse_t is %zu\n", alignof(struct sse_t));

    alignas(2048) struct data d; // this instance of data is aligned even stricter
    (void)d; // suppresses "maybe unused" warning
}
