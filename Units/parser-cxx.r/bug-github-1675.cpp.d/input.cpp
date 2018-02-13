// issue #1675 opened by bfrg on 01/02/2018
//
// Structure declarations that contain macro functions are not parsed correctly.

// structure longlong4 is completely ignored
struct __device_builtin__ __builtin_align__(16) longlong4
{
    // x,y,z,w will be parsed as (global) variables and not members
    long long int x, y, z, w;
};

// this one is parsed correctly
struct __device_builtin__ longlong3
{
    long long int x, y, z;
};
