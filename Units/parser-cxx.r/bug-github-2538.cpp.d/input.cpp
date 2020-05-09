// Recursive macro expansion causes a segmentation fault
// M here is expected to be a macro defined from outside.

int f(int v)
{
    return v & M;
}
