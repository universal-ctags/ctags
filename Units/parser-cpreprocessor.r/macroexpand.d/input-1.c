/* This code could trigger an infinite loop. */
#define IO7__POX_ERRSUM__UPE_ERROR        GEN_MASK(IO7__POX_ERRSUM__UPE_ERROR)
int
f(int err_sum)
{
  if (err_sum & IO7__POX_ERRSUM__UPE_ERROR) {
    return 0;
  }
}
