enum chip {
	A6XX = 6,
	A7XX = 7,
};

template<chip CHIP>  int CMD_REGS[] = {};
template<>           int CMD_REGS<A6XX>[] = {
	0x3,
};

template<>           int CMD_REGS<A7XX>[][1] = {
  {0x4},
};

int main(void)
{
  return CMD_REGS<A6XX>[0];
}
