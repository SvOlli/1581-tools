
all: 1581-tools.prg

1581-tools.prg: 1581-tools.s
	cl65 -t c64 -C c64-asm.cfg -o $@ $^

