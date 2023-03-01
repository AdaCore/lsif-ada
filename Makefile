
all:
	gprbuild -P gnat/lsif.gpr

run:
	.objs/lsif-ada gnat/lsif.gpr
