
all:
	gprbuild -j0 -P gnat/lsif.gpr

run:
	.objs/lsif-ada gnat/lsif.gpr
