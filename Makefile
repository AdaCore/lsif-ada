
all:
	gprbuild -j0 -P gnat/lsif.gpr -XVSS_LIBRARY_TYPE=static

run:
	.objs/lsif-ada gnat/lsif.gpr
