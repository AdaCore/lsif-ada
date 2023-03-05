
all:
	gprbuild -j0 -P gnat/lsif.gpr -XVSS_LIBRARY_TYPE=static

run:
	./bin/lsif-ada gnat/lsif.gpr
