all: idl

idl: idl.gpr src/*.ad[bs]
	gprbuild
	cp build/bin/idl-main $(HOME)/bin/idl
	(cd ../../library/kernelapi/generated ; make clean ; make)

idl.gpr: idl.alix
	alix configure
