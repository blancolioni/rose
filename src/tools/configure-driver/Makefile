all: configure-driver

configure-driver: configure.gpr src/*.ad[bs]
	gprbuild
	cp build/bin/configure-driver $(HOME)/bin/configure-driver

configure.gpr: configure-driver.alix
	alix configure
