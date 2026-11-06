ROOT=../../..
include $(ROOT)/mk/common.mk

all: kilo
	cp ./build/bin/kilo-driver ../../../build/$(ARCH)/drivers/kilo

# The spec of Rose.Core.Editors.Kilo, its main and its metadata, the
# packages Rose.Core and Rose.Core.Editors, the console's Terminal, and
# Transactional_Writable_File with the interfaces they use, are generated
# from IDL (ADR 0002) for the user target: kilo is built with the userland
# run-time system.

kilo:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-editors-kilo.idl $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/rose-core-editors.idl \
	    $(INTERFACES)/terminal.idl \
	    $(INTERFACES)/stream.idl \
	    $(INTERFACES)/transactional_writable_file.idl \
	    $(INTERFACES)/writable_file.idl \
	    $(INTERFACES)/transactional.idl \
	    $(INTERFACES)/file.idl \
	    $(INTERFACES)/releasable_reader.idl \
	    $(INTERFACES)/releasable_writer.idl \
	    $(INTERFACES)/seekable_reader.idl \
	    $(INTERFACES)/reader.idl \
	    $(INTERFACES)/writer.idl \
	    $(INTERFACES)/resource.idl \
	    $(INTERFACES)/seekable.idl
	gprbuild $(GPRBUILD_CARGS)
