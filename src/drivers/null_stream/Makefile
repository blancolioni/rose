ROOT=../../..
include $(ROOT)/mk/common.mk

all: null_stream
	cp ./build/bin/null_stream ../../../build/$(ARCH)/drivers/null_stream

# null_stream is built with the userland run-time system, and serves the
# null stream through the user-mode IDL target (ADR 0035, section 4). It
# is a command, Rose.Core.Null_Stream, which init launches once, for the
# system directory: the spec of the command, its main and its metadata,
# the package Rose.Core, and the interfaces it uses, with their parents,
# are generated here.

null_stream:
	mkdir -p build/obj
	mkdir -p build/bin
	mkdir -p build/generated
	$(IDLE) --target user -I $(INTERFACES) -o build/generated \
	    rose-core-null_stream.idl \
	    $(INTERFACES)/rose-core.idl \
	    $(INTERFACES)/streams-stream.idl \
	    $(INTERFACES)/streams-reader.idl \
	    $(INTERFACES)/streams-writer.idl \
	    $(INTERFACES)/files-writable_directory.idl \
	    $(INTERFACES)/files-fixed_directory.idl \
	    $(INTERFACES)/files-directory.idl \
	    $(INTERFACES)/common-tree.idl \
	    $(INTERFACES)/files-file.idl \
	    $(INTERFACES)/streams-readable.idl \
	    $(INTERFACES)/files-writable_file.idl \
	    $(INTERFACES)/files-transactional_writable_file.idl \
	    $(INTERFACES)/common-transactional.idl \
	    $(INTERFACES)/streams-releasable_reader.idl \
	    $(INTERFACES)/streams-releasable_writer.idl \
	    $(INTERFACES)/streams-seekable_reader.idl \
	    $(INTERFACES)/streams-seekable.idl \
	    $(INTERFACES)/common-resource.idl
	gprbuild $(GPRBUILD_CARGS)
