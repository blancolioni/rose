ROOT=../../..
include $(ROOT)/mk/common.mk

# The interface packages of the host backings (#1101), and of the
# interfaces they name, for the host target (idle#133): each package has
# only its object type, since a host program has no caps.

HOST_INTERFACES= \
	common-resource common-transactional common-tree \
	files-directory files-fixed_directory files-writable_directory \
	files-file files-writable_file files-transactional_writable_file \
	streams-readable streams-reader streams-writer \
	streams-releasable_reader streams-releasable_writer \
	streams-seekable streams-seekable_reader streams-stream \
	time-clock

all:
	mkdir -p build/generated build/obj
	$(IDLE) --target host -I $(INTERFACES) -o build/generated \
	    $(HOST_INTERFACES:%=$(INTERFACES)/%.idl)

clean:
	rm -rf build
