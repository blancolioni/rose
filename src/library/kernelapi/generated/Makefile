IDL=cap block_device file_system directory file \
    stream_reader stream_writer stream seekable storage state \
    ata partitions keyboard_handler interrupt_handler \
    launch exec region executable memory process segment server \
    timeout timer constructor cap_set terminal event_source event_listener \
    map heap receiver kernel_process kernel_log

IDLE ?= idle
INTERFACES=../../../../interfaces/interfaces.txt
SOURCES=$(IDL:%=../../../../interfaces/%.idl)

all: check $(IDL)

# Check the interfaces together before generating any of them: endpoint
# ids, file names, missing interfaces, and .idl files that this list
# leaves out.

check:
	$(IDLE) --check --endpoints $(INTERFACES) $(SOURCES)

clean:
	rm -f *.ad[bs]

rose-interfaces-%.ads: ../../../../interfaces/%.idl
	$(IDLE) --target kernel --endpoints $(INTERFACES) -o . $<

$(IDL): %: rose-interfaces-%.ads

.PHONY: all check clean
