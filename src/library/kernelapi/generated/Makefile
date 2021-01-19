IDL=cap block_device file_system directory file file_system \
    stream_reader stream_writer stream seekable storage \
    ata partitions keyboard_handler interrupt_handler \
    launch exec region executable memory process segment server \
    timeout timer constructor cap_set terminal event_source event_listener \
    map heap receiver kernel_process

INTERFACES=../../../../interfaces/interfaces.txt
all: $(IDL)

clean:
	rm -f *.ad[bs]

rose-interfaces-%.ads: ../../../../interfaces/%.idl
	idl $< $(INTERFACES)

$(IDL): %: rose-interfaces-%.ads
