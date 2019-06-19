IDL=cap block_device file_system directory file file_system \
    stream_reader stream_writer stream seekable storage \
    ata partitions keyboard_handler interrupt_handler installer \
    launch exec region executable memory process process_memory \
    timeout timer constructor cap_set

all: $(IDL)

clean:
	rm -f *.ad[bs]

rose-interfaces-%.ads: ../../../../interfaces/%.idl
	idl $<

$(IDL): %: rose-interfaces-%.ads
