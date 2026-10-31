ROOT=../../../..
include $(ROOT)/mk/common.mk

IDL=cap block_device file_system directory file \
    writable_directory writable_file user \
    resource file_reader seekable_file_reader \
    stream_reader stream_writer stream seekable storage storage_checkpoint \
    bank state \
    ata partitions keyboard_handler interrupt_handler \
    launch exec region executable memory memory_restore process segment \
    server \
    timeout timer constructor cap_set terminal event_source event_listener \
    commands map heap receiver named kernel_process kernel_log log_reader \
    command_endpoint restorable output_redirect auth auth_admin checkpointer \
    clock clock_admin rights rights_admin display surface

# Declarations for commands (ADR 0002): the roles they inherit, and the
# package of the core commands. They are checked with the interfaces, but
# nothing is generated for them here; each command generates its own units.

COMMANDS=rose-core standard_input standard_output standard_error standard_io \
    create_endpoint delete_cap rescind_cap find_command console timer_service clock_service \
    clock_admin_service rights_admin_service

SOURCES=$(IDL:%=../../../../interfaces/%.idl)
COMMAND_SOURCES=$(COMMANDS:%=../../../../interfaces/%.idl)

all: check $(IDL)

# Check the interfaces together before generating any of them: endpoint
# ids, file names, missing interfaces, and .idl files that this list
# leaves out. Endpoint ids are derived from the source, so there is no
# table of them.

check:
	$(IDLE) --check $(SOURCES) $(COMMAND_SOURCES)

clean:
	rm -f *.ad[bs]

# Every interface depends on all of them, since a parent can change.
rose-interfaces-%.ads: ../../../../interfaces/%.idl $(SOURCES)
	$(IDLE) --target kernel -o . $<

$(IDL): %: rose-interfaces-%.ads

.PHONY: all check clean
