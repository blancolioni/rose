ROOT=../../../..
include $(ROOT)/mk/common.mk

IDL=caps-cap storage-block_array files-file_system files-directory \
    files-file common-transactional files-transactional_writable_file \
    files-writable_directory files-writable_file users-user \
    common-resource streams-releasable_reader \
    streams-releasable_writer streams-seekable_reader streams-reader \
    streams-writer streams-stream streams-seekable storage-store \
    checkpoints-storage_checkpoint storage-dependent storage-bank \
    storage-process_banks checkpoints-state devices-ata \
    storage-partitions storage-partition_setup \
    devices-interrupt_handler processes-launch processes-exec \
    storage-region processes-executable processes-memory \
    checkpoints-memory_restore processes-process processes-segment \
    processes-server time-timeout time-timer caps-constructor \
    caps-cap_set caps-releasable_cap_set screens-terminal \
    devices-event_source devices-event_listener commands-lookup \
    commands-map processes-heap caps-receiver common-named \
    processes-kernel_process checkpoints-kernel_log \
    checkpoints-log_reader commands-command_endpoint \
    processes-restorable processes-signalled streams-output_redirect \
    users-auth users-auth_admin checkpoints-checkpointer time-clock \
    time-clock_admin users-rights users-rights_admin screens-surface \
    processes-replacer processes-replacement processes-saveable \
    processes-replaceable

# Declarations for commands (ADR 0002): the roles they inherit, and the
# package of the core commands. They are checked with the interfaces, but
# nothing is generated for them here; each command generates its own units.

COMMANDS=rose-core streams-standard_input streams-standard_output \
    streams-standard_error streams-standard_io caps-create_endpoint \
    caps-delete_cap caps-rescind_cap commands-find_command \
    screens-console time-timer_service time-clock_service \
    time-clock_admin_service users-rights_service \
    users-rights_admin_service storage-storage_service \
    caps-create_signal screens-screen_service caps-cap_set_service \
    processes-replace_process

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
