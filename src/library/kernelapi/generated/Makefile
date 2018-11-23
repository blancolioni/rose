IDL=file_system directory file file_system stream_reader stream_writer stream seekable

all: $(IDL)

clean:
	rm -f *.ad[bs]

rose-interfaces-%.ads: ../../../../interfaces/%.idl
	idl $<

file_system: rose-interfaces-file_system.ads

directory: rose-interfaces-directory.ads

file: rose-interfaces-file.ads

stream_reader: rose-interfaces-stream_reader.ads

stream_writer: rose-interfaces-stream_writer.ads

stream: rose-interfaces-stream.ads

seekable: rose-interfaces-seekable.ads

