IDL=file_system directory file stream_reader stream_writer stream

all: $(IDL)

clean:
	rm -f *.ad[bs]

file_system: rose-interfaces-file_system.ads

directory: rose-interfaces-directory.ads

file: rose-interfaces-file.ads

stream_reader: rose-interfaces-stream_reader.ads

stream_writer: rose-interfaces-stream_writer.ads

stream: rose-interfaces-stream.ads

rose-interfaces-file_system.ads: ../../../../interfaces/file_system.idl
	idl ../../../../interfaces/file_system.idl

rose-interfaces-directory.ads: ../../../../interfaces/directory.idl
	idl ../../../../interfaces/directory.idl

rose-interfaces-file.ads: ../../../../interfaces/file.idl
	idl ../../../../interfaces/file.idl

rose-interfaces-stream_reader.ads: ../../../../interfaces/stream_reader.idl
	idl ../../../../interfaces/stream_reader.idl

rose-interfaces-stream_writer.ads: ../../../../interfaces/stream_writer.idl
	idl ../../../../interfaces/stream_writer.idl

rose-interfaces-stream.ads: ../../../../interfaces/stream.idl
	idl ../../../../interfaces/stream.idl
