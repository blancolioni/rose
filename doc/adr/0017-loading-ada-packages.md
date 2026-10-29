# 17. Loading Ada packages as separate objects instead of linking programs

- **Status:** Proposed
- **Date:** 2026-10-29
- **Related:** [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands), [ADR 0008](0008-install-boot-and-pager-path.md) (the pager path), #364 (this decision), #200 (mem's page cache), #276 (instance tables), #170 (exception propagation), #357 to #363 (the userland run-time system), #362 (tasks)

## Context

Every Rose program is linked statically. `gprbuild` links the program's units with the userland run-time system (`--RTS=rts/build/i686-rose`), `Rose.Server` and the generated interface packages, into one ELF executable. `Exec.Install` stores that image in a region, and `elf` maps its `PT_LOAD` segments into a new process.

The run-time system is growing. Tagged types, allocators, finalization, streams and containers have all landed (#357 to #361), and every program that uses any of it carries its own copy.

### What an image holds

Sizes from the current build, in bytes:

| Program | File | Code | Data | Zero data (bss) | Debug info |
|---|---|---|---|---|---|
| `cat` | 1,027,792 | 210,272 | 1,424 | 41,261 | 623,501 |
| `timer` | 174,948 | 41,753 | 176 | 72,496 | not measured |
| `run_self_tests` | 6,070,752 | 1,886,936 | 2,473 | 64,133 | 3,333,234 |

- **Most of the file isn't loaded.** In `cat`, the debug sections are about 600 KB of a 1 MB file, and the symbol and string tables are most of the rest. The loadable part is about 250 KB. Debug information is not what this ADR is about, and it isn't what loading packages would fix (see Alternatives).
- **A large part of the code is run-time system.** By the names of its sections, about 100 KB of `cat`'s 210 KB of code is `Ada.*` and `System.*` units, and another 20 KB or so is `Rose.*`. These are in every program, and the figures are approximate.
- **Nothing is shared between programs.** mem caches read-only pages and shares them between processes (#200), but it keys the cache by region and page. Two programs have two image regions, so the same run-time system code is read, cached and held in memory once for each.
- **Zero data is paid for by every server package.** `timer` is 72 KB of zero data in a 114 KB image, because each generated server package declares an instance table (#276). That is a separate problem with its own fix, and a shared package would still have one table for each process that uses it.
- **The link isn't free.** The current link doesn't discard unreferenced sections. A change to the run-time system, to `Rose.Server` or to an interface needs every program that uses it to be relinked and installed again.

### Constraints

- **Ada has no link errors.** A consistent set of compiled units can't have a missing symbol, apart from `pragma Import` and the other foreign units. The binder's checks, which compare the spec checksums in the `.ali` files, are what make a set consistent.
- **Persistent processes run for ever.** A restored process continues with the code and the data layout it was checkpointed with. A new body can't replace the old one under a process that is running it.
- **A process has at most 8 segments.** `Max_Segments` in `mem-processes.adb` is 8, and a program uses about six: its code, read-only data and data, the stack and the heap (`elf-loader.adb`). A segment per package is not possible.
- **i386.** A call to another unit is a relative `call`. Position-independent code takes EBX in every function, so it costs more here than on most targets. User space is about 3.5 GB, and the heap already starts at 16#4000_0000#.
- **The pager path can't use it.** The boot modules and the pager path (ADR 0008) start before anything can map a package.

### Precedent

The Aqua VM in aquarius links one object file for each package, on first reference (`aqua_vm/docs/dynamic-linking.md`). It plants a trap in each call site that refers to a package that isn't loaded, and rewrites the call when the package arrives. Its x86 notes (`dynamic-linking-x86.md`) say what changes for real hardware: the call is better sent through a pointer in a data page, because rewriting code makes the code page private to its process.

## Decision

### 1. The unit of loading is the compilation unit

- **A package version is a unit and the checksum of its spec,** from its `.ali` file. A dependent is compiled against a version.
- **A body is a build of a version,** named by a digest of its object file. Two bodies of one version can replace each other, subject to section 2.
- **The `.ali` files are the dependency information.** There is no new format for it.
- **Installing a program installs the bodies it needs that aren't there already.** The install image holds object files, and no link step runs on the target.

### 2. Each body is placed once, at an address that is the same in every process

- **The installer places a body.** It gives the body three ranges from a system-wide allocator, one each for code, read-only data and data, and relocates the object once for those addresses. The result is stored in regions: code and read-only data to be shared, and data as a template.
- **Pages are shared because the addresses are.** A relocated page is the same in every process only if the address is the same in every process. The cache in mem (#200) then shares a package's code between every program that uses it, with no change to mem.
- **A body's code refers only to itself.** A call to another package goes to a stub in the body's own image. The stub jumps through a slot in the body's own data, and the slot holds the address of the called subprogram. Code is never rewritten.
- **The slots are filled when the program is launched,** with the addresses of the bodies that `exec` picked (section 4). Aquarius's call-site patch becomes a pointer slot, as in the precedent.
- **A reference that isn't a call is fixed when the body is installed.** This covers another package's variables, constant data, dispatch tables, exception data and elaboration flags, and it ties the body to the body that it was installed against. A package that is referred to in this way can't be replaced without installing its dependents again (see the open questions).
- **The run-time system is not special.** `Ada.*`, `System.*`, `Rose.Server` and the generated interface packages are packages like any other. The compiler's helpers (`memcpy`, `__udivdi3`, `__gnat_*`) and the C and assembly in `src/rts/support` are foreign units (section 8).

### 3. Windows, not segments

A process has 8 segments (see the constraints). The library is mapped as a few windows instead:

- **Three windows of the address space:** library code, library read-only data, and library data. Each is one segment, backed by one library region that holds every placed body at the addresses that it was given. The code and read-only data windows are shared. The data window is private, and each page is copied from the template when it is first written.
- **The program's own image is a package image.** It holds the main unit, the binder's file for the program, and `start.o`. It is mapped as it is now, so the program's own units stay out of the library.
- **Pages are brought in as they are touched.** Mapping a window doesn't read it. A process pays for the pages of the packages that it calls.
- **The cost is that a window maps every installed package.** A process can read the code of a package that it doesn't use. Which packages it can call is fixed by its slots, and the code of an open-source system isn't secret, but section 7 notes what that does to access control.

### 4. Linking when a program is launched

This is the first stage. `exec` does the work, and the process starts as before.

1. **The program has a manifest,** made when it is installed from the `.ali` files: the units in its closure, in the order the binder gave them, each with its spec checksum.
2. **`exec` checks the manifest against what is installed.** Each unit must have a body installed for the version that the program was built against. A missing body or a mismatched checksum fails the launch, before anything runs. This is the binder's consistency check, done at launch.
3. **`elf` maps the three windows** and the program's image, and fills the slots.
4. **Elaboration runs in the binder's order,** from the program's own binder file, before `main`. A package isn't elaborated on its first call, which isn't Ada's semantics.

### 5. Loading on first reference is deferred

The slot is what lets a call start at a resolver and then point at the subprogram, and the design keeps it. The resolver isn't built in the first stage:

- **Demand paging already does most of it.** A page of a package that isn't called isn't read (section 3), so loading on first call saves the mapping and little else.
- **Elaboration needs the closure anyway** (section 4). Packages have to be there, in order, before `main`.

It can be added later without changing the code of any package, if measurement shows a need.

### 6. Versions, and persistence

- **A process keeps the bodies it was started with.** The bodies are part of its state, and a restored process gets the same ones back. A body's data layout belongs to its version, so a process can't be moved to new code.
- **A new body under the same spec is installed beside the old one.** New launches get the newer body, if section 2's condition allows it. Running processes keep the old one.
- **A new spec is a new version.** Every package that depends on it is a new body to build and install.
- **Bodies are counted.** `exec` counts the processes that use each body, and an old body is removed when its count is zero and a newer one is installed. A persistent process that never ends keeps its bodies.
- **A package that was inlined or instantiated elsewhere is a dependency.** The `.ali` files say where, and a new body of such a unit gives new bodies to the units that took it in.
- **Generics are not a special case.** GNAT expands each instantiation into the unit that instantiates it, so one unit is still one object. Fixing a generic's body means building its instantiators again.

### 7. Who may load what

- **Only `exec` and `elf` map the windows.** The windows' regions are caps that they hold, and a program doesn't get them.
- **There is no `dlopen`.** A program's closure is fixed when it is installed, and it can't load a unit that isn't in it.
- **A finer rule needs a finer mapping.** A cap for each package needs a segment for each package, which the limit of 8 doesn't allow. See the open questions.

### 8. What stays as it is

- **The boot modules and the pager path** (ADR 0008) stay statically linked, and so does `elf`, since it does the mapping. So does anything that is launched before `exec` can launch.
- **Foreign units are placed too.** An object from `src/rts/support`, or from a `pragma Import`, has a manifest that names its exports and what it needs. A name that nothing exports is an error when the program is installed, not when it runs.
- **A symbol index is made when a body is installed,** from the `.ali` files and the object's symbol table. GNAT's names (`p__q__f`, the `__2` suffixes for overloads, shortened file names) can't be mapped back to units by parsing them.

### 9. Tools get easier

Every body has one address range for the whole system, so a program counter in the library is a unit and a version in every process. `addr2line`, panic tracing and gdb need one table, not the load address of each package in each process.

## Consequences

- **Code is shared.** The run-time system is in memory once, and in the store once, however many programs use it.
- **A program is small,** because it holds only its own units, its binder's file and its manifest. Installing it places the packages that aren't installed yet, and doesn't copy the ones that are.
- **The loadable image of `cat` shrinks by about the size of its run-time system** (roughly 100 KB of 253 KB, to be measured), and the file shrinks by that much. The debug information doesn't change.
- **Fixing a body is replacing one file,** when no dependent refers to it by anything but a call. The next launch uses it.
- **Every cross-package call costs a stub and an indirect jump.** The size of that cost is the first thing to measure.
- **Address space is allocated for the whole system.** Every body keeps its range for as long as the body is installed. Today's installed code is a few megabytes, against several hundred megabytes of window.
- **An unlucky replacement affects every program that uses the package.** A bad body is bad everywhere, until it is replaced.
- **Elaboration is as it is now:** the whole closure, in the binder's order, before `main`.

## Alternatives considered

- **Only trim the static images.** Link with `--gc-sections` and keep the debug information out of the installed file. Neither is done today. They are worth doing whatever else is decided, because they deal with what each image holds, and this ADR deals with what the images repeat. The debug information is most of a program's file, and unreferenced code is some of its code. Neither fixes the duplication between programs, the instance tables, or the relinking. I haven't measured what `--gc-sections` saves.
- **One shared image for the run-time system,** placed at a fixed address, and programs linked against it. This fits in three more segments and needs no stubs, because the program can call straight into it. It gets most of the sharing for a much smaller change. It keeps what static linking has: a change to the run-time system needs every program relinked, and there is no version of a unit. It is the first thing to try if the cost in section 2 is too high, or the windows in section 3 can't be built.
- **Position-independent objects, relocated for each process.** On i386 this costs a register in every function, and each process's relocated pages are its own, so nothing is shared. This is the case that sharing is meant to avoid.
- **Rewriting call sites on first call,** as the Aqua VM does. It makes each code page private to its process, and it needs the code pages to be writable for a moment. The slots do the same job without touching code.
- **In the kernel.** Rose's kernel holds mechanism, and a loader that reads `.ali` files and builds symbol indexes isn't that.

## Open questions

- **Segments.** The limit of 8 is `Max_Segments` in `mem-processes.adb`. The windows work within it. The alternative is a table that grows, with a segment for each package, which allows a cap for each package at the cost of memory in mem for each process.
- **Sparse regions.** A library region is mostly holes. Can store hold one, and does it allocate on first write (#305)?
- **Where the windows go.** The program is at its linker's address and the heap at 16#4000_0000#. The layout has to leave room for both, and for the stack.
- **What may be replaced.** Section 2 says a reference that isn't a call ties a body to the one it was installed against. Is that the rule, or can the installer tell from the `.ali` files which references move?
- **Which body a launch gets.** The newest installed body of a version, or the one the program was installed against, or something that a user can choose.
- **Exception propagation.** There is one unwind table for each program today (`__eh_frame_hdr_start` in `linker.ld`, #170). The unwinder has to find the table of a package that it unwinds through, which GNU systems do with `dl_iterate_phdr`. Rose needs its own way, probably a table for the whole library.
- **Tasks.** How the slots and the elaboration fit with the run-time system's tasks (#362).
- **The per-process instance tables** (#276) are in each package's data. A shared package's table is one copy for each process, and zero data costs only the pages that are touched, but that has to be checked.
- **What a process sees at a restore.** A restored process keeps its bodies. The windows are mapped again from the library regions, and a body that was removed after the checkpoint has to be kept for it.
- **The measurements.** How much sharing buys across the installed programs, what a stub costs in a call-heavy program, and what `--gc-sections` saves on its own.

## Follow-up work

- Link with `--gc-sections`, and keep debug information out of installed images. Measure both, separately from this ADR.
- Measure the sharing that this would give across the programs that are installed.
- A prototype: place one package and its dependents, with stubs and slots, and run a program that uses it.
- A manifest for installed programs, made from the `.ali` files.
- The windows in `elf`, and the slots filled at launch.
- A way for the unwinder to find a package's tables.
- The symbol index, and address-to-unit lookup for the tools.
- Foreign units, with manifests for the support library.
