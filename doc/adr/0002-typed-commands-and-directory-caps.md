# 2. Typed commands, directory capabilities and typed streams

- **Status:** Proposed
- **Date:** 2026-10-23
- **Related:** [ADR 0001](0001-capability-revocation.md) (capability revocation), #118 (membrane library), #119 (petal runs commands inside a membrane), Petal Shell project

## Context

Rose is a persistent capability system. A process can only act through the capabilities it holds, and there is no global file namespace. This ADR sets out how the shell, commands and inter-process data should work with that model, rather than copying the Unix model of argv strings, a working directory and byte pipes.

### What exists today

- **Petal formal arguments.** Built-in commands declare formals that are named (long and short names) or positional, with defaults (`Petal.Formal_Arguments`). The argument types are Boolean, Integer, String, read stream and write stream.
- **External commands don't run yet.** Petal's `Execute` prints `launching:` with the command's launch cap and returns. External programs have no way to declare their own formals.
- **Launch is untyped.** `Launch.Launch` takes a `Capability_Array` and an `Environment` storage array. The caller and the program agree on what they mean by convention only.
- **Directory and file interfaces.** `Directory` offers lookup (`Find_Entry`, `Get_Ordinary_File`, `Get_Directory`, `Read_File`) and creation (`Create_File`, `Create_Directory`) on the same interface. `File` is `Stream` plus `Seekable`, and `Stream` is `Stream_Reader` plus `Stream_Writer`, which carry raw storage arrays.
- **The IDL compiler.** `src/tools/idl` compiles `interfaces/*.idl` into Ada client and server packages in `src/library/kernelapi/generated`. It is built with alix, the package manager that predates Alire. Its output is restricted to the subset of Ada available to kernel-level code, which has no tagged types or interfaces. So an IDL interface becomes a pair of packages, not an Ada interface type.

### Constraints

- **No global namespace.** Each process sees a set of unrelated capabilities. Directory caps exist independently of each other, and there is no root or parent to walk up to.
- **Persistence.** Processes and their caps survive restores, so anything granted to a long-running job stays granted until it's revoked.
- **Revocation (ADR 0001).** The kernel provides per-grant revocation for derived caps, and membranes provide transitive revocation in user space. Petal is the first membrane user (#119).
- **Single-threaded servers.** Servers run without tasking, so anything that sits between a command and its caps must not block other work.

## Decision

### 1. A command is a procedure with a typed signature

A program's arguments are declared in IDL (section 2), and its entry point is the Ada procedure generated from them: typed arguments with modes, binding by name or position, and default values. The signature is visible to the caller without running the program.

- **The shell does the binding.** Petal resolves each actual argument against the signature, applies defaults, and reports type errors, missing files or missing permissions before the program starts, naming the argument that failed. Help text and completion come from the same signature.
- **The signature is the sandbox manifest.** The program receives exactly the capabilities its arguments resolve to and nothing else. Anything else, such as network access or launching other programs, must appear in the signature as a parameter. There is no ambient authority to fall back on.
- **Modes map to capabilities:**

  | Parameter | The program receives |
  |---|---|
  | `in File` | A read-only file cap |
  | `out File` | A write cap on a new file (see section 4) |
  | `in out File` | A read-write cap on an existing file (see section 4) |
  | `in Directory` | A lookup-only directory cap for that subtree |
  | `in out Directory` | A directory cap that can also create and modify entries |
  | `out` of a non-file type | Nothing; the value is a typed result that the shell binds to a variable |

- **Parameter types are IDL interfaces.** `in File` means "any object that implements `File`", so a pipe, a synthesised object or a remote resource can be passed wherever a file is expected.
- **Typed results.** A command returns structured values through `out` parameters, such as `stat (path => "x", size => s, modified => m)`, instead of text that the caller has to parse.
- **Standard streams are supplied by the shell.** Every command receives standard input, standard output and standard error without declaring them, and uses them through `Ada.Text_IO` as now. They are not ambient authority: the shell grants them for each launch, and they cross the job's membrane like any other cap. Everything else must be in the signature.
- **List arguments.** A command's last positional argument may have a list type, such as `Rose.Command_Line.String_List`. It takes all remaining positional words, which can be none. At most one argument in a signature may do this. Named arguments and short options can appear anywhere among the words, so `echo -n Welcome to Rose` binds `New_Line => False` and `Arguments => ("Welcome", "to", "Rose")`.
- **Short options.** An argument can have a short option (`Short_Name`) and, for a `Boolean`, the value the option gives it (`Short_Value`). For example, `-n` sets `New_Line` to `False`. A `Boolean` argument can also be given by name as `--name` or `--no-name`.

### 2. Commands are declared in IDL, and the main program is generated

A command is declared once, in IDL, together with its help text: a `command ... is ... end` block with the command's aspects, and one declaration for each argument, like objects in an Ada package. The syntax is proposed in the IDL chapter of the book (#146) and in #137. The IDL compiler generates everything except the command's own logic.

- **The spec is generated.** For `command Echo`, the compiler writes `echo.ads`:

  ```ada
  with Rose.Command_Line;
  procedure Echo
    (New_Line  : Boolean := True;
     Arguments : Rose.Command_Line.String_List);
  ```

  The author writes only the body, `echo.adb`, in plain Ada with no argument parsing or capability handling. A body that doesn't match the IDL fails to compile, so the program and its signature can't drift apart.

- **The real entry point is a generated one-shot server.** The compiler also writes `echo_main.adb`, which is the program's `Main`:

  ```ada
  with Rose.Command_Line.Launch;
  with Echo;
  procedure Echo_Main is
     use Rose.Command_Line.Launch;
     Call : Launch_Call := Receive;   --  the one invocation the shell sends
  begin
     Echo (New_Line  => Boolean_Argument (Call, 1),
           Arguments => String_List_Argument (Call, 2));
     Complete (Call);                 --  success: the shell commits out files
  exception
     when E : others =>
        Fail (Call, E);               --  structured error: the shell discards outputs
  end Echo_Main;
  ```

  - **`Receive`** waits for the shell's single invocation. Arguments are encoded as for any IDL method call: scalars in words, strings and lists in the shared buffer, and files and directories as caps. Launching a command is calling a method, so decoding uses the same generated server-side code.
  - **The `*_Argument` functions** turn each argument into an Ada value. A cap becomes a client object of the argument's interface type, so an `in File` argument is an object implementing the Ada `File` interface (user-mode target, section 10).
  - **The shell applies defaults**, since it has the signature, so the main always receives every argument. The defaults in the generated spec only matter for Ada code that calls the procedure directly.
  - **`out` parameters** are locals in the main. Their values are returned in `Complete`.
  - **`Complete` and `Fail`** tell the shell whether to commit (section 4). An unhandled exception becomes a structured error the shell can display, not a crash with an exit code. Standard error is for diagnostic text.

- **A command receives its arguments by invocation, not at process start.** This is a deliberate choice.
  - **At start**, a command process holds only a receive cap for its command endpoint. The launcher gives the shell the matching endpoint cap.
  - **The shell then calls that endpoint** with a reply cap, sending the arguments, their caps and the standard streams. The generated main's `Receive` waits for that call, and `Complete` or `Fail` is the reply.
  - **Why:** arguments use the same encoding as every other IDL method call, decoded by the same generated server code, so there is one encoding and not two. The reply cap gives the shell its completion signal without a separate result cap. Starting a command is calling a method on it.
  - **Alternatives considered.** Passing everything at process start, as `Launch` does today with a `Capability_Array` and `Environment` bytes, saves a step. But it needs a second encoding for arguments and a separate cap for the result. Creating the process with the invocation already pending would avoid the extra step too, but it adds a special case to process creation for little gain.
  - **Cost:** one extra round trip per launch, and a short period when the process exists without its arguments. A blocking send from the shell waits until the process is ready, so there is no race.
  - **Compatibility is not a constraint.** `Launch` and the exec server change to fit this model; the current `Capability_Array` and `Environment` convention is not kept for commands.

- **The metadata goes in the executable.** The compiler also emits the signature, help text and package details as a constant in the executable, for example in a dedicated ELF section. Installing a command reads them from the executable without running it. This replaces the separate formal-arguments file that petal's `install` command takes today.

- **`Rose.Command_Line`** is a core IDL package for command-line types, starting with `String_List`. It also holds the launch support (`Rose.Command_Line.Launch`) that generated mains call.

### 3. The registry, and options the shell handles itself

- **Registry entries.** The shell's command registry maps each command name to:
  - its signature: each argument's name, type, mode, default, optional short option and help text;
  - the command's summary and extended description;
  - its package: name, version, copyright and help footer;
  - the launch cap.
- **`--help` and `--version` belong to the shell.** They are reserved, so no command may declare arguments with those names. If either appears among a command's arguments, the shell answers from the registry and launches nothing. The other arguments are ignored and not checked. Commands never see these options.
- **Help is generated**, in the layout of GNU tools: a usage line built from the signature, the command's summary, the options with each argument's help, the command's extended description, and a footer from its package with the command's name filled in.
- **Versions belong to packages.** Commands belong to a package, for example a set of core utilities, which has one version and copyright. A command can override them. `--version` prints the command's effective version and copyright.
- **Packages are declared in IDL.** A `package` declaration gives a package's name, version and copyright, and each command declaration names the package it belongs to. The IDL compiler copies the package details into each command's embedded metadata, so the registry needs nothing besides the executable.
- **Core commands are installed at boot.** `init` installs the core-utilities package from the boot image, as it installs the command library today, so `echo` and the other core commands are in the registry before petal shows its first prompt.
- **`--` ends option processing**, so `echo -- --help` prints `--help`. Quoting makes no difference, since `"--help"` is the same word once the quotes are removed.

### 4. The shell owns the commit

Because the shell opens the files, it controls when changes to them become visible.

- **`out File`:** the program writes a new, unnamed object. The shell binds it to the target name only if the program exits successfully. A program that fails or crashes leaves the old contents, or no file, behind.
- **`in out File`:** the program works on a copy-on-write version. The shell commits it on success and discards it otherwise.
- **Mechanism.** This is left to the file system and persistence work. Copy-on-write object versions are preferred over write-to-temporary-then-rename, since the persistence layer already tracks page versions.

### 5. Directories are independent capabilities

- **The shell's environment is a table of named directory caps**, such as `home`, `projects` and `scratch`, instead of a root and a working directory. A path like `projects/rose/README` is resolved by walking from the `projects` cap. There is no `..` above the cap.
- **Programs that find their files as they run get a directory grant.** A build tool that reads a config file and then works out which sources it needs takes `in Directory` or `in out Directory` for the subtree, not a list of files.
- **Grants are attenuated.** Read-only lookup, create-and-modify, and "no subdirectories" are distinct grants. The IDL should split `Directory` so the type itself gives the right: a lookup-only `Directory`, and a separate interface that adds `Create_File` and `Create_Directory`. Narrower grants come from derive (ADR 0001, layer 1) or from a membrane filtering methods (ADR 0001, layer 2).

### 6. Streams carry typed values with nominal interfaces

- **The element type of a stream is an IDL type.** A listing command produces a stream of `File_Info` values, and `sort size` consumes a stream of anything that implements an interface with a `size` field.
- **Nominal, not structural.** A value conforms to an interface only when its type declares that it does. Two fields that share a name and type but differ in meaning (size in bytes vs. in blocks, local time vs. UTC) can't be matched by accident.
- **Retroactive conformance.** A third party, or the user's shell configuration, can declare that an existing type implements an interface, with a field mapping if the names differ. Nobody has to wait for the upstream tool to change.
- **Explicit structural cast.** At the point of use, `ls | as File_Info | sort size` asks the shell to match by shape for that one pipeline. This can't happen silently.
- **Namespaced, versioned interfaces.** Interfaces live in namespaces and carry versions, for example `core.File_Info@2`, so they can change without breaking consumers of older versions. A small core set covers the common cases.
- **Units in the type system.** Core types include quantities such as `Bytes` and `Duration` instead of bare integers.
- **Text fallback.** Every IDL type has a canonical text rendering, generated by the IDL compiler, so untyped tools and plain byte streams (`Stream_Reader` and `Stream_Writer`) still work.

### 7. Separate channels for data, errors and progress

A command has three output paths: `out` parameters for its result, a typed stream for bulk data, and a structured error channel. Errors are IDL values, not text on stderr with an integer exit code. Progress reporting and cancellation are part of the protocol between shell and command.

### 8. Background jobs are explicit and return a handle

- **Opt-in.** A command runs in the background only when asked, with a trailing `&` or similar.
- **A handle, not a job number.** Starting a background job returns a value that can be awaited, piped from later, queried or cancelled.
- **Outliving the session is a capability.** A job that should keep running after the session ends needs an explicit grant, so long-lived processes can't happen by accident. The shell can list each job's caps.

### 9. One membrane per job

Each command runs inside its own membrane (#118, #119). Its arguments, and every cap it obtains through them, such as a file looked up through a directory grant, cross that membrane. Ending or killing the job revokes the membrane, which takes away everything the job reached. This is the per-job revocation that ADR 0001 assigns to petal.

### 10. The IDL compiler is rewritten with two targets

The language features above need the IDL compiler to do more than it does now, so it will be rewritten rather than extended, and built with Alire instead of alix.

- **Kernel target.** This produces the current style of output: client and server packages in the restricted Ada subset, with no tagged types or interfaces. Kernel-level code keeps using it.
- **User-mode target.** This uses the full Ada language. An IDL interface becomes an Ada interface type, and its implementations become tagged types, so `in File` accepts any object that implements `File`, and typed streams can be generic over their element type.
- **Language additions:**
  - command declarations: argument declarations with modes, defaults, short options and list arguments, help text and package details, generating the spec, the main program and the embedded metadata (section 2)
  - record types for stream elements
  - namespaces and versions
  - unit types
  - error types
  - conformance declarations
  - generated text renderings

## Consequences

**Benefits**
- Commands are plain Ada procedures. Argument parsing, `--help` and `--version` are written once, in the shell and the IDL compiler, not in every command.
- Sandboxing needs no separate manifest: a command gets exactly what its signature names, and the shell checks arguments before anything runs.
- Failed commands can't leave half-written output.
- Pipelines pass typed values with a deliberate claim about what they mean, while untyped tools still work through text.
- A job's access is revoked when the job ends, including access it obtained along the way.
- One IDL source defines kernel interfaces, user-mode interfaces, command signatures and stream element types.

**Costs**
- **The IDL compiler is a rewrite**, with a second code generator, a larger language, and generated main programs.
- **Executables carry metadata.** The executable format and the installer need an agreed place for command metadata.
- **Interfaces need owners.** Someone has to define and evolve the core interfaces, and retroactive conformance adds a place where mappings can be wrong.
- **Commit needs file system support** for copy-on-write versions and binding a new object to a name.
- **Membrane cost per job.** Every call a command makes through its grants takes an extra hop through its membrane (see ADR 0001 for the costs and limits).
- **Directory interfaces change.** Splitting `Directory` breaks its current users.

## Open questions

- **Arrays of files and globs.** `in array of File` with the shell expanding a glob works for small sets. Large sets need lazy opening, or they use up the caps and handles they're given.
- **Overloading.** Allowing `copy (File, File)` alongside `copy (Directory, Directory)` is useful, but the rules for choosing between them must be simple enough for a user at the prompt to predict.
- **Attenuation.** Should narrower directory grants come mainly from separate IDL types, from derive, or from membrane filters? Each has different costs for persistence and checks.
- **Text rendering.** What should the canonical text format be, and should it round-trip?
- **Background jobs across restores.** How does a user find and reconnect to their jobs after a restore?

## Follow-up work

- Rewrite the IDL compiler with kernel and user-mode targets, built with Alire (#136).
- Add command declarations to IDL: generate the spec, the main program and the embedded metadata, and add the `Rose.Command_Line` package with `String_List` and the launch support, and `package` declarations whose details are copied into each command's metadata (#137).
- Change `Launch` and the exec server so a command process starts with only a receive cap for its command endpoint, and the launcher returns the matching endpoint cap to the shell (#143).
- Petal: a command registry that reads metadata from executables, handles `--help`, `--version` and `--`, binds arguments to signatures, and launches external commands with exactly the caps their arguments resolve to (#138). The built-in `echo` is removed in the same change, and `echo` becomes the first external command, installed at boot by `init` as part of the core-utilities package.
- Split the `Directory` interface into lookup and create-and-modify parts (#139).
- Define the core stream interfaces (`core.File_Info` and similar) and unit types (#140).
- Shell-owned commit for `out` and `in out` files, with the file system and persistence layers (#141).
- Background job handles in petal (#142).
