# 13. Typed streams and their element types

- **Status:** Proposed
- **Date:** 2026-10-28
- **Related:** [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands, sections 1, 6, 7 and 10), [ADR 0011](0011-end-of-file-at-the-console.md) (end of file), [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (argument modes, streams are never versioned), #140 (core stream interfaces and unit types), #419 (buffer types of `Stream_Reader` and `Stream_Writer`), #397 (console control characters), #360 (`Ada.Streams` in the userland RTS), #341 (pipes), #146 (IDL chapter), blancolioni/idle#1

## Context

ADR 0002 section 6 says that streams carry typed values with nominal interfaces, and #140 asks for the core set of types. The ADR says what a typed stream is for, but not how one is declared, how a command asks for one, or how the shell connects two commands that use them.

### What exists today

- **Streams carry bytes.** `Stream_Reader` and `Stream_Writer` pass a storage array. `Stream_Reader.Read` uses `Storage_Array` and `Storage_Count`, and `Stream_Writer.Write` uses `Stream_Element_Array`. #419 is about making them agree.
- **Text is bytes.** `Ada.Text_IO` in the userland RTS reads and writes characters over a byte stream. The console and keyboard servers are byte-oriented, and #397 adds control characters.
- **Roles name the standard streams.** ADR 0002 section 1 makes `Standard_Input`, `Standard_Output` and `Standard_Error` roles, each holding one `Stream_Reader` or `Stream_Writer`. A command that declares none has no way to do I/O, and the generated configuration forbids `Ada.Text_IO` for it.
- **There is no typed stream.** Nothing says what the element of a stream is, so `ls | sort size` can only work if the shell guesses.

### What ADR 0002 leaves open

- **How a role is typed.** Section 1 says a command that writes typed values "inherits a role whose output stream has that element type", and doesn't say how a role gets its element type.
- **What a text stream is.** Section 6 gives every type a text rendering as the fallback for "untyped tools and plain byte streams". It doesn't say what text is in the type system.
- **How two commands are matched.** Section 6 describes nominal conformance and the explicit cast, but not where the shell applies them.
- **How the stream's operations work.** Is one value one invocation, or are values sent in batches?

### Constraints

- **Servers are single-threaded** (ADR 0002), and an invocation is a round trip. A stream that costs one invocation per element is slow for a long listing.
- **Streams persist.** A pipe between two jobs survives a checkpoint, so the wire format has to be stable, not whatever the compiler lays out.
- **The kernel target has no generics.** ADR 0002 section 10 gives the user-mode target tagged types and generics, and the kernel target a restricted subset.

## Decision

### 1. Streams are layered

There are three layers, each on the one below.

1. **Byte streams.** `Stream_Reader` and `Stream_Writer` over `Stream_Element_Array`, as #419 asks. These are the untyped fallback, and the right type for binary data. The kernel target uses these and nothing else.
2. **Typed streams.** `Stream_Reader (Element)` and `Stream_Writer (Element)`, for an IDL type `Element` (section 2). These are user-mode only.
3. **Adapters.** The shell and the run-time system adapt one to another where there is a rule for it (sections 4 and 6). An `Ada.Streams` adapter over a byte stream is one of these, and is not the typing mechanism (section 8).

### 2. A typed stream is generic over its element type

In the user-mode target, an interface can have a type parameter (ADR 0002 section 10):

```ada
interface Stream_Reader (Element : any) is
   procedure Read
     (Items : out Element_Array;
      Last  : out Natural);
end Stream_Reader;

interface Stream_Writer (Element : any) is
   procedure Write (Items : Element_Array);
end Stream_Writer;
```

- **Nobody writes an interface for each element type.** The IDL compiler instantiates the generic for each element type that a command or interface names.
- **The operations mirror the byte streams.** `Read` and `Write` take arrays, and a `Read` that returns nothing (`Last < Items'First`) is the end of the stream, as in ADR 0011.
- **Values travel in batches.** One invocation carries many elements, so a long listing needs one round trip per buffer, not one per entry (section 7).
- **The element is an IDL type.** It has a namespace and a version (`core.File_Info@2`), and an IDL-generated codec and text rendering. It is not an Ada type that happens to be streamable (section 8).

A type parameter is written like an Ada generic formal, and is either unconstrained or constrained:

- **`Element : any`** accepts any element type. It is a placeholder name: `Type` is a keyword, Ada's `(<>)` means a discrete type in a generic formal, and `private`, the nearest Ada word for "any", doesn't fit here. See the open questions.
- **`Element : Has_Size`** accepts only an element type that conforms to the interface `Has_Size` (section 6). A role or interface uses this to say what it needs of its elements, and not which record it wants.
- **A default** follows the constraint, as in `Element : any := Code_Point`.
- **The colon reads differently from Ada.** In `X : T`, Ada declares a value of type `T`. Here `Element` is declared in a type-parameter position, and `Has_Size` constrains the type that it stands for, as a generic formal does.

### 3. Text is a stream of `Code_Point`

Text is `Stream_Writer (Code_Point)`, not a separate kind of stream.

- **`Code_Point` is a Unicode scalar value.** It is a constrained type, 0 to 16#10FFFF# without the surrogates, so a value that isn't a character can't be sent.
- **It is not called `Character`.** In Ada, `Character` means `Standard.Character`, and a reader of an IDL file would assume it. `Wide_Wide_Character` is wider than Unicode, and its name says nothing about what the type is for. `Code_Point` is the Unicode term.
- **The element type isn't the wire encoding.** An interface says "a stream of `Code_Point`". The generated codec can pack them as UTF-8 in the buffer, so text doesn't cost four bytes a character.
- **Bytes are converted at the console.** The console and keyboard servers convert between `Code_Point` and bytes, including the keyboard layout and the display's character set. This is the same boundary as #397.

### 4. The standard roles take an element type

The standard roles get a type parameter, with `Code_Point` as its default:

```ada
role Standard_Input  (Element : any := Code_Point) : Stream_Reader (Element);
role Standard_Output (Element : any := Code_Point) : Stream_Writer (Element);
role Standard_Error  (Element : any := Code_Point) : Stream_Writer (Element);
```

A command names the element type that it produces or consumes:

```ada
command Rose.Core.List : Standard_Output (File_Info)
  with Summary => "List the entries of a directory."
is
   Directory : in Directory
     with Help => "the directory to list";
end Rose.Core.List;

command Rose.Core.Echo : Standard_Output
  with Summary => "Write its arguments."
is
   ...
end Rose.Core.Echo;
```

- **Every command has one output slot, and one input slot.** The slot is named by its role, so the shell can find them. Only the element type varies.
- **`Standard_Output` with no argument is the text role,** `Standard_Output (Code_Point)`. All the commands in ADR 0002 that write text are unchanged.
- **`Ada.Text_IO` is legal only in a command that declares a text role.** `List` declares `Standard_Output (File_Info)`, so its generated configuration has `pragma Restrictions (No_Dependence => Ada.Text_IO)`. This is ADR 0002's rule that a command has no I/O library without the roles for it, applied by element type. A command with typed output can't print stray text. Its errors go through `Fail` and the structured error channel (ADR 0002 section 7).
- **A command with typed output that also wants to log text** declares a second role. Section 7 of ADR 0002 and the open questions below cover this.
- **`out` parameters are unchanged.** They are for results (ADR 0012 section 1). A command whose result is a stream, such as one that opens a `File`, returns a cap.
- **Streams are never versioned** (ADR 0012 section 3). A typed stream is live in a transactional command, as the standard streams are now.

### 5. The shell connects a producer to a consumer

In `ls | sort size`, the shell binds `ls`'s `Standard_Output (X)` to `sort`'s `Standard_Input (Y)`:

1. **If X satisfies Y,** the shell connects them. A consumer that names a record (`Standard_Input (File_Info)`) is satisfied by that record. A consumer that names a constraint, as `sort` does with `Standard_Input (Element : Has_Size)`, is satisfied by any X that conforms to the interface (section 6). A consumer declared `any` is satisfied by any X.
2. **If Y is `Code_Point`,** the shell inserts a text adapter. It renders each X with its generated text rendering, so any typed command can feed a text command. This is the fallback ADR 0002 promised.
3. **If X is `Code_Point` and Y isn't,** or in any other case, the pipeline is an error, unless the user writes an explicit cast, as `ls | as File_Info | sort size` (ADR 0002 section 6).

The shell does this when it builds the pipeline, before anything runs. A mismatch is reported at the prompt, and no job has started.

### 6. Conformance is nominal, and declared

- **A type conforms to an interface only when it says so.** A record declares the interfaces it implements, so two fields that share a name and a type but differ in meaning (bytes or blocks, local time or UTC) can't be matched by accident.
- **A consumer asks for an interface, not a record.** `sort size` wants any element that implements an interface with a `Size` field (`Has_Size`), not `File_Info` in particular.
- **A third party can declare conformance later,** with a field mapping where the names differ, as in ADR 0002 section 6. The declaration is part of the shell's configuration.
- **A conformance mapping is a function of one value.** It can rename a field or convert units, and it can't look at the rest of the stream.

A sketch of the syntax. It is not settled (see the open questions):

```ada
--  In namespace core. The version is part of the type's name.
record File_Info with Version => 2 is
   Name     : String;
   Size     : Bytes;
   Modified : Time;
end File_Info;

interface Has_Size is
   function Size return Bytes;
end Has_Size;

--  Written by the author of the record, or by anyone else later.
File_Info conforms to Has_Size;
Package_Info conforms to Has_Size with Size => Installed_Size;
```

### 7. The encoding and the batch

- **Elements are encoded as method arguments are.** A record's fields are scalars in words and strings in the shared buffer, by the same generated code that encodes any invocation (ADR 0002 section 2). There is one encoding, not two.
- **A `Write` sends a batch.** The sender fills the buffer with as many whole elements as fit, and sends one invocation. A `Read` returns as many as are ready, and blocks only if there are none.
- **The text adapter and the console are the only places that unpack `Code_Point`.** Everything else treats text as a stream like any other.

### 8. `Ada.Streams` is an adapter, not the typing layer

`'Write` and `'Read` are a serialisation, and the typing needs more than that.

- **They have no name.** A stream of `'Write` output doesn't say it is `core.File_Info@2`, so there is nothing to check a conformance against.
- **They have no version.** The representation is the compiler's, and a pipe that survives a restore can't depend on it.
- **There is already an encoding.** The IDL codec is generated, versioned, and the same as the encoding of every other invocation.

An adapter is still useful. A `Root_Stream_Type` over a byte stream lets ordinary Ada code use `'Read` and `'Write` on a Rose stream, and it is what the containers in GNAT 15 need (#360). Whether userland programs may use streams in general is part of #360, and this ADR doesn't decide it.

## Consequences

**Benefits**
- A command's signature says what it writes and reads, and the shell can check a pipeline before it runs.
- Text is a typed stream, and untyped tools still work through the text adapter and the byte streams.
- One interface declaration serves every element type, and the generated codec and text rendering come with the element.
- `Ada.Text_IO` can be forbidden for commands that don't write text, so a command can't mix text into typed output.
- The same generated encoding serves invocations and streams.

**Costs**
- **Idle needs:** type parameters on interfaces, parameterised roles, record types as elements, conformance declarations, generated text renderings, and the `as` cast. This is the user-mode target of ADR 0002 section 10.
- **`Code_Point` needs support:** the constrained type in the RTS, UTF-8 packing in the codec, and conversion in the console and keyboard servers.
- **Interfaces need owners.** The core set (`File_Info`, `Bytes`, `Duration` and their interfaces) has to be defined and versioned, and retroactive conformance adds a place where a mapping can be wrong.
- **The kernel target has no typed streams.** Kernel-level code keeps using byte streams.

## Alternatives considered

- **A result stream, as `Results : out Stream (File_Info)`.** An `out` argument means the command creates and serves the stream (ADR 0012 section 1). In `ls | sort`, the shell would then forward every element, or proxy the stream. With a role, the shell creates the connection and gives each end to its job. A command whose result really is a stream can still return one.
- **`command List : Stream_Writer (File_Info)`.** A command that inherits an interface provides it (ADR 0002 section 1, roles are not interfaces), so this says that `List` accepts `File_Info`, the opposite of what's meant. An interface also gives the shell no name to bind by.
- **A role for each element type,** such as `File_Info_Output`. The shell then has no way to find a command's output, since it has to know which role is the primary one. A parameterised role keeps one slot.
- **`Ada.Streams` as the typing layer.** Section 8 gives the reasons.
- **`Wide_Wide_Character` as the text element.** It allows values that aren't Unicode scalar values, and the name suggests the `Character` family that Ada programmers know. `Code_Point` says what it is.
- **Byte streams only, with typing by convention.** This is today's model, and it leaves the shell to guess at `ls | sort size`.

## Open questions

- **The word for "any type".** `any` is a placeholder until there is a better one.
- **The syntax for a parameterised role.** The sketches above are a proposal. An alternative is a role for each element type declared in one line, as `role File_Info_Output : Stream_Writer (File_Info)`, with a rule that picks the output role.
- **The names of the byte and typed streams.** `Stream_Reader` is a byte stream today and `Stream_Reader (Element)` is the typed one. They could share a name if bytes are the typed stream with element `Stream_Element` and the same encoding, or the typed ones could get new names.
- **More than one typed stream.** A command with `Standard_Output (File_Info)` and a second typed stream, such as a stream of errors or progress (ADR 0002 section 7), needs a second role and a rule for which one a pipe connects to. This is the same question as the live log in a transactional command in ADR 0012.
- **A filter's two element types.** `Standard_IO` inherits all three standard roles with the same element type, and a filter that reads `File_Info` and writes `Code_Point` has two.
- **The syntax for conformance and records.** The declarations in section 6 are a sketch, and so are the field mappings and the `as` cast.
- **How conformance is checked.** For `sort size`, does the shell check each element, or only the stream's element type? Is a conformance declared in the shell's configuration visible to a job inside its membrane?
- **A command generic over its element can't decode it.** An `any` consumer such as `head` or `wc -l` can only pass elements through undecoded. A constrained consumer such as `sort size` has to read a `Size` out of elements whose concrete type it doesn't know. Either the stream carries a type tag so that the codec can be found at run time, or the shell gives the command the conformance mapping.
- **The wire encoding of `Code_Point`.** UTF-8 in the buffer is the proposal. The alternative is a fixed width, which is simpler to decode and larger.
- **The batch size and blocking.** How many elements a `Write` sends before it flushes, and whether a `Read` that has some elements waits for more. A line-oriented consumer wants one answer and a long listing another.
- **Versions.** What `core.File_Info@2` means for a consumer written for `@1`, and whether a pipe between versions gets an adapter or an error.

## Follow-up work

- #419: make `Stream_Reader` and `Stream_Writer` agree on `Stream_Element_Array`.
- Idle: type parameters on interfaces and roles, record elements, conformance, text renderings and the `as` cast (blancolioni/idle, issue to be opened).
- `Code_Point` in the RTS, the codec, and the console and keyboard servers (#397).
- #140: define `core.File_Info`, `Bytes`, `Duration` and the first interfaces, once this ADR is accepted.
- Petal: build pipelines under section 5 (#341), and the `as` cast.
- ADR 0002: section 1 and section 6 point here for the typed roles and the pipeline rule.
- #146: the IDL chapter documents interface parameters, roles and conformance.
