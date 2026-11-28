# 48. Records, views and generic operations

- **Status:** Proposed
- **Date:** 2026-11-28
- **Related:** [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed values with nominal interfaces, section 6), [ADR 0013](0013-typed-streams.md) (typed streams, sections 2, 5 and 6), [ADR 0031](0031-updating-an-installation.md) (interfaces are extended or retired, section 2), [ADR 0047](0047-structured-files.md) (flat, self-describing records, section 4), #140 (core stream interfaces and unit types), idle#113 (records), idle#114 (record versions), idle#115 (shared types), idle#117 (conformance), idle#118 (text renderings)

## Context

Typed streams (ADR 0013) carry records, and the commands that read them want a meaning from each element, not a particular record. ADR 0002 section 6 and ADR 0013 section 6 say this is done by nominal conformance: a record conforms to an interface when a declaration says so, and a third party can declare it later, with a mapping of fields.

That leaves the design of three things open: what a record is, how one record is made to serve as another, and how an operation says what it accepts. This ADR designs them together, without starting from the IDL as it is.

### Examples

A consumer needs one meaning, producers carry it under different names, and some producers carry several fields that could match.

- **A place to open.** `edit` opens each element at a path and line. `grep` writes `Grep_Match`, whose path is `File`. `make` writes `Compiler_Message`, whose path is `Source`. `find` writes `File_Info`, which has a path and no line. `grep Put_Word src | edit`, `make | edit` and `find *.adb | edit` should all work with one `edit`, and none of the producers should have to know about it.
- **When something happened.** `since yesterday`, `newest 10` and `timeline` want a time. `Log_Entry` has `Logged`, and `Checkpoint` has `Committed`. `File_Info` has `Created` and `Modified`, both `Time`, so no match by name or type can say which is "when".
- **A filter keeps its type.** In `ls | since yesterday | sort size`, `since` reads `File_Info` as a time, and has to write `File_Info`, so that `sort` still finds `Size`.
- **A field chosen at the prompt.** In `sort modified` or `select name`, the user names the field, and no interface in the IDL names it.

### What exists today

- **idle PR #137** (idle#113) adds Ada-style records, encoded whole in the invocation buffer, and refused by the kernel target.
- **idle PR #138** (idle#115) lets one interface name the types of another, as in `Core.Units.Bytes`.
- **idle PR #140** (idle#114) adds versions of records, `File_Info@2`, and a type id for each.
- **idle#117** (conformance) isn't built. ADR 0013 sketches `Package_Info conforms to Has_Size with Size => Installed_Size;`.
- **Record files describe themselves** with a table of their fields (ADR 0047 section 4).

## Decision

There are three parts, kept separate: a **record** is a shape, a **view** says how one record becomes another, and an **operation** is generic over any record that has a view to the one it uses. The syntax below is a sketch. The open questions list what isn't settled.

### 1. A record is a shape

```ada
record Location is
   Path : Petal_Path;
   Line : Natural := 0;
end Location;

record Grep_Match is
   File : Petal_Path;
   Line : Natural;
   Text : String;
end Grep_Match;

record File_Info is
   Name     : Petal_Path;
   Size     : Bytes;
   Created  : Time;
   Modified : Time;
end File_Info;

record Dated is
   At : Time;
end Dated;
```

- **Every record is concrete.** There is no separate abstract kind. `Location` is what `edit` asks for, and also what a list of bookmarks can write.
- **A field's type** is a scalar, a unit type (#140), a string, an enumeration, or another record. Caps and server interfaces are not values, and are refused.
- **A default** makes a field optional: a view that doesn't give the field (section 2), or a producer whose schema doesn't have it (section 5), gets the default.
- **The kernel target refuses records,** as idle#113 does.

**Options considered:**

- **(a) Records and abstract interfaces as two kinds,** with operations asking for an interface. Two kinds for one idea, and a record that `edit` wants can't also be written by a producer.
- **(b) Records as value interfaces,** whose fields are functions and whose conformance is inheritance with renames (the earlier draft of this ADR). Conformance then belongs to the record's author, and anyone else needs a second mechanism.
- **(c) Records as shapes, and conformance in views.** Chosen.

### 2. A view says how one record becomes another

```ada
view Grep_Match as Location is
   Path   => File,
   others => <>;
end view;

view File_Info as Location is
   Path => Name;
end view;

view File_Info as Dated is
   At => Modified;
end view;

view Created of File_Info as Dated is
   At => Created;
end view;
```

- **A view is declared, not inferred.** A field of the target is filled only as the view says, so two fields that share a name and type but differ in meaning are never matched by accident.
- **Each field of the target** is given by a field of the source, a dotted path into a nested record of the source, or a constant. A field the view doesn't mention takes its default, and a field with no default must be given.
- **`others => <>` matches the remaining fields by name and type.** This is the Ada aggregate's notation, and it is a choice the view's author writes, not a rule the language applies everywhere.
- **Anyone can declare a view,** in any IDL file. Neither record's author has to know: the editor's author declares `Grep_Match as Location`.
- **A pair has at most one default view,** and any number of named ones. `File_Info` is `Dated` by `Modified` unless the user asks for `Created`.
- **Identity is free.** A `Location` is a `Location` without a view.
- **Views don't compose.** If `A as B` and `B as C` exist, `A as C` still has to be declared, since two paths from `A` to `C` would need a choice.

**Options considered:**

- **Where conformance lives:** (a) on the record, by its author; (b) in separate declarations anyone can write. Chosen: (b), since the examples need conformance that the producer's author never wrote.
- **What a view may contain:** (a) fields and constants; (b) expressions over the source's fields, such as unit conversions. Chosen: (a). A view is then data (section 5), so a consumer never runs code for a type it wasn't built with. Each quantity has one stored unit (idle#118), which removes the need for conversion.
- **One view per pair, or named views:** one view per pair makes `File_Info as Dated` a choice made once for everyone. Chosen: a default and named views.
- **Composition:** automatic composition saves declarations and needs a rule to choose between paths. Chosen: none.

### 3. An operation is generic over records with views

```ada
procedure Edit (Items : list of T) with T as Location;

function Since (Items : list of T; After : Time) return list of T
  with T as Dated;

function Total (Items : list of T) return Bytes
  with T as Sized;

function Sort (Items : list of T; Key : field of T) return list of T;

function Head (Items : list of T; Count : Natural) return list of T;
```

- **`T as Dated` is a bounded type parameter.** The operation accepts a list of any record that has a view to `Dated`, and the caller's record is `T`.
- **The record passes through.** `Since` returns `list of T`, so `File_Info` in gives `File_Info` out.
- **Inside the operation, `T` is opaque** except through its view: the generated code gives `Dated (Item).At`, and nothing else of the element.
- **An unbounded `T`,** as in `Head`, can move, keep and drop elements, and can't look into them.
- **`field of T` is a field chosen by the caller,** checked against the record when the operation is bound. It is not conformance: it covers `sort modified` and `select name`, where the user names the field and no record or view does.
- **A typed stream is a list that arrives over time.** ADR 0013's roles take the same bound: `Standard_Input (T) with T as Location`, `Standard_Output (T)`. A filter that declares both with one `T` writes what it reads.

**Options considered:**

- **(a) The operation takes the target record,** `list of Dated`, and the caller converts. The filter loses the caller's record, and `ls | since yesterday | sort size` fails.
- **(b) The operation takes a bounded parameter,** and the record passes through. Chosen.
- **For a field named at the prompt:** (a) one record for each field, `Has_Name`, `Has_Modified` and so on; (b) reflection through the list's schema, typed as `field of T`. Chosen: (b).

### 4. Records change by appending fields

- **Within a series, a record only gains fields,** each with a default, at the end. A field that is no longer wanted is retired and still sent. This is ADR 0031's rule for interfaces, applied to records. A new series can change a record.
- **There are no version numbers.** A record has one name and one type id, the hash of its namespaced name, as endpoint ids are made.
- **Its fields are listed** in a file like `endpoint-ids.txt`, which `check-endpoint-ids` holds append-only within a series.
- **Views name fields,** so a view keeps working as its source gains fields.

**Options considered:**

- **(a) Versions,** as idle#114 builds them: `File_Info@2`, each version its own type. A consumer of version 1 can't read version 2, and every view names a version.
- **(b) A new record for each change,** `File_Info_2`. The newest name changes with every release.
- **(c) Appending, with defaults.** Chosen, since the schema (section 5) lets each reader find the fields it knows.

### 5. Lists carry their schema, and views are passed as data

- **A list starts with a header:** the record's type id, then a table of its fields' names, types and positions, as a record file does (ADR 0047 section 4). Its elements follow, field by field in the table's order.
- **A view is resolved when the operation is bound.** For `ls | since yesterday`, the shell finds the default view `File_Info as Dated` and compiles it against the producer's schema into a mapping: for each field of `Dated`, a position in the producer's elements or a constant. It gives the mapping to `Since` with the list.
- **The operation reads elements through the mapping,** in its generated code. It needs nothing compiled for `File_Info`.
- **Evolution falls out of it.** A producer that has appended fields has a longer schema, and the mapping skips the new ones. A producer that lacks a field the view names gives the field's default.
- **A named view** is chosen by the caller, and petal's syntax for it isn't decided here.
- **Errors are found before anything runs:** no view to the bound, two default views for one pair, or a `field of T` that the record doesn't have. The shell reports them at the prompt (ADR 0013 section 5).
- **The text adapter** (ADR 0013 section 5) reads the same header, to render an element whose record it doesn't know (idle#118).
- **The schema is sent once per list,** not once per element. A list of `Code_Point` has no header.

This is how Haskell passes type-class dictionaries: the bound is the constraint, the view is the instance, and the mapping is the dictionary. Because a view is fields and constants, the dictionary is data, and can be built by the shell for a record the operation has never seen.

**Options considered:**

- **(a) Views linked into the operation,** chosen by a type tag. Closed: a new producer means rebuilding the consumer.
- **(b) An adapter process** between producer and consumer that converts each element to the target record. One more process and one more copy for each constrained pipe, and the filter loses the caller's record, as in section 3's option (a).
- **(c) The schema in the list, and the mapping as data.** Chosen.

### 6. Nesting and lists in records

- **A field can be a record,** and a view can reach into it with a dotted path. A nested record's fields are in the schema, as a table within the table.
- **A field can't be a list** for now. A list in a record needs a length in each element, and nothing needs one yet.
- **A record file stays flat** (ADR 0047 section 4). A record with nested fields is written to a record file only after flattening, and ADR 0047 doesn't need that yet.

## Consequences

- **Three parts, each simple.** Records are data with no behaviour, views are declarations that anyone can write, and an operation's signature says what it needs and what it gives back.
- **ADR 0013 is amended,** once this is accepted:
  - section 2's `Element : Has_Size` becomes `T as Dated`
  - section 6 is replaced by sections 2, 3 and 5 here
  - its open questions on checking conformance, unknown element types and versions are answered by sections 5, 3 and 4
- **ADR 0002 section 6 is amended:** values conform through views, and change by appending fields, not by `@N` versions.
- **ADR 0047's field table** is shared by record files and lists.
- **The `as` cast** (ADR 0002 section 6) is a view declared for one pipeline, and stays petal's.
- **Idle:**
  - idle#113 (PR #137) keeps its encoding and codecs. The declaration takes the form of section 1, and records gain defaults and field tables.
  - idle#115 (PR #138) is unchanged.
  - idle#114 (PR #140) is closed unmerged. Its type id stays, as one per record.
  - idle#117 becomes views, bounded parameters, `field of T`, and the generated mapping.
  - idle#118 renders a record by its schema.
- **Lists cost a header,** once per list, and an operation with a bound costs one mapping per call.
- **`view`, `as`, `list of` and `field of`** are new in the IDL.

## Alternatives considered

The options in each section are the alternatives considered there. One more:

- **Ada tagged types and interfaces** in the generated code, with `File_Info` implementing `Dated`. Natural for Ada, but conformance belongs to the type's author, and a consumer can't hold a value of a type it wasn't built with.

## Open questions

- **The syntax:** `record`, `view ... as ... is`, `with T as Dated`, `list of T` and `field of T` are sketches.
- **Where views are found.** All IDL files that the shell can see, or only those named in its configuration, and whether a job inside a membrane sees the shell's views.
- **How a user picks a named view** at the prompt.
- **Defaults.** Whether a default can name another field, such as `Modified := Created`.
- **The header's encoding,** and whether it is sent again after a restore.
- **More than one bound,** as `T as Dated and Location`, for an operation that needs two meanings.

## Follow-up work

- idle: records in section 1's form, with defaults and type ids; views; bounded parameters, `list of T` and `field of T`; the generated mapping (idle#113, idle#117). Withdrawing idle#114.
- rose#140: the list header and the mapping.
- petal: finding views when a pipeline is built, passing the mapping, choosing a named view, and the `as` cast.
- ADR 0013 and ADR 0002: the amendments above.
- The IDL chapter of the Rose Book.
