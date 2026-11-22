# 42. Configuration as a tree, with caps for authority

- **Status:** Proposed
- **Date:** 2026-11-22
- **Related:** #729 (this decision), #730 (the base interfaces), #731 (a configuration server), #732 (the `Rose.Configuration` library), [ADR 0010](0010-resources.md) (resources, `Releasable_Reader`), [ADR 0022](0022-display-server.md) (the display server), [ADR 0026](0026-commands-commit-their-own-versions.md) (commands commit their own versions), [ADR 0029](0029-installed-capabilities.md) (installed capabilities), [ADR 0031](0031-updating-an-installation.md) (the interfaces freeze with 0.4.0), [ADR 0034](0034-interface-packages.md) (interface packages), [ADR 0035](0035-system-directory-and-special-files.md) (special files), [ADR 0036](0036-directory-views.md) (directory views), [ADR 0040](0040-package-state.md) (package state), #933 (interfaces before the 0.4.0 tag)

## Context

Programs need configuration. The display server is the first that needs much of it: a background image, a default font, and the applications to start (ADR 0022). Editors, the REPL and games will want settings of their own.

On Unix, configuration is text files in well-known places (`/etc`, `~/.config`), in as many formats as there are programs, and it names other programs and files by path, which the program then opens with whatever authority it has. Rose has none of the well-known places, no global paths, and no ambient authority to open a path with.

What a program needs from its configuration is small: look up a value by name, read it, and walk a level of names. A directory already does that, and so would a server that keeps configuration in memory. Neither should have to be the other.

### What exists today

- **`Files.Directory`** has `Directory_Entry_Count`, `Directory_Entry_Name`, `Find_Entry`, `Directory_Entry_Kind`, `Directory_Entry_Size`, `Get_Ordinary_File` and `Get_Directory`. ADR 0036 adds `Get_Special_Reader`, and puts `Fixed_Directory` between `Directory` and `Writable_Directory`.
- **`Files.File`** has `Size`, `Open_Reader` and `Open_Seekable_Reader`. A file opens readers, and isn't a stream itself (ADR 0009).
- **There is no base interface** for "something with named entries" or "something that opens a reader".
- **The interfaces freeze with the 0.4.0 tag** (ADR 0031). Giving `File` and `Directory` a parent changes their endpoint indexes, so it has to happen before the tag (#933).

## Decision

### 1. Two base interfaces: `Readable` and `Tree`

```ada
interface Streams.Readable is
   function Open_Reader return interface Streams.Releasable_Reader;
end Streams.Readable;

interface Common.Tree is
   type Entry_Kind is (Branch, Leaf, Other);

   function Entry_Count return Natural;
   function Entry_Name (Index : Positive) return String;
   function Entry_Kind_Of (Index : Positive) return Entry_Kind;
   function Find_Entry (Name : String) return Natural;
   function Get_Branch (Index : Positive) return interface Common.Tree;
   function Get_Leaf (Index : Positive) return interface Streams.Readable;
end Common.Tree;
```

- **`Readable` is in `Streams`,** beside `Reader` and `Seekable`, because what it does is open readers.
- **`Tree` is in `Common`,** because interfaces of more than one group descend from it: `Files.Directory`, and the configuration server's interface (#731). That is ADR 0034's rule for `Common`.
- **`Find_Entry` is in the base,** because looking up each element of a path by name is the main thing configuration does. It returns 0 for a name that isn't there.
- **`Other` is an entry that a `Tree` can't give,** such as a special file in a directory, or a cap in a configuration. Code that only knows `Tree` walks branches and leaves and skips the rest. A descendant interface says what its other entries are.
- **Getters fail by raising:** `Get_Branch` on a leaf, `Get_Leaf` on a branch, and either on an index past the end.

### 2. `File` and `Directory` inherit them

```ada
interface Files.File : Streams.Readable is
   function Size return Storage_Count;
   function Open_Seekable_Reader return interface Streams.Seekable_Reader;
end Files.File;

interface Files.Directory : Common.Tree is
   type File_Kind is (Directory, Ordinary_File, Special_File);

   function Entry_File_Kind (Index : Positive) return File_Kind;
   function Entry_Size (Index : Positive) return Storage_Count;
   function Get_Ordinary_File (Index : Positive) return interface Files.File;
   function Get_Directory (Index : Positive) return interface Files.Directory;
   function Get_Special_Reader (Index : Positive) return interface Streams.Reader;
end Files.Directory;
```

- **`Open_Reader` is inherited unchanged,** so no result type changes, which Ada could not express.
- **`Directory_Entry_Count`, `Directory_Entry_Name` and `Directory`'s own `Find_Entry` go.** They are the same operations as `Tree`'s, which `Directory` now inherits. This is the time to drop them, since after the tag they could only be retired (ADR 0031, section 2).
- **`Directory_Entry_Kind` and `Directory_Entry_Size` become `Entry_File_Kind` and `Entry_Size`,** to match `Tree`'s names.
- **The file system servers implement `Tree`'s getters by forwarding:** `Get_Branch` to `Get_Directory`, and `Get_Leaf` to `Get_Ordinary_File`. `Entry_Kind_Of` is `Branch` for a directory, `Leaf` for an ordinary file, and `Other` for a special file.
- **ADR 0036's views are unchanged.** `Fixed_Directory` and `Writable_Directory` descend from `Directory`, so every view of a directory is a `Tree`, and a `Tree` gives only read-only branches and leaves, whatever view it came from.

### 3. A configuration is a `Tree`

- **A program that takes configuration declares a `Tree` argument,** such as `Settings : in Common.Tree`. A directory, a `Fixed_Directory`, a `Writable_Directory` and a configuration server's tree all narrow to it for free.
- **Configuration that is only data can be an ordinary directory.** A package's installed defaults are a lookup-only directory (section 5), and a user's own settings are in the package's state directory (section 7).
- **The tree is the structure, and a leaf is one value.** There is no file format inside a leaf: no INI, no JSON. `display/font` is a leaf holding a font's name, and `display/background` a leaf holding an image. A leaf that holds text holds UTF-8.
- **Typed values are the library's** (#732): `Get_String`, and getters for integers, Booleans and colours that parse the text, with one spelling each (`true` and `false`, `#rrggbb`). A large value, such as an image, is read through `Get_Reader`.

### 4. Authority comes from caps, not names

- **A configuration doesn't name programs or files for its program to find.** It holds the caps. The display server's `launch/` entries are `Launch` caps, so it never turns a program's name into a process, and can start only what it was given.
- **Whoever starts a program builds its configuration** and puts in the caps it holds. The program can do no more than that. These caps are an ordinary grant from the caller, at run time, like a directory it passes, so ADR 0002's rule that the caller supplies a command's caps still holds.
- **Cap entries are `Other` to a `Tree`.** The configuration server's interface, which descends from `Tree`, adds `Get_Cap (Index) return Capability` and says which of its other entries are caps (#731).
- **A cap entry isn't checked against a type** before the program uses it. The configuration comes from whoever launched the program, who already decides every cap it has, so a cap of the wrong type is a bug in the launcher and not an attack. When the kernel can say what a cap is (#1038), the library checks it, and raises on a mismatch.

### 5. Installed configuration holds data only

- **A package's installed defaults are a lookup-only directory** in its data tree, which the command declares as an installed argument (ADR 0029, section 2):

  ```ada
  Defaults : in Common.Tree
    with Installed => "settings",
         Help      => "the package's default settings";
  ```

- **It holds no caps.** A lookup-only directory has none to give, and an installed configuration server's tree is not allowed.
- **A program that needs installed caps declares each as an `Installed` argument,** of one of the types ADR 0029, section 4, allows. Configuration is never the way to give a program caps that come with its installation.
- **Why:** ADR 0029 makes every installed cap a visible argument of a restricted type, so that a command's signature still lists everything it can reach. A `Launch` cap inside an installed tree would be a second way to install caps, which the signature would show only as `Defaults : in Common.Tree`. Cap entries come only from a launcher, at run time (section 4).

### 6. Handing out a subtree limits what a program sees

- **There is no global configuration tree,** and no `/` path that reaches every program's settings. A program gets the branch that is about it.
- **Paths exist only in the library** (#732). `Get_String (Settings, "display/font")` looks up `display` and then `font` with `Find_Entry`. The IDL has no paths, so no server parses them.

### 7. Settings: installed defaults, overrides in package state

A program with settings that the user can change takes its installed defaults (section 5) and the `Package_State` role (ADR 0040), and gives both to the library:

```ada
Settings : Rose.Configuration.Settings :=
  Rose.Configuration.Open (Defaults, Package_State);
```

- **Overrides are in `settings`, in the package's state directory:** `home/package-state/<package>/settings/`. The library makes it when it first writes. The rest of the package's state directory is the program's own, for saves, history and anything else.
- **The overrides have the same shape as the defaults.** Overriding `a/b/c` is a file `c`, in a directory `b`, in a directory `a`, in `settings`.
- **Lookups try the overrides first,** then the defaults, and the first that has the path wins.
- **Layers are joined leaf by leaf, not branch by branch.** An override of `a/b/c` hides only `a/b/c`: `a/b/d` still comes from the defaults, and the children of `a/b` are those of both layers.
- **`Set` writes a leaf in the overrides,** making the directories on its path as it goes, and commits it (ADR 0026). **`Reset` deletes the leaf,** so the default shows through again. The installed defaults never change.
- **An override is a one-line text file,** because a leaf is one value (section 3). The user can read and change settings with `cat`, `echo` and `remove`, with no settings editor and no format to learn, and a backup of the home directory includes them.
- **Other layers are the same mechanism.** The library looks a path up in any list of trees, in order, and no server merges trees. A test passes a tree of its own in front of the others.

### 8. Order and change

- **A `Tree` promises nothing about the order of its entries,** only that an index stays valid while the tree is unchanged. An ordinary directory lists in whatever order its server keeps.
- **The configuration server keeps insertion order** (#731), so `launch/` starts its programs in a known order.
- **There is no change notification.** A program reads its configuration when it needs it. Notification can be added later as an interface descending from `Tree`, since adding is allowed after the tag.

## Consequences

- **#730 reshapes `File` and `Directory` before the tag.** Every caller of `Directory_Entry_Count`, `Directory_Entry_Name`, `Directory_Entry_Kind` and `Directory_Entry_Size` changes. Today that is the directory server and `isofs`, `restore`'s installer, `Rose.Directories`, petal's session, `list`, `remove`, `rmdir`, `touch`, `hold_home` and the self tests, and ADR 0040's null directory (#1045) is written against the new names. `endpoint-ids.txt` changes with them, which is allowed only before the tag.
- **ADR 0036's IDL changes too:** `Directory` gains `Tree` as its parent, in the same series as #938 (#730 comes first).
- **Configuration needs no new server to start with.** A directory is enough for data. The configuration server (#731) is needed only for cap entries, and can come after the tag, since it only adds.
- **Programs can be tested** with any `Tree`: a directory of test settings, or a small in-memory tree.
- **Every program spells its values the library's way.** There is one way to write a Boolean or a colour, not one per program.
- **Settings need no server and no format.** Defaults are installed files, overrides are files in the user's home, and the library joins them.
- **The name `settings` is taken** in every package's state directory. ADR 0040 should say so.
- **ADR 0029's restriction on installed caps holds for configuration too,** so installing a package can't give it a cap that its signature doesn't show.

## Alternatives considered

- **Configuration files in a format,** such as INI, TOML or JSON, in a directory. Every reader needs a parser, a leaf can't hold a cap, and a cap can't be written in a text file at all.
- **A configuration interface unrelated to `Directory`.** A directory of settings couldn't then be used as configuration without an adapter, and walking a tree would be written twice.
- **Keeping `Directory_Entry_Count` and `Directory_Entry_Name` beside `Tree`'s.** Two names for each operation, for ever. They can only be dropped before the tag.
- **Paths in the interface,** such as `Find_Path (Path : String)`. Every server would parse paths, with its own rules, and a path could suggest more reach than the tree has.
- **An `Entry_Kind` with a value for each kind of entry, in the base.** The base would have to know every descendant's kinds. `Other` leaves them to the descendant.
- **Installed configuration with caps,** such as `Launch` caps in an installed tree. It would be a second, hidden way to install caps (section 5).
- **Writing settings over the defaults,** in place. The defaults are shared by every user and installed read-only, so one user's change would be everyone's, and reinstalling would lose it.
- **A settings server** that holds each user's settings for every package. It would be one more persistent process, and the settings would be out of the user's sight, not files in their home.

## Open questions

- **Unsetting a default.** An override can replace a default but not remove it. A marker for "no value" could be added if something needs it.

## Follow-up work

- #730: `Streams.Readable`, `Common.Tree`, the new parents of `File` and `Directory`, and the renames, before the tag.
- #938: ADR 0036's views on top of #730.
- #731: the configuration server, with cap entries and insertion order. It can come after the tag.
- #732: the `Rose.Configuration` library: paths, typed getters, defaults, layers, and `Open`, `Set` and `Reset` over installed defaults and package state (section 7).
- ADR 0040: note that `settings` in a package's state directory is the library's.
- The display server reads its configuration through the library (ADR 0022).
