# 34. Interface packages

- **Status:** Proposed
- **Date:** 2026-11-12
- **Related:** #888 (this decision), #841 (`Block_Device` renamed), #162 (system caps as roles), #736 (endpoint ids), #827 (endpoint ids append-only), [ADR 0002](0002-typed-commands-and-directory-caps.md) (roles), [ADR 0031](0031-updating-an-installation.md) (the interfaces freeze with 0.4.0)

## Context

After `v0.4.0` is tagged, `interfaces/endpoint-ids.txt` is append-only for the whole series (ADR 0031, section 2). An interface can then be extended or retired, but not renamed or moved.

### What exists today

- **Every interface is a direct child of `Rose.Interfaces`.** There are 72, from `ATA` to `Writer`, each in its own file in `interfaces/`, and each generated as `Rose.Interfaces.<Name>`, with `.Client` and `.Server` children, for both targets.
- **idle hard-codes this.** Both targets write `Rose.Interfaces.<Name>`, and `Idle.Endpoints` writes each endpoint's text with the prefix `rose.interfaces.`.
- **An endpoint id hashes the fully qualified name** of an operation and its parameter and return types, such as `rose.interfaces.bank.reserve_storage(in word_64) return interface rose.interfaces.region`. Moving `Region` would change the ids of `Bank.Reserve_Storage`, and of every other operation that takes or returns a `Region`, as well as `Region`'s own. 55 operation lines name another interface.
- **Roles** (ADR 0002), such as `Standard_Output` or `Storage_Service`, have no endpoint ids of their own. They are sets of caps of interface types.
- **Four interfaces are used by nothing:** `Data_Source`, `Keyboard_Handler`, `Rose_Interface`, and `Display`, which waits for the display server (ADR 0022).

### What's needed

- A structure that groups the interfaces by what they are for, so that a reader finds `Bank` beside `Region`, and a new interface has an obvious place.
- One change of every id, before the tag, rather than a series of them.
- Nothing frozen that nothing uses.

## Decision

### 1. One level of groups

Each interface is a child of a **group**, which is a child of `Rose.Interfaces`: `Rose.Interfaces.Storage.Bank`, with `Rose.Interfaces.Storage.Bank.Client` and `.Server`.

- **One level only.** Twelve groups of two to thirteen interfaces each are easy to scan, and a deeper tree would mostly add names to type.
- **A group is an empty package,** `Rose.Interfaces.Storage`, which idle generates for each target with its children.
- **`Rose.Interfaces.Objects` stays where it is.** It is idle's support package, not an interface, so no group is called `Objects`.
- **A group's name is plural,** or a mass noun, so that it doesn't repeat its children's names: `Files.File`, `Users.User`.

### 2. The groups

| Group | Interfaces | Roles |
|---|---|---|
| `Caps` | `Cap`, `Cap_Set`, `Releasable_Cap_Set`, `Constructor`, `Receiver` | `Create_Endpoint`, `Create_Signal`, `Delete_Cap`, `Rescind_Cap`, `Cap_Set_Service` |
| `Common` | `Named`, `Resource`, `Transactional` | |
| `Streams` | `Reader`, `Writer`, `Stream`, `Releasable_Reader`, `Releasable_Writer`, `Seekable`, `Seekable_Reader`, `Output_Redirect` | `Standard_Input`, `Standard_Output`, `Standard_Error`, `Standard_IO` |
| `Processes` | `Process`, `Kernel_Process`, `Segment`, `Heap`, `Server`, `Launch`, `Exec`, `Executable`, `Memory`, `Signalled`, `Restorable`, `Replaceable`, `Saveable` | |
| `Storage` | `Store` (was `Storage`), `Bank`, `Region`, `Dependent`, `Process_Banks` | `Storage_Service` |
| `Checkpoints` | `Checkpointer`, `Kernel_Log`, `Log_Reader`, `Memory_Restore`, `Storage_Checkpoint`, `State` | |
| `Devices` | `ATA`, `Block` (was `Block_Device`, #841), `Partitions`, `Partition_Setup`, `Interrupt_Handler`, `Event_Listener`, `Event_Source` | |
| `Time` | `Clock`, `Clock_Admin`, `Timer`, `Timeout` | `Clock_Service`, `Clock_Admin_Service`, `Timer_Service` |
| `Files` | `File`, `Writable_File`, `Transactional_Writable_File`, `File_Control`, `Directory`, `Writable_Directory`, `File_System` | |
| `Users` | `Auth`, `Auth_Admin`, `User`, `Rights`, `Rights_Admin` | `Rights_Service`, `Rights_Admin_Service` |
| `Commands` | `Lookup` (was `Commands`), `Map`, `Command_Endpoint` | `Find_Command` |
| `Screens` | `Terminal`, `Surface` | `Console`, `Screen_Service` |

How the less obvious ones were placed:

- **`Common`** holds the mix-ins that interfaces of several groups descend from. An interface used by only one group goes in that group, as `Seekable` goes in `Streams`.
- **`Processes`** holds what a process is, and what the kernel or mem sends to one: `Signalled` and `Restorable`. `Replaceable` and `Saveable` are there because they are about a server's process, not its objects.
- **`Checkpoints`** holds each server's part in a checkpoint or a restore, so `Memory_Restore` and `Storage_Checkpoint` are there, not with `Memory` and `Storage`.
- **`Devices`** holds the drivers' interfaces, and the callbacks that drivers use: `Interrupt_Handler` and the events.
- **`Screens`,** not `Display`, so that the display server's interface can be `Screens.Display` when it comes.

Two interfaces are renamed, so that no name repeats its group:

- **`Commands` becomes `Commands.Lookup`.** It is the part of the command registry that finds commands by name (#460), which `Map` extends with `Add`.
- **`Storage` becomes `Storage.Store`.** It is the storage server itself: it takes backing store, and gives the root bank, which every other bank is below.

Roles have no ids, so the tag doesn't freeze where they are. They move with their interfaces, so that a role is found beside what it grants.

### 3. Remove what nothing uses

`Data_Source`, `Keyboard_Handler`, `Rose_Interface` and `Display` are removed before the tag. Anything in the list at the tag can never go during the series. Adding an interface later is allowed, so `Display` comes back, in `Screens`, when the display server needs it.

The rule from now on: an interface goes into a release only once something implements it.

### 4. IDL and files

- **An interface is declared with its group:** `interface Storage.Bank is ... end Storage.Bank;`.
- **References name the group too,** wherever they are: `return interface Storage.Region`, even from inside `Storage`. There are no visibility rules to learn, and the text matches the endpoint id.
- **Files follow GNAT's naming,** as `rose-core.idl` already does: `interfaces/storage-bank.idl`. `interfaces/` stays one directory, and idle finds `Storage.Region` as `storage-region.idl` on its include path.
- **An endpoint's text** is the full name: `rose.interfaces.storage.bank.reserve_storage(in word_64) return interface rose.interfaces.storage.region`.

### 5. One change, before the tag

idle's change and Rose's land together:

1. **idle:** dotted interface names, the file lookup, the generated group packages, and the endpoint text.
2. **Rose:**
   - move and rename the IDL files;
   - rename `Block_Device` to `Block` (#841), `Commands` to `Lookup` and `Storage` to `Store`;
   - remove the four unused interfaces;
   - regenerate `endpoint-ids.txt`;
   - update every `with Rose.Interfaces.X` in the tree.

The series check isn't run until the tag exists, so every id can change once now. Any other rename that is wanted for 0.4.x goes in the same change.

## Consequences

- **Every endpoint id changes once,** before the tag, and then not again during 0.4.x.
- **Every unit that uses an interface changes its `with` clauses.** The change is mechanical, but it touches most of the tree, so it should land when few other branches are open.
- **A new interface goes in an existing group,** or a new group if none fits. A new group is an addition, so it is allowed during the series.
- **An interface can't move to another group during the series.** It would have to be added there and retired where it was.
- **0.5.0 can regroup,** as it can change anything.

## Alternatives considered

- **Leave the interfaces flat.** It costs nothing now, but 72 names in one list with no grouping only gets worse, and changing it later waits for 0.5.0.
- **Deeper trees,** such as `Storage.Banks.Bank`. More names to type, for groups of three.
- **Directories for the groups,** `interfaces/storage/bank.idl`. GNAT's file naming is already used for `rose-core.idl`, keeps one include directory, and maps one-to-one to the unit name.
- **References relative to the current group,** as Ada's visibility allows. Shorter, but idle would need visibility rules, and a reader would have to know which group a file is in to read a reference.
- **Groups by server,** such as everything that `mem` serves. An interface outlives its server, and several servers implement some of them, such as `Reader`.

## Open questions

- **Other renames for the series.** Anything that is wanted before the freeze has to be decided now, as #841 was.

## Follow-up work

- idle: dotted interface names, GNAT file lookup, generated group packages, and the endpoint text.
- Rose: the move, the removals, the renames (#841, `Lookup`, `Store`), the new `endpoint-ids.txt`, and the `with` clauses.
- The book's IDL chapter: the package structure.
