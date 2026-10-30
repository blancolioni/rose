# 21. Privilege: role categories, a rights broker, and sudo

- **Status:** Proposed
- **Date:** 2026-10-30
- **Related:** [ADR 0001](0001-capability-revocation.md) (revocation), [ADR 0002](0002-typed-commands-and-directory-caps.md) (typed commands and roles), [ADR 0003](0003-directories-files-and-users.md) (users), [ADR 0004](0004-restore-notification.md) (restore notification), [ADR 0012](0012-argument-modes-writable-interfaces-and-transactions.md) (argument modes), [ADR 0018](0018-credentials-after-restore.md) (credentials after a restore), [ADR 0020](0020-clock.md) (the clock), #494 (the clock), #455 (auth), #457 (login), #458 (petal), blancolioni/idle#63 (`Boolean` in operations)

## Context

Rose has no superuser, because it has no ambient authority. A process can do only what the caps that it holds allow, and nothing in the kernel checks who it is running as. A "superuser" is therefore just someone who can get certain caps that others can't.

The clock is the first thing that needs the difference. `Clock_Admin.Set_Realtime` (ADR 0020) should not be available to every user, and nothing outside the system should be able to set the time by default. Other caps will follow: creating users (`Auth_Admin`), the storage and log partitions, the network's configuration.

### What exists today

- **Commands get caps through roles** that they declare (ADR 0002). A command with no `Clock_Service` role can't read the clock, and one that declares it gets the cap from petal. Petal gives a role only if it holds the cap itself, and fails the launch with a message if it doesn't.
- **The chain of caps is short and explicit.** The clock cap goes from init to login, to petal, to the command (#544). Each hands on what it was given.
- **Users are a name, a password and a `User` cap** (ADR 0003, #455). Holding a user's cap is being that user. `Auth.Login` checks a password and returns it.
- **Caps can be derived and rescinded** (ADR 0001), and a login's caps can be suspended until the user authenticates again after a restore (ADR 0018).
- **Nobody holds `Clock_Admin`.** The restore server gives init the admin cap when it installs the clock, and init keeps only the `Clock` ancestor.

### What it should do

- **Be explicit.** A user should have to say that a command is to run with more authority, for that command, and not hold it for the whole session.
- **Be finer than two classes.** "Public" and "privileged" are too few: the right to set the time is not the right to create users, and one administrator may have one and not the other.
- **Not add a second shell.** Petal parses a command's signature, binds its arguments and streams, and sets up its membrane. A launcher that runs another command with more caps would have to do all of that again, with different bugs.
- **Survive a restore without leaving authority behind.** A restore ends authentication (ADR 0018), so it should end elevation as well.

## Decision

### 1. Roles have a category

A role declaration can name a **category**, which says what kind of authority the role grants:

```
role Clock_Admin_Service : Clock_Admin with Category => "time";
```

- **A role with no category is public.** It is what every role is today. Anyone's command can have it, and petal binds it from the caps it holds.
- **A category is a name, with dots for parts.** `time`, `accounts`, `hardware.ports`, `storage.log`. A policy for a category covers the names below it (`hardware` covers `hardware.ports`), and the most specific policy wins. A role has one category.
- **The IDL compiler records it** in the command's metadata, as a `category` line for the role next to its `cap` line, so that petal, `--help` and anything else that reads a signature can see it before launch.
- **The compiler doesn't know what a category means.** It checks the syntax. What a category allows is the policy's business (section 4).
- **A command that inherits a role inherits its category.** A command with two categorised roles has both.

The metadata is for early refusal and for help text. It isn't what grants anything: the broker has its own table of roles and categories, built from the roles it was declared with (section 3), and petal's say-so about a category means nothing to it.

### 2. Petal never holds privileged caps

Petal binds a public role from the caps it holds, as now. For a categorised role it asks the broker, and binds what it is given.

- **Without `sudo`,** petal asks for a role with `Elevate` false. The broker grants it only if the user's policy for the category is `granted` (section 4). Otherwise petal refuses, before launching the command: `date: needs elevation (time)`.
- **`sudo COMMAND ARGUMENTS` is a petal built-in.** It removes `sudo`, finds `COMMAND` as usual, and launches it exactly as it would, but asks the broker with `Elevate` true for each categorised role. A command with only public roles runs as it would without it.
- **`sudo` is not a program,** so there is nothing that holds more authority than petal and runs on behalf of a user, and the command is launched by the same code that launches every command.
- **Petal hands the caps to the command and deletes its own copies.** They are held by the job and nowhere else in the session, and cross its membrane like any other cap (ADR 0002).
- **When the command exits,** petal tells the broker, which rescinds the derived cap (ADR 0001). A copy that the command passed on stops working too.

### 3. The broker

A new persistent server, **rights**, holds the caps that grant authority and decides who gets them.

- **It is a command, launched by init like `auth`.** It declares the categorised roles that it holds: `Clock_Admin_Service`, and later one for each privileged cap. Init is the only process that has the originals, from the installer and the kernel, so privileged caps go from their source to init to the broker, and no further.
- **Its table is its declaration.** Each role's interface and category come from the IDL, so they are checked when it is built.
- **Persistent,** so that its policy and its table of grants are in each checkpoint, and the caps that other persistent processes hold to it stay valid.

It has two interfaces, with different holders.

```
interface Rights is

   function Request
     (User    : interface User;
      Role    : String;
      Elevate : Word)
      return Capability;
   --  A cap for Role, derived from the broker's own, or none.
   --  Elevate is 1 under sudo. (A Boolean when blancolioni/idle#63 is
   --  done.)

   procedure Release (Grant : Capability);
   --  The command that was given Grant has finished.

end Rights;

interface Rights_Admin is

   procedure Allow
     (User     : String;
      Category : String;
      Mode     : Word);
   --  Set how a user may get a category (section 4).

end Rights_Admin;
```

- **`Rights` goes to petal,** through login, as the clock cap does. It is a public role.
- **`Rights_Admin` is a categorised role,** `Category => "rights"`, so changing who may do what is itself something that a user has to be allowed to do, and has to elevate for.
- **The user is who `User` says.** The broker asks the `User` cap its name. A cap can't be forged, and holding it is being that user (ADR 0003), so petal doesn't have to be trusted about who is asking.
- **A grant is a derived cap** (ADR 0001, layer 1). The broker keeps the list of live grants, so that it can rescind each of them.

### 4. Policy

A policy is a **mode** for a user and a category:

| Mode | Meaning |
|---|---|
| `none` | The user can't get the category. This is the default for every user and category. |
| `elevate` | The user gets it under `sudo` only. |
| `granted` | The user gets it in an ordinary launch. |

- **The first user starts with `elevate` for every category,** set by init when it creates them. Every later user starts with `none`, and an administrator gives them what they need with `Allow`, under `sudo`.
- **The policy is the broker's state,** in its memory. It isn't a credential, so a restore doesn't change it (section 5).
- **A category that no one has written a policy for is `none`.** A new kind of authority is closed until someone opens it.
- **There is no password prompt yet.** Anything that runs as the user, holding their `User` cap, can ask for `Elevate` true. So until re-authentication exists (see the open questions), `sudo` guards against a mistake and against a user who may not elevate. It doesn't guard against something malicious that is running as one who may.

### 5. Restore ends elevation

The broker is `Restorable` (ADR 0004). On `Restored` it rescinds every live grant, before it handles any other request.

- **Elevation is derived from an authentication,** and a restore ends authentication (ADR 0018), so the authority that it granted ends too.
- **A job that was running under `sudo` loses its privileged caps.** Its next use of one fails with `Invalid_Capability`. A long job has to be run again under `sudo`.
- **The user's own session is as ADR 0018 says.** Their ordinary caps are suspended until they unlock it, and then come back. Elevation doesn't: it is asked for again.

### 6. Audit

The broker writes a line to the log for every request, with the user, the role, the category, whether it was elevated, and whether it was granted. A refusal is as visible as a grant.

### 7. The first use: setting the time

- **A role `Clock_Admin_Service : Clock_Admin` with `Category => "time"`** (`interfaces/clock_admin_service.idl`).
- **A command that declares it sets the clock** with `Set_Realtime`, and is run as `sudo COMMAND TIME`. Without `sudo` petal says `COMMAND: needs elevation (time)`.
- **`date -s` is the natural form,** and it raises a question. A command's roles are fixed by its declaration, and plain `date` must not need elevation, so the signature has to say that the role goes with `-s`, and petal has to bind it only when `-s` is given (see the open questions).
- **A separate command doesn't have the question.** A command that only sets the time, such as `set_date`, can declare the role outright. It is the first step if the signature change isn't ready, and `date -s` can replace it later.

## Consequences

- **The kernel doesn't change.** There is no superuser flag and no uid. Everything is caps, derived and rescinded, as before.
- **Privileged caps are in three places:** their source, init, and the broker. Not in petal, login, or any user's session.
- **A privilege is visible before a command runs.** `--help` shows the category, and petal refuses early with the name of what is missing.
- **Finer than "root".** A user can be allowed `time` and not `accounts`, and a later `network.dns` can be given apart from `network`.
- **`sudo` is petal's, not a program.** Another shell would need its own, which is a reason to keep the code in a library when there is another.
- **A restore ends every elevated job.** That is the strictest choice, and the cost is that someone who ran a long job under `sudo` has to run it again.
- **idle, petal, init and a new server change.** A role aspect and a metadata line, a built-in and a refusal, the broker's start and its first policy, and the broker itself.
- **Until there is a password prompt, `sudo` is a convenience and a policy,** and not a defence against code running as an administrator.

## Alternatives considered

- **A `sudo` program that launches the command.** It would need a cap to launch anything, so it would hold more authority than any user. It would also have to bind arguments, wire streams and make a membrane, as petal does.
- **A superuser identity,** a uid that the kernel or servers check. It is ambient authority again, and everything that checks it has to be trusted to check it correctly. Rose's design is that there is nothing to check.
- **Petal holds the privileged caps and decides.** Petal is the user's own process, and runs what the user types. A bug in it, or a command that exploits it, would have every privilege.
- **Rights as groups,** held for the whole session. It is simple, and it makes the authority ambient within the session, so a mistaken command in a session with the right is as dangerous as one run by root.
- **Rights in the `User` cap,** returned by `Auth.Login`. It ties rights to a login and not to a command, and puts policy in the user server. The broker can hold the same policy while keeping it apart from the credentials.
- **Two classes only: public and privileged.** It can't tell the right to set the time from the right to make users.
- **A mark only in the broker's policy, not in the IDL.** Petal couldn't refuse early or tell `--help` what a command needs.
- **A password prompt now.** It is the right next step, and it needs a prompt that isn't petal's (a process that reads the keyboard while a command runs). It is left out so that the rest can be built and tested first.

## Open questions

- **Re-authentication.** The broker should be able to ask `Auth` to check the user's password before a grant, through a prompt that petal can't answer for the user. Probably through the console, like the lock in ADR 0018. A time after which `sudo` asks again, as sudo does, is a policy on top.
- **Which arguments want which roles.** `date -s` needs `Clock_Admin_Service`, and `date` doesn't. A role belongs to a command, so either it is bound for every launch of `date` (and a plain `date` would need elevation, which is wrong) or the signature says that a role goes with an argument. The second looks right, and is a change to the IDL's metadata.
- **How a category is named and registered.** A free name risks two spellings of one thing. A list in a file that the IDL compiler checks would prevent it.
- **Pipelines.** `sudo` applies to one command. How `sudo a | b` reads, and whether `b` can inherit it, is not decided. The proposal is that it applies to `a` only.
- **Where the log goes.** The kernel log, a file in a system directory, or the console, and how it is kept from growing without bound.
- **A privileged job at a restore.** Rescinding every grant is simple. A job that has to survive might hold a grant the broker re-issues when the user authenticates again.
- **Granting `granted`.** Whether `granted` should exist at all. The case for it is a machine that has one user, and the case against is that it brings back the groups that the alternatives rejected.

## Follow-up work

- idle: a `Category` aspect on roles, and a `category` line in command metadata.
- The `rights` server, with `Rights` and `Rights_Admin`, and a policy that survives a checkpoint.
- Init: keep the `Clock_Admin` cap from the installer, start `rights` with it, and set the first user's policy.
- Petal: ask the broker for a categorised role, refuse a launch that lacks it, and the `sudo` built-in.
- A command that sets the time, with the role `Clock_Admin_Service`, and a boot test that sets the time, reads it, and reads it again after a restore (the RTC write, which nothing covers yet).
- A password check before a grant, when the console can prompt (ADR 0018, #507).
- A section in the Rose Book.
