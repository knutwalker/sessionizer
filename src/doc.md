# Quick start

```sh
$ sessionizer
```

# Usage

`sessionizer` allows you to quickly create and switch to multiple tmux sessions.

It is meant for people who want to manage their various projects as separate tmux session, each containing their own list of windows and tools.
This is in contrast to having one tool (say, neovim) and manage different sessions in that tool.

## Workflow

Invoking `sessionizer` searches a set of paths for directories that would make a good project directory.

`sessionizer` will list open tmux sessions as well as all the directories in an `fzf` like interface (`fzf` does not need to be installed and is not used under the hood).

If an existing session is selected, `sessionizer` will either attach to that session or switch the client to that session, depending on whether you are currently inside a session or not.

When a directory is selected, `sessionizer` will create a new session for that project.
The session will use the directory as their default directory (the location that is used for new windows/panes).

In addition, two environment variables are set:

- `SESSION_NAME`: How the tmux session is named
- `SESSION_ROOT`: The selected directory (e.g. `cd $SESSION_ROOT` will go to the directory)


## Search path configuration

`sessionizer` reads the environment variable `SESSIONIZER_PATH` to determine the search paths.
The value of `SESSIONIZER_PATH` is a colon-separated list of paths, similar to the `PATH` environment variable.

A path that is listed in `SESSIONIZER_PATH` will be suggested as a project directory.
In order to search that path and also suggest subdirectories, special path components `_` and `*` are used as _wildcards_.

The following rules govern how the syntax is used to define how a path is being searched:

* Each component of the path is traversed, but not matched.
* A path must begin with a regular component and cannot be empty
* A leading `~` is expanded to the user's home directory
* A path ending in a regular component will be used directly
* If a path contains a _wildcard_, it will be used as a starting point for a search, but is not included directly
* The _wildcard_ `_` traverses any path on that level, but doesn't include it
* The _wildcard_ `*` traverses _and includes_ any path on that level
* The _wildcards_ must be their own component (e.g. `*_` means a regular component `*_`)
* `\` can be used to escape a _wildcard_ (e.g. `\_` will traverse the directory called `_` as regular component)
* A `_` _wildcard_ *must* be followed by either an `_` _wildcard_ or a `*` _wildcard_
* A `*` _wildcard_ may be followed by another `*` _wildcard_

### Examples

input path    | behavior
--------------|-------------------------------
`/dir`        | suggest `/dir` as a project
`/dir/*`      | suggest all immediate subdirectories of `/dir`, but not `/dir` itself
`/dir/*:/dir` | suggest all immediate subdirectories of `/dir`, as well `/dir` itself
`/dir/_/*`    | suggest all sub-subdirectories of `/dir`, but neither `/dir` itself nor any of its immediate subdirectories
`/dir/*/*`    | suggest all immediate subdirectories and sub-subdirectories of `/dir`, but not `/dir` itself
`/dir/\*`     | suggest `/dir/*` as a project
`/dir/**`     | suggest `/dir/**` as a project


### Fallback behavior

If `SESSIONIZER_PATH` is not set, `sessionizer` will use `CDPATH` instead.
In this case, each entry and treat every path as if it had a `*` wildcard at the end and will skip paths that are not absolute.

If `CDPATH` is also unset, `sessionizer` will query `zoxide` for a list of directories using `zoxide query --list`.

If `zoxide` is not installed, `sessionizer` will use `~/.config/*` as a fallback and print a warning.


## Useful integrations

### TMUX

To open `sessionizer` from within tmux using `<prefix> <C-f>`, add the following to your `~/.tmux.conf`:

```
bind-key -r C-f run-shell 'tmux neww sessionizer'
```

Note that this is **not required** for `sessionizer` to work.
You can also run it directly from the shell.

### Neovim

To open `sessionizer` from inside neovim using `<C-f>`, you can add the following to your config:

```lua
vim.keymap.set("n", "<C-f>", "<cmd>silent !tmux neww sessionizer<CR>")
```

I tend to always have neovim open inside tmux and tend to use the keybinding mentioned above instead of the neovim one.
I find that it is more likely to have a keybind conflict in neovim than in tmux.

### Alacritty

To use `sessionizer` as "shell" or entry point for `alacritty`, add the following to your `alacritty.toml`:

```toml
[shell]
program = "/opt/homebrew/bin/bash" # ! Change this to your shell
args = ["--login", "-c", "/usr/local/bin/sessionizer"] # ! Change this to the path of your sessionizer
```

### Shell

I have the following aliases in my shell:

```sh
alias z='sessionizer --quiet'
alias zz='sessionizer --quiet --tmux-only'
alias cdsessionroot='cd $SESSION_ROOT'
```

The first one allows me to type `z` to open `sessionizer`.
The second one allows me to type `zz` to open only running tmux sessions in `sessionizer`.
The last one allows me to quickly change to the session root directory (the surrounding single quotes `'` are important so that the value is resolved lazily).

## Session initialization

`sessionizer` can be instructed to run some code and customization whenever a new session is created.
This can be done by crating on of two files.

> [!NOTE]
> Session initialization is completely optional.
> `sessionizer` can also be used directly without any configuration.

### TOML initialization

To customize the session using TOML, create a file that is:

   - located in the session root
   - called either **`.sessionizer.toml`** or **`sessionizer.toml`**
         (If both are present, the dotted file takes precedence)
   - has permissions `0400`, `0600`, or `0700`

The TOML file has the following structure (see `example/sessionizer.toml`):

```toml
[env]
ENV_VAR_NAME = "env var value"

[[windows]]
name = "window name"
dir = "window path"
command = "window command"
remain = true

[[run]]
command = "command to run"
```

#### `[env]` section

The `[env]` section is a table of key value pairs.
They define _additional_ environment variables that will be set for every window in the session.

> [!IMPORTANT]
> The values of these variables are visible outside of tmux by inspecting the program invocation.
> Keeping sensitive information (such as API keys) may be exposed to other users on the same machine.

#### `[[windows]]` section

`[windows]` is an array of tables.
New entries can be added by using the `[[windows]]` syntax, which can be repeated multiple times.

For every entry in the `[[windows]]` array, a new tmux window will be spawned.
That window will always spawn in the background.

The table for each `[[windows]]` section supports the following keys:

key | usage | required | aliases
----|-------|----------|--------
`name`    | The name of new window (usually displayed in the tmux status bar)             | No (but recommended, otherwise a name will be derived)              | 
`dir`     | The base directory of that new window. Paths are relative to the session root | No (defaults to session root)                                       | `path`, `workdir`, `wd`, `pwd`, `cwd`
`command` | A command to run inside the new window.                                       | No                                                                  | `cmd`, `run`
`on_exit` | What happens to the window when the command finishes (see below).             | No (default behavior depends on the remain-on-exit setting in tmux) | `keep-alive`, `remain`

##### `[[windows.on_exit]]`

The value for `on_exit` can be either a boolean or a string and allows the following values:

values                                   | usage
-----------------------------------------|------
`"destroy"`, `"kill"`, `false`           | Window will be destroyed after the command finishes. Using `false` might make more sense of the key is called `remain`.
`"keep"`, `"shell"`, `"stay"`, `true`    | Window will be kept alive and drop to the shell when the command finishes. Using `true` might make more sense of the key is called `remain`.
`"deactivate"`, `"inactive"`, `"remain"` | Window will be deactivated after the command finishes. It can be reactivated using the tmux command `respawn-window` (see `man tmux` for details)

`on_exit` is only interpreted when a `command` is set, otherwise it has no effect.

#### `[[run]]` section

`[run]` is an array of tables.
New entries can be added by using the `[[run]]` syntax, which can be repeated multiple times.

For every entry in the `[[run]]` array, a new command will be executed in the first tmux window of the new session.

The table for each `[[run]]` section supports the following key:

key       | usage                                     | required | aliases
----------|-------------------------------------------|----------|-------------
`command` | A command to run inside the first window. | No       | `cmd`, `run`

> [!NOTE]
> This option is similar to using a `command` for a new `[[windows]]` with the `on_exit` option set to `keep`.

### Script based initialization

To customize the session using script file, create a file that is:

   - located in the session root
   - called either **`.sessionizer.init`** or **`sessionizer.init`**
         (If both are present, the dotted file takes precedence)
   - has permissions `0500`, or `0700`

> [!NOTE]
> If both, the TOML and the script file are present, the TOML file takes precedence and the script file will be ignored.
> In order to use both files, add a `[[run.command]]` with the value `source .sessionizer.toml` to the TOML file.

The file is basically a shell script that will be `source`d in the new session.
It can contain any command that you would otherwise type in the shell when starting a session.

> [!TIP]
> To get support in `vim`/`neovim`, you can add the [modeline](https://vimhelp.org/options.txt.html#modeline) `# vim: set ft=bash:`.
> You can adjust the `ft` based on the shell you are using.

# Inspiration

`sessionizer` started of as a more personalized workflow of [The Primeagen's tmux-sessionizer](https://github.com/ThePrimeagen/.dotfiles/blob/602019e902634188ab06ea31251c01c1a43d1621/bin/.local/scripts/tmux-sessionizer).

All additions and modification are based on my personal preferences.
I will likely not add features if I don't find myself wanting or using them (e.g. I typically don't care about predefined layout and panes, so there is no support for those).

