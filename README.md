# sessionizer [![CI Status][ci-badge]][ci-url] [![Crates.io][crates-badge]][crates-url] [![Docs][docs-badge]][docs-url] ![License: MIT OR Apache-2.0][license-badge] ![Rust Version: 1.75.0][rust-version-badge]

[ci-badge]: https://github.com/knutwalker/sessionizer/actions/workflows/checks.yml/badge.svg
[ci-url]: https://github.com/knutwalker/sessionizer
[crates-badge]: https://img.shields.io/crates/v/sessionizer?style=shield
[crates-url]: https://crates.io/crates/sessionizer
[docs-badge]: https://img.shields.io/badge/docs-latest-blue.svg?style=shield
[docs-url]: https://docs.rs/sessionizer
[license-badge]: https://img.shields.io/badge/license-MIT%20OR%20Apache--2.0-blue.svg?style=shield
[rust-version-badge]: https://img.shields.io/badge/rustc-1.75.0-orange.svg?style=shield

Select a new tmux session from a list of running sessions or a selection of projects.

> [!WARNING]
> Very much a work in progress.
> As it is, it's probably only working for me.
> Configurability might come later.

# Installation

> [!IMPORTANT]
> Windows is not supported. Using WSL might work, though it is not tested.

## Via Cargo

### Released version

```sh
cargo install sessionizer
```

### Development version

```sh
cargo install --git https://github.com/knutwalker/sessionizer
```

## From source

```sh
make; make install
```

These steps might have to be adapted to your environment (e.g. elevating privileges for `make install`).
On macOS, you have to use `gmake` from homebrew.

## Quick start

```sh
$ sessionizer
```

## Usage

`sessionizer` allows you to quickly create and switch to multiple tmux sessions.

It is meant for people who want to manage their various projects as separate tmux session, each containing their own list of windows and tools.
This is in contrast to having one tool (say, neovim) and manage different sessions in that tool.

### Workflow

Invoking `sessionizer` searches a set of paths for directories that would make a good project directory.

 > 
 > [!CAUTION]
 > The heuristics for which paths are search and what makes a “good” project are currently based
 > my personal preferences and will likely not work for others.

`sessionizer` will list open tmux sessions as well as all the directories in an `fzf` like interface (`fzf` does not need to be installed and is not used under the hood).

If an existing session is selected, `sessionizer` will either attach to that session or switch the client to that session, depending on whether you are currently inside a session or not.

When a directory is selected, `sessionizer` will create a new session for that project.
The session will use the directory as their default directory (the location that is used for new windows/panes).

In addition, two environment variables are set:

* `SESSION_NAME`: How the tmux session is named
* `SESSION_ROOT`: The selected directory (e.g. `cd $SESSION_ROOT` will go to the directory)

#### Useful integrations

##### TMUX

To open `sessionizer` from within tmux using `<prefix> <C-f>`, add the following to your `~/.tmux.conf`:

```rust
bind-key -r C-f run-shell 'tmux neww sessionizer'
```

Note that this is **not required** for `sessionizer` to work.
You can also run it directly from the shell.

##### Neovim

To open `sessionizer` from inside neovim using `<C-f>`, you can add the following to your config:

```lua
vim.keymap.set("n", "<C-f>", "<cmd>silent !tmux neww sessionizer<CR>")
```

I tend to always have neovim open inside tmux and tend to use the keybinding mentioned above instead of the neovim one.
I find that it is more likely to have a keybind conflict in neovim than in tmux.

##### Alacritty

To use `sessionizer` as “shell” or entry point for `alacritty`, add the following to your `alacritty.toml`:

```toml
[shell]
program = "/opt/homebrew/bin/bash" # ! Change this to your shell
args = ["--login", "-c", "/usr/local/bin/sessionizer"] # ! Change this to the path of your sessionizer
```

##### Shell

I have the following aliases in my shell:

```sh
alias z='sessionizer --quiet'
alias zz='sessionizer --quiet --tmux-only'
alias cdsessionroot='cd $SESSION_ROOT'
```

The first one allows me to type `z` to open `sessionizer`.
The second one allows me to type `zz` to open only running tmux sessions in `sessionizer`.
The last one allows me to quickly change to the session root directory (the surrounding single quotes `'` are important so that the value is resolved lazily).

### Session initialization

`sessionizer` can be instructed to run some code and customization whenever a new session is created.
This can be done by crating on of two files.

 > 
 > [!NOTE]
 > Session initialization is completely optional.
 > `sessionizer` can also be used directly without any configuration.

#### TOML initialization

To customize the session using TOML, create a file that is:

* located in the session root
* called either **`.sessionizer.toml`** or **`sessionizer.toml`**
  (If both are present, the dotted file takes precedence)
* has permissions `0400`, `0600`, or `0700`

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

##### `[env]` section

The `[env]` section is a table of key value pairs.
They define *additional* environment variables that will be set for every window in the session.

 > 
 > [!IMPORTANT]
 > The values of these variables are visible outside of tmux by inspecting the program invocation.
 > Keeping sensitive information (such as API keys) may be exposed to other users on the same machine.

##### `[[windows]]` section

`[windows]` is an array of tables.
New entries can be added by using the `[[windows]]` syntax, which can be repeated multiple times.

For every entry in the `[[windows]]` array, a new tmux window will be spawned.
That window will always spawn in the background.

The table for each `[[windows]]` section supports the following keys:

|key|usage|required|aliases|
|---|-----|--------|-------|
|`name`|The name of new window (usually displayed in the tmux status bar)|No (but recommended, otherwise a name will be derived)||
|`dir`|The base directory of that new window. Paths are relative to the session root|No (defaults to session root)|`path`, `workdir`, `wd`, `pwd`, `cwd`|
|`command`|A command to run inside the new window.|No|`cmd`, `run`|
|`on_exit`|What happens to the window when the command finishes (see below).|No (default behavior depends on the remain-on-exit setting in tmux)|`keep-alive`, `remain`|

###### `[[windows.on_exit]]`

The value for `on_exit` can be either a boolean or a string and allows the following values:

|values|usage|
|------|-----|
|`"destroy"`, `"kill"`, `false`|Window will be destroyed after the command finishes. Using `false` might make more sense of the key is called `remain`.|
|`"keep"`, `"shell"`, `"stay"`, `true`|Window will be kept alive and drop to the shell when the command finishes. Using `true` might make more sense of the key is called `remain`.|
|`"deactivate"`, `"inactive"`, `"remain"`|Window will be deactivated after the command finishes. It can be reactivated using the tmux command `respawn-window` (see `man tmux` for details)|

`on_exit` is only interpreted when a `command` is set, otherwise it has no effect.

##### `[[run]]` section

`[run]` is an array of tables.
New entries can be added by using the `[[run]]` syntax, which can be repeated multiple times.

For every entry in the `[[run]]` array, a new command will be executed in the first tmux window of the new session.

The table for each `[[run]]` section supports the following key:

|key|usage|required|aliases|
|---|-----|--------|-------|
|`command`|A command to run inside the first window.|No|`cmd`, `run`|

 > 
 > [!NOTE]
 > This option is similar to using a `command` for a new `[[windows]]` with the `on_exit` option set to `keep`.

#### Script based initialization

To customize the session using script file, create a file that is:

* located in the session root
* called either **`.sessionizer.init`** or **`sessionizer.init`**
  (If both are present, the dotted file takes precedence)
* has permissions `0500`, or `0700`

 > 
 > [!NOTE]
 > If both, the TOML and the script file are present, the TOML file takes precedence and the script file will be ignored.
 > In order to use both files, add a `[[run.command]]` with the value `source .sessionizer.toml` to the TOML file.

The file is basically a shell script that will be `source`d in the new session.
It can contain any command that you would otherwise type in the shell when starting a session.

 > 
 > [!TIP]
 > To get support in `vim`/`neovim`, you can add the [modeline][__link0] `# vim: set ft=bash:`.
 > You can adjust the `ft` based on the shell you are using.

### Inspiration

`sessionizer` started of as a more personalized workflow of [The Primeagen’s tmux-sessionizer][__link1].

All additions and modification are based on my personal preferences.
I will likely not add features if I don’t find myself wanting or using them (e.g. I typically don’t care about predefined layout and panes, so there is no support for those).


## License

sessionizer is licensed under either of the following, at your option:

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT License ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

---
 [__link0]: https://vimhelp.org/options.txt.html#modeline
 [__link1]: https://github.com/ThePrimeagen/.dotfiles/blob/602019e902634188ab06ea31251c01c1a43d1621/bin/.local/scripts/tmux-sessionizer
