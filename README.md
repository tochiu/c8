<h1 align="center">
  <b>C8</b>
</h1>

<h4 align="center"><b>CHIP-8 / S-CHIP / XO-CHIP</b> tui toolkit featuring a virtual machine, debugger, and disassembler</h4>

<p align="center">
  <a href="#about">About</a> •
  <a href="#features">Features</a> •
  <a href="#installation">Installation</a> •
  <a href="#usage">Usage</a> •
  <a href="#motivation">Motivation</a>
</p>

---

<p align="center">
 <img src="https://github.com/tochiu/c8/blob/main/img/super_neatboy_debug.jpg?raw=true" alt="C8 Screenshot">
</p>

---

## About

C8 is a terminal user interface tooklit made to run and debug **CHIP-8**, **S-CHIP**, and **XO-CHIP** games. At its core you can 
* run a rom with `c8 run [ROM_PATH]`
    * add `--debug` to enable debug mode
    * add `--kind` followed by `vip`, `chip8`, `schip`, or `xochip` to force other CHIP-8 variants if auto-select fails
    * add `--hz` followed by your target instructions per second if needed
* disassemble a rom into a file with `c8 dasm [ROM_PATH] > [OUTPUT_FILE_PATH]`
* check a rom for potential issues* with `c8 check [ROM_PATH]`

## Features
| Feature                                            | C8
|----------------------------------------------------|-
| Full Chip-8 + VIP, SuperChip, and XO-Chip Support  | ✔
| Full Sound Support                                 | ✔
| Full 4-bit Color Support                           | ✔
| `debug` Undo, Redo, and Step through Execution     | ✔
| `debug` Virtual Machine State Introspection        | ✔
| `debug` Register and Address Watchpoints           | ✔
| `debug` Instruction Breakpoints                    | ✔
| `debug` Program Execution History                  | ✔
| `debug` Keyboard State Modification                | ✔
| `debug` Dump Current Program Memory State          | ✔
| Static Tracing Disassembler                        | ✔
| Configurable Execution Speed                       | ✔
| Compatibility Profiles                             | ✔
| Pre-defined and Custom Color Palettes              | 🚧
| Individual Configurable Quirks                     | 🚧
###### Features labeled `debug` are only available in debug mode

### Todo
- [ ] Reload Rom (Preserve RPL Flags)
- [ ] Pre-defined and Custom Color Palettes
- [ ] Individual Configurable Quirks

## Installation

C8 is tested mostly on Windows but should work on Mac and Linux. If on Linux, see [below](#caveats) for required system packages before you continue.

At this moment C8 can only be installed from source with [cargo](https://crates.io/). Rust 1.65.0 or greater is required.

```
git clone https://github.com/tochiu/c8.git
cd c8
cargo install --path ./
```
To be sure C8 is installed, run the classic IBM Logo ROM from the repository directory
```
c8 run roms/c8/ibm_logo.ch8
```

### Caveats

On Linux, the X11 development libraries are required to query keyboard state from the OS since terminals generally do not support key up events. In addition, the Advanced Linux Sound Architecture (ALSA) development libraries are required on the system.

On Ubuntu/Debian:
```
sudo apt install libx11-dev
sudo apt install librust-alsa-sys-dev
```

On Fedora/RHEL/CentOS:
```
sudo dnf install xorg-x11-server-devel
sudo dnf install alsa-lib-devel
```

On newer versions of MacOS, you may run into issues where you only see meta keys such as shift,
backspace, et cetera. This is due to a permission issue. To work around this:

* open the MacOS system preferences
* go to Security -> Privacy
* scroll down to Accessibility and unlock it
* add your terminal to the list

<h1><b>⚠️ BELOW THIS LINE IS A WORK IN PROGRESS ⚠️</b></h1>

## Usage

### c8 run
```
Usage: c8 run [OPTIONS] <ROM>

Arguments:
  <ROM>  Path of the ROM to load

Options:
  -d, --debug        Runs the ROM in debug mode
      --hz <HZ>      Sets the instructions executed per second [default: 2000]
  -l, --log <LEVEL>  Enable logging [possible values: trace, debug, info, warn, error]
      --kind <KIND>  Sets the ROM kind [possible values: chip8, schip, cosmacvip, xochip]
  -h, --help         Print help information
```
### c8db (debugger) commands
```
Usage: <COMMAND>

Commands: 
  continue  Continue running the program until the next breakpoint, watchpoint or error [aliases: c, cont]
  step      Run the next N (default = 1) instructions of the program [aliases: s]
  hertz     Set the instructions executed per second of the program [aliases: hz, ips, rate, freq, frequency]
  redo      Redo the next N (default = 1) instructions within the program history [aliases: ff, fast-forward, >>]
  undo      Undo the last N (default = 1) instructions within the program history [aliases: rw, rewind, <<]
  history   Navigate the program history view [aliases: hist]
  output    Navigate the output view [aliases: o, out]
  memory    Navigate the memory view [aliases: m, mem]
  goto      Go to a location in memory [aliases: g]
  follow    Follow a pointer in memory [aliases: f]
  unfollow  Unfollow the followed pointer [aliases: uf]
  break     Set a breakpoint at an address [aliases: b]
  watch     Watch a register, pointer, or address for change [aliases: w]
  show      Execute show subcommand
  hide      Execute hide subcommand
  info      Execute info subcommand [aliases: i]
  key       Execute keyboard subcommand [aliases: k]
  clear     Execute clear subcommand [aliases: clr]
  dump      Execute dump subcommand [aliases: d]
  help      Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help information
  -V, --version  Print version information
```

### c8 dasm
```
Disassembles a CHIP-8 ROM

Usage: c8 dasm [OPTIONS] <ROM>

Arguments:
  <ROM>  Path of the ROM to load

Options:
  -l, --log <LEVEL>  Enable logging [possible values: trace, debug, info, warn, error]
      --kind <KIND>  Sets the ROM kind [possible values: chip8, schip, cosmacvip, xochip]
  -h, --help         Print help information
```
### c8 check
```
Statically checks a CHIP-8 ROM for potential issues

Usage: c8 check [OPTIONS] <ROM>

Arguments:
  <ROM>  Path of the ROM to load

Options:
  -l, --log <LEVEL>  Enable logging [possible values: trace, debug, info, warn, error]
      --kind <KIND>  Sets the ROM kind [possible values: chip8, schip, cosmacvip, xochip]
  -h, --help         Print help information
```
## Motivation
This is my first _completed_ rust project (haha). A friend of mine sent me an [article](https://tobiasvl.github.io/blog/write-a-chip-8-emulator/) on how to get started with writing emulators with CHIP-8. It was a super interesting read and a good excuse to learn Rust! After I finished the emulator, I thought I could go further. So here we are. If you're thinking about writing your own CHIP-8 emulator, you should! It's a great start to emulation development and building on top of it with other CHIP-8 variants is an excellent exercise in writing extensible software.