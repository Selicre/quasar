# Quasar - a modern SNES assembler

![Logo](https://raw.githubusercontent.com/Selicre/quasar/994d6438cd091a6176b28a9d011f72a82c1698be/logo.svg)

Quasar is an assembler that targets the WDC 65816 architecture and lets you create patches for various SNES ROMs, primarily Super Mario World. The goal of this project is to allow beginners to quickly diagnose the issues with their code and be able to fix them at compile time, rather than having to use a debugger to diagnose crashes.

## Features

Quasar is currently in active development, so all of this is what you should expect by the time it's done:

 - Compatibility with asar
   - Some patches that work in asar may throw errors, but no patch should assemble incorrectly
   - Emulation of asar quirks (e.g. no comparison ops, no whitespace in expressions)
 - Expressive, detailed errors
 - Linting for runtime errors
   - Using the wrong operand size in immediate addressing
   - Executing data
   - Going past segment bounds
 - Opt-in peephole optimizations for certain code patterns
   - `REP #$20 : CLC` -> `REP #$21`
   - Removal of unused instructions

## Roadmap

  - Implement a subset of asar functionality to be more approachable for beginners
  - Implement the rest of (sane) functionality to be useful as a complete replacement
  - Implement own syntax to be useful as a general assembler while still being compatible with asar patches

Eventually, quasar will change from being a subset of asar with good errors into being a superset.
