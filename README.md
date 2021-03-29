A set of stack templates and scripts for setting up my directory to compete in Google Code Jam using Haskell/Stack. For my tooling I'm currently using stack, neovim and Haskell Language Server with coc. I've tried to include most of the packages available to Google server's Haskell setup in the stack project configuration.

# Usage

Make sure stack is installed.

```
./jam [name-of-package] [first-problem] [second-problem] [third-problem] … [fifth-problem]
```

Example:

```
./jam 2011qualifier bot candy magicka goro
```

Then you may need to run `gen-hie » hie.yaml` inside the new directory to set up the the Haskell Language Server cradle.

The script creates a file in the `src` directory for every problem name given, with some boilerplate input written. It runs on any number of problem names from one to five. I haven't seen any more than five problems in a single Code Jam round so I've left it at that.

## Running a problem

```
stack build
stack run [problem-name]
```

Unfortunately the way my stack project is set up seems to build all of the source files just to run one of the executables, which means you can't leave unfinished problems with compilation errors while jumping to another problem.

## Debugging a problem with GHCI

```
stack repl [package-name]:exe:[problem-name]
```

# Packages included by default

- vector
- containers
- unordered-containers
- array
- transformers
- primitive

# Snippets

I've collected some snippets into the `snippets` directory from completing past problems that might come in handy in future problems.
