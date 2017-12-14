# Tuura process mining library
[![Build Status](https://travis-ci.org/tuura/process-mining.svg?branch=master)](https://travis-ci.org/tuura/process-mining) [![Build status](https://ci.appveyor.com/api/projects/status/880cv23mcpfx6n4k/branch/master?svg=true)](https://ci.appveyor.com/project/snowleopard/process-mining/branch/master)

This is a collection of algorithms and command line tools for process mining that use Conditional Partial Order Graphs (and more generally Parameterised Graphs) as the underlying modelling formalism.

## Getting the sources

Navigate to a directory to store the library and run this command to clone the code:

`git clone https://github.com/tuura/process-mining.git`

## Building the library and tools

First of all, the Haskell tool Stack must be installed on your machine. This can be downloaded from [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

Enter the process-mining directory and compile the process-mining library and the tools using the following commands:

```bash
stack setup --no-system-ghc
stack build
```

The command line tools can be run using the following commands:

PGminer

```bash
stack exec pgminer -- <Arguments go here>
```

Building PGminer with Stack will produce a binary, which can be found in the `.stack-work` directory, inside the process-mining directory. This binary can be safely moved to a new directory if necessary. If using this binary, then this can simply be executed by running:

```bash
./pgminer <Arguments go here>
```

Macroscope

```bash
stack exec macroscope -- <Arguments go here>
```

## Concurrency extraction with PGminer

PGminer is a tool for extracting concurrency from event logs. It takes a set of event traces, detects potentially concurrent events, and produces a set of partial orders, to be further compressed into a Conditional Partial Order Graph.

PGMiner can also be used to mine event logs containing a set of simulation traces of digital circuits. From graphs generated through mining these simulations, it can generate Boolean functions for a given variable. These functions can then be used to generate behavioural models via another tool, [Plato](https://github.com/tuura/plato)

### Using PGminer

The following usage info can be obtained by running `stack exec pgminer -- --help` or `pgminer --help`:

```
Usage: pgminer [input file] [OPTION...]
  -c         --concurrency-report  Print a list of concurrent pairs
  -o FILE    --output=FILE         Write output to a file
  -s         --split               Split traces with multiple event occurrences
  -b STRING  --bool=STRING         Variable for which a boolean function should be generated
  -h         --help                Show this help message
```

Given an event log (one space-separated trace per line) in standard input or an input file, PGminer prints a set of obtained partial orders to the standard output or to a specified output file. The discovered concurrency relation can also be printed out using `-c` option.

#### Example 1: L1 = {abcd, acbd}

Within the process-mining directory (or the same directory as the PGminer executable) create a simple text file with the filename `L1.log`. The body of this text file should contain the following event log representation of L1:

```
a b c d
a c b d
```

Now, to extract the concurrentcy from this event log, run the command:

```bash
stack exec pgminer -- L1.log -o L1.res -c
```

This will find one pair of concurrent events: `(b, c)`. The set of extracted partial orders will be written to the file `L1.res`. In this case there will be only one partial order represented by the expression `a -> b + a -> c + b -> d + c -> d`.

Events `b` and `c` are declared concurrent because they appear in different orders in the event log (`b` before `c` and reverse) and the order of their occurrence is not indicated by any other events. With `b` and `c` concurrent both traces collapse into the same partial order `a -> (b + c) -> d`, as reported in `L1.res`.

#### Example 2: L2 = {abcd,acbd,abce}

Within the process-mining directory (or the same directory as the PGminer executable) create a simple text file with the filename `L2.log`. The body of this text file should contain the following event log representation of L2:

```
a b c d
a c b d
a b c e
```

Now, to extract the concurrentcy from this event log, run the command:

```bash
stack exec pgminer -- L2.log -o L2.res
```

This will not find any concurrent events, because event `e` indicates the order between `b` and `c`: whenever we observe event `e` in a trace we can be sure that `b` occurs before `c` in that trace. Thus, the resulting set of partial orders found in `L2.res` will contain three total orders matching the given event log:

```
p1 = a -> b + b -> c + c -> d
p2 = a -> b + b -> c + c -> e
p3 = a -> c + b -> d + c -> b
```

#### Example 3: Generating Boolean functions

Within the process-mining directory (or the same directory as the PGminer executable) create a simple text file with the filename `L3.log`. The body of this text file should contain the following, which is the simulation traces of a simple circuit:

```
a+ b+ c+
b+ a+ c+
a+ c+
b+ c+
b- a- c-
a- b- c-
```

Each line of this is the simulation trace of a circuit, identifying the order in which signals transition. This can be mined by PGminer, identifying concurrency. A common variable (or signal) in each of these traces is `c`. In some cases, signals transition high (identified by `+`) and in others, they transition low (`-`). Some signals must transition in either of these directions in order to cause the signal `c` to transition.

We can use PGminer to produce Boolean functions, one for `c+` and one for `c-`. These functions will identify the way signals `a` and `b` must have transitioned in order for the given `c` transition to occur, using AND (`&`) and OR (`|`) functions.

To mine this specification and produce a Boolean function, we must use the `-b` or `--bool` flag, and provide a signal transition for which the generated Boolean function will cause. First of all, for `c+` we run:

```bash
stack exec pgminer -- L3.log -b c+
```

This will produce the following function:

```
a & b | a | b
```

This states that, either `a` AND `b` must be high, OR `a` only must be high, OR `b` only must be high, in order for `c+` to occur. This is not the most simplified Boolean function, but this is a correct function, and tools such as [Plato](https://github.com/tuura/plato) will simplify this further.

To generate a function for `c-`, we use a similar command as for `c+`:

```bash
stack exec pgminer -- L3.log -b c-
```

The result will be the following function:

```
a' & b'
```

This states that, in order for `c-` to occur, both `a` AND `b` must be low.

## Building, testing and running with Stack

You can build the library and executables using Stack. We intend to release the sources on Hackage in the near future.

### Build

	stack build

### Test

	stack test

### Run PGminer

	stack exec pgminer -- file.log

Here `file.log` is an input event log. Other arguments for PGminer, such as `--split` or the output file path should be placed after the `--` separator, in order to differentiate arguments for Cabal from arguments for PGminer.
