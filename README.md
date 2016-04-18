# Tuura process mining library
[![Build Status](https://travis-ci.org/tuura/process-mining.svg?branch=master)](https://travis-ci.org/tuura/process-mining) [![Build status](https://ci.appveyor.com/api/projects/status/880cv23mcpfx6n4k/branch/master?svg=true)](https://ci.appveyor.com/project/snowleopard/process-mining/branch/master)

This is a collection of algorithms and command line tools for process mining that use Conditional Partial Order Graphs (and more generally Parameterised Graphs) as the underlying modelling formalism.

## Concurrency extraction with PGminer

PGminer is a tool for extracting concurrency from event logs. It takes a set of event traces, detects potentially concurrent events, and produces a set of partial orders, to be further compressed into a Conditional Partial Order Graph.

### Getting the sources

Navigate to a directory to store PGminer and use this command to clone the code:

`git clone https://github.com/tuura/process-mining.git`

### Building PGminer

Enter the process-mining directory and compile PGminer using the following command:

```bash
ghc --make -isrc -ipgminer -O2 pgminer/Main.hs -o pgminer
```

This will produce a PGminer executable.

### Using PGminer

The following usage info can be obtained by running `pgminer --help`:
```
Usage: pgminer [input file] [OPTION...]
  -c       --concurrency-report  Print a list of concurrent pairs
  -o FILE  --output=FILE         Write output to a file
  -s       --split               Split traces with multiple event occurrences
  -h       --help                Show this help message
```
Given an event log (one space-separated trace per line) in standard input or an input file, PGminer prints a set of obtained partial orders to the standard output or to a specified output file. The discovered concurrency relation can also be printed out using `-c` option.

#### Example 1: L1 = {abcd, acbd}

Within the same directory as the PGminer executable, create a simple text file with the filename `L1.log`. The body of this text file should contain the following event log representation of L1:

```
a b c d
a c b d
```

Now, to extract the concurrentcy from this event log, run the command:

```bash
./pgminer L1.log -o L1.res -c
```

This will find one pair of concurrent events: `(b, c)`. The set of extracted partial orders will be written to the file `L1.res`. In this case there will be only one partial order represented by the expression `a -> b + a -> c + b -> d + c -> d`.

Events `b` and `c` are declared concurrent because they appear in different orders in the event log (`b` before `c` and reverse) and the order of their occurrence is not indicated by any other events. With `b` and `c` concurrent both traces collapse into the same partial order `a -> (b + c) -> d`, as reported in `L1.res`.

#### Example 2: L2 = {abcd,acbd,abce}

Within the same directory as the PGminer executable, create a simple text file with the filename `L2.log`. The body of this text file should contain the following event log representation of L2:

```
a b c d
a c b d
a b c e
```

Now, to extract the concurrentcy from this event log, run the command:

```bash
./pgminer L2.log -o L2.res
```

This will not find any concurrent events, because event `e` indicates the order between `b` and `c`: whenever we observe event `e` in a trace we can be sure that `b` occurs before `c` in that trace. Thus, the resulting set of partial orders found in `L2.res` will contain three total orders matching the given event log:

```
p1 = a -> b + b -> c + c -> d
p2 = a -> b + b -> c + c -> e
p3 = a -> c + b -> d + c -> b
```

## Building, testing and running with Cabal

You can build the library and PGminer using Cabal. We intend to release the sources on Hackage in the near future.

### Build

	cabal build

### Test

	cabal test --show-details=always

### Run PGminer

	cabal run pgminer -- file.log

Here `file.log` is an input event log. Other arguments for PGminer, such as `--split` or the output file path should be placed after the `--` separator, in order to differentiate arguments for Cabal from arguments for PGminer. 
