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
ghc --make -isrc -O2 pgminer.hs
```

This will produce a PGminer executable.

### Using PGminer

PGminer expects only one parameter, the `filename.log` with an event log (one space-separated trace per line). It prints the discovered concurrency relation on the standard output and writes a set of obtained partial orders to `filename.cpog`.

#### Example 1: L1 = {abcd, acbd}

Within the same directory as the PGminer executable, create a simple text file with the filename `L1.log`. The body of this text file should contain the following event log representation of L1:

```
a b c d
a c b d
```

Now, to extract the concurrentcy from this event log, run the command:

```bash
./pgminer L1.log
```

This will find one pair of concurrent events: `(b, c)`. The set of extracted partial orders will be written to the file `L1.cpog`. In this case there will be only one partial order represented by the expression `a -> b + a -> c + b -> d + c -> d`.

Events `b` and `c` are declared concurrent because they appear in different orders in the event log (`b` before `c` and reverse) and the order of their occurrence is not indicated by any other events. With `b` and `c` concurrent both traces collapse into the same partial order `a -> (b + c) -> d`, as reported in `L1.cpog`.

#### Example 2: L2 = {abcd,acbd,abce}

Within the same directory as the PGminer executable, create a simple text file with the filename `L2.log`. The body of this text file should contain the following event log representation of L2:

```
a b c d
a c b d
a b c e
```

Now, to extract the concurrentcy from this event log, run the command:

```bash
./pgminer L2.log
```

This will not find any concurrent events, because event `e` indicates the order between `b` and `c`: whenever we observe event `e` in a trace we can be sure that `b` occurs before `c` in that trace. Thus, the resulting CPOG found in `L2.cpog` will contain three total orders matching the given event log:

```
g1 = a -> b + b -> c + c -> d
g2 = a -> b + b -> c + c -> e
g3 = a -> c + b -> d + c -> b
```

## Building and testing with Cabal

You can build and test the library using Cabal. We intend to release the library on Hackage in the near future.

### Build

	cabal build

### Test

	cabal test --show-details=always

### Run PGminer

	cabal run pgminer -- eventLog.tr

`eventLog.tr` can be replaced by any event log you wish to extract the concurrency from. Arguments for PGminer, such as `-split` or the file path should be placed after the `--` in this command, in order to differentiate arguments for cabal from arguments for PGminer. 
