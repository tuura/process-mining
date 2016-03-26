# process-mining
[![Build Status](https://travis-ci.org/tuura/process-mining.svg?branch=master)](https://travis-ci.org/tuura/process-mining) [![Build status](https://ci.appveyor.com/api/projects/status/880cv23mcpfx6n4k/branch/master?svg=true)](https://ci.appveyor.com/project/snowleopard/process-mining/branch/master)

A library for process mining

### PGminer compilation instructions

#### Clone the code

Navigate to a directory to store PGminer and use this command to clone the code:

`git clone https://github.com/tuura/process-mining.git`

#### Compile PGminer

Enter the process-mining directory. Compile PGminer using the following command:

`ghc --make -isrc pgminer.hs`

This will produce a PGminer executable. 

### Examples of running PGminer

#### Example 1: L1 = {abcd, acbd}

Within the same directory as the PGminer executable, create a simple text file with the filename `L1.tr`.

The body of this text file should contain the following event log representation of L1:

`a b c d
a c b d`

Now, to find any concurrent pairs in this event log, run the command

`./pgminer L1.tr`

This will find the following concurrent pairs:

`[("b","c")]`

And the resulting Conditional Partial Order Graph algebraic representation of the concurrency will be found in the file `L1.cpog` in this directory.

The reasoning for this concurrent pair is that between the two traces in this event log, there are two possible paths from `a` to `d`, and the difference is that `b` and `c` can occur in any order. These are recognised as concurrent and extracted and the information is used for simplifying the derived CPOG, found in `L1.cpog`

#### Example 2: L2 = {abcd,acbd,abce}

Within the same directory as the PGminer executable, create a simple text file with the filename `L2.tr`.

The body of this text file should contain the following event log representation of L1:

`a b c d
a c b d
a b c e`

Now, to find any concurrent pairs in this event log, run the command

`./pgminer L2.tr`

This will find the following concurrent pairs:

`[]`

This event log has no concurrency.

The resulting Conditional Partial Order Graph algebraic representation of the concurrency will be found in the file `L2.cpog` in this directory.

In this case, no concurrency is found as event e indicates the order between b and c. Whenever we observe event e in a trace we can be sure that b occurs before c in that trace. Thus, the resulting CPOG found in `L2.cpog` is the same as the event log. 

### Cabal testing

#### Build

	cabal build

#### Test

	cabal test --show-details=always

