# hnock

[![Build Status](https://travis-ci.org/jtobin/hnock.svg?branch=master)](https://travis-ci.org/jtobin/hnock)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/jtobin/hnock/blob/master/LICENSE)

A [Nock][nock] interpreter.

## Install

Use a simple

```
stack install
```

to build the `hnock` binary and get it moved somewhere on your PATH.  If you
just want to build the binary, you can use `stack build`.

## Usage

From bash, simply pipe Nock expressions into the `hnock` executable:

```
$ echo '*[[[4 5] [6 14 15]] [0 7]]' | hnock
[14 15]
```

For playing around in GHCi, import the `Nock` library and use `hnock` to parse
and evaluate Nock expressions:

```
*Nock> hnock "*[[[4 5] [6 14 15]] [0 7]]"
[14 15]
```

To evaluate raw nock Nouns, i.e. to compute `nock(a)` for some noun `a`, use
the `nock` function:

```
*Nock> let expression = hnock "[[[4 5] [6 14 15]] [0 7]]"
*Nock> expression
[[[4 5] [6 [14 15]]] [0 7]]
*Nock> nock expression
[14 15]
```

## Testing

Use a simple `stack test` to run the test suite.

[nock]: https://urbit.org/docs/learn/arvo/nock/definition/
