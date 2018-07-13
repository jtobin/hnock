# hnock

A Nock interpreter.

## Usage

From bash, simply pipe Nock expressions into the executable `hnock`:

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
`enock`:

```
*Nock> let expression = hnock "[[[4 5] [6 14 15]] [0 7]]"
*Nock> expression
[[[4 5] [6 [14 15]]] [0 7]]
*Nock> enock expression
[14 15]
```

