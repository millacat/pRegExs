# Probabilistic Regular Expressionse
A DSL for probabilistic regular expressions implemented as an embedded language in `F#`.
The regular expressions are defined as a sum type being partly recursive:

```type Probability = float

type Regex =
| Sym of char
| Epsilon
| Conc of Regex ∗ Regex
| Or of Regex ∗ Regex ∗ Probability
| Star of Regex ∗ Probability
| Maybe of Regex ∗ Probability
| Plus of Regex ∗ Probability```

Run using interpreter:
`fsharpi regex.fsx`

Or compile and execute:
`fsharpc regex.fsx`
`mono regex.exe`

