// Embedded language

let rnd = System.Random()

type Probability = float

type Regex =
    | Sym of char
    | Epsilon
    | Conc of Regex * Regex
    | Or of Regex * Regex * Probability
    | Star of Regex * Probability
    | Maybe of Regex * Probability // matches Regex One time w/ prob. p, zero w p-1
    | Plus of Regex * Probability  // matches One or more times


// A41.3
// Generating random strings from a regex
let rec rndStr (reg : Regex) : string =
    match reg with
    | Epsilon -> ""
    | Sym c   -> string(c)
    | Conc (r1, r2)  -> rndStr r1 + rndStr r2
    | Or (r1, r2, p) -> if rnd.NextDouble() <= p then rndStr r1 else rndStr r2
    | Star (r, p)    -> if rnd.NextDouble() <= p
                        then ""
                        else rndStr r + rndStr (Star (r, p))
    | Maybe (r, p) -> if rnd.NextDouble() <= p then rndStr r else ""
    | Plus (r, p)  -> if rnd.NextDouble() <= p
                      then rndStr r // match one time
                      else rndStr r + (rndStr <| Plus (r, p)) // more times


// Checking that p is between 0 and 1

let valid (p : float) : bool =
    0. < p && p < 1.

// Checking validity of probability of probabilistic regular expression

let validity (reg : Regex) : bool =
    match reg with
    | Or (_,_, p) -> valid p
    | Star (_, p) -> valid p
    | Maybe (_, p)-> valid p
    | Plus (_, p) -> valid p
    | _ -> true


// Generate random strings 'n' times from regex and returning result as a list
// Returns an empty list if n is negative

let genNTimes (reg : Regex) (n : int) : string list =
    if n < 0 then []
    else if not(validity reg) then []
    else
      let mutable l : string list = []
      for i in 1..n do
          l <- rndStr reg :: l
      l


// Counts how many a's and how many b's is generated when calling genNTimes w/
// regex: a|0.4b and 'n' and returns result as a tuple: a * b

let countAsAndBs (n : int) : int * int =
    let cntA = genNTimes (Or(Sym('a'), Sym('b'), 0.4)) n
               |> List.filter (fun e -> e = "a")
               |> List.length
    (cntA, n-cntA)


// A41.4
// Calculating probability of creating string 'w' from regular expression 'reg'
let rec prb (reg : Regex) (w : string) : Probability =
  if not(validity reg) then
    failwith "Not a valid probabilistic regular expression"
  else
    match reg, w with
    | Epsilon, w -> match w with
                    | "" -> 1.
                    | _  -> 0.
    | Sym c, w -> if string(c) = w then 1. else 0.
    | Or (r1, r2, p), w -> p * prb r1 w + (1.-p) * prb r2 w
    | Conc (r1, r2), w  -> let mutable p = 0.
                           for i = 0 to w.Length-1 do
                               p <- p + (prb r1 w.[0..i]) * (prb r2 w.[i+1 ..])
                           p
    | Star (r, p), w -> match w with
                        | "" -> p
                        | _  -> (1.-p) * prb (Conc(r, (Star(r, p)))) w
    | Maybe (r, p), w -> p * prb r w + (1.-p) * prb Epsilon w
    | Plus (r, p), w -> match w with
                        | "" -> 1.
                        | _  -> p * prb (Conc(r, Plus(r, p))) w

// Simple print
let prn (a : 'a) : unit = printfn "%A\n" a

// Print string
let prs (s : string) : unit = printfn "\n---- %s\n" s

let tests () : unit =
    prs "Amount of a's and b's when generating 1000 sequences:"
    prn <| countAsAndBs 1000

    prs "Regex : a*_0.3 b , str : \"aaab\":"
    printfn "%.5f" <| prb (Conc (Star(Sym('a'), 0.3), Sym('b'))) "aaab"

    prs "Regex : a*_0.3 b , str : \"bb\":"
    prn <| prb (Conc (Star(Sym('a'), 0.3), Sym('b'))) "bb"

    prs "Abracadabra, my dudes"
    printfn "%.18f"
     <| prb (Star(Or(Sym('a'),
                     Or(Sym('b'),
                        Or(Sym('c'),
                           Or(Sym('d'), Sym('r'), 0.5), 0.4), 0.3), 0.2), 0.2))
                             "abracadabra"

    prs "Prob. for Str: \"aaa\" with reg: a+"
    prn <| prb (Plus (Sym('a'), 0.5)) "aaa"

    prs "Generating random strings from (a|_0.5b)bc:"
    for i = 1 to 10 do
      prn <| rndStr (Conc(Or (Sym('a'), Sym('b'), 0.5), Conc(Sym('b'), Sym('c'))))

    prs "Generating random strings from (ab+_0.5) |_0.7 bc:"
    for i = 1 to 10 do
      prn <| rndStr (Or(Plus(Conc(Sym('a'), Sym('b')), 0.5),
                                           (Conc(Sym('b'), Sym('c'))), 0.7))

    prs "Probability for str \"a\" with reg a?:"
    prn <| prb (Maybe(Sym('a'), 0.5)) "a"

    prs "Probability for str \"aa\" with reg a+:"
    prn <| prb (Plus(Sym('a'), 0.5)) "aa"

    prs "Probability for str \"aaa\" with reg a+:"
    prn <| prb (Plus(Sym('a'), 0.5)) "aaa"

    prs "Probability for str \"aaa\" with reg ab+:"
    prn <| prb (Plus(Conc(Sym('a'),Sym('b')), 0.5)) "aaa"

    prs "Probability for str \"ababab\" with reg ab+:"
    prn <| prb (Plus(Conc(Sym('a'),Sym('b')), 0.5)) "ababab"

    prs "Probability for str \"abababc\" with reg ab+c:"
    prn <| prb (Conc(Plus(Conc(Sym('a'),Sym('b')), 0.5), Sym('c'))) "abababc"

    prn <| prb (Conc(Or(Star(Sym('a'), 0.5), Sym('b'), 0.5), Sym('b'))) "ab"

tests()

