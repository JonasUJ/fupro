// Exercise 2.1

open System
open System.Text

let rec downto1 n =
    if n = 0 then [] else n :: downto1 (n - 1)

let rec downto2 =
    function
    | 0 -> []
    | n -> n :: downto2 (n - 1)

// Exercise 2.2

let rec removeOddIdx =
    function
    | [] -> []
    | head :: tail -> head :: skip tail

and skip =
    function
    | [] -> []
    | _ :: tail -> removeOddIdx tail

// Exercise 2.3

let rec combinePair =
    function
    | x :: y :: tail -> (x, y) :: combinePair tail
    | _ -> []

// Exercise 2.4

type complex = { real: float; imag: float }

let mkComplex real imag = { real = real; imag = imag }

let complexToPair c = (c.real, c.imag)

let (|+|) { real = a; imag = b } { real = c; imag = d } = { real = a + c; imag = b + d }

let (|*|) { real = a; imag = b } { real = c; imag = d } =
    { real = a * c - b * d
      imag = b * c + a * d }

let negate c = { real = -c.real; imag = -c.imag }

let (|-|) lhs rhs : complex = lhs |+| negate rhs

let inverse { real = a; imag = b } =
    { real = a / (a * a + b * b)
      imag = -b / (a * a + b * b) }

let (|/|) lhs rhs = lhs |*| inverse rhs

// Exercise 2.5

let explode1 (s: string) = Seq.toList s

let rec explode2 =
    function
    | "" -> []
    | s -> s.[0] :: explode2 s.[1..]

// Exercise 2.6

let implode cs =
    List.fold (fun (sb: StringBuilder) (c: char) -> sb.Append c) (StringBuilder()) cs
    |> fun s -> s.ToString()

let implodeRev cs =
    List.foldBack (fun (c: char) (sb: StringBuilder) -> sb.Append c) cs (StringBuilder())
    |> fun s -> s.ToString()

// Exercise 2.7

let toUpper = explode1 >> List.map Char.ToUpper >> implode

// Exercise 2.8

let rec ack =
    function
    | 0, n-> n + 1
    | m, 0-> ack (m - 1, 1)
    | m, n-> ack (m - 1, ack (m, n - 1))

// Exercise 2.9

let time f =
    let start = DateTime.Now
    let res = f ()
    let finish = DateTime.Now
    (res, finish - start)

let timeArg1 f a = time (fun () -> f a)

// Exercise 2.10

let rec downto3 f n e =
    match n with
    | _ when n > 0 -> downto3 f (n - 1) (f n e)
    | _ -> e

let fac n = downto3 (*) n 1

let rec range g n = downto3 (fun k lst -> g k :: lst) n [] 

// Assignment 2.11

type word = (char * int) list

let hello: word = [ ('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1) ]

type squareFun = word -> int -> int -> int

// Assignment 2.12

let singleLetterScore (w: word) pos acc = snd w.[pos] * 1 + acc
let doubleLetterScore (w: word) pos acc = snd w.[pos] * 2 + acc
let tripleLetterScore (w: word) pos acc = snd w.[pos] * 3 + acc

// Assignment 2.13

let doubleWordScore (_: word) (_: int) acc = acc * 2
let tripleWordScore (_: word) (_: int) acc = acc * 3

// Assignment 2.14

let isVowel letter =
    "aeiouæøå".Contains(fst letter |> Char.ToLower)

let isConsonant = isVowel >> not

let oddConsonants (w: word) (_: int) acc =
    ((List.filter isConsonant w).Length % 2 * -2 + 1) * acc

type square = (int * squareFun) list

let SLS: square = [ (0, singleLetterScore) ]
let DLS: square = [ (0, doubleLetterScore) ]
let TLS: square = [ (0, tripleLetterScore) ]

let DWS: square = SLS @ [ (1, doubleWordScore) ]
let TWS: square = SLS @ [ (1, tripleWordScore) ]

// Assignment 2.15

let calculatePoints (squares: square list) (w: word) =
    squares
    |> List.mapi (fun i -> (fun s -> fst s, snd s w i) |> List.map)
    |> List.fold (@) []
    |> List.sortBy fst
    |> List.map snd
    |> List.fold (>>) id
    <| 0
