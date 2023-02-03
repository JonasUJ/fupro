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

type complex = Complex of real: float * imag: float

let mkComplex real imag = Complex(real, imag)

let complexToPair (Complex (real, imag)) = (real, imag)

let (|+|) (Complex (a, b)) (Complex (c, d)) = Complex(a + c, b + d)
let (|*|) (Complex (a, b)) (Complex (c, d)) = Complex(a * c - b * d, b * c - a * d)
let negate (Complex (a, b)) = Complex(-a, -b)
let (|-|) lhs rhs : complex = lhs |+| negate rhs

let invert (Complex (a, b)) =
    Complex(a / (a * a + b * b), -b / (a * a + b * b))

let (|/|) lhs rhs = lhs |*| invert rhs

// Exercise 2.5

let explode1 (s: string) = s.ToCharArray() |> List.ofArray

let explode2 (s: string) =
    [ for i in 0 .. s.Length do
          s.Chars i ]

// Exercise 2.6

let implode cs =
    List.fold (fun (sb: StringBuilder) (c: char) -> sb.Append c) (StringBuilder()) cs
    |> fun s -> s.ToString()

let rec rev =
    function
    | head :: tail -> rev tail @ [ head ]
    | lst -> lst

let implodeRev cs = implode (rev cs)

// Exercise 2.7

let toUpper = explode1 >> List.map Char.ToUpper >> implode

// Exercise 2.8

// Do we really have to do CPS just to test the time functions? I get a stack overflow otherwise...
let ack (m, n) =
    let rec _ack =
        function
        | 0, n, f -> f (n + 1)
        | m, 0, f -> _ack (m - 1, 1, f)
        | m, n, f -> _ack (m, n - 1, (fun x -> _ack (m - 1, x, f)))

    _ack (m, n, id)

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

let rec range g n =
    if n <= 0 then [] else range g (n - 1) @ [ g n ]

// Assignment 2.11

type word = (char * int) list

let hello: word = [ ('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1) ]

type squareFun = word -> int -> int -> int

// Assignment 2.12

let singleLetterScore (w: word) pos acc = snd w[pos] * 1 + acc
let doubleLetterScore (w: word) pos acc = snd w[pos] * 2 + acc
let tripleLetterScore (w: word) pos acc = snd w[pos] * 3 + acc

// Assignment 2.13

let doubleWordScore (_: word) (_: int) acc = acc * 2
let tripleWordScore (_: word) (_: int) acc = acc * 3

// Assignment 2.14

let isConsonant (l: char * int) =
    "BCDFGHJKLMNPQRSTVWXZbcdfghjklmnpqrstvwxz".Contains(fst l)

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
