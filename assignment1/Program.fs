// Exercise 1.1

let sqr x = x * x

// Exercise 1.2

let pow x n = System.Math.Pow (x, n)

// Exercise 1.3

let rec sum =
    function
    | 0 -> 0
    | n -> n + sum (n - 1)

// Exercise 1.4

let rec fib =
    function
    | 0 -> 0
    | 1 -> 1
    | n -> (fib (n - 1)) + (fib (n - 2))

// Exercise 1.5

let dup s: string = s + s

// Exercise 1.6

let rec dupn s =
    function
    | 0 -> ""
    | n -> s + dupn s (n - 1)

// Exercise 1.7

let rec bin =
    function
    | _, 0 -> 1
    | 0, _ -> 0
    | n, k when n = k -> 1
    | n, k -> bin (n - 1, k - 1) + bin (n - 1, k)

// Exercise 1.8

let timediff (t1hh, t1mm) (t2hh, t2mm) = (t2hh - t1hh) * 60 + t2mm - t1mm

// Exercise 1.9

let minutes (hh, mm) = hh * 60 + mm

// Exercise 1.10

let curry f = fun a -> fun b -> f (a, b)
let uncurry f = fun (a, b) -> f a b

// Assignment 1.11

let empty pair _ = pair

// Assignment 1.12

let add newPos cv word =
    function
    | pos when pos = newPos -> cv
    | pos -> word pos

// Assignment 1.13

let hello =
    empty (char 0, 0)
    |> add 0 ('H', 4)
    |> add 1 ('E', 1)
    |> add 2 ('L', 1)
    |> add 3 ('L', 1)
    |> add 4 ('O', 1)

// Assignment 1.14

let singleLetterScore word pos = snd (word pos) * 1
let doubleLetterScore word pos = snd (word pos) * 2
let trippleLetterScore word pos = snd (word pos) * 3


