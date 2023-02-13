// exercise 5.1

open System

let sum m n =
    let rec sum' m' n' acc =
        if n' = 0 then acc else sum' m' (n' - 1) (m' + n' + acc)

    sum' m n 0

// Exercise 5.2

let length lst =
    let rec length' lst' acc =
        match lst' with
        | [] -> acc
        | _ :: tail -> length' tail (acc + 1)

    length' lst 0

// Exercise 5.3

let foldBack folder lst acc =
    let rec foldBack' cont =
        function
        | [] -> cont acc
        | x :: tail -> foldBack' (folder x >> cont) tail

    foldBack' id lst

// Exercise 5.4

let factC x =
    let rec factC' cont =
        function
        | 0 -> cont 1
        | x' -> factC' ((*) x' >> cont) (x' - 1)

    factC' id x

// Exercise 5.5

let fibA x =
    let rec fibA' acc1 acc2 =
        function
        | 0 -> acc1
        | i -> fibA' acc2 (acc1 + acc2) (i - 1)

    fibA' 0 1 x

let fibC x =
    let rec fibC' cont =
        function
        | 0 -> cont 0
        | 1 -> cont 1
        | i -> fibC' (fun k1 -> fibC' (fun k2 -> cont (k1 + k2)) (i - 2)) (i - 1)

    fibC' id x

// Exercise 5.6

(* The continuation is not tail recursive, causing it to
 * fill all the stack space we had just saved by creating
 * continuations in the first place.
 *)

// Exercise 5.7

type word = (char * int) list

type aExp =
    | N of int (* Integer literal *)
    | V of string (* Variable reference *)
    | WL (* Word length *)
    | PV of aExp (* Point value lookup at word index *)
    | Add of aExp * aExp (* Addition *)
    | Sub of aExp * aExp (* Subtraction *)
    | Mul of aExp * aExp (* Multiplication *)
    | CharToInt of cExp (* NEW: Cast to integer *)

and cExp =
    | C of char (* Character literal *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp (* Convert character to upper case *)
    | ToLower of cExp (* Convert character to lower case *)
    | IntToChar of aExp (* NEW: Cast to character *)

let rec arithEvalSimple expr (w: word) (state: Map<string, int>) =
    match expr with
    | N i -> i
    | V ident -> state.[ident]
    | WL -> w.Length
    | PV expr' -> snd w.[arithEvalSimple expr' w state]
    | Add (lhs, rhs) -> (arithEvalSimple lhs w state) + (arithEvalSimple rhs w state)
    | Sub (lhs, rhs) -> (arithEvalSimple lhs w state) - (arithEvalSimple rhs w state)
    | Mul (lhs, rhs) -> (arithEvalSimple lhs w state) * (arithEvalSimple rhs w state)
    | CharToInt expr' -> int (charEvalSimple expr' w state) - int '0'
and charEvalSimple expr w state =
    match expr with
    | C c -> c
    | CV expr' -> fst w.[arithEvalSimple expr' w state]
    | ToUpper expr' -> charEvalSimple expr' w state |> Char.ToUpper
    | ToLower expr' -> charEvalSimple expr' w state |> Char.ToLower
    | IntToChar expr' -> arithEvalSimple expr' w state + int '0' |> char

// Exercise 5.8

let rec arithEvalTail expr (w: word) (state: Map<string, int>) cont =
    match expr with
    | N i -> cont i
    | V ident -> cont state.[ident]
    | WL -> cont w.Length
    | PV expr' -> arithEvalTail expr' w state (fun i -> snd w.[i] |> cont)
    | Add (lhs, rhs) -> arithEvalTail lhs w state (fun k1 -> arithEvalTail rhs w state (fun k2 -> k1 + k2 |> cont))
    | Sub (lhs, rhs) -> arithEvalTail lhs w state (fun k1 -> arithEvalTail rhs w state (fun k2 -> k1 - k2 |> cont))
    | Mul (lhs, rhs) -> arithEvalTail lhs w state (fun k1 -> arithEvalTail rhs w state (fun k2 -> k1 * k2 |> cont))
    | CharToInt expr' -> charEvalTail expr' w state (fun c -> int c - int '0' |> cont)
and charEvalTail expr (w: word) (state: Map<string, int>) cont =
    match expr with
    | C c -> cont c
    | CV expr' -> arithEvalTail expr' w state (fun i -> fst w.[i] |> cont)
    | ToUpper expr' -> charEvalTail expr' w state (Char.ToUpper >> cont)
    | ToLower expr' -> charEvalTail expr' w state (Char.ToLower >> cont)
    | IntToChar expr' -> arithEvalTail expr' w state (fun i -> i + int '0' |> char |> cont)