// Assignment 3.1

open System

type aExp =
    | N of int // Integer value
    | V of string // Variable
    | WL // Length of the word
    | PV of aExp // Point value of character at specific word index
    | Add of aExp * aExp // Addition
    | Sub of aExp * aExp // Subtraction
    | Mul of aExp * aExp // Multiplication

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)

let rec arithEvalSimple =
    function
    | N i -> i
    | V _
    | WL
    | PV _ -> 0 // bogus value because we're told not to handle this case
    | Add (lhs, rhs) -> (arithEvalSimple lhs) + (arithEvalSimple rhs)
    | Sub (lhs, rhs) -> (arithEvalSimple lhs) - (arithEvalSimple rhs)
    | Mul (lhs, rhs) -> (arithEvalSimple lhs) * (arithEvalSimple rhs)

// Assignment 3.2

type state = Map<string, int>

let rec arithEvalState a (s: state) =
    match a with
    | N i -> i
    | V v -> s.TryFind v |> Option.defaultValue 0
    | WL
    | PV _ -> 0 // bogus value because we're told not to handle this case
    | Add (lhs, rhs) -> (arithEvalState lhs s) + (arithEvalState rhs s)
    | Sub (lhs, rhs) -> (arithEvalState lhs s) - (arithEvalState rhs s)
    | Mul (lhs, rhs) -> (arithEvalState lhs s) * (arithEvalState rhs s)

// Assignment 3.3

let arithSingleLetterScore = PV(V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV(V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV(V "_pos_")) .+. (V "_acc_")

let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

type word = (char * int) list

let rec arithEval a (w: word) (s: state) =
    match a with
    | N i -> i
    | V v -> s.TryFind v |> Option.defaultValue 0
    | WL -> w.Length
    | PV i -> snd w[arithEval i w s]
    | Add (lhs, rhs) -> (arithEval lhs w s) + (arithEval rhs w s)
    | Sub (lhs, rhs) -> (arithEval lhs w s) - (arithEval rhs w s)
    | Mul (lhs, rhs) -> (arithEval lhs w s) * (arithEval rhs w s)

// Assignment 3.4

type cExp =
    | C of char (* Character value *)
    | ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
    | ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
    | CV of aExp (* Character lookup at word index *)

let rec charEval c (w: word) (s: state) =
    match c with
    | C c -> c
    | ToUpper c -> Char.ToUpper(charEval c w s)
    | ToLower c -> Char.ToLower(charEval c w s)
    | CV a -> fst w[arithEval a w s]

// Assignment 3.5

type bExp =
    | TT (* true *)
    | FF (* false *)
    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)
    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)
    | IsDigit of cExp (* check for digit *)
    | IsLetter of cExp (* check for letter *)
    | IsVowel of cExp (* check for vowel *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)

let (.||.) b1 b2 =
    ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)

let (.<=.) a b =
    a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let isVowel c = "aeiouæøå".Contains(Char.ToLower c)

let rec boolEval b (w: word) (s: state) =
    match b with
    | TT -> true
    | FF -> false
    | AEq (lhs, rhs) -> (arithEval lhs w s) = (arithEval rhs w s)
    | ALt (lhs, rhs) -> (arithEval lhs w s) < (arithEval rhs w s)
    | Not bExp -> not (boolEval bExp w s)
    | Conj (lhs, rhs) -> (boolEval lhs w s) && (boolEval rhs w s)
    | IsDigit c -> Char.IsDigit(charEval c w s)
    | IsLetter c -> Char.IsLetter(charEval c w s)
    | IsVowel c -> isVowel (charEval c w s)

// Assignment 3.6

let isConsonant c = Not(IsVowel c)

// Assignment 3.7

type stmnt =
    | Skip (* does nothing *)
    | Ass of string * aExp (* variable assignment *)
    | Seq of stmnt * stmnt (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt (* while statement *)

let rec evalStmnt stm (w: word) (s: state) =
    match stm with
    | Skip -> s
    | Ass (ident, value) -> s.Add(ident, arithEval value w s)
    | Seq (stm1, stm2) -> s |> evalStmnt stm1 w |> evalStmnt stm2 w
    | ITE (guard, stm1, stm2) ->
        if boolEval guard w s then
            evalStmnt stm1 w s
        else
            evalStmnt stm2 w s
    | While (guard, stm) ->
        if boolEval guard w s then
            s |> evalStmnt stm w |> evalStmnt (While(guard, stm)) w
        else
            s

// Assignment 3.8

type squareFun = word -> int -> int -> int

let stmntToSquareFun stm w pos acc =
    evalStmnt stm w (Map.ofList [ ("_pos_", pos); ("_acc_", acc) ])
    |> Map.find "_result_"

let singleLetterScore = stmntToSquareFun (Ass("_result_", arithSingleLetterScore))
let doubleLetterScore = stmntToSquareFun (Ass("_result_", arithDoubleLetterScore))
let tripleLetterScore = stmntToSquareFun (Ass("_result_", arithTripleLetterScore))

let doubleWordScore = stmntToSquareFun (Ass("_result_", arithDoubleWordScore))
let tripleWordScore = stmntToSquareFun (Ass("_result_", arithTripleWordScore))

let containsNumbers =
    stmntToSquareFun (
        Seq(
            Ass("_result_", V "_acc_"),
            While(
                V "i" .<. WL,
                ITE(
                    IsDigit(CV(V "i")),
                    Seq(Ass("_result_", V "_result_" .*. N -1), Ass("i", WL)),
                    Ass("i", V "i" .+. N 1)
                )
            )
        )
    )

// Assignment 3.9

let oddConsonants =
    Seq(
        Ass("_result_", V "_acc_"),
        While(
            V "i" .<. WL,
            Seq(ITE(IsVowel(CV(V "i")), Skip, Ass("_result_", V "_result_" .*. N -1)), Ass("i", V "i" .+. N 1))
        )
    )

// Assignment 3.10

type square = (int * squareFun) list
type square2 = (int * stmnt) list

let SLS = [ (0, Ass("_result_", arithSingleLetterScore)) ]
let DLS = [ (0, Ass("_result_", arithDoubleLetterScore)) ]
let TLS = [ (0, Ass("_result_", arithTripleLetterScore)) ]

let DWS = [ (1, Ass("_result_", arithDoubleWordScore)) ] @ SLS
let TWS = [ (1, Ass("_result_", arithTripleWordScore)) ] @ SLS

let calculatePoints (squares: square list) (w: word) =
    squares
    |> List.mapi (fun i -> (fun s -> fst s, snd s w i) |> List.map)
    |> List.fold (@) []
    |> List.sortBy fst
    |> List.map snd
    |> List.fold (>>) id
    <| 0

let calculatePoints2 =
    List.map (List.map (fun (i: int, stmt) -> i, stmntToSquareFun stmt))
    >> calculatePoints
