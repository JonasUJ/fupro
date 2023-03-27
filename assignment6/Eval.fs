module Eval

open System
open StateMonad
open Types

(* Code for testing *)

let hello = [ ('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1) ]
let state = mkState [ ("x", 5); ("y", 42) ] hello [ "_pos_"; "_result_" ]
let emptyState = mkState [] [] []

let op f a b = 
    a >>= (fun lhs -> b >>= (fun rhs -> ret (f lhs rhs)))

let add = op (+)

let sub = op (-)
    
let mul = op (*)

let opFail f a b =
    a
    >>= (fun lhs -> b >>= (fun rhs -> if rhs = 0 then fail DivisionByZero else ret (f lhs rhs)))
    
let div = opFail (/)
    
let modulo = opFail (%)

let isVowel c = Seq.contains (Char.ToLower c) "aeiouæøå"

type aExp =
    | N of int
    | V of string
    | WL
    | PV of aExp
    | Add of aExp * aExp
    | Sub of aExp * aExp
    | Mul of aExp * aExp
    | Div of aExp * aExp
    | Mod of aExp * aExp
    | CharToInt of cExp

and cExp =
    | C of char (* Character value *)
    | CV of aExp (* Character lookup at word index *)
    | ToUpper of cExp
    | ToLower of cExp
    | IntToChar of aExp

type bExp =
    | TT (* true *)
    | FF (* false *)

    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)

    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)

    | IsVowel of cExp (* check for vowel *)
    | IsLetter of cExp (* check for letter *)
    | IsDigit of cExp (* check for digit *)

let (.+.) a b = Add(a, b)
let (.-.) a b = Sub(a, b)
let (.*.) a b = Mul(a, b)
let (./.) a b = Div(a, b)
let (.%.) a b = Mod(a, b)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj(b1, b2)

let (.||.) b1 b2 =
    ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.->.) b1 b2 =
    (~~b1) .||. b2 (* boolean implication *)

let (.=.) a b = AEq(a, b)
let (.<.) a b = ALt(a, b)
let (.<>.) a b = ~~(a .=. b)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b)

let (.>=.) a b =
    ~~(a .<. b) (* numeric greater than or equal to *)

let (.>.) a b =
    ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let rec arithEval a : SM<int> =
    match a with
    | N n -> ret n
    | V var -> lookup var
    | WL -> wordLength
    | PV expr -> arithEval expr >>= pointValue
    | Add (lhs, rhs) -> add (arithEval lhs) (arithEval rhs)
    | Sub (lhs, rhs) -> sub (arithEval lhs) (arithEval rhs)
    | Mul (lhs, rhs) -> mul (arithEval lhs) (arithEval rhs)
    | Div (lhs, rhs) -> div (arithEval lhs) (arithEval rhs)
    | Mod (lhs, rhs) -> modulo (arithEval lhs) (arithEval rhs)
    | CharToInt expr -> charEval expr >>= (int >> ret)
and charEval c : SM<char> =
    match c with
    | C c -> ret c
    | CV expr -> arithEval expr >>= characterValue
    | ToUpper expr -> charEval expr >>= (Char.ToUpper >> ret)
    | ToLower expr -> charEval expr >>= (Char.ToLower >> ret)
    | IntToChar expr -> arithEval expr >>= (char >> ret)

and boolEval b : SM<bool> =
    match b with
    | TT -> ret true
    | FF -> ret false
    | AEq (lhs, rhs) -> arithEval lhs >>= (fun lhs' -> arithEval rhs >>= (fun rhs' -> lhs' = rhs' |> ret))
    | ALt (lhs, rhs) -> arithEval lhs >>= (fun lhs' -> arithEval rhs >>= (fun rhs' -> lhs' < rhs' |> ret))
    | Not expr -> boolEval expr >>= (not >> ret)
    | Conj (lhs, rhs) -> boolEval lhs >>= (fun lhs' -> boolEval rhs >>= (fun rhs' -> ret (lhs' && rhs')))
    | IsVowel expr -> charEval expr >>= (isVowel >> ret)
    | IsLetter expr -> charEval expr >>= (Char.IsLetter >> ret)
    | IsDigit expr -> charEval expr >>= (Char.IsDigit >> ret)


type stmnt = (* statements *)
    | Declare of string (* variable declaration *)
    | Ass of string * aExp (* variable assignment *)
    | Skip (* nop *)
    | Seq of stmnt * stmnt (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt (* while statement *)

let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

type StateBuilder() =

    member this.Bind(f, x) = f >>= x
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Delay(f) = f ()
    member this.Combine(a, b) = a >>= (fun _ -> b)

let prog = new StateBuilder()

let arithEval2 a = failwith "Not implemented"
let charEval2 c = failwith "Not implemented"
let rec boolEval2 b = failwith "Not implemented"

let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *)

let stmntToSquareFun stm = failwith "Not implemented"

let stmntToBoardFun stm m = failwith "Not implemented"

type squareStmnt = Map<int, stmnt>
let stmntsToSquare stms = failwith "Not implemented"

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun }

let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
