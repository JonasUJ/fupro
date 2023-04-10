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

let isVowel c =
    Seq.contains (Char.ToLower c) "aeiouæøå"

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
    | AEq (lhs, rhs) ->
        arithEval lhs
        >>= (fun lhs' -> arithEval rhs >>= (fun rhs' -> lhs' = rhs' |> ret))
    | ALt (lhs, rhs) ->
        arithEval lhs
        >>= (fun lhs' -> arithEval rhs >>= (fun rhs' -> lhs' < rhs' |> ret))
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

let rec stmntEval stmnt : SM<unit> =
    match stmnt with
    | Declare var -> declare var
    | Ass (var, expr) -> arithEval expr >>= update var
    | Skip -> ret ()
    | Seq (one, two) -> stmntEval one >>>= stmntEval two
    | ITE (guard, one, two) ->
        push >>>= boolEval guard
        >>= (fun b -> if b then stmntEval one else stmntEval two)
        >>>= pop
    | While (guard, stmt) ->
        push >>>= boolEval guard
        >>= (fun b ->
            if b then
                stmntEval stmt >>>= stmntEval (While(guard, stmt))
            else
                stmntEval Skip)
        >>>= pop

(* Part 3 (Optional) *)

type StateBuilder() =

    member this.Bind(f, x) = f >>= x
    member this.Return(x) = ret x
    member this.ReturnFrom(x) = x
    member this.Delay(f) = f ()
    member this.Combine(a, b) = a >>= (fun _ -> b)

let prog = new StateBuilder()

let rec op2 f lhs rhs =
    prog {
        let! lhs' = arithEval2 lhs
        let! rhs' = arithEval2 rhs
        return f lhs' rhs'
    }

and opFail2 f lhs rhs =
    prog {
        let! lhs' = arithEval2 lhs
        let! rhs' = arithEval2 rhs

        if rhs' = 0 then
            return! fail DivisionByZero
        else
            return f lhs' rhs'
    }

and arithEval2 a =
    match a with
    | N n -> prog { return n }
    | V var -> prog { return! lookup var }
    | WL -> prog { return! wordLength }
    | PV expr ->
        prog {
            let! v = arithEval2 expr
            return! pointValue v
        }
    | Add (lhs, rhs) -> op2 (+) lhs rhs
    | Sub (lhs, rhs) -> op2 (-) lhs rhs
    | Mul (lhs, rhs) -> op2 (*) lhs rhs
    | Div (lhs, rhs) -> opFail2 (/) lhs rhs
    | Mod (lhs, rhs) -> opFail2 (%) lhs rhs
    | CharToInt expr ->
        prog {
            let! v = charEval2 expr
            return int v
        }

and charEval2 c =
    match c with
    | C c -> prog { return c }
    | CV expr ->
        prog {
            let! v = arithEval2 expr
            return! characterValue v
        }
    | ToUpper expr ->
        prog {
            let! v = charEval2 expr
            return Char.ToUpper v
        }
    | ToLower expr ->
        prog {
            let! v = charEval2 expr
            return Char.ToLower v
        }
    | IntToChar expr ->
        prog {
            let! v = arithEval2 expr
            return char v
        }

and boolEval2 b =
    match b with
    | TT -> prog { return true }
    | FF -> prog { return false }
    | AEq (lhs, rhs) ->
        prog {
            let! lhs' = arithEval2 lhs
            let! rhs' = arithEval2 rhs
            return lhs' = rhs'
        }
    | ALt (lhs, rhs) ->
        prog {
            let! lhs' = arithEval2 lhs
            let! rhs' = arithEval2 rhs
            return lhs' < rhs'
        }
    | Not expr ->
        prog {
            let! v = boolEval2 expr
            return not v
        }
    | Conj (lhs, rhs) ->
        prog {
            let! lhs' = boolEval2 lhs
            let! rhs' = boolEval2 rhs
            return lhs' && rhs'
        }
    | IsVowel expr ->
        prog {
            let! v = charEval2 expr
            return isVowel v
        }
    | IsLetter expr ->
        prog {
            let! v = charEval2 expr
            return Char.IsLetter v
        }
    | IsDigit expr ->
        prog {
            let! v = charEval2 expr
            return Char.IsDigit v
        }

let rec stmntEval2 stm =
    match stm with
    | Declare var -> prog { do! declare var }
    | Ass (var, expr) ->
        prog {
            let! v = arithEval2 expr
            do! update var v
        }
    | Skip -> prog { return () }
    | Seq (one, two) ->
        prog {
            do! stmntEval2 one
            do! stmntEval2 two
        }
    | ITE (guard, one, two) ->
        prog {
            do! push
            let! v = boolEval2 guard
            if v then do! stmntEval2 one else do! stmntEval2 two
            do! pop
        }
    | While (guard, stmt) ->
        prog {
            do! push
            let! v = boolEval2 guard

            if v then
                do! stmntEval2 stmt
                do! stmntEval2 (While(guard, stmt))
            else
                do! stmntEval2 Skip

            do! pop
        }

(* Part 4 (Optional) *)

let stmntToSquareFun stm =
    let inner w pos acc =
        let state =
            mkState [ ("_pos_", pos); ("_acc_", acc); ("_result_", 0) ] w [ "_pos_"; "_acc_"; "_result_" ]

        let monad = stmntEval stm >>>= lookup "_result_"
        evalSM state monad

    inner

let stmntToBoardFun stm t (x, y) =
    let state =
        mkState [ ("_x_", x); ("_y_", y); ("_result_", 0) ] [] [ "_x_"; "_y_"; "_result_" ]

    let monad = stmntEval stm >>>= lookup "_result_"

    match evalSM state monad with
    | Success id -> Success(Map.tryFind id t)
    | Failure error -> Failure error

type squareStmnt = Map<int, stmnt>

let stmntsToSquare stms =
    Map.map (fun _ -> stmntToSquareFun) stms

type board =
    { center: coord
      defaultSquare: square
      squares: boardFun }

let mkBoard c ds boardStmnt m =
    { center = c
      defaultSquare = Map.find ds m |> stmntsToSquare
      squares = Map.map (fun _ -> stmntsToSquare) m |> stmntToBoardFun boardStmnt }
