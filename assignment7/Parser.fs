module ImpParser

open Eval
open Types

(*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

open JParsec.TextParser // Example parser combinator library. Use for CodeJudge.
// open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.

let pIntToChar = pstring "intToChar"
let pPointValue = pstring "pointValue"

let pCharToInt = pstring "charToInt"
let pToUpper = pstring "toUpper"
let pToLower = pstring "toLower"
let pCharValue = pstring "charValue"

let pTrue = pstring "true"
let pFalse = pstring "false"
let pIsDigit = pstring "isDigit"
let pIsLetter = pstring "isLetter"
let pIsVowel = pstring "isVowel"

let pif = pstring "if"
let pthen = pstring "then"
let pelse = pstring "else"
let pwhile = pstring "while"
let pdo = pstring "do"
let pdeclare = pstring "declare"

let whitespaceChar = satisfy System.Char.IsWhiteSpace
let pletter = satisfy System.Char.IsLetter
let palphanumeric = satisfy System.Char.IsLetterOrDigit

let spaces = many whitespaceChar
let spaces1 = many1 whitespaceChar

let (.>*>.) p1 p2 = p1 .>> spaces .>>. p2
let (.>*>) p1 p2 = p1 .>> spaces .>> p2
let (>*>.) p1 p2 = p1 >>. spaces >>. p2

let parenthesise p = pchar '(' >*>. p .>*> pchar ')'

let listToString lst =
    System.String(List.toArray lst) |> string

let pid =
    pchar '_' <|> pletter .>>. many palphanumeric
    |>> (fun (a, b) -> a :: b |> listToString)


let unop op a = op >*>. a
let binop op p1 p2 = p1 .>*> op .>*>. p2

let TermParse, tref = createParserForwardedToRef<aExp> ()
let ProdParse, pref = createParserForwardedToRef<aExp> ()
let AtomParse, aref = createParserForwardedToRef<aExp> ()

let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
do tref := choice [ AddParse; SubParse; ProdParse ]

let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
do pref := choice [ MulParse; DivParse; ModParse; AtomParse ]

let PointValueParse =
    pPointValue >*>. parenthesise TermParse |>> PV <?> "PointValue"


let NegParse = pchar '-' >>. TermParse |>> (fun expr -> Mul(N -1, expr)) <?> "Neg"
let NParse = pint32 |>> N <?> "Int"
let VParse = pid |>> V <?> "Pid"
let ParParse = parenthesise TermParse

let AexpParse = TermParse

let CParse, cref = createParserForwardedToRef<cExp> ()

let quoted p = pchar '\'' >>. p .>> pchar '\''
let CharLiteralParse = quoted anyChar |>> C <?> "CharLiteral"
let CharValueParse = pCharValue >*>. parenthesise AexpParse |>> CV <?> "CharValue"

let IntToCharParse =
    pIntToChar >*>. parenthesise AexpParse |>> IntToChar <?> "IntToChar"

let ToUpperParse = pToUpper >*>. parenthesise CParse |>> ToUpper <?> "ToUpper"
let ToLowerParse = pToLower >*>. parenthesise CParse |>> ToLower <?> "ToLower"
let CexpParse = CParse

do
    cref
    := choice [ CharLiteralParse; CharValueParse; IntToCharParse; ToUpperParse; ToLowerParse ]

let ChatToIntParse =
    pCharToInt >*>. parenthesise CexpParse |>> CharToInt <?> "CharToInt"

do
    aref
    := choice [ PointValueParse; ChatToIntParse; NegParse; NParse; VParse; ParParse ]

let BexpParse = pstring "not implemented"

let stmntParse = pstring "not implemented"


let parseSquareProg _ = failwith "not implemented"

let parseBoardProg _ = failwith "not implemented"

let mkBoard (bp: boardProg) : board = failwith "not implemented"
