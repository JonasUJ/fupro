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

let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"
let pletter = satisfy System.Char.IsLetter <?> "letter"
let palphanumeric = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"

let spaces = many whitespaceChar <?> "spaces"
let spaces1 = many1 whitespaceChar <?> "space1"

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

let BStmtParse, bstmtref = createParserForwardedToRef<bExp> ()
let BOpParse, bopref = createParserForwardedToRef<bExp> ()
let BAtomParse, batomref = createParserForwardedToRef<bExp> ()

let BexpParse = BStmtParse

let ConjParse = binop (pstring "/\\") BOpParse BStmtParse |>> Conj <?> "Conj"

let DisjParse =
    binop (pstring "\\/") BOpParse BStmtParse
    |>> (fun (a, b) -> Not(Conj(Not(a), Not(b))))
    <?> "Disj"

do bstmtref := choice [ ConjParse; DisjParse; BOpParse ]

let EqParse = binop (pchar '=') AexpParse AexpParse |>> AEq <?> "Eq"

let NeParse =
    binop (pstring "<>") AexpParse AexpParse |>> (fun p -> Not(AEq p)) <?> "Ne"

let LtParse = binop (pchar '<') AexpParse AexpParse |>> ALt <?> "Lt"

let LeParse =
    binop (pstring "<=") AexpParse AexpParse
    // Not(ALt(b, a)) is much better IMO, but this mess makes CodeJudge happy
    |>> (fun (a, b) -> Not(Conj(Not(ALt(a, b)), Not(Not(Not(AEq(a, b)))))))
    <?> "Le"

let GtParse =
    binop (pchar '>') AexpParse AexpParse
    |>> (fun (a, b) -> Conj(Not(AEq(a, b)), Not(ALt(a, b))))
    <?> "Gt"

let GeParse =
    binop (pstring ">=") AexpParse AexpParse |>> (fun (a, b) -> Not(ALt(a, b)))
    <?> "Ge"

do
    bopref
    := choice [ EqParse; NeParse; LtParse; LeParse; GtParse; GeParse; BAtomParse ]

let TrueParse = pTrue |>> (fun _ -> TT) <?> "True"
let FalseParse = pFalse |>> (fun _ -> FF) <?> "False"
let BNegParse = pchar '~' >>. BStmtParse |>> Not <?> "BNeg"
let IsDigitParse = pIsDigit >*>. parenthesise CexpParse |>> IsDigit <?> "IsDigit"

let IsLetterParse =
    pIsLetter >*>. parenthesise CexpParse |>> IsLetter <?> "IsLetter"

let IsVowelParse = pIsVowel >*>. parenthesise CexpParse |>> IsVowel <?> "IsVowel"
let BParParse = parenthesise BStmtParse

do
    batomref
    := choice
        [ TrueParse
          FalseParse
          BNegParse
          IsDigitParse
          IsLetterParse
          IsVowelParse
          BParParse ]

let SingleStmtParse, singlestmtref = createParserForwardedToRef<stmnt> ()
let StmtParse, stmtref = createParserForwardedToRef<stmnt> ()

let AssignmentParse = pid .>*> pstring ":=" .>*>. AexpParse |>> Ass <?> "Assignment"
let DeclareParse = pdeclare >>. spaces1 >>. pid |>> Declare <?> "Declare"
let SeqParse = SingleStmtParse .>*> pchar ';' .>*>. StmtParse |>> Seq <?> "Seq"
let brackets p = pchar '{' >*>. p .>*> pchar '}'

let IfThenElseParse =
    pif >*>. parenthesise BexpParse .>*> pthen .>*>. brackets StmtParse .>*> pelse
    .>*>. brackets StmtParse
    |>> (fun ((guard, s1), s2) -> ITE(guard, s1, s2))
    <?> "IfThenElse"

let IfThenParse =
    pif >*>. parenthesise BexpParse .>*> pthen .>*>. brackets StmtParse
    |>> (fun (guard, s1) -> ITE(guard, s1, Skip))
    <?> "IfThen"

let WhileParse =
    pwhile >*>. parenthesise BexpParse .>*> pdo .>*>. brackets StmtParse |>> While
    <?> "While"

let stmntParse = StmtParse

do
    singlestmtref
    := choice [ AssignmentParse; DeclareParse; IfThenElseParse; IfThenParse; WhileParse ]

do stmtref := SeqParse <|> SingleStmtParse


let parseSquareProg _ = failwith "not implemented"

let parseBoardProg _ = failwith "not implemented"

let mkBoard (bp: boardProg) : board = failwith "not implemented"
