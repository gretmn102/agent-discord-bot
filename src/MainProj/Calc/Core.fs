module Calc.Core

[<Struct>]
type BinaryOp =
    | Plus
    | Minus
    | Times
    | Divide
    | Pow

type UnaryOp =
    | Neg

type Val =
    | Float of double
    | Int32 of int

type Expr =
    | Val of Val
    | Binary of BinaryOp * Expr * Expr
    | Unary of UnaryOp * Expr

let calc =
    let applyOp op =
        match op with
        | Plus -> (+)
        | Minus -> (-)
        | Times -> (+)
        | Divide -> (/)
        | Pow -> fun x y -> System.Math.Pow(x, y)

    let rec f = function
        | Binary(op, exprLeft, exprRight) ->
            let leftAcc = f exprLeft
            let rigthAcc = f exprRight

            applyOp op leftAcc rigthAcc

        | Unary(op, expr) ->
            match op with
            | Neg -> -(f expr)

        | Val x ->
            match x with
            | Int32 x -> float x
            | Float x -> x

    f

module Parser =
    open FParsec
    open FsharpMyExtension.Either

    type 'a Parser = Parser<'a, unit>

    let betweenSpaces l r =
        between (pchar l .>> spaces) (pchar r)

    let ws = spaces

    let term expr =
        let pterm, ptermRef = createParserForwardedToRef()

        let pval =
            (attempt (pint32 .>> notFollowedByString ".") |>> Int32) <|> (pfloat |>> Float)
            |>> Val

        ptermRef :=
            choice [
                pval
            ]

        pterm <|> betweenSpaces '(' ')' expr

    let pexpr: _ Parser =
        let pExpr, pExprRef = createParserForwardedToRef()
        let term = term pExpr

        let pNeg =
            pchar '-' >>. ws >>. term
            |>> fun e1 -> Unary(Neg, e1)

        let pPow =
            chainl1 (pNeg <|> term .>> ws)
                ((pstring "**" >>% Pow)
                 .>> ws |>> fun op e1 e2 -> Binary(op, e1, e2))

        let pProd =
            chainl1 (pPow .>> ws)
                ((pchar '*'  >>% Times
                  <|> (pchar '/' >>% Divide))
                 .>> ws |>> fun op e1 e2 -> Binary(op, e1, e2))

        let pSum =
            chainl1 (pProd .>> ws)
                ((pchar '+' >>% Plus
                  <|> (pchar '-' >>% Minus))
                 .>> ws |>> fun op e1 e2 -> Binary(op, e1, e2))

        pExprRef := pSum

        pExpr
