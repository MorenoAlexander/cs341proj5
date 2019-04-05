//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a tuple containing 
// 3 values:  
//
//   (result, msg, program)
//
// where result is true or false (legal or not legal), 
// msg is a success or syntax error message, and program
// is a list of instructions if parsing was successful.
//
// <<YOUR NAME>>
// U. of Illinois, Chicago
// CS 341, Spring 2019
// Project #05
//

#light

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // These are debug routines that output the tokens, or
  // program, respectively.  They are written so you can
  // inject these into a pipeline to output the current 
  // state of the tokens or program.
  //
  let private __outputTokens (tokens, program) =
    printfn "Tokens: %A" tokens
    (tokens, program)

  let private __outputProgram (tokens, program) =
    printfn "Program: %A" program
    (tokens, program)


  //
  // matchToken
  //
  let private matchToken expected_token (tokens, program) =
    let (token, _) = List.head tokens
    //
    // if the token matches the expected token, keep parsing by
    // returning the rest of the tokens.  Otherwise throw an
    // exception because there's a syntax error, effectively 
    // stopping compilation:
    //


    if expected_token = token then  
      (List.tail tokens, program)
    else
      failwith ("expecting " + (string expected_token) + ", but found " + (string token))

  //ASM conversion-------------------------------------------------------------------------------------------------------
  

  let rec AssignASM (tokens,program) inst= 
    match tokens with
    | (lexer.Tokens.Semicolon,_)::rest -> (rest,inst::program)
    | (lexer.Tokens.Assign,_)::rest -> AssignASM (rest,program) inst
  
  let rec ASM (tokens,program) =
    match tokens with
    |  [] -> program
    |  (lexer.Tokens.Int,_)::(lexer.Tokens.ID,str)::(lexer.Tokens.Semicolon,_)::rest -> ASM (rest,(["$DECL";str]::program))
    |  (lexer.Tokens.ID,strID)::rest ->  ASM (AssignASM (rest,program) ["$ASSIGN";strID])
    |  (_,_)::rest -> ASM (rest,(program))
    
  let convertToASM tokens program =
    List.rev (ASM (tokens,program))
  //End ASM conversion----------------------------------------------------------------------------------------------------


  let empty (tokens, program) =
    (tokens, program)
    |> matchToken lexer.Tokens.Semicolon


  let parseInt (tokens,program) =
    (tokens,program)
    |> matchToken lexer.Tokens.Int
    |> matchToken lexer.Tokens.ID
    |> matchToken lexer.Tokens.Semicolon

  let parseInput (tokens,program) =
    (tokens,program)
    |> matchToken lexer.Tokens.Cin
    |> matchToken lexer.Tokens.Input
    |> matchToken lexer.Tokens.ID
    |> matchToken lexer.Tokens.Semicolon





  let exprvalue (tokens,program)=
    let (h,_) = List.head tokens
    match h with
    | lexer.Tokens.Str_Literal -> matchToken lexer.Tokens.Str_Literal (tokens,program)
    | lexer.Tokens.ID -> matchToken lexer.Tokens.ID (tokens,program)
    | lexer.Tokens.Int_Literal -> matchToken lexer.Tokens.Int_Literal (tokens,program)
    | lexer.Tokens.Bool_Literal -> matchToken lexer.Tokens.Bool_Literal (tokens,program)
    | h -> (tokens,program)

  let isExprOP hd =
    match hd with
    | lexer.Tokens.Divide -> true // /
    | lexer.Tokens.Minus -> true  // -
    | lexer.Tokens.Plus -> true   // +
    | lexer.Tokens.Times -> true  // *
    | lexer.Tokens.Power -> true  // ^
    | lexer.Tokens.EQ -> true
    | hd -> false

  let exprOP  (tokens,program) =
    let (h,_) = List.head tokens
    match h with
    | lexer.Tokens.Divide -> matchToken lexer.Tokens.Divide (tokens,program)
    | lexer.Tokens.Minus -> matchToken lexer.Tokens.Minus (tokens, program)
    | lexer.Tokens.Plus -> matchToken lexer.Tokens.Plus (tokens, program)
    | lexer.Tokens.Times -> matchToken lexer.Tokens.Times (tokens,program)
    | lexer.Tokens.Power -> matchToken lexer.Tokens.Power (tokens, program)
    | lexer.Tokens.LT -> matchToken lexer.Tokens.LT (tokens, program)
    | lexer.Tokens.GT -> matchToken lexer.Tokens.GT (tokens, program)
    | lexer.Tokens.LTE -> matchToken lexer.Tokens.LTE (tokens, program)
    | lexer.Tokens.GTE -> matchToken lexer.Tokens.GTE (tokens, program)
    | lexer.Tokens.EQ -> matchToken lexer.Tokens.EQ (tokens, program)
    | lexer.Tokens.NE -> matchToken lexer.Tokens.NE (tokens, program)

  //expressions
  let expr (tokens, program) =
    let (hd,_) = (List.tail tokens).Head
    if (isExprOP hd) = true then
      (tokens,program)
      |> exprvalue
      |> exprOP
      |> exprvalue
    else
      exprvalue (tokens,program)

  //cout
  let parseCout (tokens, program) = 
    (tokens,program)
    |> matchToken lexer.Tokens.Cout
    |> matchToken lexer.Tokens.Output
    |> exprvalue
    |> matchToken lexer.Tokens.Semicolon
    
  let parseID (tokens, program) =
    (tokens,program)
    |> matchToken lexer.Tokens.ID
    |> matchToken lexer.Tokens.Assign
    |> expr
    |> matchToken lexer.Tokens.Semicolon





  let rec ifstmt (tokens,program) =
    let rec thenstmts (tokens,program) =
      let h = List.head tokens
      match h with 
      | (tok,_) when tok = lexer.Tokens.CloseBrace -> matchToken lexer.Tokens.CloseBrace (tokens,program)
      | (tok,_) when tok = lexer.Tokens.Int -> thenstmts (parseInt (tokens,program))
      | (tok,_) when tok = lexer.Tokens.Cin -> thenstmts (parseInput (tokens, program))
      | (tok,_) when tok = lexer.Tokens.Cout -> thenstmts (parseCout (tokens, program))
      | (tok,_) when tok = lexer.Tokens.ID -> thenstmts (parseID (tokens, program))
      | (tok,_) when tok = lexer.Tokens.If -> thenstmts (ifstmt (tokens, program))
      | (tok,_) when tok = lexer.Tokens.Semicolon -> thenstmts (empty (tokens, program))

    let elsepart (tokens,program) = 

      let (h,_) = List.head tokens

      if h = lexer.Tokens.Else then
          (tokens,program)
          |> matchToken lexer.Tokens.Else
          |> matchToken lexer.Tokens.OpenBrace
          |> thenstmts
      else
          (tokens, program)
    (tokens,program)
    |> matchToken lexer.Tokens.If
    |> matchToken lexer.Tokens.OpenParen
    |> expr
    |> matchToken lexer.Tokens.CloseParen
    |> matchToken lexer.Tokens.OpenBrace
    |> thenstmts
    |> elsepart





  let rec morestmts (tokens,program) =
    let h = List.head tokens
    match h with 
    | (tok,_) when tok = lexer.Tokens.CloseBrace -> (tokens,program)
    | (tok,_) when tok = lexer.Tokens.Int -> morestmts (parseInt (tokens,program))
    | (tok,_) when tok = lexer.Tokens.Cin -> morestmts (parseInput (tokens, program))
    | (tok,_) when tok = lexer.Tokens.Cout -> morestmts (parseCout (tokens, program))
    | (tok,_) when tok = lexer.Tokens.ID -> morestmts(parseID (tokens, program))
    | (tok,_) when tok = lexer.Tokens.If -> morestmts (ifstmt (tokens, program))
    | (tok,_) when tok = lexer.Tokens.Semicolon -> morestmts(empty (tokens, program))

  let stmts (tokens,program) =
    (tokens,program)
    |> morestmts


  //
  // simpleC
  // 
  let private simpleC (tokens, program) = 
    (tokens, program)
    |> matchToken lexer.Tokens.Void
    |> matchToken lexer.Tokens.Main
    |> matchToken lexer.Tokens.OpenParen
    |> matchToken lexer.Tokens.CloseParen
    |> matchToken lexer.Tokens.OpenBrace
    |> stmts
    |> matchToken lexer.Tokens.CloseBrace
    |> matchToken lexer.Tokens.EOF


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // a tuple containing 3 values:  
  //
  //   (result, msg, program)
  //
  // where result is true or false (legal or not legal), 
  // msg is a success or syntax error message, and program
  // is a list of instructions if parsing was successful.
  //
  let parse tokens = 
    try
      let (_, program) = simpleC (tokens, [])
      (true, "success", List.rev program)
    with 
      | ex -> (false, ex.Message, [])
