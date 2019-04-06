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
// <<Alexander Moreno>>
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
    | (lexer.Tokens.ID,strID)::rest -> AssignASM (rest,program) (inst @ ["ID";strID])
    | (lexer.Tokens.Plus,_)::rest -> AssignASM (rest,program) (inst @ ["+"])
    | (lexer.Tokens.Minus,_)::rest -> AssignASM (rest,program) (inst @ ["-"])
    | (lexer.Tokens.Times,_)::rest -> AssignASM (rest,program) (inst @ ["*"])
    | (lexer.Tokens.Divide,_)::rest -> AssignASM (rest,program) (inst @ ["/"])
    | (lexer.Tokens.Power,_)::rest -> AssignASM (rest,program) (inst @ ["^"])
    | (lexer.Tokens.LT,_)::rest -> AssignASM (rest,program) (inst @ ["<"])
    | (lexer.Tokens.LTE,_)::rest -> AssignASM (rest,program) (inst @ ["<="])
    | (lexer.Tokens.GT,_)::rest -> AssignASM (rest,program) (inst @ [">"])
    | (lexer.Tokens.GTE,_)::rest -> AssignASM (rest,program) (inst @ [">="])
    | (lexer.Tokens.EQ,_)::rest -> AssignASM (rest,program) (inst @ [">"])
    | (lexer.Tokens.Bool_Literal,b)::rest -> AssignASM (rest,program) (inst @ ["Bool_Literal";b])
    | (lexer.Tokens.Str_Literal,str)::rest -> AssignASM (rest,program) (inst @ ["Str_Literal";str])
    | (lexer.Tokens.Int_Literal,num)::rest -> AssignASM (rest,program) (inst @ ["Int_Literal";num])
    | (lexer.Tokens.EOF,_)::rest -> failwith ("expected token,but found "+ string lexer.Tokens.EOF)
    | (lexer.Tokens.Unknown,_)::rest -> failwith ("expected token,but found "+ string lexer.Tokens.Unknown)

  let CoutASM (tokens, program) inst=
      match tokens with
      | (lexer.Tokens.Endl,_)::(lexer.Tokens.Semicolon,_)::rest -> (rest,(inst @ ["Endl";"endl"])::program)
      | (lexer.Tokens.Semicolon,_)::rest -> failwith ("")
      | (_,_)::rest -> AssignASM (tokens,program) inst


      
  let rec ASM (tokens,program) =
    match tokens with
    |  [] -> program
    |  (lexer.Tokens.Int,_)::(lexer.Tokens.ID,str)::(lexer.Tokens.Semicolon,_)::rest -> ASM (rest,(["$DECL";str]::program))
    |  (lexer.Tokens.ID,strID)::rest ->  ASM (AssignASM (rest,program) ["$ASSIGN";strID])
    |  (lexer.Tokens.Cout,_)::(lexer.Tokens.Output,_)::rest -> ASM (CoutASM (rest,program) ["$OUTPUT"])
    |  (lexer.Tokens.Cin,_)::(lexer.Tokens.Input,_)::(lexer.Tokens.ID,strID)::(lexer.Tokens.Semicolon,_)::rest -> ASM (rest,["$INPUT";strID]::program) 
    |  (_,_)::rest -> ASM (rest,(program))
    //|  (lexer.Tokens.If,_)::(lexer.Tokens.OpenParen,_)::rest -> 
    
  let convertToASM (tokens,program) =
    (ASM (tokens,program))
    
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
    | h -> failwith ("expecting identifier or literal, but found "+ (string h))

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
    | h -> failwith ("expecting operator, but found " + (string h))

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
    let (h,_) = List.item 2 tokens
    if h = lexer.Tokens.Endl then
        (tokens,program)
        |> matchToken lexer.Tokens.Cout
        |> matchToken lexer.Tokens.Output
        |> matchToken lexer.Tokens.Endl
        |> matchToken lexer.Tokens.Semicolon
    else
        (tokens,program)
        |> matchToken lexer.Tokens.Cout
        |> matchToken lexer.Tokens.Output
        |> expr
        |> matchToken lexer.Tokens.Semicolon
    
  let parseID (tokens, program) =
    (tokens,program)
    |> matchToken lexer.Tokens.ID
    |> matchToken lexer.Tokens.Assign
    |> expr
    |> matchToken lexer.Tokens.Semicolon


   


  let rec ifstmt (tokens,program) =
    let thenstmts (tokens,program) =
      let h = List.head tokens
      match h with 
      | (tok,_) when tok = lexer.Tokens.Int -> parseInt (tokens,program)
      | (tok,_) when tok = lexer.Tokens.Cin -> parseInput (tokens, program)
      | (tok,_) when tok = lexer.Tokens.Cout -> parseCout (tokens, program)
      | (tok,_) when tok = lexer.Tokens.ID -> parseID (tokens, program)
      | (tok,_) when tok = lexer.Tokens.If -> ifstmt (tokens, program)
      | (tok,_) when tok = lexer.Tokens.Semicolon -> empty (tokens, program)

    let elsepart (tokens,program) = 

      let (h,_) = List.head tokens

      if h = lexer.Tokens.Else then
          (tokens,program)
          |> matchToken lexer.Tokens.Else
          |> thenstmts
      else
          (tokens, program)
    (tokens,program)
    |> matchToken lexer.Tokens.If
    |> matchToken lexer.Tokens.OpenParen
    |> expr
    |> matchToken lexer.Tokens.CloseParen
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
    let (d,_) = List.head tokens
    if d = lexer.Tokens.CloseBrace then
        failwith ("expecting statement, but found " + string lexer.Tokens.CloseBrace )
    else
        morestmts (tokens,program)


  //
  // simpleC
  // 
  let _simpleC (tokens,program) =
    (tokens,program)
    |> matchToken lexer.Tokens.Void
    |> matchToken lexer.Tokens.Main
    |> matchToken lexer.Tokens.OpenParen
    |> matchToken lexer.Tokens.CloseParen
    |> matchToken lexer.Tokens.OpenBrace
    |> stmts
    |> matchToken lexer.Tokens.CloseBrace
    |> matchToken lexer.Tokens.EOF

  let private simpleC (tokens, program) = 
    let (tokList,prog) = (_simpleC (tokens,program),convertToASM (tokens,program))
    (tokList,prog)




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
