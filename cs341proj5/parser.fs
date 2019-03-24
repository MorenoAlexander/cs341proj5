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
// <<AlexanderMoreno>>
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


  //
  // simpleC
  // 
  let private simpleC (tokens, program) = 
    matchToken lexer.Tokens.EOF (tokens, program)


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
