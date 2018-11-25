private package IDL.Parser.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier, Tok_Integer_Constant, Tok_String_Constant,

       Tok_Interface, Tok_Record, Tok_Is, Tok_Begin, Tok_End,
       Tok_Procedure, Tok_Function, Tok_Range, Tok_Return,
       Tok_In, Tok_Invoke, Tok_Out, Tok_Type, Tok_Constant, Tok_New,
       Tok_With,

       Tok_Colon, Tok_Semi, Tok_Left_Paren, Tok_Right_Paren, Tok_Comma,
       Tok_Dot, Tok_Dot_Dot, Tok_Arrow, Tok_Becomes, Tok_Apostrophe,
       Tok_Ampersand, Tok_Forward_Slash);

end IDL.Parser.Tokens;
