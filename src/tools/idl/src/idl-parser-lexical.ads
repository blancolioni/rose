with GCS.Lexer;
with GCS.Styles;

with IDL.Parser.Tokens;                 use IDL.Parser.Tokens;

pragma Elaborate_All (GCS.Lexer);

private package IDL.Parser.Lexical is
  new GCS.Lexer (Token              => Token,
                 Tok_None           => Tok_None,
                 Tok_End_Of_File    => Tok_End_Of_File,
                 Tok_Bad_Character  => Tok_Bad_Character,
                 Tok_Identifier     => Tok_Identifier,
                 Tok_String         => Tok_String_Constant,
                 Tok_Character      => Tok_None,
                 Tok_Integer        => Tok_Integer_Constant,
                 Tok_Float          => Tok_None,
                 First_Keyword      => Tok_Interface,
                 Keywords           => "interface record is begin end " &
                   "procedure function return " &
                   "in invoke out type constant new " &
                   "with",
                 First_Symbol       => Tok_Colon,
                 Symbols            => ": ; ( ) , . .. => := ' & /",
                 Identifier_Start   => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "_",
                 Identifier_Body    => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "0123456789" &
                                       "_",
                 Line_Comment_Start => "--",
                 Properties         => (GCS.Styles.Ada_Property_List));
