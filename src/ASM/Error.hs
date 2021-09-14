module ASM.Error where

data Error
 = E1001
 | E1002
 | E1003
 | E1004
 | E1005
 | E1006
 | E1007
 | E1008
 | E1009
 | E1010
 | E1011
 | E1012
 | E1013 String String
 | E1014 String
 | E1015
 | E1019
 | E1021
 | E1022 Int
 | E1023 Int
 | E1024
 deriving Eq

instance Show Error where
  show e =
    case e of
      E1001 -> "1001: mnemonic expected"
      E1002 -> "1002: invalid extension"
      E1003 -> "1003: no space before operands"
      E1004 -> "1004: too many closing parentheses"
      E1005 -> "1005: missing closing parentheses"
      E1006 -> "1006: missing operand"
      E1007 -> "1007: scratch at end of line"
      E1008 -> "1008: section flags expected"
      E1009 -> "1009: inalid data operand"
      E1010 -> "1010: memory flags expected"
      E1011 -> "1011: identifier expected"
      E1012 -> "1012: assembly aborted"
      E1013 s1 s2 -> "1013: unexpected " ++ s1 ++ " without " ++ s2
      E1014 s -> "1014: pointless default value for required parameter " ++ s
      E1015 -> "1015: invalid section type ignored, assuming progbits"
      E1019 -> "1019: syntax error"
      E1021 -> "1021: section name expected"
      E1022 n -> "1022: .fail " ++ show n ++ " encountered"
      E1023 n -> "1023: .fail " ++ show n ++ " encountered"
      E1024 -> "1024: alignment too bigv"
