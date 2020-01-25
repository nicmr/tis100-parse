module TIS100Parse where

import Prelude

import Control.Alt ((<|>))
import Data.Array (many, some)
import Data.Char.Unicode (isDigit, isAlpha, isAscii)
import Data.Int as Ints
import Data.Maybe (Maybe(..), isJust)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.Parser (Parser, failWithPosition, position)
import Text.Parsing.Parser.Combinators (choice, optionMaybe)
import Text.Parsing.Parser.String (satisfy, skipSpaces, string)

-- import Text.Parsing.Parser.Combinators (endBy1, sepBy1, optionMaybe, try, chainl, between)

-- A fully qualified TIS-100 syntactic token with all its parameters
data Tis100Token =
  Instruction InstructionToken
  | Label String
  | Comment String

-- A single instruction
data InstructionToken =
  Nop
  | Mov RegisterToken RegisterToken
  | Sav
  | Swp
  | Add RegisterToken
  | Sub RegisterToken
  | Neg
  | JumpToLabel JumpToLabelInstruction
  | Jro RegisterToken

-- A single jump instruction
data JumpToLabelInstruction =
  Jmp String
  | Jez String
  | Jnz String
  | Jlz String
  | Jgz String

-- just the basic body of a jump instruction without the parameters required to make it a valid instruction
data JumpToLabelToken =
  JmpToken | JezToken | JnzToken | JlzToken | JgzToken

-- all adressable registers: ports and internal registers
-- register 'BAK' is non-adressable so it's omitted
-- also includes support for constant integers, which may be used in place of all registers
data RegisterToken =
  Acc | Left | Right | Up | Down | Any | Last | Nil 
  | Constant Int

main :: Effect Unit
main = do
  log "hello"

program :: Parser String (Array Tis100Token)
program =
  many (instruction <|> label <|> comment)

label :: Parser String Tis100Token
label = do
  skipSpaces
  l <- some $ satisfy asciiAlpha 
  _ <- string ":"
  pure $ Label (fromCharArray l)
  
comment :: Parser String Tis100Token
comment = do
  skipSpaces
  _ <- string "--"
  c <- many $ satisfy asciiAlpha
  _ <- string "\n"
  pure $ Comment (fromCharArray c)

instruction :: Parser String Tis100Token
instruction = do
  skipSpaces
  instr <- choice
    [ swp
    , nop
    , sav
    , jumpToLabel
    ]
  pure $ Instruction instr

-- Instructions that take no parameters

nop :: Parser String InstructionToken
nop = do
  _ <- string "nop"
  pure Nop

sav :: Parser String InstructionToken
sav = do
  _ <- string "sav"
  pure Sav

swp :: Parser String InstructionToken
swp = do
  _ <- string "swp"
  pure Swp

neg :: Parser String InstructionToken
neg = do
  _ <- string "neg"
  pure Neg

-- Instructions that take a single register as a parameter

add :: Parser String InstructionToken
add = do
  _ <- string "add"
  skipSpaces
  reg <- registerToken
  pure $ Add reg

sub :: Parser String InstructionToken
sub = do
  _ <- string "sub"
  skipSpaces
  reg <- registerToken
  pure $ Sub reg

jro :: Parser String InstructionToken
jro = do
  _ <- string "jro"
  skipSpaces
  reg <- registerToken
  pure $ Jro reg

-- Instructions that take a single jump label as a paramter

-- wrapper around jumpToLabelInstruction
jumpToLabel :: Parser String InstructionToken
jumpToLabel = do
  j <- jumpToLabelInstruction
  pure $ JumpToLabel j

jumpToLabelInstruction :: Parser String JumpToLabelInstruction
jumpToLabelInstruction = do
  let jmp = string "jmp" >>= \_ -> pure JmpToken
      jez = string "jez" >>= \_ -> pure JezToken
      jnz = string "jnz" >>= \_ -> pure JnzToken
      jgz = string "jgz" >>= \_ -> pure JgzToken
      jlz = string "jlz" >>= \_ -> pure JlzToken
  jmpToken <- choice [jmp, jez, jnz, jlz, jgz]
  skipSpaces
  myLabel <- some $ satisfy asciiAlpha
  pure $ case jmpToken of
    JmpToken -> Jmp (fromCharArray myLabel)
    JezToken -> Jez (fromCharArray myLabel)
    JnzToken -> Jnz (fromCharArray myLabel)
    JgzToken -> Jgz (fromCharArray myLabel)
    JlzToken -> Jlz (fromCharArray myLabel)

-- instructions that take two registers as parameters

mov :: Parser String InstructionToken
mov = do
  _ <- string "mov"
  from <- registerToken
  to <- registerToken
  pure $ Mov from to

-- end of instructions


-- parses a single register token, which can be either a real register or an integer constant
registerToken :: Parser String RegisterToken
registerToken = normalRegister <|> constInteger

-- parses a normal register identifier
normalRegister :: Parser String RegisterToken
normalRegister = do
  reg <- (string "acc" >>= \_ -> pure Acc)
        <|> (string "left" >>= \_ -> pure Left)
        <|> (string "right" >>= \_ -> pure Right)
        <|> (string "up" >>= \_ -> pure Up)
        <|> (string "down" >>= \_ -> pure Down)
        <|> (string "nil" >>= \_ -> pure Nil)
        <|> (string "any" >>= \_ -> pure Acc)
        <|> (string "last" >>= \_ -> pure Last)
  pure reg

-- parses a single integer between -999 and 999
constInteger :: Parser String RegisterToken
constInteger = do
  minus <- optionMaybe $ satisfy (\c -> c == '-')
  digits <- some $ satisfy isDigit
  let number = Ints.fromString $ fromCharArray digits
  pos <- position
  case number of
    Nothing -> 
      failWithPosition "Cannot parse integer" pos
    Just a ->
      let with_sign = if isJust minus then negate a else a
      in
      if with_sign <= 999 && with_sign >= (-999) then
        pure $ Constant a
      else
        failWithPosition "Cannot declare integers outside of range -999..999" pos

-- true if the character is both ascii and alpha
asciiAlpha :: Char -> Boolean
asciiAlpha c =
  (isAlpha c) && (isAscii c)