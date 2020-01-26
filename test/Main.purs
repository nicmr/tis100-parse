module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import TIS100.Parse (program, Tis100Token(..), InstructionToken(..), RegisterToken(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Text.Parsing.Parser (runParser)

main :: Effect Unit
main = launchAff_ $
  runSpec [consoleReporter] do
    describe "Individual constructs" do
      it "should parse a comment correctly" do
        case runParser "#funfun\n" program of
          Left err -> fail $ "failed to parse Comment: " <> show err
          Right ok -> [Comment "funfun"] `shouldEqual` ok

      it "should parse a label correctly" do
        case runParser "start:" program of
          Left err -> fail $ "failed to parse Label: " <> (show err)
          Right ok -> [Label "start"] `shouldEqual` ok

      it "should parse a mov instruction correctly" do
        case runParser "mov up down" program of
          Left err -> fail $ "failed to parse instruction: " <> show err
          Right ok -> [Instruction (Mov Up Down)] `shouldEqual` ok




