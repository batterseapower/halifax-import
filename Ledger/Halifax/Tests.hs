module Main where

import Ledger.Halifax.Core
import Ledger.Halifax.Rules
import Ledger.Halifax.RulesParser
import Ledger.Halifax.StatementParser

import Paths_halifax_ledger

import Control.Monad

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)


main :: IO ()
main = defaultMain tests


readTestData :: FilePath -> IO String
readTestData = readFile <=< getDataFileName

tests :: [Test]
tests = [testCase "Statement parsing" test_statement_parse,
         testCase "Rule parsing" test_rules_parse]

test_statement_parse :: Assertion
test_statement_parse = do
    page_1 <- readTestData "Statement-Page-1.html"
    page_2 <- readTestData "Statement-Page-2.html"
    let parsed = parseStatement [page_1, page_2]
    parsed @?= undefined

test_rules_parse :: Assertion
test_rules_parse = do
    parsed <- fmap parseRules $ readTestData "Rules.rules"
    parsed @?= [Rule { ru_regex = "Amazon.co.uk", ru_new_counterparty = Nothing, ru_new_category = "Expense:Books" },
                Rule { ru_regex = "Mozy\\.Com 1111111111 US", ru_new_counterparty = Just "Mozy.Com", ru_new_category = "Expense:Backups" }]