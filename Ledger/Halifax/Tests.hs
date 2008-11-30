module Main where

import Paths_halifax_ledger

import Control.Monad

import Data.Time.Calendar
import Data.Time.Clock

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Ledger.Halifax.Core
import Ledger.Halifax.Rules
import Ledger.Halifax.RulesParser
import Ledger.Halifax.StatementParser


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
    let (account, transactions) = parseStatement [page_1, page_2]
        utcDay day = UTCTime { utctDay = day, utctDayTime = secondsToDiffTime 0 }
    account @?= Account {
            acc_sort_code = "11-22-33",
            acc_number = 1234567,
            acc_roll_number = "D/12345678-9"
        }
    transactions @?= [
            Transaction {
                tr_time = utcDay $ fromGregorian 2008 11 29,
                tr_comments = [],
                tr_counterparty = "ATM",
                tr_category = Nothing,
                tr_type = Debit,
                tr_amount = 1
            },
            Transaction {
                tr_time = utcDay $ fromGregorian 2008 11 24,
                tr_comments = ["Location 1"],
                tr_counterparty = "Debit Card",
                tr_category = Nothing,
                tr_type = Debit,
                tr_amount = 101 / 100
            },
            Transaction {
                tr_time = utcDay $ fromGregorian 2008 11 24,
                tr_comments = ["Location 2"],
                tr_counterparty = "Debit Card",
                tr_category = Nothing,
                tr_type = Debit,
                tr_amount = 102 / 100
            },
            Transaction {
                tr_time = utcDay $ fromGregorian 2008 11 22,
                tr_comments = [],
                tr_counterparty = "Deposit Cheque",
                tr_category = Nothing,
                tr_type = Credit,
                tr_amount = 104 / 100
            },
            Transaction {
                tr_time = utcDay $ fromGregorian 2008 11 17,
                tr_comments = [],
                tr_counterparty = "Direct Debit",
                tr_category = Nothing,
                tr_type = Debit,
                tr_amount = 105 / 100
            },
            Transaction {
                tr_time = utcDay $ fromGregorian 2008 11 17,
                tr_comments = [],
                tr_counterparty = "ATM",
                tr_category = Nothing,
                tr_type = Debit,
                tr_amount = 106 / 100
            },
            Transaction {
                tr_time = utcDay $ fromGregorian 2008 11 17,
                tr_comments = ["Location 1"],
                tr_counterparty = "Debit Card",
                tr_category = Nothing,
                tr_type = Debit,
                tr_amount = 107 / 100
            },
            Transaction {
                tr_time = utcDay $ fromGregorian 2008 10 07,
                tr_comments = ["Location 2"],
                tr_counterparty = "Debit Card",
                tr_category = Nothing,
                tr_type = Debit,
                tr_amount = 109 / 100
            },
            Transaction {
                tr_time = utcDay $ fromGregorian 2008 10 07,
                tr_comments = [],
                tr_counterparty = "Cash",
                tr_category = Nothing,
                tr_type = Credit, 
                tr_amount = 110 / 100
            },
            Transaction {
                tr_time = utcDay $ fromGregorian 2008 10 06,
                tr_comments = ["Location 1"],
                tr_counterparty = "Debit Card",
                tr_category = Nothing,
                tr_type = Debit,
                tr_amount = 111 / 100
            }
        ]

test_rules_parse :: Assertion
test_rules_parse = do
    parsed <- fmap parseRules $ readTestData "Rules.rules"
    parsed @?= [Rule { ru_regex = "Amazon.co.uk", ru_new_counterparty = Nothing, ru_new_category = "Expense:Books" },
                Rule { ru_regex = "Mozy\\.Com 1111111111 US", ru_new_counterparty = Just "Mozy.Com", ru_new_category = "Expense:Backups" }]