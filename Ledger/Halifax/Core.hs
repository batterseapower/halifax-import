module Ledger.Halifax.Core where

import Data.Maybe
import Data.Time

import System.Locale


type Amount = Rational

data Account = Account {
        acc_sort_code :: String,
        acc_number :: Int,
        acc_roll_number :: String
    }
    deriving (Eq, Show)

data TransactionType = Debit | Credit
    deriving (Eq, Show)

type Category = String

data Transaction = Transaction {
        tr_time :: UTCTime,
        tr_comments :: [String],
        tr_counterparty :: String,
        tr_category :: Maybe Category,
        tr_type :: TransactionType,
        tr_amount :: Amount
    }
    deriving (Eq, Show)

-- Output something like this:
--
-- 2004/05/27 Book Store
--  Expenses:Books                 $20.00
--  Assets:Bank:Current
showLedgerTransaction :: Category -> Transaction -> Either String String
showLedgerTransaction account_category transaction
  | isJust mb_category = Right $ unlines [
        unwords [date, who],
        " " ++ to ++ amount_padding ++ amount,
        " " ++ from
    ]
  | otherwise = Left who
  where
    date = formatTime defaultTimeLocale "%Y/%m/%d" (tr_time transaction)
    who = tr_counterparty transaction
    mb_category = tr_category transaction
    category = fromJust mb_category
    (from, to) = case tr_type transaction of
        Debit  -> (account_category, category)
        Credit -> (category, account_category)
    amount_padding = replicate (31 - length to) ' '
    amount = "£" ++ show (fromRational (tr_amount transaction) :: Double)