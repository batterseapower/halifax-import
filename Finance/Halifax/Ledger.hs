module Finance.Halifax.Ledger (outputLedger) where

import Finance.Halifax.Core
import Finance.Halifax.Options
import Finance.Halifax.Utilities

import System.IO
import System.Locale

import Control.Monad

import Data.List
import Data.Maybe
import Data.Time.Format


outputLedger :: Options -> Account -> [Transaction] -> IO ()
outputLedger options _account transactions = do
    putStrLn (unlines ok_entries)
    when (notNull bad_entries) $ do
        hPutStrLn stderr "Some entries in the input to the Ledger formatter did not have any category assigned. Add rules to fix these:"
        hPutStrLn stderr (unlines $ nub bad_entries)
  where
    account_category = fromMaybe "Assets:Bank:Current" (opt_ledger_account_category options)
    (bad_entries, ok_entries) = splitEithers $ map (showLedgerTransaction account_category) transactions

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
    amount = "£" ++ showFloatAsCurrencylike (fromRational (tr_amount transaction) :: Double)
