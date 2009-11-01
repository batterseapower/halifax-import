module Finance.Halifax.CSV where

import Finance.Halifax.Core
import Finance.Halifax.Options
import Finance.Halifax.Utilities

import System.Locale

import Data.Time.Format
import Data.List

-- This CSV is designed to exactly match that you get from Halifax's front end
outputCSV :: Options -> Account -> [Transaction] -> IO ()
outputCSV _options _account transactions = putStrLn $ unlines $ header : map transaction_line transactions
  where
    header = "Date,Amount,Description"
    transaction_line transaction = intercalate "," [date, amount, description]
      where
        date = formatTime defaultTimeLocale "%d/%m/%y" (tr_time transaction)
        amount = showFloatAsCurrencylike (fromRational ((case tr_type transaction of Debit -> negate; Credit -> id)
                                                        (tr_amount transaction)) :: Double)
        description = tr_counterparty transaction ++ (if null (tr_comments transaction)
                                                      then ""
                                                      else " - " ++ unwords (tr_comments transaction))
