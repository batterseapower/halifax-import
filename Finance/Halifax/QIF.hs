module Finance.Halifax.QIF where

import Finance.Halifax.Core
import Finance.Halifax.Options
import Finance.Halifax.Utilities

import System.Locale

import Data.Time.Format

-- http://web.intuit.com/support/quicken/docs/d_qif.html
-- http://svn.gnucash.org/trac/browser/gnucash/branches/2.2/src/import-export/qif-import/file-format.txt

outputQIF :: Options -> Account -> [Transaction] -> IO ()
outputQIF _options _account transactions = putStrLn $ unlines $ header ++ concatMap transaction_lines transactions
  where
    tab s = "    " ++ s
    
    -- Each transaction must end with a symbol, indicating the end of entry. Each item
    -- in the transaction must display on a separate line. When Quicken exports an account
    -- register or list, it adds a line to the top of the file that identifies the type of
    -- account or list.
    header =
        [ ""
        , tab "!Type:Bank" -- !Type:Cash or !Type:Invst might also be useful
        , ""
        ]
    
    -- I just blindly copied this from the QIF output Halifax was giving me
    transaction_lines transaction =
        -- Date
        [ "D" ++ formatTime defaultTimeLocale "%d/%m/%y" (tr_time transaction)
        , ""
        -- Amount
        , tab $ tab $ "T" ++ showFloatAsCurrencylike (fromRational ((case tr_type transaction of Debit -> negate; Credit -> id)
                                                                    (tr_amount transaction)) :: Double)
        -- Payee
        , "P" ++ tr_counterparty transaction ++ (if null (tr_comments transaction)
                                                 then ""
                                                 else " - " ++ unwords (tr_comments transaction))
        -- End of entry
        , "^"
        , ""
        ]
