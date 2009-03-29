module Finance.Halifax.Options where

import Finance.Halifax.Core (Category)

import System.Console.GetOpt

import Control.Monad (mplus)

import Data.Char
import Data.List
import Data.Monoid


data OutputMethod = Ledger
                  | QIF

instance Show OutputMethod where
    show Ledger = "ledger"
    show QIF    = "qif"

instance Read OutputMethod where
    readsPrec _ s | "ledger" `isPrefixOf` lower_s = [(Ledger, drop 6 s)]
                  | "qif"    `isPrefixOf` lower_s = [(QIF, drop 3 s)]
                  | otherwise                     = []
      where lower_s = map toLower s


data Options = Options {
        opt_rules_file :: Maybe FilePath,
        opt_ledger_account_category :: Maybe Category,
        opt_output_method :: Maybe OutputMethod
    }

instance Monoid Options where
    mempty = Options {
            opt_rules_file = Nothing,
            opt_ledger_account_category = Nothing,
            opt_output_method = Nothing
        }
    mappend o1 o2 = Options {
            opt_rules_file = opt_rules_file o1 `mplus` opt_rules_file o2,
            opt_ledger_account_category = opt_ledger_account_category o1 `mplus` opt_ledger_account_category o2,
            opt_output_method = opt_output_method o1 `mplus` opt_output_method o2
        }

option_descriptions :: [OptDescr Options]
option_descriptions = [
        Option ['f'] ["output-format"] (ReqArg (\s -> mempty { opt_output_method = Just (read s) }) "ledger|ofx")
            "Format to output the parsed file in",
        Option ['r'] ["rules"] (ReqArg (\s -> mempty { opt_rules_file = Just s }) "filename")
            "Rules file to use for processing transactions",
        Option ['c'] ["ledger-category"] (ReqArg (\s -> mempty { opt_ledger_account_category = Just s }) "category")
            "Category to attribute this account to in Ledger output"
    ]