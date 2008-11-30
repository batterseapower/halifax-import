module Main where

import Control.Monad

import Data.List
import Data.Maybe
import Data.Monoid

import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO

import Ledger.Halifax.Core
import Ledger.Halifax.Rules
import Ledger.Halifax.RulesParser (parseRules)
import Ledger.Halifax.StatementParser (parseStatement)
import Ledger.Halifax.Utilities


data Options = Options {
        opt_rules_file :: Maybe FilePath,
        opt_account_category :: Maybe Category
    }

instance Monoid Options where
    mempty = Options {
            opt_rules_file = Nothing,
            opt_account_category = Nothing
        }
    mappend o1 o2 = Options {
            opt_rules_file = opt_rules_file o1 `mplus` opt_rules_file o2,
            opt_account_category = opt_account_category o1 `mplus` opt_account_category o2
        }

option_descriptions :: [OptDescr Options]
option_descriptions = [
        Option ['r'] ["rules"] (ReqArg (flip Options Nothing . Just) "filename") "Rules file to use for processing transactions",
        Option ['a'] ["account"] (ReqArg (Options Nothing . Just) "category") "Category to attribute this account to"
    ]


main :: IO ()
main = do
    args <- getArgs
    let (optionss, page_paths, errors) = getOpt Permute option_descriptions args
    if notNull errors
      then do
          mapM putStrLn errors
          putStrLn $ usageInfo "halifax-ledger" option_descriptions
      else do
          let options = mconcat optionss
              account_category = fromMaybe "Assets:Bank:Current" (opt_account_category options)
          rules <- maybe (return []) (fmap parseRules . readFile) $ opt_rules_file options
          pages <- mapM readFile page_paths
          let (account, transactions) = parseStatement pages
              transactions' = map (applyRules rules) transactions
              (bad_entries, ok_entries) = splitEithers $ map (showLedgerTransaction account_category) transactions'
          --print account
          putStrLn (unlines ok_entries)
          when (notNull bad_entries) $ do
              hPutStrLn stderr "Some entries did not have any category matched by the rules:"
              hPutStrLn stderr (unlines $ nub bad_entries)
                  