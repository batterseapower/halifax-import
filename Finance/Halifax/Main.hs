module Main where

import Data.Monoid

import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit
import System.IO

import Finance.Halifax.CSV
import Finance.Halifax.Ledger
import Finance.Halifax.QIF
import Finance.Halifax.Options
import Finance.Halifax.Rules
import Finance.Halifax.RulesParser (parseRules)
import Finance.Halifax.StatementParser (parseStatement)
import Finance.Halifax.Utilities


main :: IO ()
main = do
    args <- getArgs
    let (optionss, page_paths, errors) = getOpt Permute option_descriptions args
    if notNull errors
      then do
          mapM_ putStrLn errors
          putStrLn $ usageInfo "halifax-ledger" option_descriptions
          exitWith (ExitFailure 1)
      else do
          let options = mconcat optionss
          
          -- Read the page files and the statement they contain
          pages <- mapM readFile page_paths
          let (account, transactions) = parseStatement pages
          hPutStrLn stderr (show transactions)
          
          -- Read the rules (if any) and apply them to the transactions from the pages
          rules <- maybe (return []) (fmap parseRules . readFile) $ opt_rules_file options
          let transactions' = map (applyRules rules) transactions
          
          -- Output the data in the appropriate format
          let output_method = case opt_output_method options `orElse` QIF of
                                  QIF    -> outputQIF
                                  Ledger -> outputLedger
                                  CSV    -> outputCSV
          output_method options account transactions'