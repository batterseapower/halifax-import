module Ledger.Halifax.RulesParser (parseRules) where

import Data.Maybe
import Data.List

import Ledger.Halifax.Rules
import Ledger.Halifax.Utilities


parseRules :: String -> [Rule]
parseRules = catMaybes . unfoldr unfold_one_rule . map trim . filter (not . ("#" `isPrefixOf`)) . lines
  where
    unfold_one_rule lines_left = case lines_left of
        ("":rest)
          -> Just (Nothing, rest)
        (regex_line:category_line:rest)
          | null rest || null (head rest)
          -> Just (Just (Rule regex_line Nothing category_line), drop 1 rest)
        (regex_line:counterparty_line:category_line:rest)
          -> Just (Just (Rule regex_line (Just counterparty_line) category_line), drop 1 rest)
        _ -> Nothing