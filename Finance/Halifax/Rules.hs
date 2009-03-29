module Finance.Halifax.Rules where

import Data.Maybe
import Data.List

import Text.Regex.Posix

import Finance.Halifax.Core
import Finance.Halifax.Utilities


data Rule = Rule {
        ru_regex :: String,
        ru_new_counterparty :: Maybe String,
        ru_new_category :: Category
    }
    deriving (Eq)

instance Show Rule where
    show rule = concat $ intersperse "\r\n" $ catMaybes [Just (show (ru_regex rule)), ru_new_counterparty rule, Just (ru_new_category rule)]
    showList rules _ = unlines $ intersperse "" $ map show rules

applyRules :: [Rule] -> Transaction -> Transaction
applyRules rules transaction = fromMaybe transaction $ firstJust (map (flip applyRule transaction) rules)

applyRule :: Rule -> Transaction -> Maybe Transaction
applyRule rule transaction
  | tr_counterparty transaction =~ ru_regex rule
  = Just $ transaction { tr_counterparty = fromMaybe (tr_counterparty transaction) (ru_new_counterparty rule), tr_category = Just (ru_new_category rule) }
  | otherwise
  = Nothing