module Finance.Halifax.StatementParser (Transaction(..), parseStatement) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Time

import System.Locale

import Text.HTML.TagSoup

import Finance.Halifax.Core
import Finance.Halifax.Utilities

parseStatement :: [String] -> (Account, [Transaction])
parseStatement pages = (parseAccountInfo account_info, transactions)
  where
    -- Halifax seems to include stray non-ASCII characters in the files, for some reason (these are the ones that mean we have to read in binary mode).
    tagss = map (parseTags . map (\c -> if isAscii c then c else ' ')) pages
    account_info = fst $ fragment (~== "<table class=bankingSummaryContainer>") (~== "<table class=bankingSummaryInfo>") (head tagss)
    
    transaction_infoss = map (fst . fragment (~== "<table class=DataTable>") (~== "</table>")) tagss
    transaction_rowss = map (drop 1 . partitions (~== "<tr>")) transaction_infoss
    transactions = combinePartialTransactions (map parseTransactionRow (concat transaction_rowss))

parseAccountInfo :: [Tag] -> Account
parseAccountInfo tags = Account {
        acc_sort_code   = get_summary_for "Sort code",
        acc_number      = read $ get_summary_for "Account number",
        acc_roll_number = get_summary_for "Roll number"
    }
  where
    get_summary_for key = filter (not . isSpace) . maybe (error $ "No value for " ++ key) id . flip lookup summaries $ key
    summaries = unfoldr unfold_summary tags
    unfold_summary tags' = case dropWhile (~/= "<td class=summaryBoxesText>") tags' of
        []     -> Nothing
        tags'' -> let (name_tags, tags''') = span (~/= "</td>") tags''
                      (value_tags, tags'''') = fragment (~== "<td class=summaryBoxesValues") (~== "</td>") tags'''
                  in Just ((innerText name_tags, innerText value_tags), tags'''')

parseTransactionRow :: [Tag] -> (Maybe UTCTime, String, Maybe (TransactionType, Amount))
parseTransactionRow tags = (mb_date, text, mb_event)
  where
    [date_cell, text_cell, out_cell, in_cell, _balance_cell] = partitions (~== "<td>") tags
    mb_date = parseTime defaultTimeLocale "%d %b %Y" $ innerText date_cell
    text = innerText text_cell
    mb_out = parseAmount_maybe (innerText out_cell)
    mb_in = parseAmount_maybe (innerText in_cell)
    
    mb_event = case (mb_out, mb_in) of
        (Just amount, Nothing) -> Just (Debit, amount)
        (Nothing, Just amount) -> Just (Credit, amount)
        (Nothing, Nothing)     -> Nothing
        _                      -> error "parseTransactionRow: got both an ingoing and outgoing amount!"

parseAmount_maybe :: String -> Maybe Amount
parseAmount_maybe text
  | [amount] <- [fromInteger pounds + (fromInteger pence / 100)
                     | (pounds, _) <- readsPrec 0 (filter (/= ',') pounds_text)
                     , (pence, _) <- readsPrec 0 (tail dot_pence_text)]
  = Just amount
  | otherwise
  = Nothing
  where
    text' = trim text
    (pounds_text, dot_pence_text) = span (/= '.') text'


combinePartialTransactions :: [(Maybe UTCTime, String, Maybe (TransactionType, Amount))] -> [Transaction]
combinePartialTransactions = go (error "No date detected before the first transaction!") []
  where
    go time acc_extras partial_transactions = case partial_transactions of
        [] -> if null acc_extras then [] else error ("Unexpected left over extras:\n" ++ unlines acc_extras)
        ((mb_new_time, text, mb_event) : rest) ->
            let time' = fromMaybe time mb_new_time
            in case mb_event of
                Nothing                   -> go time' (text : acc_extras) rest
                Just (event_type, amount) -> Transaction time' (reverse acc_extras) text Nothing event_type amount : go time' [] rest