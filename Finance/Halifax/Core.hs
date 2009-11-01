module Finance.Halifax.Core where

import Data.Time


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