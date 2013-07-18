module Reminder(Recurrence(..), Reminder(..)) where

import qualified Data.Time.Calendar as C

data Recurrence = Once | Weekly Int | Monthly Int
data Reminder = Reminder
	{
		reminderID :: Integer,
		start :: C.Day,
		recurrence :: Recurrence
	}

match :: C.Day -> Reminder -> Bool
match target (Reminder{start=start, recurrence=recur}) = case recur of
	Once -> target == start
	_ -> undefined
