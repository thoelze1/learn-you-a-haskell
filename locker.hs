import qualified Data.Map as Map
import Text.Read as Read
  
data LockerState = Taken | Free deriving (Show, Eq)    
type Code = String  
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing           -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"  
        Just (Taken, _)   -> Left $ "Locker " ++ show lockerNumber ++ " is taken!"
        Just (Free, code) -> Right code

{-
instance Functor (Map k) where
  fmap = Map.map
-}

rpnCalc :: (Read a, Fractional a) => String -> Maybe a
rpnCalc = helper [] . words
  where
    helper :: (Read a, Fractional a) => [a] -> [String] -> Maybe a
    helper (a:b:xs) ("*":es) = helper ((a * b):xs) es
    helper (a:b:xs) ("+":es) = helper ((a + b):xs) es
    helper (a:b:xs) ("-":es) = helper ((a - b):xs) es
    helper (a:b:xs) ("/":es) = helper ((a / b):xs) es
    helper xs (e:es) = case Read.readMaybe e of
      Nothing -> Nothing
      (Just x) -> helper (x:xs) es
    helper _ _ = Nothing
