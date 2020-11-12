{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes    #-}

module Main where

import Control.Monad.State

------------
data Free f a
    = Free (f (Free f a))
    | Done a

instance (Functor f) => Functor (Free f) where
  fmap f (Done x) = Done (f x)
  fmap f (Free m) = Free $ fmap f <$> m

-- F = Free f,  (Free f) (a -> b) <*> (Free f) a -> (Free f) b
instance (Functor f) => Applicative (Free f) where
  pure = Done
  (Done g) <*> x = g <$> x
  (Free m) <*> x = Free ((<*> x) <$> m)

instance (Functor f) => Monad (Free f) where
    return = Done
    Done x >>= g = g x
    Free m >>= g = Free ((>>= g) <$> m)

-- instance (Functor f, Show f) => Show (Free f) where
--     show = 

-- identity functor usage
data Identitat a = Identitat a deriving (Show, Functor)

type Id = Free Identitat

id1 :: Id Int
id1 = Done 3

id2 :: Id Int
id2 = Free $ Identitat $ Free $ Identitat (Done 10)

id3 :: Id Int
id3 = Free $ Identitat $ Free $ Identitat $ Free $ Identitat (Done 42)

extractId :: Id a -> a
extractId (Done x) = x
extractId (Free (Identitat m)) = extractId m

-----------------------------------------------------------------------

newtype UserId = UserId
    { unUserId :: Int
    }

data BillingF next
    = GetUserBalance UserId (Double -> next)
    | ChargeUser UserId Double next
    deriving Functor

type Billing = Free BillingF

getUserBalance :: UserId -> Billing Double
getUserBalance userId = Free (GetUserBalance userId Done)

chargeUser :: UserId -> Double -> Billing ()
chargeUser uid amt = Free (ChargeUser uid amt (Done ()))

-----------------------------------------------------------------------

foldFree
    :: (Functor f, Monad m)
    => (forall a. f a -> m a)
    -> Free f a
    -> m a
foldFree morph (Done a) = return a
foldFree morph (Free f) = do
    a <- morph f
    foldFree morph a
------------------------------------------------------------------------

data TerminalF next
    = GetLine (String -> next)
    | PrintLine String next
    | OpenFile String (String -> next)
    deriving Functor

type Terminal = Free TerminalF

getLineCmd :: Terminal String
getLineCmd = Free (GetLine (\str -> Done str))

printLineCmd :: String -> Terminal ()
printLineCmd str = Free (PrintLine str (Done ()))

openFileCmd :: String -> Terminal String
openFileCmd filename = Free (OpenFile filename (\str -> Done str))
-----------------------------------------------------------------------

interpret :: Terminal a -> IO a
interpret = foldFree morph
  where
    morph :: TerminalF a -> IO a
    morph (GetLine next) =
        next <$> getLine
    morph (PrintLine s n) = do
        putStrLn s
        pure n
    morph (OpenFile filename next) =
        next <$> readFile filename

-----------------------------------------------------------------------

showProgram :: Show a => Terminal a -> String
showProgram (Free (GetLine next)) =
    "GetLine\r\n" ++ (showProgram $ next "read line from console")
showProgram (Free (PrintLine s next)) =
    "PrintLine: " ++ s ++ "\r\n" ++ showProgram next
showProgram (Free (OpenFile file next)) =
    "ReadFile: " ++ file ++ "\r\n" ++ (showProgram $ next "read file contents")
showProgram (Done r) =
    "EndOfProgram: " ++ show r ++ "\r\n"

-----------------------------------------------------------------------

-- interpretTest :: Terminal a -> State Mock a
-- interpretTest = foldFree morph
--   where
--     morph = undefined
    -- morph :: TerminalF a -> IO a
    -- morph (GetLine next) =
    --     next <$> getLine
    -- morph (PrintLine s n) = do
    --     putStrLn s
    --     pure n
    -- morph (OpenFile filename next) =
    --     next <$> readFile filename

-----------------------------------------------------------------------
myProgram :: Terminal ()
myProgram = do
  printLineCmd "filename you want to open:"
  filename <- getLineCmd
  content <- openFileCmd filename
  let names = lines content
  mapM_ printLineCmd names
  printLineCmd "This is the End"

main :: IO () 
main = do
--   print $ extractId $ square <$> id1
--   print $ extractId $ square <$> id2
--   print $ extractId $ suma <$> id1 <*> id2
--   print $ extractId $ suma <$> id2 <*> id3
  -- let programa = getLineCmd >>= printLineCmd
  interpret myProgram
  -- putStr $ showProgram myProgram
    where
  square x = x * x
  suma x y = x + y