{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Control.Monad.State

data Name = Name { _firstName :: String, _surname :: String }
  deriving Show
makeLenses ''Name

orwell :: Name
orwell = Name "George" "Orwell"

data Book = Book
  {
    _title :: String,
    _author :: Name
  } deriving Show
makeLenses ''Book

nineteenEightyFour :: Book
nineteenEightyFour = Book "Nineteen Eighty-Four" orwell

data Shape = Circle { _radius :: Int }
               | Rect { _width :: Int, _height :: Int }
               | Triangle { _side1 :: Int, _side2 :: Int, _side3 :: Int }
  deriving Show
makeLenses ''Shape
makePrisms ''Shape

main :: IO ()
main = do
  -- Getters
  print $ view _1 ('a', 'b')                            -- 'a'
  print $ ('a', 'b') ^. _1                              -- 'a'
  print $ view title nineteenEightyFour                 -- "Nineteen Eighty-Four"
  print $ nineteenEightyFour ^. title                   -- "Nineteen Eighty-Four"
  -- Lens composition
  print $ view (author . surname) nineteenEightyFour    -- "Orwell
  -- Setters
  print $ set _1 "apple" ("banana", "orange")           -- ("apple","orange")
  print $ _1 .~ "apple" $ ("banana", "orange")          -- ("apple","orange")
  print $ set title "1984" nineteenEightyFour           -- Book {_title = "1984", _author = Name {_firstName = "George", _surname = "Orwell"}}
  print $ title .~ "1984" $ nineteenEightyFour          -- Book {_title = "1984", _author = Name {_firstName = "George", _surname = "Orwell"}}
  -- Setters that also return result
  print $ ('a','b') & _1 <.~ 'c'                        -- ('c',('c','b'))
  print $ title <.~ "1984" $ nineteenEightyFour         -- ("1984",Book {_title = "1984", _author = Name {_firstName = "George", _surname = "Orwell"}})
  -- Setters that also return old value
  print $ ('a','b') & _1 <<.~ 'c'                       -- ('a',('c','b'))
  print $ title <<.~ "1984" $ nineteenEightyFour        -- ("Nineteen Eighty-Four",Book {_title = "1984", _author = Name {_firstName = "George", _surname = "Orwell"}})
  -- Function application
  print $ over _2 (+100) ("dalmations", 1)              -- ("dalmations",101)
  print $ _2 %~ (+100) $ ("dalmations", 1)              -- ("dalmations",101)
  print $ over title (++"!!!") nineteenEightyFour       -- Book {_title = "Nineteen Eighty-Four!!!", _author = Name {_firstName = "George", _surname = "Orwell"}}
  print $ title %~ (++"!!!") $ nineteenEightyFour       -- Book {_title = "Nineteen Eighty-Four!!!", _author = Name {_firstName = "George", _surname = "Orwell"}}
  -- Function application that also returns result
  print $ _2 <%~ (+100) $ ("dalmations", 1)             -- (101,("dalmations",101))
  print $ title <%~ (++"!!!") $ nineteenEightyFour      -- ("Nineteen Eighty-Four!!!",Book {_title = "Nineteen Eighty-Four!!!", _author = Name {_firstName = "George", _surname = "Orwell"}})
  -- Function application that also returns old value
  print $ _2 <<%~ (+100) $ ("dalmations", 1)            -- (1,("dalmations",101))
  print $ title <<%~ (++"!!!") $ nineteenEightyFour     -- ("Nineteen Eighty-Four",Book {_title = "Nineteen Eighty-Four!!!", _author = Name {_firstName = "George", _surname = "Orwell"}})

  -- Stateful getters
  print $ evalState (use _1) ("hello","world")          -- "hello"
  print $ evalState (use title) nineteenEightyFour      -- "Nineteen Eighty-Four"
  -- Stateful setters
  print $ execState (assign _1 'c') ('a','b')                -- ('c','b')
  print $ execState (_1 .= 'c') ('a','b')                    -- ('c','b')
  print $ execState (assign title "1984") nineteenEightyFour -- Book {_title = "1984", _author = Name {_firstName = "George", _surname = "Orwell"}}
  print $ execState (title .= "1984") nineteenEightyFour     -- Book {_title = "1984", _author = Name {_firstName = "George", _surname = "Orwell"}}
  print $ execState (do _1 .= 'c'; _2 .= 'd') ('a','b')      -- ('c','d')
  -- Stateful setters that also return result
  print $ runState (_1 <.= 'c') ('a','b')                    -- ('c',('c','b'))
  -- Stateful setters that also return old values
  print $ runState (_1 <<.= 'c') ('a','b')                   -- ('a',('c','b'))
  -- Stateful function application
  print $ execState (_2 %= tail) ("hello","world")           -- ("hello","orld")
  -- Stateful function application that also returns result
  print $ runState (_2 <%= tail) ("hello","world")           -- ("orld",("hello","orld"))
  -- Stateful function application that also returns old value
  print $ runState (_2 <<%= tail) ("hello","world")          -- ("world",("hello","orld"))

  -- Using prisms to go down one branch
  print $ preview _Left (Left "hello")                         -- Just "hello"
  print $ Left "hello" ^? _Left                                -- Just "hello"
  print $ preview _Left (Right "world" :: Either String String) -- Nothing
  print $ (Right "world" :: Either String String) ^? _Left      -- Nothing
  print $ preview _Right (Right "world")                       -- Just "world"
  print $ Right "world" ^? _Right                              -- Just "world"
  print $ preview _Right (Left "hello" :: Either String String) -- Nothing
  print $ (Left "hello" :: Either String String) ^? _Right      -- Nothing
  print $ preview _Cons ([] :: String)                          -- Nothing
  print $ ([] :: String) ^? _Cons                               -- Nothing
  print $ preview _Cons "hello world"                          -- Just ('h',"ello world")
  print $ "hello world" ^? _Cons                               -- Just ('h',"ello world")
  print $ preview radius (Circle 5)                            -- Just 5
  print $ (Circle 5) ^? radius                                 -- Just 5
  print $ preview radius (Rect 3 5)                            -- Nothing
  print $ (Rect 3 5)^? radius                                  -- Nothing
  print $ preview width (Rect 3 5)                             -- Just 3
  print $ (Rect 3 5)^? width                                   -- Just 3
  print $ preview width (Circle 5)                             -- Nothing
  print $ (Circle 5) ^? width                                  -- Nothing

  -- Using prisms to go "back up" a branch
  print $ (review _Left "hello" :: Either String String)        -- Left "hello"
  print $ (review _Right "world" :: Either String String)       -- Left Right "world"
  print $ review _Circle 5                                     -- Circle {_radius = 5}
  print $ review _Rect (7, 8)                                  -- Rect {_width = 7, _height = 8}
  print $ review _Triangle (1,2,3)                             -- Triangle {_side1 = 1, _side2 = 2, _side3 = 3}

  -- preview _ParseTrue "True"
  -- review _ParseTrue True
  -- preview _ParseFalse "True"
  -- review _ParseFalse True
  -- print $ review radius 7        -- Left "hello"
  -- Operators that begin with ^ are kinds of views.
  -- Operators that end with ~ are like over or set.
  -- Operators that have . in them are usually somehow "basic"
  -- Operators that have % in them usually take functions
  -- Operators that have = in them are just like their cousins where = is replaced by ~, but instead of taking the whole object as an argument, they apply their modifications in a State monad.

-- import Data.Monoid
-- (Sum 1, 2) & _1 <>~ Sum 3         -- (Sum {getSum = 4},2)
