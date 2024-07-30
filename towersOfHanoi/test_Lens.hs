{-# LANGUAGE TemplateHaskell #-}
import Data.Lens.Lazy
     ( (~=), access, (%=), mapLens, (^=), (^.), (^%=), (^%%=), (^$) )
import Data.Lens.Template ( makeLenses )
import Data.Char ( toUpper )
import Data.Map ( Map, fromList )
import Control.Monad.State.Lazy ( Monad(return), State, runState )
-- Next we define the all too familiar Person data type, which has an Address data type as one of its fields
data Person = Person {
   _fullName :: String,
   _familiarName :: String,
   _surName  :: String,
   _address  :: Address } 
              
data Address = Address {
   _which :: String,
   _street :: String,
   _city :: String,
   _state :: String,
   _zip  :: String }