module NumberFormat (
    Decimal(..)
  , RoundingMode(..)
  , NumberFormat(..)
  , IntegerFormat(..)
  , DecimalFormat(..)
  , formatNumber) where

import GHC.Float (float2Double)

class (Num a, Ord a) => Decimal a where
    intPart  :: a -> Integer
    fracPart :: a -> Double

instance Decimal Int where
    intPart  x = toInteger x
    fracPart x = 0

instance Decimal Integer where
    intPart  x = x
    fracPart x = 0

instance Decimal Float where
    intPart  x = floor x
    fracPart x = float2Double x - fromIntegral (floor x)

instance Decimal Double where
    intPart  x = floor x
    fracPart x = x - fromIntegral (floor x)


data RoundingMode = RoundToZero
                  | RoundAwayZero
                  | RoundDown
                  | RoundUp
                  | RoundEven

data NumberFormat = NumberFormat {
    _posSuffix :: String
  , _posPrefix :: String
  , _negSuffix :: String
  , _negPrefix :: String
  , _blankChr  :: Char
  , _intPart   :: IntegerFormat
  , _fracPart  :: DecimalFormat
}

data IntegerFormat = IntegerFormat {
    _thousandSep  :: Char
  , _intNumDigits :: Int
}

data DecimalFormat = NoFormat | DecimalFormat {
    _radixSep      :: Char
  , _fracNumDigits :: Int
  , _roundingMode  :: RoundingMode
}

formatNumber :: Decimal a => NumberFormat -> a -> String
formatNumber nf x | x < 0     = _negPrefix nf ++ fmt (abs x) ++ _negSuffix nf
                  | otherwise = _posPrefix nf ++ fmt x ++ _posSuffix nf
  where
    fmt x = formatIntPart (_intPart nf) ip ++ formatFracPart (_fracPart nf) fp
      where
        ip  = intPart x
        fp  = fracPart x

formatFracPart :: DecimalFormat -> Double -> String
formatFracPart df x = case df of
    NoFormat              -> ""
    DecimalFormat sep n _ -> sep : if x == 0
        then "0"
        else take n . drop 2 . show $ x

formatIntPart :: IntegerFormat -> Integer -> String
formatIntPart (IntegerFormat sep _) d = go d
  where
    go d | d < 1000  = show d
         | otherwise = go q ++ (sep : padshow r) where (q, r) = quotRem d 1000

    padshow d | d < 1     = "000"
              | d < 10    = "00" ++ show d
              | d < 100   = "0"  ++ show d
              | otherwise =         show d



---------------------------------
-- International Number Formats
---------------------------------

defaultFormat = NumberFormat {
    _posPrefix = ""
  , _negPrefix = ""
  , _posSuffix = ""
  , _negSuffix = ""
  , _blankChr  = ' '
  , _intPart  = IntegerFormat { _thousandSep = ',', _intNumDigits = 0 }
  , _fracPart = DecimalFormat { _radixSep  = '.', _fracNumDigits = 15, _roundingMode = RoundEven }
}

formatSIFrench = defaultFormat {
    _intPart  = IntegerFormat { _thousandSep = ' ', _intNumDigits = 0 }
  , _fracPart = DecimalFormat { _radixSep  = ',', _fracNumDigits = 15, _roundingMode = RoundEven }
}
}

formatSIEnglish = defaultFormat {
    _intPart  = IntegerFormat { _thousandSep = ' ', _intNumDigits = 0 }
  , _fracPart = DecimalFormat { _radixSep  = '.', _fracNumDigits = 15, _roundingMode = RoundEven }
}

formatPoint = defaultFormat {
    _intPart  = IntegerFormat { _thousandSep = ',', _intNumDigits = 0 }
  , _fracPart = DecimalFormat { _radixSep  = '.', _fracNumDigits = 15, _roundingMode = RoundEven }
}

formatComma = defaultFormat {
    _intPart  = IntegerFormat { _thousandSep = '.', _intNumDigits = 0 }
  , _fracPart = DecimalFormat { _radixSep  = ',', _fracNumDigits = 15, _roundingMode = RoundEven }
}

formatSwiss = defaultFormat {
    _intPart  = IntegerFormat { _thousandSep = '\'', _intNumDigits = 0 }
  , _fracPart = DecimalFormat { _radixSep  = '.', _fracNumDigits = 15, _roundingMode = RoundEven }
}