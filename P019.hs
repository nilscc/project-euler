data Year = Year { year        :: Int   -- ^ e.g. 2012
                 , firstSunday :: Int   -- ^ day of the first sunday in the year
                 } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- number of days in each month

jan, mar, apr, mai, jun, jul, aug, sep, okt, nov, dec :: Int
feb :: Year -> Int

jan = 31
feb (Year y _)
  | y `mod` 400 == 0 = 29
  | y `mod` 100 /= 0
  , y `mod` 4   == 0 = 29
  | otherwise        = 28
mar = 31
apr = 30
mai = 31
jun = 30
jul = 31
aug = 31
sep = 30
okt = 31
nov = 30
dec = 31

numberOfDays :: Year    -- ^ year
             -> Int
numberOfDays y =
  jan + feb y + mar + apr + mai + jun + jul + aug + sep + okt + nov + dec


--------------------------------------------------------------------------------
-- special cases

sundays :: Year         -- ^ year
        -> [Int]        -- ^ list of days in the year
sundays y@Year{ firstSunday = f } =
  [ d | d <- [1..numberOfDays y]
      , (d - f) `mod` 7 == 0 ]

isFirst :: Year         -- ^ year
        -> Int          -- ^ day in the year
        -> Bool
isFirst y d =
  d `elem` scanl (+) 1 [jan, feb y, mar, apr, mai, jun, jul, aug, sep, okt, nov]


--------------------------------------------------------------------------------

nextYear :: Year -> Year
nextYear yr@(Year y _) =
  Year (y+1) (last (sundays yr) + 7 - numberOfDays yr)

prevYear :: Year -> Year
prevYear (Year y f) =
  Year (y-1) $ case (numberOfDays (Year (y-1) 0) + f) `mod` 7 of
                    0  -> 7
                    f' -> f'

getYear :: Int -> Year
getYear y
  | y >= 1900 = iterate nextYear year1900 !! (y - 1900)
  | otherwise = iterate prevYear year1900 !! (1900 - y)
 where
  year1900    = Year 1900 7

instance Enum Year where
  succ                = nextYear
  pred                = prevYear
  toEnum              = getYear
  fromEnum (Year y _) = y


--------------------------------------------------------------------------------
-- Project euler problem solving!

p19 :: Int
p19 = length
  [ sunday | year'  <- [getYear 1901 .. getYear 2000]
           , sunday <- sundays year'
           , isFirst year' sunday
           ]
