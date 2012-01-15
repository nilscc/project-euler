main :: IO ()
main = print . sum . map numberOfLetters $ [1..1000]

numberOfLetters :: Int -> Int
numberOfLetters n =
  case toDigits n of
       [0] -> 0
       [1] -> one
       [2] -> two
       [3] -> three
       [4] -> four
       [5] -> five
       [6] -> six
       [7] -> seven
       [8] -> eight
       [9] -> nine
       [1,0] -> ten
       [1,1] -> eleven
       [1,2] -> twelve
       [1,3] -> thirteen
       [1,4] -> fourteen
       [1,5] -> fifteen
       [1,6] -> sixteen
       [1,7] -> seventeen
       [1,8] -> eighteen
       [1,9] -> nineteen
       [2,x] -> twenty + numberOfLetters x
       [3,x] -> thirty + numberOfLetters x
       [4,x] -> forty + numberOfLetters x
       [5,x] -> fifty + numberOfLetters x
       [6,x] -> sixty + numberOfLetters x
       [7,x] -> seventy + numberOfLetters x
       [8,x] -> eighty + numberOfLetters x
       [9,x] -> ninety + numberOfLetters x
       [h,0,0] -> numberOfLetters h + hundred
       [h,t,o] -> numberOfLetters h + hundred + and' + numberOfLetters (10*t + o)
       [th,0,0,0] -> numberOfLetters th + thousand
       _ -> error $ "No idea how to spell " ++ show n

toDigits :: Int -> [Int]
toDigits = map (read . return) . show

one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve,
  thirteen, fourteen, fifteen, sixteen, seventeen, eighteen, nineteen, twenty,
  thirty, forty, fifty, sixty, seventy, eighty, ninety, hundred, thousand, and' :: Int

one = length "one"
two = length "two"
three = length "three"
four = length "four"
five = length "five"
six = length "six"
seven = length "seven"
eight = length "eight"
nine = length "nine"
ten = length "ten"
eleven = length "eleven"
twelve = length "twelve"
thirteen = length "thirteen"
fourteen = length "fourteen"
fifteen = length "fifteen"
sixteen = length "sixteen"
seventeen = length "seventeen"
eighteen = length "eighteen"
nineteen = length "nineteen"
twenty = length "twenty"
thirty = length "thirty"
forty = length "forty"
fifty = length "fifty"
sixty = length "sixty"
seventy = length "seventy"
eighty = length "eighty"
ninety = length "ninety"
hundred = length "hundred"
thousand = length "thousand"
and' = length "and"
