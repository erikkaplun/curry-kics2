--module persons(correctEmail,correctDate) where

import HTML
import WUIjs
import Time(validDate)

-- a form that just shows its argument:
resultForm :: a -> IO HtmlForm
resultForm v = return $ form "Result" [htxt ("Modified value: "++show v)]

-- a date WUI:
wDate = wTriple (wSelectInt [1..31]) (wSelectInt [1..12]) wInt
           `withConditionJS` correctDate
           `withError` "Illegal date:"

correctDate (d,m,y) = validDate y m d

-- an email WUI:
wEmail = wStringSize 20 `withConditionJS` correctEmail
                        `withError` "Invalid email address:"

correctEmail s = not (null (snd (break ('@'==) s)))

-- a person WUI:
wPerson = w4Tuple (wRequiredStringSize 12) (wRequiredStringSize 12) wEmail wDate

-- a WUI for persons:
main = mainWUI (wList wPerson) persons resultForm

persons = [("Bob","Carter","bob@carter.com",(3,10,1965)),
           ("Bill","Jones","billy@acm.org",(29,2,1982)),
           ("Joe","Smith","smith.org",(20,3,1978))]

-- makecurrycgi -wuijs persons

-- JS translator:
-- /home/mh/home/curry/idcompiler/tools/curry2js/Curry2JS -wui -o persons_wui.js persons
