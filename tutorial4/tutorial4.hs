-- Informatics 1 - Functional Programming 
-- Tutorial 4
--
-- Due: the tutorial of week 6 (27/28 Oct)

import Data.List (nub)
import Data.Char
import Test.QuickCheck
import Network.HTTP (simpleHTTP,getRequest,getResponseBody)

-- <type decls>

type Link = String
type Name = String
type Email = String
type HTML = String
type URL = String

-- </type decls>
-- <sample data>

testURL     = "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/testpage.html"

testHTML :: String
testHTML =    "<html>"
           ++ "<head>"
           ++ "<title>FP: Tutorial 4</title>"
           ++ "</head>"
           ++ "<body>"
           ++ "<h1>A Boring test page</h1>"
           ++ "<h2>for tutorial 4</h2>"
           ++ "<a href=\"http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br>"
           ++ "<b>Lecturer:</b> <a href=\"mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br>"
           ++ "<b>TA:</b> <a href=\"mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a>"
           ++ "</body>"
           ++ "</html>"

testLinks :: [Link]
testLinks = [ "http://www.inf.ed.ac.uk/teaching/courses/inf1/fp/\">FP Website</a><br><b>Lecturer:</b> "
            , "mailto:dts@inf.ed.ac.uk\">Don Sannella</a><br><b>TA:</b> "
            , "mailto:m.k.lehtinen@sms.ed.ac.uk\">Karoliina Lehtinen</a></body></html>" ]


testAddrBook :: [(Name,Email)]
testAddrBook = [ ("Don Sannella","dts@inf.ed.ac.uk")
               , ("Karoliina Lehtinen","m.k.lehtinen@sms.ed.ac.uk")]

-- </sample data>
-- <system interaction>

getURL :: String -> IO String
getURL url = simpleHTTP (getRequest url) >>= getResponseBody

emailsFromURL :: URL -> IO ()
emailsFromURL url =
  do html <- getURL url
     let emails = (emailsFromHTML html)
     putStr (ppAddrBook emails)

emailsByNameFromURL :: URL -> Name -> IO ()
emailsByNameFromURL url name =
  do html <- getURL url
     let emails = (emailsByNameFromHTML html name)
     putStr (ppAddrBook emails)

-- </system interaction>
-- <exercises>

-- 1.
sameString :: String -> String -> Bool
sameString s1 s2 = map toUpper s1 == map toUpper s2


-- 2.
prefix :: String -> String -> Bool
prefix s1 s2 = sameString s1 (take (length s1) s2)

prop_prefix_pos :: String -> Int -> Bool
prop_prefix_pos str n =  prefix substr (map toLower str) &&
		         prefix substr (map toUpper str)
                           where
                             substr  =  take n str

prop_prefix_neg :: String -> Int -> Bool
prop_prefix_neg str n = sameString str substr || (not $ prefix str substr)
                          where substr = take n str
        
        
-- 3.
contains :: String -> String -> Bool
contains "" sub | sub == "" = True
                | otherwise = False
contains str sub | prefix sub str = True
                 | otherwise = contains (drop 1 str) sub 

prop_contains :: String -> Int -> Int -> Bool
prop_contains str n1 n2 = contains str (map toUpper sub) && contains str (map toLower sub)
                      where l = length str
                            tail = (mod n2 (l+1))
                            head = (mod n1 (tail+1))
                            sub = drop head (take tail str)
-- 4.
takeUntil :: String -> String -> String
takeUntil sub "" = ""
takeUntil sub str | prefix sub str = ""
                  | otherwise = head str : takeUntil sub (drop 1 str)

dropUntil :: String -> String -> String
dropUntil sub "" = ""
dropUntil sub str | prefix sub str = drop (length sub) str
                  | otherwise = dropUntil sub (drop 1 str)


-- 5.
split :: String -> String -> [String]
split sep "" = [""] 
split sep str | contains str sep = (takeUntil sep str) : (split sep (dropUntil sep str))
              | otherwise = [str]

reconstruct :: String -> [String] -> String
reconstruct _ [] = []
reconstruct sep (str:l) | length l == 0 = str
                        | otherwise = str ++ sep ++ (reconstruct sep l)

prop_split :: Char -> String -> String -> Bool
prop_split c sep str = reconstruct sep' (split sep' str) `sameString` str
  where sep' = c : sep

-- 6.
linksFromHTML :: HTML -> [Link]
linksFromHTML html = tail (split "<a href=\"" html)

testLinksFromHTML :: Bool
testLinksFromHTML  =  linksFromHTML testHTML == testLinks


-- 7.
takeEmails :: [Link] -> [Link]
takeEmails lst = [lnk |lnk <- lst, prefix "mailto:" lnk]


-- 8.
link2pair :: Link -> (Name, Email)
link2pair lnk = ((l !! 1), (l !! 0))
              where l = split "\">" (drop 7 (takeUntil "</a>" lnk))


-- 9.
emailsFromHTML :: HTML -> [(Name,Email)]
emailsFromHTML html = [link2pair lnk | lnk  <- takeEmails (linksFromHTML html)]

testEmailsFromHTML :: Bool
testEmailsFromHTML  =  emailsFromHTML testHTML == testAddrBook


-- 10.
findEmail :: Name -> [(Name, Email)] -> [(Name, Email)]
findEmail part lst = [(name,email)|(name,email) <- lst, contains name part] 


-- 11.
emailsByNameFromHTML :: HTML -> Name -> [(Name,Email)]
emailsByNameFromHTML html nam = findEmail nam (emailsFromHTML html)


-- Optional Material

-- 12.
hasInitials :: String -> Name -> Bool
hasInitials str nam = str == [head x |x <- (split " " nam)]

-- 13.
emailsByMatchFromHTML :: (Name -> Bool) -> HTML -> [(Name, Email)]
emailsByMatchFromHTML f html = [x|x <-(emailsFromHTML html),f (fst x)]

emailsByInitialsFromHTML :: String -> HTML -> [(Name, Email)]
emailsByInitialsFromHTML str html = emailsByMatchFromHTML (hasInitials str) html

-- 14.

-- If your criteria use parameters (like hasInitials), change the type signature.
myCriteria :: Name -> Bool
myCriteria name = (length (split " " name)) < 4

emailsByMyCriteriaFromHTML :: HTML -> [(Name, Email)]
emailsByMyCriteriaFromHTML html = emailsByMatchFromHTML (myCriteria) html

-- 15
ppAddrBook :: [(Name, Email)] -> String
ppAddrBook addr = unlines [if contains name "," 
                           then name ++ (take (leng -length name+1) (cycle " "))  ++ email 
                           else (convert name)++ (take (leng -length name) (cycle " "))  ++ email
                          | (name,email) <- addr ]
                         where leng = longest addr+2
longest :: [(Name,Email)] -> Int
longest addr = maximum [length name| (name,_) <- addr]

convert :: Name -> Name
convert nam = (last naml) ++ ", " ++ (head naml)
            where naml = split " " nam
