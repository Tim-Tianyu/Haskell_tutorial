
-- Informatics 1 - Functional Programming 
-- Lab week tutorial part II
--
--

import PicturesSVG
import Test.QuickCheck



-- Exercise 9:

pic1 :: Picture
pic1 = (above (beside knight (invert knight)) (beside (invert knight) knight))

pic2 :: Picture
pic2 = (above (beside knight (invert knight)) (flipV(beside knight (invert knight))))
-- Exercise 10:
-- a)

emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)

otherEmptyRow :: Picture
otherEmptyRow = flipV emptyRow

-- c)

middleBoard :: Picture
middleBoard = repeatV 2 (above emptyRow otherEmptyRow) 

-- d)
white = (beside rook(beside knight (beside bishop (beside queen (beside king (beside bishop (beside knight rook)))))))
whiteRow :: Picture
whiteRow = over white otherEmptyRow

blackRow :: Picture
blackRow = over (invert white) emptyRow

-- e
wPawns = repeatH 8 pawn
whitePawns = over wPawns emptyRow
blackPawns = over (invert wPawns) otherEmptyRow
populatedBoard :: Picture
populatedBoard = (above blackRow (above blackPawns (above middleBoard (above whitePawns whiteRow))))



-- Functions --

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)


-- Exercise 11:

twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoAbove (twoBeside x)
