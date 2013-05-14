module SpaceCraft where

import Graphics.UI.WX

type Coord = (Int, Int)
data TipusSC = Tipus1

data SpaceCraft 
	= S
	   Coord
	   Int
	   TipusSC
	-- deriving (Eq, Show)

---------
-- Get
---------

pos_spacecraft :: SpaceCraft -> Coord
pos_spacecraft (S pos _ _) = pos

x_spacecraft :: SpaceCraft -> Int
x_spacecraft (S (x, _) _ _) = x

y_spacecraft :: SpaceCraft -> Int
y_spacecraft (S (_, y) _ _) = y

radi_spacecraft :: SpaceCraft -> Int
radi_spacecraft (S (_, _) radi _) = radi

tipus_spacecraft :: SpaceCraft -> TipusSC
tipus_spacecraft  (S (_, _) _ tipus) = tipus

centre_spacecraft :: SpaceCraft -> Coord
centre_spacecraft sc = (x_spacecraft sc + 20, y_spacecraft sc + 24)


---------
-- Set
---------

set_x_spacecraft :: SpaceCraft -> Int -> SpaceCraft
set_x_spacecraft (S (x, y) r c) noux = (S (noux, y) r c)

set_y_spacecraft :: SpaceCraft -> Int -> SpaceCraft
set_y_spacecraft (S (x, y) r c) nouy = (S (x, nouy) r c)



inc_x_spacecraft :: SpaceCraft -> Int -> SpaceCraft
inc_x_spacecraft (S (x, y) r c) inc = (S (x + inc, y) r c)


imgSpaceCraft = (bitmap "spacecraft.bmp")
imgSpaceCraftmini = (bitmap "spacecraft_mini.bmp")

-------------------------------------------------
pintarSpaceCraft :: DC a -> SpaceCraft -> Bool -> IO ()
-------------------------------------------------
--pintarSpaceCraft dc s = circle dc (pt (x_spacecraft s) (y_spacecraft s)) (radi_spacecraft s) [brushColor := red, brushKind := BrushSolid]
pintarSpaceCraft dc s True = drawBitmap dc imgSpaceCraft (pt (x_spacecraft s) (y_spacecraft s)) True []
pintarSpaceCraft dc s False = return ()



data Bala = B Coord

pos_bala :: Bala -> Coord
pos_bala (B (x, y)) = (x, y)

x_bala :: Bala -> Int
x_bala (B (x, _)) = x

y_bala :: Bala -> Int
y_bala (B (_, y)) = y

inc_y_bala :: Bala -> Int -> Bala
inc_y_bala (B (x, y)) inc = (B (x, y + inc))

--distancia :: (Int,Int) -> (Int,Int) -> Int
distancia (x1, y1) (x2, y2) = round (sqrt ( ( ((fromIntegral x2)-(fromIntegral x1))^2) + (((fromIntegral y2)-(fromIntegral y1))^2) )) 

