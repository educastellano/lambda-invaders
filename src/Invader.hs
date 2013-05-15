module Invader where

import Graphics.UI.WX
import System.Random
import System.IO.Unsafe (unsafePerformIO,unsafeInterleaveIO)

type Coord = (Int, Int)

data Invader 
	= I
	   Coord
	   Coord
	   Int
	   Estat
	   Tipus
	-- deriving (Eq, Show)

data Estat = Fort | Normal | Feble | Bonus | Mort deriving (Eq)
data Tipus = Alien0 | Alien1 | Alien2 deriving (Eq)


---------
-- Get
---------

pos_invader :: Invader -> Coord
pos_invader (I pos _ _ _ _) = pos

x_invader :: Invader -> Int
x_invader (I (x, _) _ _ _ _) = x

y_invader :: Invader -> Int
y_invader (I (_, y) _ _ _ _) = y

xOffset_invader :: Invader -> Int
xOffset_invader (I _ (x, _) _ _ _) = x

yOffset_invader :: Invader -> Int
yOffset_invader (I _ (_, y) _ _ _) = y

radi_invader :: Invader -> Int
radi_invader (I (_, _) _ radi _ _) = radi

estat_invader :: Invader -> Estat
estat_invader  (I (_, _) _ _ estat _) = estat

tipus_invader :: Invader -> Tipus
tipus_invader  (I (_, _) _ _ _ tipus) = tipus


centre_invader :: Invader -> Coord
centre_invader i = ((x_invader i) + (xOffset_invader i) + (radi_invader i), (y_invader i) + (yOffset_invader i) + (radi_invader i))


dec_Estat :: Invader -> Invader
dec_Estat (I pos offs radi estat tipus) = if estat == Fort then
						(I pos offs radi Normal tipus)
					    else if estat == Normal then
					    	(I pos offs radi Feble tipus)
					    else -- if estat == Feble i aleatori then
					    	if ((myrandom 0 0 5) == 1) && (estat /= Bonus) && (estat /= Mort) then
							(I pos offs radi Bonus tipus)
						else
							(I pos offs radi Mort tipus)


-- Genera una llista de numeros aleatoris entre ini i fi, i retorna el numero situat a la posicio n
myrandom :: Int -> Int -> Int -> Int
myrandom n ini fi = nEle n 0 (randomRs (ini,fi) (unsafePerformIO newStdGen))

-- Obte l'element n-�ssim d'una llista
nEle n i (x:xs) = if i < n then
			nEle n (i+1) xs
		   else
		   	x



imgInvaderNormal0 = (bitmap "0invader_normal.bmp")
imgInvaderFeble0 = (bitmap "0invader_feble.bmp")
imgInvaderFort0 = (bitmap "0invader_fort.bmp")

imgInvaderNormal1 = (bitmap "1invader_normal.bmp")
imgInvaderFeble1 = (bitmap "1invader_feble.bmp")
imgInvaderFort1 = (bitmap "1invader_fort.bmp")

imgInvaderNormal2 = (bitmap "2invader_normal.bmp")
imgInvaderFeble2 = (bitmap "2invader_feble.bmp")
imgInvaderFort2 = (bitmap "2invader_fort.bmp")

imgInvaderBonus = (bitmap "bonus.bmp")
imgInvaderMort = (bitmap "mort.bmp")


------------------------------------------
pintarInvader :: DC a -> Invader -> IO ()
------------------------------------------
-- pintarInvader dc i = circle dc (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) (radi_invader i) [brushColor := red, brushKind := BrushSolid]
-- pintarInvader dc i = drawBitmap dc imgInvaderNormal0 (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
-- TODO fix this function
pintarInvader dc i 
		= if (tipus_invader i) == Alien0 then
			if (estat_invader i) == Fort then
				drawBitmap dc imgInvaderFort0 (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
			else if (estat_invader i) == Normal then
				drawBitmap dc imgInvaderNormal0 (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
			else if (estat_invader i) == Feble then
				drawBitmap dc imgInvaderFeble0 (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
			else if (estat_invader i) == Bonus then
				drawBitmap dc imgInvaderBonus (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
			else --Mort
				drawBitmap dc imgInvaderMort (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
		  else if (tipus_invader i) == Alien1 then
		  	if (estat_invader i) == Fort then
				drawBitmap dc imgInvaderFort1 (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
			else if (estat_invader i) == Normal then
				drawBitmap dc imgInvaderNormal1 (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
			else if (estat_invader i) == Feble then
				drawBitmap dc imgInvaderFeble1 (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
			else if (estat_invader i) == Bonus then
				drawBitmap dc imgInvaderBonus (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
			else --Mort
				drawBitmap dc imgInvaderMort (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
		  else -- if (tipus_invader i) == Alien2 then
			if (estat_invader i) == Fort then
				drawBitmap dc imgInvaderFort2 (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
			else if (estat_invader i) == Normal then
				drawBitmap dc imgInvaderNormal2 (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
			else if (estat_invader i) == Feble then
				drawBitmap dc imgInvaderFeble2 (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
			else if (estat_invader i) == Bonus then
				drawBitmap dc imgInvaderBonus (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []
			else --Mort
				drawBitmap dc imgInvaderMort (pt ((x_invader i) + (xOffset_invader i)) ((y_invader i) + (yOffset_invader i))) True []

				
---------------------------------------------------------
aplicarMoviment :: Invader -> Int -> Invader
---------------------------------------------------------
aplicarMoviment i mov = if (xOffset_invader i) == 0 && (yOffset_invader i) == 0 then
				(I (pos_invader i) (mov,0) (radi_invader i) (estat_invader i) (tipus_invader i))
			else if (xOffset_invader i) == mov && (yOffset_invader i) == 0 then
				(I (pos_invader i) (mov,mov) (radi_invader i) (estat_invader i) (tipus_invader i))
			else if (xOffset_invader i) == mov && (yOffset_invader i) == mov then
				(I (pos_invader i) (0,mov) (radi_invader i) (estat_invader i) (tipus_invader i))
			else -- (xOffset_invader i) == 0 && (yOffset_invader i) == mov then
				(I (pos_invader i) (0,0) (radi_invader i) (estat_invader i) (tipus_invader i))

				
--------------------------------------------------------------			
creaInvader :: Coord -> Coord -> Int -> Int -> Int -> Invader
--------------------------------------------------------------
-- Crea un invader a partir d-un numero a l-estat i al tipus
creaInvader pos offset radi randEstat randTipus 
		= if randTipus == 0 then
			if randEstat == 0 then
				I pos offset radi Fort Alien0
			else if randEstat == 1 then
				I pos offset radi Normal Alien0
			else if randEstat == 2 then
				I pos offset radi Feble Alien0
			else --Bonus
				I pos offset radi Bonus Alien0
		  else if randTipus == 1 then
		  	if randEstat == 0 then
				I pos offset radi Fort Alien1
			else if randEstat == 1 then
				I pos offset radi Normal Alien1
			else if randEstat == 2 then
				I pos offset radi Feble Alien1
			else --Bonus
				I pos offset radi Bonus Alien1
		  else -- if randTipus == 2 then
			if randEstat == 0 then
				I pos offset radi Fort Alien2
			else if randEstat == 1 then
				I pos offset radi Normal Alien2
			else if randEstat == 2 then
				I pos offset radi Feble Alien2
			else --Bonus
				I pos offset radi Bonus Alien2
------------------------------
puntuacio :: Invader -> Int
------------------------------
puntuacio (I _ _ _ estat tipus) = if estat == Bonus then
					1000
				  else
				  	if tipus == Alien0 then
						10
					else if tipus == Alien1 then
						30
					else --Alien2
						50


				
