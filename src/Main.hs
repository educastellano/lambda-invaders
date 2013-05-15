{- demonstrates the use of a simple menu, statusbar, and dialog -}
module Main where

import Graphics.UI.WX
import Invader
import SpaceCraft


main :: IO ()
main
  = start hello

hello :: IO ()
hello
  = do -- the application frame
       
       -- Inicialitzem el joc
       vinvaders <- variable [value := iniInvaders 60 2 10]
       vspacecraft <- variable [value := (S (400,450) 20 Tipus1)]
       vtickinv <- variable [value := 0] -- Per controloar q els invaders no es moguin a cada tick del timer, sinó cada 1 segon
       vbales <- varCreate []
       vbalesInv <- varCreate []
       vpuntuacio <- variable [value := 0]
       vvides <- variable [value := 3]
       vfiJoc <- variable [value := False]
  
       f      <- frameFixed         [text := "Lambda Invaders!", clientSize := sz 900 600, bgcolor := black]                               

       -- create file menu  
       file   <- menuPane      [text := "&File"]
       new    <- menuItem file [text := "&New Game", on command := newGame vinvaders vspacecraft vpuntuacio vvides vfiJoc]
       quit   <- menuQuit file [help := "Quit the game", on command := close f]

       -- create Help menu
       hlp    <- menuHelp      []
       about  <- menuAbout hlp [help := "About Lambda Invaders!"]

       -- create statusbar field
       status <- statusField   [text := "Welcome to Lambda Invaders!"]

       t <- timer f [interval := 25, on command := avancarEstat vspacecraft vinvaders vtickinv vbales vbalesInv vpuntuacio vvides vfiJoc f]
       
       -- set the statusbar and menubar
       set f [  on paint := pintarTot vinvaders vspacecraft vbales vbalesInv vpuntuacio vvides vfiJoc
       	        ,statusBar := [status]
                ,menuBar   := [file,hlp]
             -- as an example, put the menu event handler for an about box on the frame.
                ,on (menu about) := infoDialog f "About Lambda Invaders" "This is a "
		,on leftKey := onLeftKey vspacecraft
		,on rightKey := onRightKey vspacecraft
		-- ,on spacebarKey := onSpaceKey vspacecraft  -- Com es captura l'event de l'espai?
		,on (charKey 'x') := onSpaceKey vspacecraft vbales
             ]


imgScore = bitmap "score.bmp"
imgLives = bitmap "lives.bmp"
imgGameOver = bitmap "gameover.bmp"
imgWinner = bitmap "winner.bmp"
	     
newGame vinvaders vspacecraft vpuntuacio vvides vfiJoc
	= do 
	    set vinvaders [value := iniInvaders 60 2 10]
	    set vspacecraft [value := (S (400,450) 20 Tipus1)]
	    set vpuntuacio [value := 0]
	    set vvides [value := 3]
	    set vfiJoc [value := False]
	     
onLeftKey vspacecraft = 
	do 
	  spacecraft <- get vspacecraft value
	  set vspacecraft [value := inc_x_spacecraft spacecraft (-10)]
	  
onRightKey vspacecraft = 
	do 
	  spacecraft <- get vspacecraft value
	  set vspacecraft [value := inc_x_spacecraft spacecraft 10]


-- Disparem. Afegim una bala a la llista de bales
onSpaceKey vspacecraft vbales = 
	do 
	  spacecraft <- get vspacecraft value
	  bales <- get vbales value
	  set vbales [value := ((B ((x_spacecraft spacecraft) + (radi_spacecraft spacecraft), (y_spacecraft spacecraft) - 5)):bales)]

---------------------------------------------
iniInvaders :: Int -> Int -> Int -> [Invader]
---------------------------------------------
-- Inicialitza (Crea) els invaders
-- Paràmetres: Número d'invaders -> Fila Inicial -> Columna Inicial -R> Llista d'invaders
iniInvaders 0 _ _= []
iniInvaders n f c = if (mod (n-1) 10) == 0 then
			(creaInvader (c*30,f*30) (0,0) 10 (myrandom n 0 2) (myrandom n 0 2)):(iniInvaders (n-1) (f+1) (c-9))
		  else
		  	(creaInvader (c*30,f*30) (0,0) 10 (myrandom n 0 2) (myrandom n 0 2)):(iniInvaders (n-1) f (c+1))


pintarTot vinvaders vspacecraft vbales vbalesInv vpuntuacio vvides vfiJoc dc view = 
	do 
	  invaders <- get vinvaders value
	  spacecraft <- get vspacecraft value
	  bales <- get vbales value
	  balesInv <- get vbalesInv value
	  puntuacio <- get vpuntuacio value
	  vides <- get vvides value
	  fiJoc <- get vfiJoc value
	  pintarInvaders invaders dc view
	  pintarSpaceCraft dc spacecraft (not fiJoc)
	  pintarBales dc bales 4 blue
	  pintarBales dc balesInv 4 red
	  pintarPuntuacio dc puntuacio
	  pintarVides dc vides
	  pintarFi dc fiJoc vides

-----------------------------------------------------
pintarInvaders :: [Invader] -> DC a -> Rect -> IO ()
-----------------------------------------------------
pintarInvaders [] dc va = return ()
pintarInvaders (m:ms) dc va  
			= do pintarInvader dc m
			     pintarInvaders ms dc va

-----------------------------------------------------
pintarBales :: DC a -> [Bala] -> Int -> Color -> IO ()
-----------------------------------------------------
pintarBales dc [] radi _ = return ()
pintarBales dc (b:bs) radi color
		= do circle dc (pt (x_bala b) (y_bala b)) radi [brushColor := color, brushKind := BrushSolid]
		     pintarBales dc bs radi color

-----------------------------------------
pintarPuntuacio :: DC a -> Int -> IO ()
-----------------------------------------
pintarPuntuacio dc p = do
			drawBitmap dc imgScore (pt 20 20) True []
			drawText dc (show p) (pt 120 17) [color := green, fontSize := 20 ]

-----------------------------------------
pintarVides :: DC a -> Int -> IO ()
-----------------------------------------
pintarVides dc p = do 
			--drawText dc "LIVES " (pt 740 30) [color := green]
			drawBitmap dc imgLives (pt 700 20) True []
			pintarNVides dc p
			
-----------------------------------------
pintarNVides :: DC a -> Int -> IO ()
-----------------------------------------
pintarNVides dc 0 = return ()
pintarNVides dc p = do 
			drawBitmap dc imgSpaceCraftmini (pt (770+(p*30)) 20) True []
			pintarNVides dc (p-1)
		     

-----------------------------------------
pintarFi :: DC a -> Bool -> Int -> IO ()
-----------------------------------------
pintarFi dc False _ = return ()
pintarFi dc True v = if v > 0 then
			drawBitmap dc imgWinner (pt 350 200) True []
		     else
			drawBitmap dc imgGameOver (pt 280 300) True []
			

			
avancarEstat vspacecraft vinvaders vtickinv vbales vbalesInv vpuntuacio vvides vfiJoc f = 
	do
	  -- Apliquem moviment als invaders
	  invaders <- get vinvaders value
  	  tickinv <- get vtickinv value
	  set vinvaders [value := movimentInvaders invaders 20 tickinv]
	  
	  -- comptador per moure els invaders cada x temps i no cada refresc d pantalla
	  set vtickinv [value := (mod (tickinv + 1) 50)]
	  
	  -- Avancem les bales llencades per la nau espacial
	  bales <- get vbales value
	  set vbales [value := avancarBales bales (-15)]
	  
	  -- Llancem/avancem les bales dels invaders
	  balesInv <- get vbalesInv value
	  fiJoc <- get vfiJoc value
	  set vbalesInv [value := crearBalaInv (avancarBales balesInv 15) invaders (myrandom 0 (-1500) 59) 0 fiJoc]

	  -- Comprovem collisions entre les bales i els invaders
  	  invaders <- get vinvaders value
  	  bales <- get vbales value
	  puntuacio <- get vpuntuacio value
	  set vpuntuacio [value := puntuacioEstat invaders bales puntuacio]
	  set vinvaders [value := collisioInvadersBales invaders bales]
	  set vbales [value := collisioBalesInvaders bales invaders]
	  
	  -- Comprovem col·lisions entre les bales dels invaders i la nau
	  balesInv <- get vbalesInv value
	  spacecraft <- get vspacecraft value
	  vides <- get vvides value
	  fiJoc <- get vfiJoc value
	  set vbalesInv [value := collisioBalesSCraft spacecraft balesInv]
	  set vvides [value := collisioBalesSCraftVides spacecraft balesInv vides fiJoc]
	  set vspacecraft [value := collisioBalesSpaceCraft spacecraft balesInv]
	  
	  -- Si s'han acabat les vides o hem mort tots els invaders, finalitzem el joc
	  vides <- get vvides value
    	  invaders <- get vinvaders value
	  set vfiJoc [value := vides == 0 || totsMorts invaders]
	  
	  repaint f
	  
	  
---------------------------------------------------------
movimentInvaders :: [Invader] -> Int -> Int -> [Invader]
---------------------------------------------------------
-- Els invaders es mouen cada 50*20 ms = 1 seg
movimentInvaders [] _ _ = []
movimentInvaders (i:is) mov 0 = (aplicarMoviment i mov):(movimentInvaders is mov 0)
movimentInvaders invaders mov t = invaders

----------------------------------------------------------
avancarBales :: [Bala] -> Int -> [Bala]
-----------------------------------------------------------
avancarBales [] inc = []
avancarBales (b:bs) inc = (inc_y_bala b inc):(avancarBales bs inc)


-------------------------------------------------------
collisio :: Invader -> Bala -> Bool
-------------------------------------------------------
collisio invader bala = (distancia (centre_invader invader) (pos_bala bala)) <= (radi_invader invader) && (estat_invader invader) /= Mort

-------------------------------------------------------
collisioSC :: SpaceCraft -> Bala -> Bool
-------------------------------------------------------
collisioSC spacecraft bala = distancia (centre_spacecraft spacecraft) (pos_bala bala) <= (radi_spacecraft spacecraft)



---------------------------------------------------------
collisioInvadersBales :: [Invader] -> [Bala] -> [Invader]
---------------------------------------------------------
collisioInvadersBales invaders [] = invaders
collisioInvadersBales invaders (b:bs) = collisioInvadersBales (collisioInvadersBala invaders b) bs 

---------------------------------------------------------
collisioInvadersBala :: [Invader] -> Bala -> [Invader]
---------------------------------------------------------
-- "Decrementa" l'estat dels invaders que col·lisionen
collisioInvadersBala [] _ = []
collisioInvadersBala (i:is) bala = if collisio i bala then
					(dec_Estat i):(collisioInvadersBala is bala)
				   else
				   	i:(collisioInvadersBala is bala)

					
---------------------------------------------------------
collisioBalesInvaders :: [Bala] -> [Invader] -> [Bala]
---------------------------------------------------------
collisioBalesInvaders bales [] = bales
collisioBalesInvaders bales (i:is) = collisioBalesInvaders (collisioBalesInvader bales i) is 

---------------------------------------------------------
collisioBalesInvader :: [Bala] -> Invader -> [Bala]
---------------------------------------------------------
-- Treu de la llista de bales, les bales que col·lisionen i tb les que ja han sobrepassat la pantalla
collisioBalesInvader [] _ = []
collisioBalesInvader (b:bs) invader = if (collisio invader b) || (y_bala b < 0) then
					collisioBalesInvader bs invader
				   else
				   	b:(collisioBalesInvader bs invader)


---------------------------------------------------------
puntuacioEstat :: [Invader] -> [Bala] -> Int -> Int
---------------------------------------------------------
-- Retorna la puntuacio del joc en aquest estat (invaders bales)
puntuacioEstat invaders [] p = p
puntuacioEstat invaders (b:bs) p = puntuacioEstat invaders bs (puntuacioEstatBala invaders b p) 

---------------------------------------------------------
puntuacioEstatBala :: [Invader] -> Bala -> Int -> Int
---------------------------------------------------------
puntuacioEstatBala [] _ p = p
puntuacioEstatBala (i:is) bala p = if collisio i bala then
		 			puntuacioEstatBala is bala (p + (puntuacio i))
				   else
				   	puntuacioEstatBala is bala p

------------------------------------------------------------
crearBalaInv :: [Bala] -> [Invader] -> Int -> Int -> Bool -> [Bala]
------------------------------------------------------------
crearBalaInv bales _ _ _ True = bales
crearBalaInv bales [] _ _ False = bales
crearBalaInv bales (i:is) n k False = if n < 0 then
					  bales
				      else
				      	  if k == (n-1) && estat_invader i /= Mort then
						(B (x_invader i + 12, y_invader i + 20)):(crearBalaInv bales (i:is) n (k+1) False)
					  else
					  	if k == (n-1) then
							crearBalaInv bales is (n+1) k False
						else
							crearBalaInv bales is n (k+1) False

{------------------------------------------------------------
crearBalaInv :: [Bala] -> [Invader] -> Int -> Int -> Bool -> [Bala]
------------------------------------------------------------
crearBalaInv bales _ _ _ True = bales
crearBalaInv bales [] _ _ False = bales
crearBalaInv bales (i:is) n k False = if n < 0 then
					  bales
				      else
				      	  if (n-1) == k && estat_invader i /= Mort then
						(B (x_invader i + 12, y_invader i + 20)):(crearBalaInv bales (i:is) n (k+1) False)
					  else
						crearBalaInv bales is n (k+1) False
	-}					
------------------------------------------------------------
collisioBalesSCraft :: SpaceCraft -> [Bala] -> [Bala]
------------------------------------------------------------
-- Treu de la llista de bales, les bales dels invaders que col·lisionen amb la nau i tb les que ja han sobrepassat la pantalla
collisioBalesSCraft _ [] = []
collisioBalesSCraft spacecraft (b:bs) = if collisioSC spacecraft b || (y_bala b > 600) then
						collisioBalesSCraft spacecraft bs
					else
						b:(collisioBalesSCraft spacecraft bs)

------------------------------------------------------------------------
collisioBalesSCraftVides :: SpaceCraft -> [Bala] -> Int -> Bool -> Int
------------------------------------------------------------------------
-- Comprova les col·lisions i retorna el numero de vides
collisioBalesSCraftVides _ _ vides True = vides
collisioBalesSCraftVides _ [] vides False = vides
collisioBalesSCraftVides spacecraft (b:bs) vides False = if collisioSC spacecraft b then
					     			vides-1
							 else
							 	collisioBalesSCraftVides spacecraft bs vides False

------------------------------------------------------------
collisioBalesSpaceCraft :: SpaceCraft -> [Bala] -> SpaceCraft
------------------------------------------------------------
-- Comprova les col·lisions i retorna la nau en una nova posició
collisioBalesSpaceCraft s [] = s
collisioBalesSpaceCraft s (b:bs) = if collisioSC s b then
						(S (100, y_spacecraft s) (radi_spacecraft s) (tipus_spacecraft s))
					else
						collisioBalesSpaceCraft s bs

--------------------------------
totsMorts :: [Invader] -> Bool
--------------------------------
totsMorts [] = True
totsMorts (i:is) = if estat_invader i == Mort then
			totsMorts is
		   else
		   	False

