-- Optics.hs
-- Autor: Lars Kruse
import Data.Function
import Data.List 
import System.IO
import Graphics.EasyPlot


-- für Punkte im Algorithmus verwendeter Datentyp
data Point = Point {coord :: [Double]}
	deriving (Eq, Show)


-- ändert den Datentyp in Point und fügt einen Boolean an. Dabei gilt: 
--			True = noch nicht bearbeitet
--			False = schon bearbeitet

-- @param x 			Liste von Punkten wobei die Koordinaten als Liste von Doubles vorliegen	
-- @return			Liste mit Tupeln: Punkten im Punkt Format und Bool auf True gesetze
toList :: [[Double]] -> [(Point,Bool)]
toList x 
	| x == [] = []
	| otherwise = (Point $ head x,True) : toList (tail x)


-- berechnet den Abstand zweier Punkte
-- @param point1 , point2 	die Punkte deren Abstand bestimmt werden soll
-- @return 			Abstand der Punkte als Double
diff :: Point -> Point -> Double
diff point1 point2 = sqrt  . sum $ map (**2) $ zipWith (-) (coord point1) (coord point2)


-- bestimmt alle Nachbarn eines Punktes (alle Punkte die in der epsilon Umgebung liegen)
-- @param 	p		Punkt dessen Nachbarn bestimmt werden sollen
--		points		Liste aller Punkte
--		eps		maximaler Abstand den zwei Punkte haben dürfen um als benachbart zu gelten
-- @return			Liste mit allen Nachbarn von p
getNeighbors :: (Point,Bool) -> [(Point,Bool)] -> Double -> [(Point,Bool)]
getNeighbors p points eps
	| null points = []
	| diff (fst (head points)) (fst p) < eps = (head points) : getNeighbors p (tail points) eps
	| otherwise = getNeighbors p (tail points) eps

-- berechnet die Core-Distance des Punktes p
-- @param 	p		Punkt dessen Core-Distance bestimmt werden soll
--		points		Liste aller Punkte
-- 		minPts		Anzahl der Punkte die benötigt werden, damit ein Punkte als Kernpunkt gilt
--		eps 		Maximaler Abstand der beachtet wird
-- @return			Core-Distance als Double
coreDistance :: Point -> [Point] -> Int -> Double -> Double
coreDistance p points minPts eps
	| length points >= minPts && maybeCoreDist < eps = maybeCoreDist
	| otherwise = 0
		where 
			maybeCoreDist = ( sort ( map ( diff p) points)) !! (minPts - 1)


-- entfernt alte Punkte 
-- @param 			Liste mit Punkten mit Duplikaten 
-- @return			Liste mit Punkten ohne Duplikate
rmDup :: [(Point,Bool,Double)] -> [(Point,Bool,Double)]
rmDup [] = []
rmDup (x:xs) = x : rmDup (filter (\y -> not(getPoint x == getPoint y)) xs)

rmDupTup :: [(Point,Bool)] -> [(Point,Bool)]
rmDupTup [] = []
rmDupTup (x:xs) = x : rmDupTup (filter (\y -> not (fst x == fst y)) xs) 
--

-- Zugriff auf Elemente
getPoint :: (a, b, c) -> a
getPoint (a,_,_) = a

getProc :: (a,b,c) -> b
getProc (_,b,_) = b

getDist :: (a,b,c) -> c
getDist (_,_,c) = c
--

-- setzt alle Bools einer Liste auf False
-- @param used		Liste mit Punkten mit unterschiedlichen Bools
-- @return		Liste mit Punkten wobei alle Bools auf False gesetzt sind
setFalse :: [(Point,Bool,Double)] -> [(Point,Bool,Double)]
setFalse used
	| used == [] =[]
	| otherwise = (getPoint a, False, getDist a) : setFalse (tail used)
	where 
		a = head used


-- setzt alle Bools einer Liste auf False
-- @param used		Liste mit Punkten mit unterschiedlichen Bools
-- @return		Liste mit Punkten wobei alle Bools auf False gesetzt sind
setFalseTup :: [(Point,Bool,Double)] -> [(Point,Bool)]
setFalseTup used
	| used == [] = []
	| otherwise = (getPoint a, False) : setFalseTup (tail used)
	where
		a = head used
	

-- 
-- @param
-- @return
compareReachDist :: Point -> [(Point,Bool,Double)] -> Double
compareReachDist p seeds  
	| getPoint (head seeds) ==  p  = getDist ( head seeds )
	| otherwise = compareReachDist p $ tail seeds


-- aufruf von optics mit Parametern im Punkt Format
-- @param	list		Liste aller Punkte
--		clusterDist	Distanz ab der ein neuer Cluster beginnt
--		minPts		Anzahl der Punkte die benötigt werden, damit ein Punkte als Kernpunkt gilt
--		eps 		Maximaler Abstand der beachtet wird
-- @return			Liste deren Elemente Cluster sind. Die einzelnen Punkte werden wieder als [Double] betrachtet 
opticsMain :: [[Double]] -> Double -> Int -> Double  -> [[[Double]]]
opticsMain list clusterDist minPts eps = fromPointToDouble(output (optics (toList list) (minPts+1) eps) clusterDist)

-- Erstellung eines Erreichbarkeitsgraphen zur Bestimung geeigneter Parameter für die Erstellung der Cluster
-- @param list Liste aller Punkte
--		clusterDist	reachDistance ab der ein neuer Cluster beginnt
--		minPts		Anzahl der Punkte die benötigt werden, damit ein Punkte als Kernpunkt gilt
--		eps 		Maximaler Abstand der beachtet wird
-- @return 			eine Liste aller Reachdistances die von Optics berechnet werden, in der Reihenfolge, 
--				in der Optics die Punkte anordnet
reachabilityPlot :: [[Double]] -> Int -> Double -> [(Double,Double)]
reachabilityPlot list minPts eps = outputReach (optics (toList list) (minPts+1) eps) 0 


-- 1. Teil der Optics Algorithmus
-- @param	list		Liste aller Punkte
--		minPts		Anzahl der Punkte die benötigt werden, damit ein Punkte als Kernpunkt gilt
--		eps 		Maximaler Abstand der beachtet wird
-- @return
optics ::[(Point,Bool)]-> Int -> Double -> [(Point, Bool,Double)]
optics list minPts eps
	| list == [] = []
	| snd x && coreDist  /= 0 = (fst x, False, eps) : (setFalse opticsTwoList)  ++ optics updateListTwo minPts eps
	| snd x && coreDist  == 0 = (fst x, False, eps) : optics updateList minPts eps
	| not (snd x) = optics (tail list) minPts eps
	| otherwise = []

	where
		update = rmDup (sortBy ( compare `on` getDist) $ updateSeeds updateList (getNeighbors x updateList eps) (fst x, False, eps) [] minPts eps)
		x = head list
		coreDist = coreDistance (fst x) (fst $ unzip list) minPts eps
		updateListTwo = rmDupTup ((setFalseTup opticsTwoList) ++ [(fst x,False)] ++ list)
		opticsTwoList = opticsTwo updateList update minPts eps
		updateList = (list \\ [x]) ++ [(fst x, False)]


-- 2. Teil des Optics Algorithmus
-- @param	list		Liste aller Punkte
--		heap		Liste von Punkten mit reachDistance 
--		minPts		Anzahl der Punkte die benötigt werden, damit ein Punkte als Kernpunkt gilt
--		eps 		Maximaler Abstand der beachtet wird
-- @return			Liste der Punkte mit ReachDistance in richtiger Reihenfolge
opticsTwo :: [(Point,Bool)] -> [(Point,Bool,Double)] -> Int -> Double -> [(Point,Bool,Double)]
opticsTwo list heap minPts eps 
	|(filter (\x -> getProc x) heap) /= []  = (getPoint p, getProc p, (getDist p))  : opticsTwo updateList update minPts eps 
	|otherwise = []
	where
		update = rmDup ( sortBy (compare `on` getDist) $ updateSeeds updateList (getNeighbors (p1,p2) updateList eps) p ((heap \\ [p]) ++ [(p1, False, p3)]) minPts eps)
		updateList = rmDupTup ([(p1, False)] ++ list)
		p = head $ filter (\x -> getProc x) heap
		p1 = getPoint p
		p2 = getProc p
		p3 = getDist p
		

-- update Funktion des Optics Algorithmus
-- @param	list		Liste aller Punkte
--		neighbors	Nachbarn des Punktes p
--		p		der letze bearbeitete Punkt
--		seeds		Liste der Punkte nach ReachDistance sortiert
--		minPts		Anzahl der Punkte die benötigt werden, damit ein Punkte als Kernpunkt gilt
--		eps 		Maximaler Abstand der beachtet wird
-- @return			seeds nachdem alle Punkte bearbeitet wurden
updateSeeds :: [(Point,Bool)] -> [(Point,Bool)] -> (Point,Bool,Double) -> [(Point,Bool,Double)] -> Int -> Double -> [(Point,Bool,Double)]
updateSeeds list neighbors p seeds minPts eps 
	| null neighbors = seeds
	| snd o && not elemInSeeds = (fst o, snd o, newReachDist) : updat
	| snd o && elemInSeeds && (compareReachDist (fst o) seeds > newReachDist) = (fst o, snd o, newReachDist) : updat
	| otherwise = updat

	where 
		o = head neighbors
		elemInSeeds = elem (fst o) (getPoint $ unzip3 seeds)
		newReachDist = maximum [diff (fst o) (p1), coreDistance (p1) (fst $ unzip list)  minPts eps]
		updat = updateSeeds list (tail neighbors) p seeds minPts eps
		p1 = getPoint p



-- Unterteilung der Punkte in Cluster anhand der Reachdistance der Punkte 
-- @param	list		Liste aller Punkte mit Reachdistance
--		cluster		Reachdistance ab der ein neuer Cluster beginnt
-- @return			Liste aller Cluster
divCluster :: [(Point,Bool,Double)] -> Double -> [[Point]]
divCluster list cluster
	| list == [] = []

	-- Der aktuelle Punkt und der nächte Punkt in der Liste liegen in einem Cluster
	| (len) >= 2 && getCoreHead >= cluster && getCoreSnd < cluster  = [[getPoint (head list)] ++ (getPoint $ unzip3 $ takeWhileLowerTail)]  ++ divCluster (list \\(head list : takeWhileLowerTail)) cluster

	-- Der aktuelle Punkt ist der letzte in der Liste und die coredistance ist zu groß 
	| ((len) < 2 && getCoreHead >= cluster) = divCluster (tail list) cluster
	
	-- Der aktuelle Punkt und der darauf folgende haben beide eine zu große coredistance -> aktueller Punkt nicht im Cluster
	| ((len) >= 2 && getCoreHead >= cluster && getCoreSnd >= cluster) = divCluster (tail list) cluster

	-- die coredistance des aktuellen Punktes ist klein genug -> liegt im aktuellen Cluster
	| otherwise = [getPoint $ unzip3 $ takeWhileLower] ++ divCluster (list \\ takeWhileLower) cluster
	where
		len = length list
		getCoreHead = getDist (head list)
		getCoreSnd = getDist (list !! 1)
		takeWhileLower = takeWhile (\x -> ((getDist x) < cluster)) list
		takeWhileLowerTail = takeWhile (\x -> ((getDist x) < cluster)) (tail list)
		
outputReach :: [(Point,Bool,Double)] -> Double  -> [(Double,Double)]
outputReach list n 
	| list == [] = []
	| otherwise = (n , getDist (head list)) : outputReach (tail list) (n+1) 



-- Vereinigung aller Listen in einerListe
-- @param list	Liste mit in Listen unterteilten Punkten
-- @return	Liste mit Punkten
vereinigung :: [[Point]] -> [Point]
vereinigung list
	| list == [] = []
	| otherwise = head list ++ vereinigung (tail list)
	

-- Konvertierung der Liste mit Tripeln in eine Liste, welche nur die einzelnen Cluster enthält
-- @param
-- @return
output :: [(Point,Bool,Double)]-> Double -> [[Point]]
output erg clusterDist = (divCluster erg clusterDist) ++ ausreisser 
	where ausreisser = [((getPoint $ unzip3 erg) \\ (vereinigung (divCluster erg clusterDist)))]


-- 
-- @param 	Liste mit Clustern als Punkte
-- @return	Liste mit Clustern als Double Liste
fromPointToDouble :: [[Point]] -> [[[Double]]]
fromPointToDouble [] = []
fromPointToDouble (x:xs) = toDouble x : fromPointToDouble xs


-- 
-- @param	Ein Cluster dessen Puntke im Point Format vorliegen
-- @return	Ein Cluster mit Punkten als Double Listen
toDouble :: [Point] -> [[Double]]
toDouble [] = []
toDouble (x:xs) = (coord x) : toDouble xs



-- aendert das Format von einer Liste von Punkten als Double Liste in Listen, die jeweils die x-, y-, und z-Koordinaten enthalten, sodass pyhton diese plotten kann
-- @param Liste von Clusern, dabei enthaelt jede Liste der Punkte die Koordinaten eines Punktes
-- @return Liste von Clustern, dabei enthaelt die erste Liste der Punkte alle x- Koordinaten, die zweite alle y-Koordinaten ...gb
toPython :: [[[Double]]] -> [[[Double]]] 
toPython list 
	| list == [] = list 
	| len == 2 || len == 3 = transposeOutput list
	| otherwise = list 
		where 
			len = length $ head $ head list 
			transposeOutput [] = []
			transposeOutput (x:xs) = transpose (x) : transposeOutput (xs)

checkIfEmpty :: [[[Double]]] -> [[[Double]]]
checkIfEmpty list
	| last list == [] = list \\ [(last list)]
	| otherwise = list



--INPUT

-- @param String mit allen noch einzulesenden Punkten
-- @return String mit Werten des naechsten Punktes

inputLine :: String -> String
inputLine st 
	| st == [] = []
	| head st == '\n' = []
	| otherwise = [head st] ++ (inputLine (tail st))



-- Umwandlung des eingelesenen Strings in Listen von Double Werten (Parameterform fuer opticsMain)
-- @param liste aller noch einzulesenden Punkte als Strings
-- @return liste aller Punkte als Liste von Doublewerten
inputToList :: String -> [[Double]]
inputToList st 
	| st == [] = []
	| otherwise =  [map (read:: String -> Double) $ words line] ++ inputToList newSt
		where line = inputLine $ st
		      newSt =(st \\ ['\n'] ) \\ line
-- INPUT Ende

main :: IO ()
main = do

 fileIn<- openFile "input.txt" ReadMode
 fileInSt<- hGetContents fileIn
 outh <- openFile "output.txt" WriteMode
 hPutStrLn outh (show (checkIfEmpty $ toPython $ opticsMain (inputToList fileInSt) 20 2 100 ) )
 hClose outh
 hClose fileIn

--main :: IO()
--main = do
--  fileIn<- openFile "input.txt" ReadMode
 -- fileInSt <- hGetContents fileIn
--  a <- plot (JPEG "erreichbarkeitsDia.jpg") $ Data2D [Title "Optics"] [] (reachabilityPlot (inputToList fileInSt) 2 100 )
--  hClose fileIn
