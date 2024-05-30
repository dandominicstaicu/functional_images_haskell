{-# LANGUAGE TupleSections #-}
module Shallow where

import Data.List hiding (union)
import qualified Data.Set as S
import Debug.Trace

{-
    Punct bidimensional, reprezentat ca pereche de coordonate reale (x, y).
    type introduce un sinonim de tip, similar cu typedef din C.
-}
type Point = (Float, Float)

{-
    Tip de funcție care primește un punct, și este parametrizat în raport
    cu tipul rezultatului.
-}
type Pointed a = Point -> a

{-
    Regiune bidimensională, reprezentată ca o funcție caracteristică
    (Point -> Bool). Pentru un punct care aparține regiunii, se întoarce True;
    altfel, False.
-}
type Region = Pointed Bool

{-
    Transformare bidimensională, reprezentată ca o funcție peste puncte.
-}
type Transformation = Point -> Point

{-
    *** TODO ***

    Implementați funcția inside, care verifică dacă un punct aparține unei
    regiuni (ea însăși reprezentată ca o funcție caracteristică).

    Constrângeri: funcția trebuie implementată point-free.

    Hint: implementați mai întâi funcția cu explicitarea parametrului formal
    (point-wise), și de-abia apoi transformați-o în stil point-free.

    Exemple:

    > inside (0, 0) (== (0, 0))
    True

    > inside (1, 1) (== (0, 0))
    False
-}

{-
    apply the function Region on the Point
-}
inside :: Point -> Region -> Bool
inside point region = region point


{-
    *** TODO ***

    Implementați funcția fromPoints, care construiește o regiune pe baza unei
    liste de puncte.

    Constrângeri: funcția trebuie implementată point-free, fără recursivitate
    explicită.

    Exemple:

    > fromPoints [(0, 0), (1, 1)] (0, 0)
    True

    > inside (0, 0) $ fromPoints [(0, 0), (1, 1)]  -- echivalentă cu anterioara
    True

    > fromPoints [(0, 0), (1, 1)] (0, 1)
    False

    > inside (0, 1) $ fromPoints [(0, 0), (1, 1)]  -- echivalentă cu anterioara
    False
-}

{-
    fromPoints ps p checks if p is an element of list ps
    elem :: a -> [a] -> Bool
    using flip makes it [a] -> a -> Bool
-}
fromPoints :: [Point] -> Region
-- fromPoints ps = \p -> p `elem` ps
fromPoints = flip elem

{-
    *** TODO ***

    Implementați funcția rectangle, care generează o regiune aferentă
    unui dreptunghi, cu lățime și înălțime date, simetric față de originea
    (0, 0). De exemplu, un dreptunghi cu lățimea 2 și înălțimea 2 va avea
    punctul din stânga-sus (-1, 1), iar din dreapta-jos, (1, -1).

    Exemple:

    > rectangle 2 2 (0, 0)
    True

    > rectangle 2 2 (-1, 1)
    True

    > rectangle 2 2 (1, -1)
    True

    > rectangle 2 2 (2, 2)  
    False
-}

{-
    check if the coords of a point are within the rectangle
-}
rectangle :: Float -> Float -> Region
rectangle width height (x, y) = abs x <= width / 2 && abs y <= height / 2

{-
    *** TODO ***

    Implementați funcția circle, care generează o regiune aferentă unui cerc,
    cu rază dată și centrul în originea (0, 0).

    Exemple:

    > circle 1 (0, 0)
    True

    > circle 1 (1, 0)
    True
    
    > circle 1 (0, 1)
    True
    
    > circle 1 (1, 1)
    False
-}

{-
    check if a point is within the circle with the center in (0, 0) and the given radius
    check using circle equation
-}
circle :: Float -> Region
circle radius (x, y) = x ** 2 + y ** 2 <= radius ** 2

{-
    *** TODO ***

    Implementați funcția plot, care generează diagrama unei regiuni,
    pe o suprafață de desenare de dimensiuni fixate. Punctul (0, 0)
    se află în centrul suprafeței de desenare, iar lățimea și înălțimea
    unui cadran (dintre cele 4) sunt primite ca parametri. De exemplu, dacă
    lățimea este 2 și înălțimea este 1, punctul din stânga-sus al suprafeței
    este (-2, 1), iar cel din dreapta-jos, (2, -1). Pentru fiecare punct
    cu coordonate întregi de pe suprafața de desenare, se introduce caracterul
    '*', dacă punctul aparține regiunii de desenat, sau '.', altfel. Funcția
    se utilizează în conjuncție cu funcția printPlot, definită mai jos
    în schelet, pentru o mai bună vizualizare.

    Constrângeri: funcția trebuie implementată cu list comprehensions,
    fără recursivitate explicită.

    Hints:
    * fromIntegral pentru conversia de la Int la Float.
    * intercalate pentru alipirea mai multor liste folosind un element
      de legătură.

    Exemple:

    > printPlot 2 1 $ fromPoints [(0, 0), (1, 1)]
    ...*.
    ..*..
    .....

    > printPlot 2 2 $ rectangle 2 2
    .....
    .***.
    .***.
    .***.
    .....

    Deși dimensiunile dreptunghiului sunt 2 și 2, apariția a câte 3 caractere
    '*' pe orizontală și pe verticală poate fi înțeleasă dacă vă gândiți
    la coordonatele vizate, -1, 0 și 1, în toate combinațiile (x, y).

    > printPlot 2 2 $ circle 2     
    ..*..
    .***.
    *****
    .***.
    ..*..
-}
plot :: Int -> Int -> Region -> String
plot width height region =  intercalate "\n" [
    -- If the point (x, y) is in the given region, add '*' at the line, esle add '.'
    [if region (fromIntegral x, fromIntegral y) then '*' else '.'
    | x <- [-width..width]] -- List comprehension for x
    | y <- [height, height-1.. -height] -- List comprehension for y descending coords
    ]

{-
    Utilizați această funcție pentru vizualizarea diagramelor,
    după ce implementați funcția plot.
-}
printPlot :: Int -> Int -> Region -> IO ()
printPlot width height region = putStrLn $ plot width height region

{-
    *** TODO ***

    Implementați funcțiile promoteUnary și promoteBinary, care primesc
    o funcție unară (a -> b), respectiv binară (a -> b -> c), și o promovează
    pentru a opera pe rezultatul(-ele) unor funcții (Point -> a) etc.

    Constrângeri: funcția promoteUnary trebuie implementată point-free.

    Hint: dacă expandăm referirile la Pointed din tipul funcției promoteUnary,
    obținem (a -> b) -> (Point -> a) -> (Point -> b). Practic, trebuie
    construită o funcție cu tipul (Point -> b), pornind de la o funcție cu tipul
    (Point -> a) și aplicând apoi funcția cu tipul (a -> b) pe rezultatul ei.
    Extindeți apoi ideea pentru promoteBinary.

    Exemple:

    > promoteUnary (+ 1) (\(x, _) -> x) (3, 2)
    4.0

    > promoteBinary (+) (\(x, _) -> x) (\(_, y) -> y) (3, 2)
    5.0
-}
{-
    Apply g on a function and than apply f on the result;
    point-free, without mentioning the parameters
-}
promoteUnary :: (a -> b) -> Pointed a -> Pointed b
promoteUnary f g = f . g

{-
    `pointed1 point` applies function `pointed1` on a `point`,returning an `a`
    `pointed2 point` applies function `pointed2` on a `point`,returning a `b`
    f applies a binary function on the results
-}
promoteBinary :: (a -> b -> c) -> Pointed a -> Pointed b -> Pointed c
promoteBinary f pointed1 pointed2 point = f (pointed1 point) (pointed2 point)

{-
    *** TODO ***

    Implementați funcțiile complement, union și intersection, care determină
    complementul, reuniunea, respectiv intersecția a două regiuni.

    Constrângeri: funcțiile trebuie implementate point-free, utilizând
    promoteUnary sau promoteBinary, după caz.

    Exemple:

    > printPlot 2 2 $ complement $ circle 2
    **.**
    *...*
    .....
    *...*
    **.**

    > printPlot 2 2 $ union (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)])
    *....
    ..*..
    .***.
    ..*..
    ....*

    > printPlot 2 2 $ intersection (circle 1) (fromPoints [(0, 0), (-2, 2), (2, -2)])
    .....
    .....
    ..*..
    .....
    .....
-}

-- `promoteUnary not` takes a `Region` function (of type `Point -> Bool`) and returns a new `Region` function
-- which applies `not' to the result of the original function.
complement :: Region -> Region
complement = promoteUnary not

-- `promoteBinary (||)` takes two `Region` functions and returns a new `Region` function
-- which applies `(||)` to the results of the original functions.
union :: Region -> Region -> Region
union = promoteBinary (||)

-- `promoteBinary (&&)` takes two `Region` functions and returns a new `Region` function
-- which applies `(&&)` to the results of the original functions.
intersection :: Region -> Region -> Region
intersection = promoteBinary (&&)

{-
    *** TODO ***

    Implementați funcția translation, care generează o translație
    cu deplasamente primite ca parametri. Deși contraintuitiv, deplasamentele
    trebuie scăzute, nu adunate, din coordonatele punctului transformat.
    De exemplu, dacă punctul (0, 0) aparține unei regiuni de interes, atunci
    punctul (1, 2) va trebui să aparțină regiunii în urma translației
    cu deplasamentele 1 și 2. Din moment ce funcția caracteristică a regiunii
    întoarce True pentru (0, 0), nu pentru (1, 2), cele două deplasamente
    trebuie scăzute.

    Exemple:

    > translation 1 2 (1, 2)
    (0.0,0.0)
-}
translation :: Float -> Float -> Transformation
translation tx ty = \(x, y) -> (x - tx, y - ty)

{-
    *** TODO ***

    Implementați funcția scaling, care generează o scalare cu un factor primit
    ca parametru. Similar cu observația de la funcția translate, factorul
    contribuie prin împărțire, nu prin înmulțire.

    Exemple:

    > scaling 2 (2, 2)
    (1.0,1.0)
-}
scaling :: Float -> Transformation
scaling factor = \(x, y) -> (x / factor, y / factor)

{-
    *** TODO ***
    Implementați funcția applyTransformation, care aplică o transformare asupra
    unei regiuni.

    Constrângeri: funcția trebuie implementată point-free.

    Exemple:

    > printPlot 2 2 $ applyTransformation (translation 1 0) (circle 2)
    ...*.
    ..***
    .****
    ..***
    ...*.

    > printPlot 2 2 $ applyTransformation (scaling 0.5) (circle 2)    
    .....
    ..*..
    .***.
    ..*..
    .....
-}
-- make a new region from the given transformation
applyTransformation :: Transformation -> Region -> Region
applyTransformation transformation region = region . transformation

{-
    Implementați funcția combineTransformations, care combină transformările
    dintr-o listă într-o singură transformare. Ordinea de aplicare
    a transformărilor este dată de ordinea lor în listă.

    Constrângeri: funcția trebuie implementată point-free, fără recursivitate
    explicită.

    Exemple:

    > printPlot 2 2 $ applyTransformation
        (combineTransformations [translation 1 0, scaling 0.5]) (circle 2)
    .....
    ...*.
    ..***
    ...*.
    .....

    Echivalent cu:

    > printPlot 2 2 $ applyTransformation (translation 1 0) $ applyTransformation (scaling 0.5) (circle 2)
-}

combineTransformations :: [Transformation] -> Transformation
combineTransformations = foldl (flip (.)) id

-- Apply flip (.) t1 id → results in t1 . id (simplifies to t1)
-- Apply flip (.) t2 t1 → results in t2 . t1
-- Apply flip (.) t3 (t2 . t1) → results in t3 . (t2 . t1)



{-
    Funcția circles de mai jos generează o regiune formată din n cercuri de rază
    2, translatate succesiv cu 6 unități pe orizontală.

    Explicați la prezentare utilitatea evaluării leneșe pentru determinarea
    eficientă a apartenenței unui punct la regiunea construită prin reuniune.

    Hint: utilizați trace (vezi laboratorul 7) în funcția circle pentru afișarea
    punctului primit ca parametru și evaluați expresiile de mai jos:
    > inside (0, 0) $ circles 3
    > inside (6, 0) $ circles 3
    > inside (12, 0) $ circles 3
    > inside (18, 0) $ circles 3

    Exemple:

    > printPlot 15 3 $ circles 3
    ...............................
    ...............*.....*.....*...
    ..............***...***...***..
    .............*****.*****.*****.
    ..............***...***...***..
    ...............*.....*.....*...
    ...............................

    Răspuns: ...............

    Evaluarea lenesa se foloseste pentru a optimiza felul in care sunt evaluate
    expresiile prin intarzierea acestora pana cand este necesar rezultatul lor.
    In cazul de fata este un beneficiu pentru ca se construieste ceva complex
    cu union-ul din circles. Cand se verifica daca punctul apartine unuia dintre
    cercuri, daca apartine primului, nu mai este necesar sa se verifice si la urm.
    De exemplu, pt (6, 0) evaluarea lenesa va verifica doar al doilea cerc, nu si
    celelalte cerucri, daca pct se afla in al doiela.

-}
circles :: Int -> Region
circles n
    | n <= 0    = const False
    | otherwise = union (circle 2)
                        (applyTransformation (translation 6 0)
                                             (circles (n - 1)))

{-
    Explicați la prezentare cum se comportă reuniunea infinită de mai jos
    când se verifică apartenența unui punct care NU aparține regiunii.

    Răspuns: ...............

    Functia reprezinta o uniune de cercuri, fiecare de raza 2, translatata progresiv
    cu cate 6 unitati spre dreapta pe axa Ox.
    Cand se verifica daca un punct apartine unui cerc, dar acesta nu apartine de fapt
    se va evalua la infinit functia si vom avea un loop infinit pana la un eventual
    stack overflow.

-}
infiniteCircles :: Region
infiniteCircles = union (circle 2)
                        (applyTransformation (translation 6 0)
                                             infiniteCircles)

{-
    *** BONUS ***

    Implementați funcția bfs, care realizează o căutare în lățime într-un spațiu
    oarecare de stări de tipul a, pornind de la o stare inițială start și de la
    o funcție expand, care determină pentru o stare curentă lista stărilor vecin.
    Funcția întoarce o listă, eventual infinită, de perechi de forma
    (stare, distanță), unde stările sunt ordonate conform parcurgerii în lățime,
    iar distanța reprezintă numărul de expandări realizate pentru a obține
    starea respectivă.

    Atenție! Pot exista multiple căi către aceeași stare, astfel fiind necesară
    reținerea stărilor deja vizitate utilizând o mulțime (Set, vezi modulul
    Data.Set). Observați la începutul acestui fișier linia "import qualified
    Data.Set as S". Acest lucru înseamnă că toate tipurile și funcțiile din acel
    modul trebuie prefixate cu "S."; de exemplu: S.Set, S.insert etc.

    Hint: dacă ar exista o cale unică de la starea inițială la orice altă stare,
    nefiind necesară reținerea stărilor deja vizitate, și nici nu s-ar solicita
    calculul distanțelor, funcția ar putea fi implementată prin:

    bfs :: a -> (a -> [a]) -> [a]
    bfs start expand = result
      where
        result = start : concat (map expand result)

    map operează independent pe stări și este insuficient de expresiv pentru
    a permite purtarea mulțimii de stări vizitate de la o stare la alta. Pentru
    acest lucru, ar fi necesar foldl. Funcționala predefinită mapAccumL
    realizează exact această combinație, map + foldl. O puteți utiliza pentru
    a extinde implementarea de mai sus.
-}

-- Define the BFS function with type signature.
-- It takes a starting node and a function to expand nodes, returning a list of nodes and their distances from the start.
bfs :: (Ord a) => a -> (a -> [a]) -> [(a, Int)]
bfs start expand = bfs' S.empty [(start, 0)]
  where
    -- The helper function bfs' performs the actual BFS.
    -- It uses a set to track visited nodes and a list as a queue to manage the nodes to be processed.
    bfs' visited [] = []
    -- If the queue is empty, return an empty list indicating completion.
    bfs' visited ((s, dist):rest)
    -- Process each node `s` with its distance `dist` from the start, along with the rest of the queue.
      | s `S.member` visited = bfs' visited rest
      -- If the node has already been visited, continue with the rest of the queue.
      | otherwise = (s, dist) : bfs' newVisited newQueue
      -- If the node has not been visited, include it in the result and process its neighbors.
      where
        -- Retrieve all neighbors of the current node using the expand function.
        neighbors = expand s
        -- Filter out neighbors that have already been visited.
        filteredNeighbors = filter (`S.notMember` visited) neighbors
        -- Add the current node to the set of visited nodes.
        newVisited = S.insert s visited

        -- Prepare the new queue by appending the filtered neighbors to the rest of the queue.
        -- Each neighbor is assigned a distance of `dist + 1`.

        -- newQueue = rest ++ zip filteredNeighbors (repeat (dist + 1))
        newQueue = rest ++ map (, dist + 1) filteredNeighbors
        -- The map (, dist + 1) applies to each neighbor, pairing it with its new distance, creating new entries for the queue.

{-
    *** BONUS ***

    Implementați funcția regionAvoidingBfs, care determină distanța minimă
    de la orice punct la un nod de start, obținută prin deplasări către nord,
    est, sud sau vest, și ocolind regiunea primită ca parametru.

    Constrângeri: utilizați funcția bfs.

    Exemple:

    > lookup (3, 0) $ regionAvoidingBfs (-3, 0) $ circles 3
    Just 12

    Explicație: distanța de la punctul (-3, 0) la punctul (3, 0) este 12,
    și este descrisă mai jos, prin distanțele către punctele intermediare.
    Au fost folosite și cifre hexazecimale, pentru încadrarea într-un singur
    caracter. Distanța 0 corespunde punctului (-3, 0), iar distanța C (12),
    punctului (3, 0).

    ...................567...................
    ..................34*89...*.....*........
    .................12***AB.***...***.......
    .................0*****C*****.*****......
    ...................***...***...***.......
    ....................*.....*.....*........
    .........................................
-}
-- Define movements to north, east, south, and west
-- list of lambda functions where each function represents a movement in one of the four cardinal directions
moves :: [Point -> Point]
moves = [\(x, y) -> (x, y+1), \(x, y) -> (x+1, y), \(x, y) -> (x, y-1), \(x, y) -> (x-1, y)]

-- Helper function to filter valid moves
-- takes a Region (a function that specifies which points are blocked or restricted) and a Point,
-- and returns True if the point is not within the region
validMove :: Region -> Point -> Bool
validMove region p = not (region p)

-- Expand function for BFS
-- generates all valid next positions from a given point by applying each movement in moves to the point and
-- then filtering out any resulting points that fall within the restricted region
expand :: Region -> Point -> [Point]
expand region point = filter (validMove region) (map ($ point) moves)

-- performs the BFS, starting from a given start point and using the custom expand function. It uses the BFS
--  that expects a start point and a function to generate the next possible points
regionAvoidingBfs :: Point -> Region -> [(Point, Int)]
regionAvoidingBfs start region = bfs start (expand region)
