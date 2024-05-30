module Deep where

import Shallow (Point, Region, Transformation)
import qualified Shallow as S

{-
    Deep embeddings pentru regiuni și transformări. Fiecare regiune
    și transformare este reprezentată sub forma unui arbore sintactic
    (abstract syntax tree, AST) ce descrie secvența de operații care contribuie
    la construcția acelei regiuni sau transformări. De exemplu, expresia
    (circles 2), unde circles a fost definită în etapa 1, ar produce acum
    un rezultat similar cu

    Union (Circle 2.0) (Transform (Translation 6.0 0.0) (Circle 2.0)).

    Pentru a obține acest efect, toate funcțiile din etapa 1 sunt reimplementate
    astfel încât să utilizeze direct constructorul de date potrivit al unui
    tip de date. De exemplu, funcția fromPoints *este* acum constructorul
    FromPoints.

    Primul avantaj major al reprezentării bazate pe AST-uri este posibilitatea
    interpretării acesteia în diverse moduri pentru a reconstitui semnificații
    concrete variate ale regiunilor și transformărilor, e.g. regiuni ca funcții
    caracteristice, și transformări ca funcții pe puncte, ca în etapa 1.
    Vom vedea și alte semnificații concrete în etapa 3.

    Al doilea mare avantaj îl constituie posibilitatea simplificării AST-ului
    înaintea interpretării lui într-o manieră specifică. Observați deja cum
    funcțiile combineTransformations și applyTransformation de mai jos, văzute
    ca smart constructors, recunosc anumite cazuri particulare și simplifică
    AST-ul încă de la construcție.
-}
data RegionAST
    = FromPoints [Point]
    | Rectangle Float Float
    | Circle Float
    | Complement RegionAST
    | Union RegionAST RegionAST
    | Intersection RegionAST RegionAST
    | Transform TransformationAST RegionAST
    deriving (Show, Eq)

data TransformationAST
    = Translation Float Float
    | Scaling Float
    | Combine [TransformationAST]
    deriving (Show, Eq)

fromPoints :: [Point] -> RegionAST
fromPoints = FromPoints

rectangle :: Float -> Float -> RegionAST
rectangle = Rectangle

circle :: Float -> RegionAST
circle = Circle

complement :: RegionAST -> RegionAST
complement = Complement

union :: RegionAST -> RegionAST -> RegionAST
union = Union

intersection :: RegionAST -> RegionAST -> RegionAST
intersection = Intersection

translation :: Float -> Float -> TransformationAST
translation = Translation

scaling :: Float -> TransformationAST
scaling = Scaling

{-
    Smart constructor: dacă lista de transformări este singleton, înlocuiește
    lista cu unica transformare din listă; altfel, utilizează constructorul
    de date Combine.
-}
combineTransformations :: [TransformationAST] -> TransformationAST
combineTransformations [transformation] = transformation
combineTransformations transformations = Combine transformations

{-
    Smart constructor: dacă se dorește aplicarea unei liste vide de transformări
    asupra unei regiuni, întoarce regiunea ca atare; altfel, utilizează
    constructorul de date Transform.
-}
applyTransformation :: TransformationAST -> RegionAST -> RegionAST
applyTransformation (Combine []) region = region
applyTransformation transformation region = Transform transformation region

{-
    *** TODO ***

    Implementați funcția toTransformation, care constituie o interpretare
    a AST-ului unei transformări (TransformationAST), în vederea recuperării
    reprezentării concrete din etapa 1, sub forma unei funcții cu tipul
    Transformation = (Point -> Point).

    Nu este necesar să implementați de la zero, ci puteți invoca direct
    funcțiile din etapa 1, prefixându-le cu "S." (observați la începutul acestui
    fișier linia "import qualified Shallow as S"). Mai precis, funcțiile
    neprefixate, e.g. translation, sunt cele din acest fișier, iar funcțiile
    prefixate, e.g. S.translation, sunt cele din modulul Shallow.
-}
-- Transforms a TransformationAST into a concrete transformation function (Point -> Point)
toTransformation :: TransformationAST -> Transformation
toTransformation (Translation dx dy) = S.translation dx dy -- Converts Translation to a function that moves a point by (dx, dy)
toTransformation (Scaling factor) = S.scaling factor -- Converts Scaling to a function that scales a point from the origin by `factor`
-- Converts each AST to a function, then folds these functions using composition
toTransformation (Combine transformations) = combineTransformations transformations
  where
    -- foldl (flip (.)) id for the right order of applying functions
    combineTransformations = foldl (flip (.)) id . map toTransformation

{-

    Implementați funcția toRegion, care constituie o interpretare a AST-ului
    unei regiuni (RegionAST), în vederea recuperării reprezentării concrete
    din etapa 1, sub forma unei funcții caracteristice cu tipul
    Region = (Point -> Bool).
-}
toRegion :: RegionAST -> Region
-- Converts a list of points into a region that includes just those points
toRegion (FromPoints points) = S.fromPoints points
-- Creates a rectangle region of specified width and height
toRegion (Rectangle width height) = S.rectangle width height
-- Defines a circular region with the given radius
toRegion (Circle radius) = S.circle radius
-- Creates the complement of a region, i.e., points not in the original region
toRegion (Complement region) = S.complement (toRegion region)
-- Forms the union of two regions, including points in either
toRegion (Union region1 region2) = S.union (toRegion region1) (toRegion region2)
-- Forms the intersection of two regions, including points in both
toRegion (Intersection region1 region2) = S.intersection (toRegion region1) (toRegion region2)
-- Applies a transformation to a region, modifying its shape or position according to the transformation function
toRegion (Transform transformation region) = S.applyTransformation (toTransformation transformation) (toRegion region)

{-
    Varianta actualizată a a funcției inside.
-}
inside :: Point -> RegionAST -> Bool
inside = flip toRegion

{-
    Implementați funcția decomposeTransformation, care descompune o transformare
    oricât de complexă într-o listă de transformări elementare (translații
    și scalări), conservând bineînțeles ordinea acestora.

    Constrângeri: evitați recursivitatea explicită.

    Hint: valorificați ordinea de sus în jos de realizare a pattern matching-ului,
    pentru a scurta descrierea cazurilor.

    Exemple:

    > decomposeTransformation $ Translation 1 2
    [Translation 1.0 2.0]

    > decomposeTransformation $ Scaling 2
    [Scaling 2.0]

    > decomposeTransformation $
        Combine [ Translation 1 2
                , Combine [ Translation 3 4
                          , Scaling 2
                          ]
                , Scaling 3
                ]
    [Translation 1.0 2.0,Translation 3.0 4.0,Scaling 2.0,Scaling 3.0]
-}
-- decomposes a complex TransformationAST into a list of elementary transformations (translations and scalings),
-- preserving their order of application
decomposeTransformation :: TransformationAST -> [TransformationAST]
-- Directly returns a list containing the translation transformation
decomposeTransformation (Translation dx dy) = [Translation dx dy]
-- returns a list containing the scaling transformation
decomposeTransformation (Scaling factor) = [Scaling factor]
-- Recursively decomposes each transformation in the list. Combine can contain nested Combine structures or individual transformations.
-- concatMap applies decomposeTransformation to each transformation in the list and concatenates the results, preserving the order.
decomposeTransformation (Combine transformations) =
    concatMap decomposeTransformation transformations

{-
    Implementați funcția fuseTransformations, care alipește transformările
    adiacente de același fel (translații cu translații și scalări cu scalări)
    dintr-o listă de transformări elementare (translații și scalări),
    și întoarce lista transformărilor rezultante.

    Constrângeri: evitați recursivitatea explicită.

    > fuseTransformations [Translation 1 2]
    [Translation 1.0 2.0]

    > fuseTransformations [Scaling 2, Scaling 3]             
    [Scaling 6.0]

    > fuseTransformations [ Translation 1 2, Translation 3 4
                          , Scaling 2, Scaling 3
                          , Translation 5 6
                          ]
    [Translation 4.0 6.0,Scaling 6.0,Translation 5.0 6.0]
-}

-- This version of fuseTransformations attempts to fuse adjacent transformations if possible
fuseTransformations :: [TransformationAST] -> [TransformationAST]
fuseTransformations = foldr fuse []
  where
    fuse :: TransformationAST -> [TransformationAST] -> [TransformationAST]
    fuse t [] = [t]  -- If the list is empty, start with the current transformation
    -- Checks if the current transformation can be fused with the next in the list. If yes, fuse them, otherwise add to the list.
    fuse t (x:xs)
      -- Fuses t with x if they are of the same type and adds to the result list.
      | canFuse t x = fuseSingle t x : xs  -- If they can fuse, fuse them and continue
      -- Appends t to the result list if it cannot be fused with x.
      | otherwise = t : x : xs  -- Otherwise, add the current transformation to the result list

    -- Determines if two transformations can be fused based on their type.
    canFuse :: TransformationAST -> TransformationAST -> Bool
    canFuse (Translation _ _) (Translation _ _) = True
    canFuse (Scaling _) (Scaling _) = True
    canFuse _ _ = False -- Other combinations are not fusible.

    -- Performs the actual fusion of two transformations if they are of the same type.
    fuseSingle :: TransformationAST -> TransformationAST -> TransformationAST
    -- Sums the displacements for translations.
    fuseSingle (Translation dx1 dy1) (Translation dx2 dy2) = Translation (dx1 + dx2) (dy1 + dy2)
    -- Multiplies scale factors for scalings.
    fuseSingle (Scaling s1) (Scaling s2) = Scaling (s1 * s2)
    -- Default case should not be hit due to prior checks
    fuseSingle x _ = x

{-
    Implementați funcția optimizeTransformations, care simplifică toate
    transformările din AST-ul unei regiuni. Principiile sunt următoarele:

    * Transformările succesive trebuie descompuse și alipite.
    * Pentru a evidenția lanțuri cât mai lungi de transformări succesive,
      se urmărește deplasarea în sus a transformărilor din AST, astfel:
      * Complementul unei transformări este transformarea complementului.
      * O ramificare (reuniune sau intersecție) de transformări de regiuni
        presupune determinarea celui mai lung prefix de transformări comune
        de pe cele două ramuri și deplasarea acestuia deasupra ramificării,
        păstrând sub ea sufixele de transformări necomune.
    * O regiune elementară (din puncte, dreptunghi sau cerc) se consideră
      optimizată.
    * Toate cosmetizările de mai sus se realizează după optimizarea recursivă
      a subregiunilor.
    
    Constrângeri: evitați duplicarea codului.

    Hints:

    * case pentru pattern matching în interiorul altor expresii
    * smart constructors: combineTransformation, applyTransformation

    Exemple:

    > optimizeTransformations $
        Transform (Combine [ Translation 1 2
                           , Combine [ Translation 3 4
                                     , Scaling 2
                                     ]  
                           , Scaling 3
                           ])
                  (Circle 5)
    Transform (Combine [Translation 4.0 6.0,Scaling 6.0]) (Circle 5.0)

    > optimizeTransformations $
        Transform (Combine [ Translation 1 2
                           , Combine [ Translation 3 4
                                     , Scaling 2
                                     ]  
                           , Scaling 3
                           ])
                  (Transform (Scaling 4)
                             (Transform (Scaling 2) (Circle 5)))
    Transform (Combine [Translation 4.0 6.0,Scaling 48.0]) (Circle 5.0)

    > optimizeTransformations $
        Complement (Transform (Scaling 4)
                              (Transform (Scaling 2) (Circle 5)))
    Transform (Scaling 8.0) (Complement (Circle 5.0))

    > optimizeTransformations $
        Transform (Combine [ Translation 1 2
                           , Combine [ Translation 3 4
                                     , Scaling 2
                                     ]  
                           , Scaling 3
                           ])
                  (Complement (Transform (Scaling 4)
                                         (Transform (Scaling 2) (Circle 5))))
    Transform (Combine [Translation 4.0 6.0,Scaling 48.0]) (Complement (Circle 5.0))

    > optimizeTransformations $
        Union (Complement (Transform (Scaling 4)
                                     (Transform (Scaling 2) (Circle 5))))
              (Rectangle 6 7)
    Union (Transform (Scaling 8.0) (Complement (Circle 5.0))) (Rectangle 6.0 7.0)

    > optimizeTransformations $
        Union (Transform (Combine [ Translation 1 2
                                  , Combine [ Translation 3 4
                                            , Scaling 2
                                            ]  
                                  , Scaling 3
                                  ])
                         (Complement (Transform (Scaling 4)
                                                (Transform (Scaling 2) (Circle 5)))))
              (Transform (Translation 4 6) (Rectangle 6 7))
    Transform (Translation 4.0 6.0)
              (Union (Transform (Scaling 48.0) (Complement (Circle 5.0)))
                     (Rectangle 6.0 7.0))
-}

-- | Optimizes a RegionAST by reducing nested transformations and simplifying the structure.
optimizeTransformations :: RegionAST -> RegionAST
optimizeTransformations region = case region of
    -- Optimizes transformations by flattening and fusing them.
    Transform transformation subRegion ->
        let optimizedSubRegion = optimizeTransformations subRegion
            newTransformation = combineTransformationsFlatter [transformation, extractTransformations optimizedSubRegion]
            newSubRegion = stripTransformations optimizedSubRegion
        in applyTransformationFlatter newTransformation newSubRegion

    -- Moves transformations out of the Complement where possible.
    Complement subRegion ->
        let optimizedSubRegion = optimizeTransformations subRegion
        in case optimizedSubRegion of
            Transform trans innerRegion -> Transform trans (Complement innerRegion)
            _ -> Complement optimizedSubRegion

    -- Optimizes binary operations by extracting common transformations.
    Union region1 region2 -> optimizeBinary Union region1 region2
    Intersection region1 region2 -> optimizeBinary Intersection region1 region2

    -- Base regions are considered already optimized.
    _ -> region

-- Handles optimization for binary operations (Union, Intersection).
optimizeBinary :: (RegionAST -> RegionAST -> RegionAST) -> RegionAST -> RegionAST -> RegionAST
optimizeBinary constructor region1 region2 =
    let
        -- Optimize each region individually before trying to combine them.
        optimizedRegion1 = optimizeTransformations region1
        optimizedRegion2 = optimizeTransformations region2

        -- Extract common transformations from both optimized regions. This helps in simplifying the regions further by applying these transformations globally.
        commonTransformation = intersectTransformations (extractTransformations optimizedRegion1) (extractTransformations optimizedRegion2)

        -- Remove the common transformations from each region, so they can be applied later, globally to the combined region.
        newRegion1 = applyTransformationFlatter (subtractTransformations (extractTransformations optimizedRegion1) commonTransformation) (stripTransformations optimizedRegion1)
        newRegion2 = applyTransformationFlatter (subtractTransformations (extractTransformations optimizedRegion2) commonTransformation) (stripTransformations optimizedRegion2)

        -- Apply the common transformations to the result of the constructor (Union or Intersection),
        -- which simplifies the representation and potentially reduces computation during further processing.
    in applyTransformationFlatter commonTransformation (constructor newRegion1 newRegion2)


-- Combines multiple transformations into a single TransformationAST by flattening nested 'Combine' instances.
-- This results in a more compact and efficient representation for further processing.
combineTransformationsFlatter :: [TransformationAST] -> TransformationAST
combineTransformationsFlatter transformations = Combine (foldr flattenAndFuse [] transformations)
  where
    -- Recursively flattens 'Combine' into a single list of transformations, ensuring there are no nested 'Combine' constructors.
    flattenAndFuse :: TransformationAST -> [TransformationAST] -> [TransformationAST]
    flattenAndFuse (Combine ts) acc = foldr flattenAndFuse acc ts
    flattenAndFuse t acc = t : acc  -- Appends a single transformation to the accumulator.

-- Applies the most compact form of the transformations to a region, optimizing performance by minimizing transformation steps.
applyTransformationFlatter :: TransformationAST -> RegionAST -> RegionAST
applyTransformationFlatter (Combine transformations) region =
    case fuseTransformations transformations of
        [] -> region  -- No transformation needed if the list is empty.
        [single] -> applyTransformationFlatter single region  -- Apply a single transformation directly.
        ts -> Transform (Combine ts) region  -- Apply the combined transformations.
applyTransformationFlatter transformation region = Transform transformation region  -- Apply a non-combined transformation.

-- Extracts and combines all transformations from a RegionAST, ensuring that transformations are aggregated from potentially nested structures.
extractTransformations :: RegionAST -> TransformationAST
extractTransformations (Transform transformation region) = combineTransformationsFlatter [transformation, extractTransformations region]
extractTransformations _ = Combine []  -- Returns an empty Combine when there are no transformations.

-- Removes all transformation constructs from a RegionAST, returning only the base region.
-- This is useful for simplifying the region before applying any transformations or operations.
stripTransformations :: RegionAST -> RegionAST
stripTransformations (Transform _ region) = stripTransformations region
stripTransformations region = region

-- Finds common elements between two lists of TransformationAST, used to determine transformations that can be applied universally to multiple regions.
intersectTransformations :: TransformationAST -> TransformationAST -> TransformationAST
intersectTransformations (Combine t1) (Combine t2) = Combine (t1 `intersect` t2)
intersectTransformations _ _ = Combine []  -- Returns an empty Combine if there's no overlap.

-- Computes the intersection of two lists of transformations, identifying transformations that are identical in both lists.
intersect :: [TransformationAST] -> [TransformationAST] -> [TransformationAST]
intersect xs ys = [x | x <- xs, x `elem` ys]  -- Filters elements that are present in both lists.

-- Subtracts one list of transformations from another, removing transformations found in both lists.
-- This is used when optimizing to ensure transformations are not redundantly applied.
subtractTransformations :: TransformationAST -> TransformationAST -> TransformationAST
subtractTransformations (Combine t1) (Combine t2) = Combine (subtract' t1 t2)
subtractTransformations t _ = t  -- Returns the original transformation if there is nothing to subtract.

-- Helper function for 'subtractTransformations' that performs the actual list subtraction.
subtract' :: [TransformationAST] -> [TransformationAST] -> [TransformationAST]
subtract' xs ys = [x | x <- xs, x `notElem` ys]  -- Filters out elements that are present in the second list.


