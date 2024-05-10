module Deep where

import Shallow (Point, Region, Transformation)
import qualified Shallow as S

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
-- combineTransformations :: [TransformationAST] -> TransformationAST
-- combineTransformations [transformation] = transformation
-- combineTransformations transformations = Combine transformations

-- This function combines multiple transformations into one, flattening nested Combines
combineTransformations :: [TransformationAST] -> TransformationAST
combineTransformations transformations = Combine (foldr flattenAndFuse [] transformations)
  where
    flattenAndFuse :: TransformationAST -> [TransformationAST] -> [TransformationAST]
    flattenAndFuse (Combine ts) acc = foldr flattenAndFuse acc ts -- Recursively flatten Combines
    flattenAndFuse t acc = t : acc -- Regular transformations are added to the list


{-
    Smart constructor: dacă se dorește aplicarea unei liste vide de transformări
    asupra unei regiuni, întoarce regiunea ca atare; altfel, utilizează
    constructorul de date Transform.
-}
-- applyTransformation :: TransformationAST -> RegionAST -> RegionAST
-- applyTransformation (Combine []) region = region
-- applyTransformation transformation region = Transform transformation region
applyTransformation :: TransformationAST -> RegionAST -> RegionAST
applyTransformation (Combine transformations) region =
    case fuseTransformations transformations of
        [] -> region  -- If no transformations are left after fusing, return the region as is
        [single] -> applyTransformation single region
        ts -> Transform (Combine ts) region
applyTransformation transformation region = Transform transformation region



toTransformation :: TransformationAST -> Transformation
toTransformation (Translation dx dy) = S.translation dx dy
toTransformation (Scaling factor) = S.scaling factor
toTransformation (Combine transformations) = combineTransformations transformations
  where
    combineTransformations = foldl (flip (.)) id . map toTransformation

{-

    Implementați funcția toRegion, care constituie o interpretare a AST-ului
    unei regiuni (RegionAST), în vederea recuperării reprezentării concrete
    din etapa 1, sub forma unei funcții caracteristice cu tipul
    Region = (Point -> Bool).
-}
toRegion :: RegionAST -> Region
toRegion (FromPoints points) = S.fromPoints points
toRegion (Rectangle width height) = S.rectangle width height
toRegion (Circle radius) = S.circle radius
toRegion (Complement region) = S.complement (toRegion region)
toRegion (Union region1 region2) = S.union (toRegion region1) (toRegion region2)
toRegion (Intersection region1 region2) = S.intersection (toRegion region1) (toRegion region2)
toRegion (Transform transformation region) = S.applyTransformation (toTransformation transformation) (toRegion region)

{-
    Varianta actualizată a a funcției inside.
-}
inside :: Point -> RegionAST -> Bool
inside = flip toRegion

decomposeTransformation :: TransformationAST -> [TransformationAST]
decomposeTransformation (Translation dx dy) = [Translation dx dy]
decomposeTransformation (Scaling factor) = [Scaling factor]
decomposeTransformation (Combine transformations) =
    concatMap decomposeTransformation transformations


-- This version of fuseTransformations attempts to fuse adjacent transformations if possible
fuseTransformations :: [TransformationAST] -> [TransformationAST]
fuseTransformations = foldr fuse []
  where
    fuse :: TransformationAST -> [TransformationAST] -> [TransformationAST]
    fuse t [] = [t]  -- If the list is empty, start with the current transformation
    fuse t (x:xs)
      | canFuse t x = fuseSingle t x : xs  -- If they can fuse, fuse them and continue
      | otherwise = t : x : xs  -- Otherwise, add the current transformation to the result list

    canFuse :: TransformationAST -> TransformationAST -> Bool
    canFuse (Translation _ _) (Translation _ _) = True
    canFuse (Scaling _) (Scaling _) = True
    canFuse _ _ = False

    fuseSingle :: TransformationAST -> TransformationAST -> TransformationAST
    fuseSingle (Translation dx1 dy1) (Translation dx2 dy2) = Translation (dx1 + dx2) (dy1 + dy2)
    fuseSingle (Scaling s1) (Scaling s2) = Scaling (s1 * s2)
    fuseSingle x _ = x  -- Default case should not be hit due to prior checks


optimizeTransformations :: RegionAST -> RegionAST
optimizeTransformations region = case region of
    Transform transformation subRegion ->
        let optimizedSubRegion = optimizeTransformations subRegion
            newTransformation = combineTransformations [transformation, extractTransformations optimizedSubRegion]
            newSubRegion = stripTransformations optimizedSubRegion
        in applyTransformation newTransformation newSubRegion

    Complement subRegion ->
        let optimizedSubRegion = optimizeTransformations subRegion
        in case optimizedSubRegion of
            Transform trans innerRegion -> Transform trans (Complement innerRegion)
            _ -> Complement optimizedSubRegion

    Union region1 region2 ->
        optimizeBinary Union region1 region2

    Intersection region1 region2 ->
        optimizeBinary Intersection region1 region2

    _ -> region  -- Basic regions are already optimized by definition

optimizeBinary :: (RegionAST -> RegionAST -> RegionAST) -> RegionAST -> RegionAST -> RegionAST
optimizeBinary constructor region1 region2 =
    let
        optimizedRegion1 = optimizeTransformations region1
        optimizedRegion2 = optimizeTransformations region2
        commonTransformation = intersectTransformations (extractTransformations optimizedRegion1) (extractTransformations optimizedRegion2)
        newRegion1 = applyTransformation (subtractTransformations (extractTransformations optimizedRegion1) commonTransformation) (stripTransformations optimizedRegion1)
        newRegion2 = applyTransformation (subtractTransformations (extractTransformations optimizedRegion2) commonTransformation) (stripTransformations optimizedRegion2)
    in applyTransformation commonTransformation (constructor newRegion1 newRegion2)



-- Additional functions to extract and manipulate transformations from RegionAST, not shown for brevity
-- Extracts all transformations from the nested structure
extractTransformations :: RegionAST -> TransformationAST
extractTransformations (Transform transformation region) = combineTransformations [transformation, extractTransformations region]
extractTransformations _ = Combine []  -- No transformation in basic regions

-- Strips all transformations, leaving only the base region
stripTransformations :: RegionAST -> RegionAST
stripTransformations (Transform _ region) = stripTransformations region
stripTransformations region = region

-- Finds common transformations between two transformation lists
intersectTransformations :: TransformationAST -> TransformationAST -> TransformationAST
intersectTransformations (Combine t1) (Combine t2) = Combine (intersect t1 t2)
intersectTransformations _ _ = Combine []

intersect :: [TransformationAST] -> [TransformationAST] -> [TransformationAST]
intersect xs ys = [x | x <- xs, x `elem` ys]  -- Simple intersection for demonstration

-- Subtracts transformations from one list found in another
subtractTransformations :: TransformationAST -> TransformationAST -> TransformationAST
subtractTransformations (Combine t1) (Combine t2) = Combine (subtract' t1 t2)
subtractTransformations t _ = t

subtract' :: [TransformationAST] -> [TransformationAST] -> [TransformationAST]
subtract' xs ys = [x | x <- xs, not (x `elem` ys)]  -- Simple subtraction for demonstration


