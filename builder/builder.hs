--
-- BUILDER
-- 
-- Separate the construction of a complex object from its representation so that the same
-- construction process can create different representations. 
-- 

--
-- Example
-- 
-- This example demonstrates how generic builder can be used to create milkshakes for advertisement
-- and production purposes. The same ingredients and building process are used to build the two
-- different presentations.
--


-- Generic operator to pipe expressions and functions together
(|>) a f = f a


-- Define ingredients to be used by the framework
data Ingredient = Sugar | Stevia | Mango | Banana | Orange | Water | Cream | None deriving (Show, Eq)

-- Define generic builder template    
type SweetenerFunction state = Ingredient -> state -> state
type FlavorFunction state = Ingredient -> state -> state
type FinalizeFunction state product = state -> product

builderTemplate :: s -> SweetenerFunction s -> FlavorFunction s -> FinalizeFunction s p -> [Ingredient] -> p
builderTemplate seed sweetenerFunction flavorFunction finalizeFunction ingredients = do
    let acceptedFlavors = [Orange, Mango, Banana]
    let acceptedSweeteners = [Sugar, Stevia]

    let folder state ingredient 
                | elem ingredient acceptedFlavors = flavorFunction ingredient state
                | elem ingredient acceptedSweeteners = sweetenerFunction ingredient state
                | otherwise = state    

    ingredients |> foldl folder seed |> finalizeFunction


-- Define Milkshake type and functions to build parts of it
data Milkshake = Milkshake { name :: String, flavors :: [Ingredient], sweetener :: Ingredient } deriving (Show)

milkshakeSweetener :: SweetenerFunction Milkshake
milkshakeSweetener sweetener milkshake =
    milkshake { sweetener = sweetener }

milkshakeFlavor :: FlavorFunction Milkshake
milkshakeFlavor flavor milkshake = do
    let flavors' = flavors milkshake
    milkshake { flavors = flavor:flavors' }

milkshakeFinalize :: FinalizeFunction Milkshake Milkshake
milkshakeFinalize milkshake = do
    let name' = (flavors milkshake |> map show |> map (++ " ")) |> concat
    milkshake { name = name' ++ "Milkshake" }


-- Define Advertisement type and functions to build parts of it
type Advertisement = String
type AdvertisementState = String

adSweetener :: SweetenerFunction AdvertisementState
adSweetener sweetener ad
    | sweetener == Sugar = "Tasty" ++ ad
    | otherwise = "Healthy" ++ ad

adFlavor :: FlavorFunction AdvertisementState
adFlavor flavor ad =
    ad ++ " " ++ (show flavor)

adBuild :: FinalizeFunction AdvertisementState Advertisement
adBuild ad = do
    ad ++ " Milkshake!"


-- Test advertisement and milkshake builders
main :: IO ()
main = do
    let ingredients = [Cream, Mango, Banana, Stevia]

    let adBuilder = builderTemplate "" adSweetener adFlavor adBuild
    ingredients |> adBuilder |> (putStrLn . show)

    let milkshakeBuilder = builderTemplate (Milkshake "" [] None) milkshakeSweetener milkshakeFlavor milkshakeFinalize
    ingredients |> milkshakeBuilder |> (putStrLn . show)
