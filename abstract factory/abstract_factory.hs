-- Framework

class PowerSystem system where
    label :: system -> String

data Factory system = Factory { providerCreator :: () -> system,
                                connectorCreator :: () -> system,
                                consumerCreator :: Int -> system }

systemCreatorTemplate :: (PowerSystem system) => Factory system -> Int -> Int -> [system]
systemCreatorTemplate factory length power =
    let provider = (providerCreator factory)()
        connectors = replicate length ((connectorCreator factory)())
        consumer = (consumerCreator factory) power
        in
        [provider] ++ connectors ++ [provider]

printSystem :: (PowerSystem a) => [a] -> String
printSystem system =
    concat $ map label system


-- Stream system

data SteamSystem = Boiler | Pipe | Engine Int

instance PowerSystem SteamSystem where
    label Boiler = "boiler"
    label Pipe = "-pipe-"
    label (Engine power) = "engine (" ++ show power ++ ")"

steamFactory :: Factory SteamSystem
steamFactory = Factory { providerCreator = \() -> Boiler,
                         connectorCreator = \() -> Pipe,
                         consumerCreator = \power -> Engine power }


-- Electric system

data ElectricSystem = Generator | Wire | Motor Int

instance PowerSystem ElectricSystem where
    label Generator = "generator"
    label Wire = "-wire-"
    label (Motor power) = "motor (" ++ show power ++ ")"

electricFactory :: Factory ElectricSystem
electricFactory = Factory { providerCreator = \() -> Generator,
                            connectorCreator = \() -> Wire,
                            consumerCreator = \power -> Motor power }
    

-- Tester

main :: IO ()
main = do
    let steamSystemCreator = systemCreatorTemplate steamFactory
    let steamSystem = steamSystemCreator 4 100

    let electricSystemCreator = systemCreatorTemplate electricFactory
    let electricSystem = electricSystemCreator 4 100

    putStrLn $ printSystem steamSystem
    putStrLn $ printSystem electricSystem
