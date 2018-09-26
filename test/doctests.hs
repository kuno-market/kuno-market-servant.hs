import Test.DocTest

main :: IO ()
main = doctest ["-isrc",
                "lib/Network/KunoMarket/API/Public.hs",
                "lib/Network/KunoMarket/API/Private.hs",
                "lib/Network/KunoMarket/API/Global.hs"]
