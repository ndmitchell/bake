import Test.Tasty
import Test.Tasty.Hspec
import Bake.Test.GCSpec

main = testSpec "Bake" gcSpec >>= defaultMain
