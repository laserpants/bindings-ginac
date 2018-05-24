import Foreign.C.String
import Foreign.Marshal.Alloc
import Math.Ginac.FFI
import Test.Hspec

testExNew :: IO Int
testExNew = do
    ptr <- ginac_ex_new
    n <- ginac_ex_to_int ptr
    ginac_ex_free ptr
    pure n

main :: IO ()
main = hspec $

    describe "ginac_ex_new" $
      it "should return a GiNaC::ex holding a 0" $ do
        n <- testExNew
        n `shouldBe` 0
