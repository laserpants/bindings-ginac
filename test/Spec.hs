import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Math.Ginac.FFI
import Test.Hspec

extractInt :: Ptr GinacEx -> IO Int
extractInt ptr = do
    n <- ginac_ex_to_int ptr
    ginac_ex_free ptr
    pure (fromIntegral n)

testExNew :: IO Int
testExNew = ginac_ex_new >>= extractInt

testExNewFromInt :: Int -> IO Int
testExNewFromInt m = ginac_ex_new_from_int (fromIntegral m) >>= extractInt

main :: IO ()
main = hspec $ do

    describe "ginac_ex_new" $
      it "should create a GiNaC::ex object representing 0" $ do
        n <- testExNew
        n `shouldBe` 0

    let m = 5

    describe ("ginac_ex_new_from_int " ++ show m) $
      it ("should create a GiNaC::ex object == " ++ show m) $ do
        n <- testExNewFromInt m
        n `shouldBe` m
