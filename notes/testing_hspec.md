# Specification Testing with HSpec

[HSpec](https://hspec.github.io/) is a testing library that offers an expressive way to write tests.

Here's a quick example of how an HSpec test might look like, copied from the above link:

```haskell
main :: IO ()
main = hspec $ do
  describe "Prelude.read" $ do
    it "can parse integers" $ do
      read "10" `shouldBe` (10 :: Int)

    it "can parse floating-point numbers" $ do
      read "2.5" `shouldBe` (2.5 :: Float)
```

TODO
