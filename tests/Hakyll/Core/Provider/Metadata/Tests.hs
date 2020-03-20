--------------------------------------------------------------------------------
module Hakyll.Core.Provider.Metadata.Tests
    ( tests
    ) where


--------------------------------------------------------------------------------
import qualified Data.HashMap.Strict           as HMS
import qualified Data.Text                     as T
import qualified Data.Yaml                     as Yaml
import           Hakyll.Core.Metadata
import           Hakyll.Core.Provider.Metadata
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit              (Assertion, assertFailure, (@=?))
import           TestSuite.Util


--------------------------------------------------------------------------------
tests :: TestTree
tests = testGroup "Hakyll.Core.Provider.Metadata.Tests" $
    fromAssertions "page" [testPage01, testPage02, testPage03, testPage04]



--------------------------------------------------------------------------------
testPage01 :: Assertion
testPage01 =
    (meta [("foo", "bar")], "qux\n") `expectRight` parsePage
    "---\n\
    \foo: bar\n\
    \---\n\
    \qux\n"


--------------------------------------------------------------------------------
testPage02 :: Assertion
testPage02 =
    (meta [("description", descr)], "Hello I am dog\n") `expectRight`
    parsePage
    "---\n\
    \description: A long description that would look better if it\n\
    \             spanned multiple lines and was indented\n\
    \---\n\
    \Hello I am dog\n"
  where
    descr :: String
    descr =
        "A long description that would look better if it \
        \spanned multiple lines and was indented"


--------------------------------------------------------------------------------
meta :: Yaml.ToJSON a => [(String, a)] -> Metadata
meta pairs = HMS.fromList [(T.pack k, Yaml.toJSON v) | (k, v) <- pairs]

testPage03 :: Assertion
testPage03 = testParse page
    ([("title", "Next: Org-mode")], "Yes, this is cow\n")
    (unlines [ "#+title: Next: Org-mode"
             , "Yes, this is cow"
             ])


--------------------------------------------------------------------------------
-- | This is useful when the 'Left' side of 'Either' doesn't have an 'Eq'
-- instance.
expectRight :: (Eq b, Show a, Show b) => b -> Either a b -> Assertion
expectRight _        (Left  err) = assertFailure (show err)
expectRight expected (Right res) = expected @=? res

testPage04 :: Assertion
testPage04 = testParse page
    ( [ ("tags", "org, emacs, test")
      , ("date", "2014-05-23")
      ], "\nTwenty five years ago today, the hacker Karl Koch died.\n")
    (unlines [ "#+tags: org, emacs, test"
             , "#+date: 2014-05-23"
             , ""
             , "Twenty five years ago today, the hacker Karl Koch died."
             ])


--------------------------------------------------------------------------------
testParse :: (Eq a, Show a) => Parser a -> a -> String -> Assertion
testParse parser expected input = case P.parse parser "<inline>" input of
    Left err -> error $ show err
    Right x  -> expected @=? x
