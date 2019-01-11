
import Test.Tasty

import TestEntity
import TestFile
import TestGraph

main :: IO ()
main = defaultMain allTests

allTests = testGroup " Unit tests " [test_Entity, test_File, test_Graph]
