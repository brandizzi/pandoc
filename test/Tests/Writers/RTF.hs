module Tests.Writers.RTF (tests) where

import Data.Text (unpack)
import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

rtf :: (ToPandoc a) => a -> String
rtf = unpack . purely (writeRTF def{ writerWrapText = WrapNone }) . toPandoc

tests :: [TestTree]
tests = [ testGroup "Paragraph"
          [ test rtf "Bare text" $
              text "foo" =?>
                 "{\\pard \\ql \\f0 \\sa0 \\li0 \\fi0 foo\\par}"
          , test rtf "paragraph" $
               (para $ text "foo") =?>
                 "{\\pard \\ql \\f0 \\sa180 \\li0 \\fi0 foo\\par}"
          ],
          testGroup "Lists"
          [ test rtf "use \\list" $
            (bulletList $ [ para $ text "foo" ]) =?>
                "\\list foo"
          ]
        ]
