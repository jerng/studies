{-# LANGUAGE OverloadedStrings #-}

module Hell.Splice (
    spliceTemplate
  , spliceView
) where

import Control.Monad (foldM)
import qualified Data.Text as T 
import qualified Data.Text.IO as T
import Hell.Conf
import Hell.Types

spliceTemplate :: ResourceName -> IO ResourceNameText 
spliceTemplate module' = do 
  templateText <- T.readFile $ templateFromPath module'
  let moduleSliceIDs = sliceIDsOf module'
  let buildSlices = \text sliceID -> do
        let SliceID _ slice = sliceID
        let tag = (T.pack $ "{-assemble:"++ show slice ++"-}" )
        builtSlice <- buildSlice sliceID 
        return $ T.replace tag builtSlice text
  foldM buildSlices templateText moduleSliceIDs

-- | This function builds slices of text.
buildSlice :: SliceID -> IO Text
buildSlice s = case s of 
  SliceID Server ImportControllers -> do 
    cs <- controllers
    return 
      $ T.intercalate "\n" 
      $ map 
        ((T.append "import qualified Controllers.").(T.pack)) 
        cs 
  SliceID Server ImportViews -> do 
    return "import qualified Views.Default.Index"

    -- TODO: SOFT CODE THIS

--    vs <- views
--    return 
--      $ T.intercalate "\n" 
--      $ map 
--        ((T.append "import qualified Views.").(T.pack)) 
--        vs

spliceView :: Text -> Text
spliceView unsplicedText =  unrenderedToModuleText   
                            0 
                            T.empty 
                            $ textToUnrendereds unsplicedText
unrenderedToModuleText 
  ::  Int ->            -- counter
      Text ->           -- accumulator
      [Unrendered] ->   -- List of (Unrendered)s
      ResourceNameText      
unrenderedToModuleText 0 _  remainingList  = 
  -- | First Unrendered
  unrenderedToModuleText 1 "{-# LANGUAGE OverloadedStrings #-}\n\
    \module Views.Default.Index where\
    \\nimport qualified Data.Text as T\
    \\nimport Hell.Lib\
    \\n--import SomeResourceName2" remainingList

    -- TODO: SOFT CODE THIS

unrenderedToModuleText count acc [] = 
  -- | Final Unrendered
  T.concat 
    [ acc 
    , "\nmain :: Reaction -> Text\
      \\nmain reaction = T.concat ["
    , ( T.intercalate "," $ 
          map 
          (\x->
            T.concat 
            [ "text" 
            , (T.pack $ show x)
            , " reaction"
            ]) 
          [1..(count-1)] )
    , "]"
    ]
unrenderedToModuleText count acc (unrendered:remainingList) = 
-- | Any other Unrendereds
  case unrendered of
    (Plain text)
      ->  unrenderedToModuleText 
          (count+1) 
          ( T.concat
            [ acc 
            , "\n\ntext"
            , T.pack $ show count
            , " :: Reaction -> Text\ntext"
            , T.pack $ show count
            , " _ = \""
            , ( textToHsSyntax text )
            , "\"\n"
            ]
          )      
          remainingList
    (Exp text)
      
      {- IMPROVE:  currently, if Text has text in parentheses, then Text is
      processed as an expression. Otherwise, Text is processed as the key to
      (reaction) (Hell.Types.Reaction). This is a rather fragile semantic, in
      terms of language design. Perhaps we should instead, have <hs/> for
      expressions and <hsv/> for Reaction keys.
      -}

      ->  unrenderedToModuleText 
          (count+1) 
          ( T.concat
            [ acc 
            , "\n\ntext"
            , T.pack $ show count
            , " :: Reaction -> Text\ntext"
            , T.pack $ show count
            , " (Reaction status route textMap) = "
            , case T.head text of
                '(' ->  T.append
                        "T.pack $ show $ "
                        text 
                _   ->  T.concat 
                        [ "fromJust $ lookup \""
                        , text
                        , "\" textMap\n"
                        ]
            ]
          )      
          remainingList

textToUnrendereds :: Text -> [Unrendered]
textToUnrendereds text =  
  case T.splitOn "<hs>" text of
    [text]
      ->  [Plain text]
    text:remainder
      ->  ( Plain text ) : 
          ( concat 
          $ map
            ( \t-> case T.splitOn "</hs>" t of
                [withinTag, withoutTag]
                  ->  [Exp withinTag, Plain withoutTag]
                [_]
                  ->  error "\n<hs> TAG WITHOUT </hs> TAG"
                _
                  ->  error "SUCCESSIVE </hs> TAGS (Nesting not yet supported.)"
            )
            remainder )

textToHsSyntax :: Text -> Text
textToHsSyntax text = T.replace "\n" "\\\n\\" $ T.replace "\"" "\\\"" text
                    -- Formats multiline text.
                    -- Escapes double-quotes
