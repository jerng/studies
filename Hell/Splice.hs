{-# LANGUAGE OverloadedStrings #-}

module Hell.Splice (
    spliceController
  , spliceTemplate
  , spliceView
) where

import qualified Data.Text as T 
import qualified Data.Text.IO as T
import Hell.Lib

spliceController :: FilePath -> IO ResourceNameText 
spliceController c = do
  templateText <- T.readFile $ (fromPath Controllers) ++ c ++ scriptExtension
  return templateText

spliceTemplate :: ResourceName -> IO ResourceNameText 
spliceTemplate module' = do 
  templateText <- T.readFile $ templateFromPath module'
  let moduleSlices = sliceIDsOf module'
  let buildSlices = \text sliceID -> do
        let Slice _ slice = sliceID
        let tag = (T.pack $ "{-assemble:"++ show slice ++"-}" )
        builtSlice <- buildSlice sliceID 
        return $ T.replace tag builtSlice text
  foldM buildSlices templateText moduleSlices

-- | This function builds slices of text.

buildSlice :: Slice -> IO Text
buildSlice s = case s of 

  Slice Server ImportControllers -> do  
    cs <- controllers
    return $ 
      T.intercalate "\n" $ 
      map ((T.append "import qualified Controllers.").(T.pack)) cs 

  Slice Server ImportViews -> do  
    al <- views
    let cs = keysAL al
        eachC = \c -> T.concat $ map (eachV c) $ fromJust $ lookup c al 
        eachV = \c v ->
          T.concat ["import qualified Views.", T.pack c, ".", T.pack v, "\n"]
    return $ T.concat $ map eachC cs 

  Slice Server ListActions -> do
    actions <- (mapM eachC) =<< controllers 
    return $ T.intercalate "\n  , " $ concat $ actions

      where 
        eachC c = do 
          text <- T.readFile $ (fromPath Controllers) ++ c ++ scriptExtension
          return $ map f'' $ nub $ map f' $ filter f $ T.lines text

          where
            f = \line-> 
              case (T.take 1 line) of "" -> False; " " -> False; _ -> True
            f' = \line -> head $ T.words line
            f'' = \a -> T.concat  [ "((\""
                                  , T.toLower $ T.pack c
                                  , "\", \""
                                  , a,"\"), Controllers."
                                  , T.pack c
                                  , "."
                                  , a
                                  , ")"
                                  ]

  Slice Server ListViews -> do
    al <- views
    let cs = keysAL al
        eachC = \c-> 
          map (eachV c) $ fromJust $ lookup c al 
        eachV = \c v->
          let c' = T.pack c
              v' = T.pack v
          in  T.concat  [ "((\""
                        , T.toLower c'
                        , "\", \""
                        , T.toLower v'
                        , "\"), Views."
                        , c'
                        , "."
                        , v'
                        , ".main)"
                        ]
    return $ T.intercalate "\n  , " $ concat $ map eachC cs 

spliceView :: FilePath -> FilePath -> Text -> Text
spliceView c v unsplicedText = 
  unrenderedToModuleText  
  1 
  ( T.pack $  "{-# LANGUAGE OverloadedStrings #-}\n\
              \module Views." ++ c ++ "." ++ v ++ " where\n\n\
              \import qualified Data.Text as T\n\
              \import Hell.Lib\n\n" 
  ) 
  ( textToUnrendereds unsplicedText )

-- | The approach taken creates a unique function (textN) for each unrendered
-- slice. (main) then consists of a magazine that calls all these functions
-- and then concatenates them. This makes it the rendered module easy to read,
-- I hope. 
--
-- However, if slices were rendered directly into the (main) magazine 
-- that gets concatenated, a lot of boilerplate would be reduced as helper
-- functions inside each (Exp text) slice would only have to be rendered once.
-- Basically, passing of scope might (???) be simpler, and code might run
-- faster, but code would be harder to read. Unless of course, GHC is already
-- solving all these problems for us behind the scenes.
--
-- TODO: test and decide if a better approach is feasible.
unrenderedToModuleText :: Int -> Text -> [Unrendered] -> ResourceNameText
unrenderedToModuleText count acc remainingList  

  | True <- count < 1 
  = error "The counter must be greater than 0"

  -- | Final Unrendered

  | [] <- remainingList
  = T.concat 
    [ acc 
    , "main :: Report -> Text\n\
      \main reaction =\n\
      \  T.concat\n\
      \  [ "
    , T.intercalate "\n  , " $ map 
                          (\x->
                            T.concat 
                            [ "text" 
                            , (T.pack $ show x)
                            , " reaction"
                            ]
                          ) 
                          [1..(count-1)] 
    , "\n  ]\n\n\
      \main' reaction = do\
      \  T.empty"
    
    ]

  -- | Any other Unrendereds:

  | ((Plain text) :remainingList) <- remainingList
  = unrenderedToModuleText 
    (count+1) 
    ( T.concat
      [ acc 
      , "\n\ntext"
      , T.pack $ show count
      , " :: Report -> Text\ntext"
      , T.pack $ show count
      , " _ = \""
      , ( textToHsSyntax text )
      , "\"\n"
      ]
    )      
    remainingList

  | ((Exp text) :remainingList) <- remainingList
  = unrenderedToModuleText 
    (count+1) 
    ( T.concat
      [ acc 
      , "\n\ntext"
      , T.pack $ show count
      , " :: Report -> Text\ntext"
      , T.pack $ show count
      , " (Report status route textMap) = "
      , T.concat 
          [ " T.pack $ show $ " , text
          , "\n\
            \  where\n"
          , viewDictionaryHelpers
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
textToHsSyntax text = (T.intercalate "\\\n\\" ) $ T.lines $ T.replace "\"" "\\\"" text
                    -- Formats multiline text.
                    -- Escapes double-quotes
