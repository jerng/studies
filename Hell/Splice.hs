{-# LANGUAGE OverloadedStrings #-}

module Hell.Splice (
    spliceController
  , spliceTemplate
  , spliceView
) where

import Hell.Lib

spliceController :: FilePath -> IO ResourceNameText 
spliceController c = do
  templateText <- tReadFile $ (fromPath Controllers) ++ c ++ scriptExtension
  return templateText

spliceTemplate :: ResourceName -> IO ResourceNameText 
spliceTemplate module' = do 
  templateText <- tReadFile $ fromPath module'
  let moduleSlices = sliceIDsOf module'
  let buildSlices = \text sliceID -> do
        let Slice _ slice = sliceID
        let tag = (tPack $ "{-makeHell:"++ show slice ++"-}" )
        builtSlice <- buildSlice sliceID 
        return $ tReplace tag builtSlice text
  foldM buildSlices templateText moduleSlices

-- | This function builds slices of text.

buildSlice :: Slice -> IO Text
buildSlice s = case s of 

  Slice Server ImportControllers -> do  
    cs <- controllers
    return $ 
      tIntercalate "\n" $ 
      map ((tAppend "import qualified Controllers.").(tPack)) cs 

  Slice Server ImportViews -> do  
    al <- views
    let cs = keysAL al
        eachC = \c -> tConcat $ map (eachV c) $ fromJust $ lookup c al 
        eachV = \c v ->
          tConcat ["import qualified Views.", tPack c, ".", tPack v, "\n"]
    return $ tConcat $ map eachC cs 

 {- This is crufty. It take the unique first words that start at zero
     indent, in eac ./scr/c/* . It needs to be improved to parse out non
     functions, before that. Better still, it should simply not have to read
     the contents of Controller files. So perhaps, Actions should be called
     with the pattern (Xcontroller.Yaction.main) instead.
 -}

  Slice Server ListActions -> do
    actions <- (mapM eachC) =<< controllers 
    return $ tIntercalate "\n  " $ concat $ actions
      where 

        eachC c = do 
          text <- tReadFile $ (fromPath Controllers) ++ c ++ scriptExtension
          return $ map f'' $ nub $ map f' $ filter f $ tLines text
          where

            f = \line-> elem (tTake 1 line)  [ "a" , "b" , "c" , "d" , "e"
                , "f" , "g" , "h" , "i" , "j" , "k" , "l" , "m" , "n" , "o"
                , "p" , "q" , "r" , "s" , "t" , "u" , "v" , "w" , "x" , "y"
                , "z" , "_" ]
            f' = \line -> head $ tWords line
            f'' = \a -> tConcat  [ "| (\""
                                  , tToLower $ tPack c
                                  , "\", \""
                                  , a,"\") <- r = Just (Controllers."
                                  , tPack c
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
          let c' = tPack c
              v' = tPack v
          in  tConcat  [ "| (\""
                        , tToLower c'
                        , "\", \""
                        , tToLower v'
                        , "\") <- r = Just (Views."
                        , c'
                        , "."
                        , v'
                        , ".main)"
                        ]
    return $ tIntercalate "\n  " $ concat $ map eachC cs 

spliceView :: FilePath -> FilePath -> Text -> Text
spliceView c v unsplicedText = 
  unrenderedToModuleText  
  1 
  ( tPack $  "{-# LANGUAGE OverloadedStrings #-}\n\
              \module Views." ++ c ++ "." ++ v ++ " where\n\n\
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
  = tConcat 
    [ acc 
    , "main :: Report -> Text\n\
      \main report =\n\
      \  tConcat\n\
      \  [ "
    , tIntercalate "\n  , " $ map 
                          (\x->
                            tConcat 
                            [ "text" 
                            , (tPack $ show x)
                            , " report"
                            ]
                          ) 
                          [1..(count-1)] 
    , "\n  ]"
    ]

  -- | Any other Unrendereds:

  | ((Plain text) :remainingList) <- remainingList
  = unrenderedToModuleText 
    (count+1) 
    ( tConcat
      [ acc 
      , "text"
      , tPack $ show count
      , " :: Report -> Text\ntext"
      , tPack $ show count
      , " _ = \""
      , ( textToHsSyntax text )
      , "\"\n\n"
      ]
    )      
    remainingList

  | ((Exp text) :remainingList) <- remainingList
  = unrenderedToModuleText 
    (count+1) 
    ( tConcat
      [ acc 
      , "text"
      , tPack $ show count
      , " :: Report -> Text\ntext"
      , tPack $ show count
      , " report = "
      , tConcat 
          [ " toText (\n" 
          , tUnlines $ map (tAppend "    ") $ tLines text
          , "  )\n\
            \  where\n"
          , viewBsonHelpers
          ,"\n\n"
          ]
      ]
    )      
    remainingList

textToUnrendereds :: Text -> [Unrendered]
textToUnrendereds text =  
  case tSplitOn "<hs>" text of
    [text]
      ->  [Plain text]
    text:remainder
      ->  ( Plain text ) : 
          ( concat 
          $ map
            ( \t-> case tSplitOn "</hs>" t of
                [withinTag, withoutTag]
                  ->  [Exp withinTag, Plain withoutTag]
                [_]
                  ->  error "\n<hs> TAG WITHOUT </hs> TAG"
                _
                  ->  error "SUCCESSIVE </hs> TAGS (Nesting not yet supported.)"
            )
            remainder )

textToHsSyntax :: Text -> Text
textToHsSyntax text = 
  (tIntercalate "\\\n  \\" ) $ tLines $ 
                    -- Formats multiline text.
  tReplace "\"" "\\\"" text
                    -- Escapes double-quotes
