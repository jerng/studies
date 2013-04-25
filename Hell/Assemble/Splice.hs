{-# LANGUAGE OverloadedStrings #-}

module Hell.Assemble.Splice 
  ( module Hell.Assemble.Primitives
  , module Hell.Conf
  , spliceController
  , spliceTemplate
  , spliceView
  ) where

import Control.Monad (foldM)
import Data.List (nub)
import Data.List.Utils (keysAL)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Hell.Assemble.Primitives
import Hell.Conf

data SliceTag = ImportControllers 
              | ImportViews
              | ListActions
              | ListViews
              deriving (Eq,Show)

data Slice  = Slice ResourceName SliceTag
              deriving (Show)

data Unrendered = Plain T.Text 
                | Exp T.Text
                deriving (Show)

sliceIDsOf :: ResourceName -> [Slice]
sliceIDsOf Server = 
  [ Slice Server ImportControllers 
  , Slice Server ImportViews 
  , Slice Server ListActions 
  , Slice Server ListViews
  ]

spliceController :: FilePath -> IO ResourceNameText 
spliceController c = do
  templateText <- T.readFile $ (fromPath Controllers) ++ c ++ scriptExtension
  return templateText

spliceTemplate :: ResourceName -> IO ResourceNameText 
spliceTemplate module' = do 
  templateText <- T.readFile $ fromPath module'
  let moduleSlices = sliceIDsOf module'
  let buildSlices = \text sliceID -> do
        let Slice _ slice = sliceID
        let tag = (T.pack $ "{-makeHell:"++ show slice ++"-}" )
        builtSlice <- buildSlice sliceID 
        return $ T.replace tag builtSlice text
  foldM buildSlices templateText moduleSlices

-- | This function builds slices of text.

buildSlice :: Slice -> IO T.Text
buildSlice s = case s of 

  Slice Server ImportControllers -> do  
    cs <- controllers
    return $ 
      T.intercalate "\n" $ 
      map ((T.append "import qualified Controllers.").(T.pack)) cs 

  Slice Server ImportViews -> do  
    al <- views
    let cs = keysAL al
        eachC = \c -> T.concat $ map (eachV c) $ fromMaybe 
          (error "buildSlice: controller not found in list of views") $
          lookup c al 
        eachV = \c v ->
          T.concat ["import qualified Views.", T.pack c, ".", T.pack v, "\n"]
    return $ T.concat $ map eachC cs 

 {- This is crufty. It take the unique first words that start at zero
     indent, in eac ./scr/c/* . It needs to be improved to parse out non
     functions, before that. Better still, it should simply not have to read
     the contents of Controller files. So perhaps, Actions should be called
     with the pattern (Xcontroller.Yaction.main) instead.
 -}

  Slice Server ListActions -> do
    actions <- (mapM eachC) =<< controllers 
    return $ T.intercalate "\n  " $ concat $ actions
      where 

        eachC c = do 
          text <- T.readFile $ (fromPath Controllers) ++ c ++ scriptExtension
          return $ map f'' $ nub $ filter (flip notElem ["import"]) $ map f' $ filter f $ T.lines text
          where

            f = \line-> elem (T.take 1 line)  [ "a" , "b" , "c" , "d" , "e"
                , "f" , "g" , "h" , "i" , "j" , "k" , "l" , "m" , "n" , "o"
                , "p" , "q" , "r" , "s" , "t" , "u" , "v" , "w" , "x" , "y"
                , "z" , "_" ]
            f' = \line -> head $ T.words line
            f'' = \a -> T.concat  [ "| (\""
                                  , T.toLower $ T.pack c
                                  , "\", \""
                                  , a,"\") <- r = Just (Controllers."
                                  , T.pack c
                                  , "."
                                  , a
                                  , ")"
                                  ]

  Slice Server ListViews -> do
    al <- views
    let cs = keysAL al
        eachC = \c-> 
          map (eachV c) $ fromMaybe 
          (error "buildSlice: controller not found in list of views") $ 
          lookup c al 
        eachV = \c v->
          let c' = T.pack c
              v' = T.pack v
          in  T.concat  [ "| (\""
                        , T.toLower c'
                        , "\", \""
                        , T.toLower v'
                        , "\") <- r = Just (Views."
                        , c'
                        , "."
                        , v'
                        , ".main)"
                        ]
    return $ T.intercalate "\n  " $ concat $ map eachC cs 

spliceView :: FilePath -> FilePath -> T.Text -> T.Text
spliceView c v unsplicedText = 
  unrenderedToModuleText  
  1 
  ( T.concat  [ "{-# LANGUAGE OverloadedStrings #-}\n\
                \module Views.", T.pack c, ".", T.pack v, " where\n\n\
                \import Hell.Lib\n",
                Hell.Conf.viewImports, "\n" ]
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
unrenderedToModuleText :: Int -> T.Text -> [Unrendered] -> ResourceNameText
unrenderedToModuleText count acc remainingList  

  | True <- count < 1 
  = error "The counter must be greater than 0"

  -- | Final Unrendered

  | [] <- remainingList
  = T.concat 
    [ acc 
    , "main :: Document -> T.Text\n\
      \main doc =\n\
      \  T.concat\n\
      \  [ "
    , T.intercalate "\n  , " $ map 
                          (\x->
                            T.concat 
                            [ "text" 
                            , (T.pack $ show x)
                            , " doc"
                            ]
                          ) 
                          [1..(count-1)] 
    , "\n  ]"
    ]

  -- | Any other Unrendereds:

  | ((Plain text) :remainingList) <- remainingList
  = unrenderedToModuleText 
    (count+1) 
    ( T.concat
      [ acc 
      , "text"
      , T.pack $ show count
      , " :: Document -> T.Text\ntext"
      , T.pack $ show count
      , " _ = \""
      , ( textToHsSyntax text )
      , "\"\n\n"
      ]
    )      
    remainingList

  | ((Exp text) :remainingList) <- remainingList
  = unrenderedToModuleText 
    (count+1) 
    ( T.concat
      [ acc 
      , "text"
      , T.pack $ show count
      , " :: Document -> T.Text\ntext"
      , T.pack $ show count
      , " doc = "
      , T.concat 
          [ " toText (\n" 
          , T.unlines $ map (T.append "    ") $ T.lines text
          , "  )\n\
            \  where\n\
            \    maybeVal label = lookupBsonVal label doc\n"
          -- \   printMaybe maybeVal'  = case maybeVal' of\n\
          --  \     Nothing   -> debugMissingViewData \"Nothing\"\n\
          --   \     Just val  -> toText $ val"
          , "\n\n"
          ]
      ]
    )      
    remainingList

textToUnrendereds :: T.Text -> [Unrendered]
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

textToHsSyntax :: T.Text -> T.Text
textToHsSyntax text = 
  (T.intercalate "\\\n  \\" ) $ T.lines $ 
                    -- Formats multiline text.
  T.replace "\"" "\\\"" text
                    -- Escapes double-quotes
