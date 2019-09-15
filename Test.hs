import Names
import Ents(mkEnt)
import ModSysAST
import Modules
import ModSysSem
import Util.Set

import Control.Monad(zipWithM_)
import System.Exit (exitFailure)

entF x  = mkEnt x "f" emptySet 


ex1 = [
  Module 
  { modName     = ModName "A"
  , modExpList  = Nothing
  , modImports  = []
  , modDefines  = mkSet [(Name "f", entF "A") ]
  },

  Module 
  { modName     = ModName "B"
  , modExpList  = Just [EntExp (Ent (mkUnqual (Name "f")) Nothing)]
  , modImports  = 
    [ Import 
      { impSource     = ModName "A"
      , impQualified  = False
      , impAs         = ModName "A"
      , impHiding     = True
      , impList       = [] 
      }
    ]
  , modDefines  = emptySet
  },

  Module 
  { modName     = ModName "C"
  , modExpList  = Just [EntExp (Ent (mkUnqual (Name "f")) Nothing)]
  , modImports  = 
    [ Import 
      { impSource     = ModName "A"
      , impQualified  = False
      , impAs         = ModName "A"
      , impHiding     = True
      , impList       = [] 
      }
    ]
  , modDefines  = emptySet
  },

  Module 
  { modName     = ModName "D"
  , modExpList  = Just [ModuleExp (ModName "B")]
  , modImports  = 
    [ Import 
      { impSource     = ModName "B"
      , impQualified  = True
      , impAs         = ModName "B"
      , impHiding     = True
      , impList       = [] 
      }
      , Import 
      { impSource     = ModName "C"
      , impQualified  = False
      , impAs         = ModName "C"
      , impHiding     = True
      , impList       = [] 
      }
    ]
  , modDefines  = emptySet
  }
  ]


ex2 = [
  Module 
  { modName     = ModName "A"
  , modExpList  = Just [EntExp (Ent (mkUnqual (Name "f")) Nothing)]
  , modImports  = 
    [ Import 
      { impSource     = ModName "B"
      , impQualified  = False
      , impAs         = ModName "B"
      , impHiding     = True
      , impList       = [] 
      }
    ]
  , modDefines  = emptySet
  },

  Module 
  { modName     = ModName "B"
  , modExpList  = Just [ModuleExp (ModName "A")]
  , modImports  = 
    [ Import 
      { impSource     = ModName "A"
      , impQualified  = False
      , impAs         = ModName "A"
      , impHiding     = True
      , impList       = [] 
      }
    ]
  , modDefines  = mkSet [(Name "f", entF "B") ]
  }
  ]


ex3 = [
  Module 
  { modName     = ModName "A"
  , modExpList  = Just [EntExp (Ent (mkQual (ModName "B") (Name "f")) Nothing)]
  , modImports  = 
    [ Import 
      { impSource     = ModName "B"
      , impQualified  = True
      , impAs         = ModName "B"
      , impHiding     = True
      , impList       = [] 
      },

    Import 
      { impSource     = ModName "A"
      , impQualified  = False
      , impAs         = ModName "B"
      , impHiding     = True
      , impList       = [] 
      }
 
    ]
  , modDefines  = mkSet [(Name "f", entF "A") ]
  },

  Module 
  { modName     = ModName "B"
  , modExpList  = Nothing
  , modImports  = [] 
  , modDefines  = mkSet [(Name "f", entF "B") ]
  }
  ]



test mods = either (zipWithM_ printErr mods) 
                   (zipWithM_ printRels mods) 
                   (mProgram mods)
  where
  printErr m es
    | null es = return ()
    | otherwise = putStrLn ("Error(s) in module " ++ show (modName m) ++ ": " 
                  ++ show es) >> exitFailure
  printRels m (ins,outs) = 
    do putStrLn ("in scope of module " ++ show (modName m) ++ ":")
       printRel ins
       putStrLn ("exports of module " ++ show (modName m) ++ ":") 
       printRel outs
       putStrLn ""
      

printRel r = sequence_ [ putStrLn $ show x ++ " |-> " ++ show y 
                              | (x,y) <- setToList r ]

main = do putStrLn $ "test 1 " ++ replicate 50 '-'
          test ex1 
          putStrLn $ "test 2 " ++ replicate 50 '-'
          test ex2 
          putStrLn $ "test 3 " ++ replicate 50 '-'
          test ex3


