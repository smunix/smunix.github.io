#!/usr/pd/bin/runhugs 
--
-- This file defines a command
--      quickCheck <options> <files>
-- which invokes quickCheck on all properties defined in the files given as
-- arguments, by generating an input script for hugs and then invoking it.
-- quickCheck recognises the options
--      +names     print the name of each property before checking it
--      -names     do not print property names (the default)
-- Other options (beginning with + or -) are passed unchanged to hugs.
--
-- Change the first line of this file to the location of runhugs on your 
-- system.
-- Make the file executable.
--

import System
import List

main :: IO ()
main = do as<-getArgs
          sequence_ (map (process (filter isOption as)) 
	                 (filter (not.isOption) as))

process opts file =
       let (namesOpt,opts') = getOption "names" "-names" opts in
       do xs<-readFile file
          let names = nub$ filter ("prop_" `isPrefixOf`)
	                (map (fst.head.lex) (lines xs))
          if null names then
	      putStr (file++": no properties to check\n")
	    else do writeFile "hugsin"$
	              unlines ((":l "++file):
	                       [(if namesOpt=="+names" then 
			           "putStr \""++p++": \" >> "
				 else "") ++
				"quickCheck "++p | p<-names])
	            system ("hugs "++options opts'++" <hugsin")
	            return ()

isOption xs = head xs `elem` "-+"

options opts = unwords ["\""++opt++"\"" | opt<-opts]

getOption name def opts = 
  let opt = head [opt | opt<-opts++[def], isPrefixOf name (drop 1 opt)] in
    (opt, filter (/=opt) opts)
