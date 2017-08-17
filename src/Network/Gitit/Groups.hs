{-# LANGUAGE OverloadedStrings, RecursiveDo, FlexibleInstances, ScopedTypeVariables #-}
import Data.List.Split (splitOn)
import Text.Parsers.Frisby
import Debug.Trace

type UserName = String
type GroupName = String

data GroupDSL = Union [GroupDSL]
              | Negate GroupDSL
              | Only GroupName
              deriving (Show)

data GroupL = Blank
            | Comment String
            | Group GroupName GroupDSL
            deriving (Show)

-- Parsing
groupDSL :: PM s (P s GroupL)
groupDSL = mdo
  groupOnly <- newRule $ Only <$> groupName
  groupNegate <- newRule $ Negate <$> (char '!' ->> possiblegroupA)
  groupUnionA <- newRule $ Union <$> interleave1 spaces possiblegroupB
  groupIntersectionA <- newRule $ Negate <$> (Union <$> interleave1 (spaces ->> char '^' <<- spaces) (Negate <$> possiblegroupB))
  basicgroupA <- newRule $ groupNegate // groupUnionA // groupIntersectionA // groupOnly
  possiblegroupA <- newRule $ spaces ->> optional (char '(' ->> spaces) ->> basicgroupA <<- optional (spaces <<- char ')') <<- spaces
  possiblegroupB <- newRule $ groupNegate // (char '(' ->> spaces ->> basicgroupA <<- spaces <<- char ')') // groupOnly

  blankLine <- newRule $ pure Blank <<- spaces
  commentLine <- newRule $ Comment <$> (char '#' ->> many anyChar)
  groupLine <- newRule $ Group <$> (groupName <<- char ':' <<- spaces) <*> possiblegroupA
  return $ bof ->> (commentLine // groupLine // blankLine) <<- eof
  where interleave1 sep par = (:) <$> par <*> many1 (sep ->> par)
        groupName = spaces ->> (many1 $ oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.-_") <<- spaces
        spaces = many $ oneOf " \t"

parseGroupsFromString :: String -> [GroupL]
parseGroupsFromString str = map (runPeg groupDSL) $ splitOn "\n" str

