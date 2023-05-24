module Commands.Delta where

import qualified Data.Text as DT
import Text.Diff.Parse (parseDiff)
import Text.Diff.Parse.Types

import qualified Data.Text.IO as Tio
import qualified Options.Runtime as Rto


deltaCmd :: FilePath -> Rto.RunOptions -> IO ()
deltaCmd fileIn rtOpts = do
  putStrLn "@[deltaCmd] starting."
  inText <- Tio.readFile fileIn
  putStrLn $ "@[deltaCmd] read: " <> (show $ DT.length inText) <> " chars."
  let
    eiDiffs = parseDiff inText
  case eiDiffs of
    Left err -> putStrLn $ "@[deltaCmd] parseDiff err: " <> show err
    Right diffs -> mapM_ describeDiff diffs

describeDiff :: FileDelta -> IO ()
describeDiff diff =
  let
    content = case diff.fileDeltaContent of
      Binary -> "binary"
      Hunks h -> (show $ length h) <> " hunks."
  in do
  putStrLn $ "@[deltaCmd] fileDeltaStatus: " <> show diff.fileDeltaStatus
  putStrLn $ ".. fileDeltaSourceFile: " <> DT.unpack diff.fileDeltaSourceFile
  putStrLn $ ".. fileDeltaDestFile: " <> DT.unpack diff.fileDeltaDestFile
  putStrLn $ ".. content: " <> content


{-
- 3 types de fichers:
  Created: nouveau fichier
    - comment une liste de modifs,
    - tout le contenu doit etre utilise pour migration
  Deleted:
    - efface tout ce qui a ete fait avant pour le meme fichier
    - tout le contenu cree doit passer par 'drop X'
  Modified:
    - doit utilise une version precedente du contenu du fichier
    - les +/- sont soit une sequence complete d'ajout ou d'elimination (create table, drop table, pas de reference a du precendent),
      soit des morceaux de contenu existant avec 'alter table X add/drop/rename/... '

 ? comment aller chercher le contenu d'un fichier dont on a des +/-
 ? comment comprendre le type dd SQL statement
 ? quelles sont les regles de alter X

* construit toutes les differences entre <start> et <end>: git diff <hash-start> .. <hash-end>
* trouve le hash qui est avant un autre: git show --quiet <hash>^1
 =>
   commit 28e2c75fb2dc1400295546e56bc8fecee3484b64
   Author: Hugo DesRosiers <hugo.desrosiers@boardingcities.com>
   Date:   Tue Apr 25 16:18:20 2023 +0400

       Added AI context and messaging support tables, and added bootstrap data to reproduce the asset library for adev and demo.

* extrait un fichier donne a un commit donne sur stdout: git show <hash>:<path>
-}
