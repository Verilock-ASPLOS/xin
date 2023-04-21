module Task.VerilockTask where

import Synthesis.Generator (GenCase(..), GenerationConfig(..), generateCases)
import Util.Lit (Lit(..))

import Util.FileUtil (createAndWriteFile, copyFileToDirectory)
import System.FilePath ((</>))
import qualified Data.Text as T

baseGenPath :: FilePath
baseGenPath = "./gen/"

statusResourceFilePath :: FilePath
statusResourceFilePath = "./resource/Status.sv"

channelResourceFilePath :: FilePath
channelResourceFilePath = "./resource/Channel.sv"

fileSuffix :: String
fileSuffix = ".sv"

dfSuffix :: String
dfSuffix = "-d"

seed :: Int
seed = 2023

config :: GenerationConfig
config = GenerationConfig {
  _caseNum = 10,
  _path = baseGenPath,
  _initialSeed = seed,
  _maxLayer = 30,
  _moduleNumPerLayerRange = (2, 100),
  _canstantRange = (0, 100),
  _channelNumPerModuleRange = (1, 1000)
}

caseDirectoryPath :: GenCase -> FilePath
caseDirectoryPath c = basePath c ++ name c

generatedFilePath :: GenCase -> FilePath
generatedFilePath c = caseDirectoryPath c </> (name c ++ fileSuffix)

outputCase :: GenCase -> IO ()
outputCase c = do
  print (states c)
  let p = generatedFilePath c
  let source = lit . ast $ c
  createAndWriteFile p source
  putStrLn ("lines: " ++ (show . length . T.lines $ source))
  let caseDirectory = caseDirectoryPath c
  copyFileToDirectory statusResourceFilePath caseDirectory
  copyFileToDirectory channelResourceFilePath caseDirectory

generateVerilockCases :: IO ()
generateVerilockCases = mapM_ outputCase (generateCases config)