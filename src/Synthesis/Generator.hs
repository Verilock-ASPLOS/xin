{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}

module Synthesis.Generator where

import qualified Data.List.NonEmpty as NE
import Control.Applicative (Applicative (liftA2))
import Control.Lens (makeLenses, (&), (+~), (.~), (^.))
import Control.Monad (replicateM)
import Control.Monad.Trans.State (State, get, put, state, runState)
import Synthesis.Blocks (doubleEqual, ge, gt, identifier, le, lt, mkIncludeChannel, mkLogicVariableDeclaration, mkModule, mkModuleHeader, mkModuleParameters, mkSourceText, mkTopModuleHeader, mkSendStatementConstant, mkReceiveStatement, mkExpressionBinary, mkCondPredict, mkExpressionFromIdentifier, mkNumberExpression, mkChannelInstantiation, mkAlwaysBlock, mkConditionalBlock, mkSeqBlock, mkWhileBlock, mkParBlock, mkCustomModuleInstantiation, identifierOneSpace)
import System.Random (StdGen, mkStdGen, uniform, uniformR)
import SystemVerilog.AST.Expressions.Operators (BinaryOperator)
import SystemVerilog.AST.General.Identifiers (Identifier)
import SystemVerilog.AST.SourceText.SystemVerilogSourceText (Description, ModuleAnsiHeader, SourceText)
import SystemVerilog.AST.BehavioralStatements.Statements (Statement)
import SystemVerilog.AST.BehavioralStatements.ConditionalStatements (CondPredicate)
import SystemVerilog.AST.SourceText.ModuleItems (NonPortModuleItem)
import Data.List (singleton)
import SystemVerilog.AST.Expressions.Expressions (Expression (MkExpressionBinary))

data Direction = In | Out deriving (Eq, Show)

opposite :: Direction -> Direction
opposite In = Out
opposite Out = In

type Layer = Int

type Seed = StdGen

type Index = Int

data GenerationConfig = GenerationConfig
  { _caseNum :: Int,
    _path :: FilePath,
    _initialSeed :: Int,
    _maxLayer :: Layer,
    _moduleNumPerLayerRange :: (Int, Int),
    _canstantRange :: (Int, Int),
    _channelNumPerModuleRange :: (Int, Int)
  }
  deriving (Eq, Show)

data GenCase = GenCase
  { basePath :: FilePath,
    name :: String,
    ast :: SourceText,
    states :: GenStates
  }
  deriving (Eq, Show)

-- seed, module index, channel index, variable index, upper channels
data GenStates = GenStates
  { _config :: GenerationConfig,
    _seed :: Seed,
    _layer :: Layer,
    _moduleI :: Index,
    _channelI :: Index,
    _variableI :: Index,
    _upperChannels :: [(Identifier, Direction)]
  }
  deriving (Eq, Show)

makeLenses ''GenStates
makeLenses ''GenerationConfig

input :: [(Identifier, Direction)] -> [(Identifier, Direction)]
input = filter (\x -> snd x == In)

output :: [(Identifier, Direction)] -> [(Identifier, Direction)]
output = filter (\x -> snd x == Out)

partitionUppers ::
  NE.NonEmpty (Identifier, Direction) ->
  State GenStates ([(Identifier, Direction)], NE.NonEmpty (Identifier, Direction))
partitionUppers uppers = do
  foldl concatPair ([], NE.singleton . NE.head $ uppers) <$> mapM splitUps (NE.tail uppers)
  where
    splitUps c = do
      b <- randomBool
      if b
        then return ([c], [])
        else return ([], [c])
    concatPair (al, bl) (al', bl') = (al ++ al', NE.appendList bl bl')

partitionInnerChannels ::
  NE.NonEmpty Identifier ->
  State GenStates (NE.NonEmpty (Identifier, Direction), NE.NonEmpty (Identifier, Direction))
partitionInnerChannels news = do
  counterPart <$> mapM splitNews news
  where
    splitNews i = do
      b <- randomBool
      if b
        then return (i, In)
        else return (i, Out)
    counterPart l = (l, (\(i,d) -> (i,opposite d)) <$> l)

partitionChannels ::
  NE.NonEmpty (Identifier, Direction) ->
  NE.NonEmpty Identifier ->
  State GenStates (NE.NonEmpty (Identifier, Direction), NE.NonEmpty (Identifier, Direction))
partitionChannels ups news =
  liftA2 concatChannels (partitionUppers ups) (partitionInnerChannels news)
  where
    concatChannels (al, bl) (al', bl') = (NE.appendList al' al, NE.append bl bl')

-- the length of the input is greater than 1
partitionCommunications :: [Statement] -> State GenStates ([Statement], [Statement])
partitionCommunications l = do
  gs <- get
  let (i, ng) = uniformR (1, (length l) - 1) (gs ^. seed)
  put (gs & seed .~ ng)
  return (splitAt i l)

topModuleIdentifier :: Identifier
topModuleIdentifier = identifier "Top"

topModuleHeader :: ModuleAnsiHeader
topModuleHeader = mkTopModuleHeader topModuleIdentifier

caseName :: Index -> String
caseName i = "gen" ++ show i

genModuleIdStr :: State GenStates String
genModuleIdStr = state (\gs -> ("M" ++ show (gs ^. moduleI), gs & moduleI +~ 1))

genModuleId :: State GenStates Identifier
genModuleId = identifierOneSpace <$> genModuleIdStr

genChannelIdStr :: State GenStates String
genChannelIdStr = state (\gs -> ("C" ++ show (gs ^. channelI), gs & channelI +~ 1))

genChannelId :: State GenStates Identifier
genChannelId = identifier <$> genChannelIdStr

genVariableIdStr :: State GenStates String
genVariableIdStr = state (\gs -> ("x" ++ show (gs ^. variableI), gs & variableI +~ 1))

genVariableId :: State GenStates Identifier
genVariableId = identifier <$> genVariableIdStr

bumpLayer :: State GenStates ()
bumpLayer = state (\gs -> ((), gs & layer +~ 1))

randomModuleNumbers :: State GenStates Int
randomModuleNumbers = do
  gs <- get
  let (r, ng) = uniformR (gs ^. config . moduleNumPerLayerRange) (gs ^. seed)
  put (gs & seed .~ ng)
  return r

randomBool :: State GenStates Bool
randomBool = do
  gs <- get
  let (r, ng) = uniform (gs ^. seed)
  put (gs & seed .~ ng)
  return r

randomConstant :: State GenStates Int
randomConstant = do
  gs <- get
  let (r, ng) = uniformR (gs ^. config . canstantRange) (gs ^. seed)
  put (gs & seed .~ ng)
  return r

randomDouble :: State GenStates Double
randomDouble = do
  gs <- get
  let (r, ng) = uniformR (0.0, 1.0) (gs ^. seed)
  put (gs & seed .~ ng)
  return r

pureRandomDouble :: State GenStates Double
pureRandomDouble = do
  gs <- get
  let (r, _) = uniformR (0.0, 1.0) (gs ^. seed)
  return r

randomChannelNumbers :: State GenStates Int
randomChannelNumbers = do
  gs <- get
  let (n, ng) = uniformR (gs ^. config . channelNumPerModuleRange) (gs ^. seed)
  put (gs & seed .~ ng)
  return n

-- according to the value of the channel numbers, it results in a non-empty list
randomChannels :: State GenStates (NE.NonEmpty Identifier)
randomChannels = do
  n <- randomChannelNumbers
  NE.fromList <$> replicateM n genChannelId

randomBinaryOperator :: State GenStates BinaryOperator
randomBinaryOperator = do
  gs <- get
  let (r, ng) = uniformR (1 :: Int, 5) (gs ^. seed)
  put (gs & seed .~ ng)
  return (intToOp r)
  where
    intToOp i = case i of
      1 -> gt
      2 -> ge
      3 -> lt
      4 -> le
      5 -> doubleEqual
      _ -> gt

shouldExpand :: State GenStates Bool
shouldExpand = do
  gs <- get
  let l = gs ^. layer
  let ml = gs ^. config . maxLayer
  if
    | l == 0 -> return True
    | l >= ml -> return False
    | otherwise ->
        let ld = fromIntegral l :: Double
            p = 1.0 / (ld ** 0.5)
         in do
              r <- randomDouble
              return (p >= r)

header ::
  Identifier ->
  [(Identifier, Direction)] ->
  ModuleAnsiHeader
header moduleId uppers = mkModuleHeader moduleId (mkModuleParameters . map fst $ uppers)

stmt :: Identifier -> (Identifier, Direction) -> State GenStates Statement
stmt _ (i, Out) = mkSendStatementConstant i <$> randomConstant
stmt r (i, In)  = return (mkReceiveStatement i r)

randomCondition :: Identifier -> State GenStates CondPredicate
randomCondition i = do
  op <- randomBinaryOperator
  mkCondPredict
    . mkExpressionBinary (mkExpressionFromIdentifier i) op
    . mkNumberExpression
    <$> randomConstant

randomExpression :: Identifier -> State GenStates Expression
randomExpression i = do
  op <- randomBinaryOperator
  MkExpressionBinary
    . mkExpressionBinary (mkExpressionFromIdentifier i) op
    . mkNumberExpression
    <$> randomConstant

-- reverse the sequence of the else block
generateCondition :: Identifier -> [Statement] -> State GenStates [Statement]
generateCondition rv stmts = do
  predicate <- randomCondition rv
  return [mkConditionalBlock predicate (mkSeqBlock stmts) (mkSeqBlock . reverse $ stmts)]

generateWhile :: Identifier -> [Statement] -> State GenStates [Statement]
generateWhile rv stmts = do
  e <- randomExpression rv
  return [mkWhileBlock e (mkSeqBlock stmts)]

generateForkJoin :: [Statement] -> [Statement]
generateForkJoin = singleton . mkParBlock

generateOneLayerLogic :: Identifier -> [Statement] -> State GenStates [Statement]
generateOneLayerLogic rv stmts = do
  gs <- get
  let (r, ng) = if length stmts <= 5
                  then uniformR (1 :: Int, 4) (gs ^. seed)
                  else uniformR (1 :: Int, 3) (gs ^. seed)
  put (gs & seed .~ ng)
  intToControl r
  where
    intToControl i = case i of
      1 -> return stmts -- sequence
      2 -> generateCondition rv stmts -- condition
      3 -> generateWhile rv stmts -- while
      _ -> return (generateForkJoin stmts) -- fork-join

generateTwoLayerLogic :: Identifier -> [Statement] -> State GenStates [Statement]
generateTwoLayerLogic rv stmts = do
  gs <- get
  (c1, c2) <- partitionCommunications stmts
  let (r, ng) = uniformR (1::Int, 3) (gs ^. seed)
  put (gs & seed .~ ng)
  intToControl r c1 c2
  where
    intToControl i lp rp = case i of
      1 -> liftA2 (++) (generateOneLayerLogic rv lp) (generateOneLayerLogic rv rp)
      2 -> do
        inner <- intToControl 1 lp rp
        generateCondition rv inner
      _ -> do
        inner <- intToControl 1 lp rp
        generateWhile rv inner

-- only support at most two nested layers control structure
generateLogic :: Identifier -> [Statement] -> State GenStates [Statement]
generateLogic rv stmts = do
  oneLayer <- randomBool
  if oneLayer || (length stmts <= 1)
    then generateOneLayerLogic rv stmts
    else generateTwoLayerLogic rv stmts

generateAlways :: Identifier -> [Statement] -> State GenStates [NonPortModuleItem]
generateAlways rv stmts = do
  singleton . mkAlwaysBlock <$> generateLogic rv stmts

generateLeafModule ::
  Identifier ->
  [(Identifier, Direction)] ->
  State GenStates [Description]
generateLeafModule moduleId uppers = do
  let h = header moduleId uppers
  rv <- genVariableId
  let vDecl = mkLogicVariableDeclaration rv
  stmts <- mapM (stmt rv) uppers
  items <- generateAlways rv stmts
  return [mkModule h (vDecl : items)]

generateMediateModule ::
  Identifier ->
  NE.NonEmpty (Identifier, Direction) ->
  State GenStates [Description]
generateMediateModule moduleId uppers = do
  (s, c) <- partitionUppers uppers
  news <- randomChannels
  let chanDecl = mkChannelInstantiation <$> NE.toList news
  lModuleId <- genModuleId
  lVar <- genVariableId
  rModuleId <- genModuleId
  rVar <- genVariableId
  (l, r) <- partitionChannels c news
  let lModuleDecl = mkCustomModuleInstantiation lModuleId lVar (fst <$> l)
  let rModuleDecl = mkCustomModuleInstantiation rModuleId rVar (fst <$> r)
  let h = header moduleId (NE.toList uppers)
  rv <- genVariableId
  let vDecl = mkLogicVariableDeclaration rv
  stmts <- mapM (stmt rv) s
  items <- generateAlways rv stmts
  let parentModule = if (length s) == 0
                      then [mkModule h (chanDecl ++ [lModuleDecl, rModuleDecl])]
                      else [mkModule h (vDecl : chanDecl ++ [lModuleDecl, rModuleDecl] ++ items)]
  bumpLayer
  (parentModule ++)
    <$>
    liftA2 (++) (generateSingleModule lModuleId l) (generateSingleModule rModuleId r)

generateSingleModule ::
  Identifier ->
  NE.NonEmpty (Identifier, Direction) ->
  State GenStates [Description]
generateSingleModule moduleId uppers = do
  expand <- shouldExpand
  if expand
    then generateMediateModule moduleId uppers
    else generateLeafModule moduleId (NE.toList uppers)

generateTopModule :: State GenStates [Description]
generateTopModule = do
  moduleId <- genModuleId
  news <- randomChannels
  let chanDecl = mkChannelInstantiation <$> NE.toList news
  lModuleId <- genModuleId
  lVar <- genVariableId
  rModuleId <- genModuleId
  rVar <- genVariableId
  (l, r) <- partitionInnerChannels news
  let lModuleDecl = mkCustomModuleInstantiation lModuleId lVar (fst <$> l)
  let rModuleDecl = mkCustomModuleInstantiation rModuleId rVar (fst <$> r)
  let h = header moduleId []
  bumpLayer
  ([mkModule h (chanDecl ++ [lModuleDecl, rModuleDecl])] ++)
    <$>
    liftA2 (++) (generateSingleModule lModuleId l) (generateSingleModule rModuleId r)

generateMudules :: GenerationConfig -> Seed -> ([Description], GenStates)
generateMudules cfg s =
  let initialState =
        GenStates
          { _config = cfg,
            _seed = s,
            _layer = 0,
            _moduleI = 1,
            _channelI = 1,
            _variableI = 1,
            _upperChannels = []
          }
   in runState generateTopModule initialState

generateSourceText :: GenerationConfig -> Seed -> (SourceText, GenStates)
generateSourceText cfg s =
  let (st, fStates) = generateMudules cfg s
    in (mkSourceText [mkIncludeChannel] st, fStates)

generateCase :: GenerationConfig -> (Index, Seed) -> GenCase
generateCase cfg (i, s) = 
  let (st, fStates) = generateSourceText cfg s
    in GenCase
       (cfg ^. path)
       (caseName i)
       st
       fStates

generateCases :: GenerationConfig -> [GenCase]
generateCases cfg =
  generateCase cfg <$> map (\x -> (x, mkSeed x)) indices
  where
    mkSeed = mkStdGen . ((cfg ^. initialSeed) +)
    indices = [1 .. (cfg ^. caseNum)]