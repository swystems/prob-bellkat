{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Bifunctor
import           Data.Default
import           Data.Text
import           Data.Void
import           GHC.Generics
import qualified Options.Applicative                   as OA
import           System.Exit
import           System.IO
import           Text.Megaparsec                       hiding (parseTest)

import           BellKAT.ActionEmbeddings
import           BellKAT.Definitions
import           BellKAT.Implementations.Configuration
import           BellKAT.Parser
import           BellKAT.Parser.SurfacePolicy
import           BellKAT.Prelude.Common                hiding (main)
import qualified BellKAT.ProbabilisticPrelude          as PP
import qualified BellKAT.QuantumPrelude                as QP

data KCEOMode = KCEOModeProbabilistic | KCEOModeQuantum

data ProbabilityMode = PMDouble | PMRational

data KatCLIExtraOpts = KCEO
    { kceoOpts            :: KatCLIOpts
    , kceoNetworkFilepath :: FilePath
    , kceoMode            :: KCEOMode
    , kceoProbabilityMode :: ProbabilityMode
    , kceoTargetEvent     :: String
    , kceoPolicyFilepath  :: FilePath
    }

data JsonQKatConfig t = JKQC
    { jkqcNetworkCapacity :: Maybe (NetworkCapacity t)
    , jkqcCutoff          :: Maybe Int
    , jkqcPAC             :: ProbabilisticActionConfiguration
    } deriving stock (Generic)

instance (Ord t, Default t, FromJSON t) => FromJSON (JsonQKatConfig t) where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

kceoParser :: OA.Parser KatCLIExtraOpts
kceoParser = KCEO
    <$> katCLIOptsParser
    <*> OA.strOption (OA.long "network" <> OA.short 'n' <> OA.help "Network's JSON file")
    <*> (OA.flag' KCEOModeProbabilistic (OA.long "probabilitic") <|> OA.flag' KCEOModeQuantum (OA.long "quantum"))
    <*> OA.flag PMDouble PMRational (OA.long "rational")
    <*> OA.strOption (OA.long "policy" <> OA.short 'p' <> OA.help "QKAT policy file")
    <*> OA.strOption (OA.long "event" <> OA.short 'e' <> OA.help "Event description")

main :: IO ()
main = do
    opts <- runKatParser "QKAT" kceoParser
    surfacePolicy <- loadSurfacePolicy $ kceoPolicyFilepath opts
    case kceoMode opts of
      KCEOModeQuantum -> do
        nw <- loadNetworkConfiguration $ kceoNetworkFilepath opts
        let nb = QP.NetworkBounds (jkqcNetworkCapacity nw) (jkqcCutoff nw)
        targetEvent <- evaluateBooleanTest <$>
            handleParserError (parseTest "" $ kceoTargetEvent opts)
        let policy = bimap evaluateBooleanTest (\a -> TaggedAction def a def)
                        $ desugarSurfacePolicy surfacePolicy
        case kceoProbabilityMode opts of
          PMDouble -> QP.qbkatMainWithOptsD
                (kceoOpts opts) (jkqcPAC nw) nb targetEvent policy []
          PMRational -> QP.qbkatMainWithOpts
                (kceoOpts opts) (jkqcPAC nw) nb targetEvent policy []
      KCEOModeProbabilistic -> do
        nw <- loadNetworkConfiguration $ kceoNetworkFilepath opts
        targetEvent <- evaluateBooleanTest <$>
                handleParserError (parseTest "" $ kceoTargetEvent opts)
        let policy = bimap (evaluateBooleanTest . fmap (const def)) (\a -> TaggedAction def a def) $ desugarSurfacePolicy surfacePolicy
        case kceoProbabilityMode opts of
          PMDouble -> PP.pbkatMainWithOptsD
                (kceoOpts opts) (jkqcPAC nw) (jkqcNetworkCapacity nw) targetEvent policy
          PMRational -> PP.pbkatMainWithOpts
                (kceoOpts opts) (jkqcPAC nw) (jkqcNetworkCapacity nw) targetEvent policy

loadSurfacePolicy :: FilePath -> IO (SurfacePolicy () Action)
loadSurfacePolicy path =
    readFile path >>= handleParserError . parseSurfacePolicy path

handleParserError :: Either (ParseErrorBundle Text Void) a -> IO a
handleParserError (Left err) = hPutStrLn stderr (errorBundlePretty err) >> exitFailure
handleParserError (Right x) = pure x

loadNetworkConfiguration :: (Ord t, Default t, FromJSON t) => FilePath -> IO (JsonQKatConfig t)
loadNetworkConfiguration path = eitherDecodeFileStrict path >>= \case
        Left err -> do
            hPutStrLn stderr $
                "Can't parse network configuration " <> path <> ": " <> err
            exitFailure
        Right nw -> pure nw
