module CCCEdict
  ( Definition (..),
    parseEntry,
    parseLines,
  )
where

import Control.Category ((>>>))
import Control.Monad (unless, when)
import Control.Monad.State.Class (MonadState (get), gets, put, state)
import Control.Monad.State.Strict (StateT, evalStateT)
import Control.Monad.Trans (lift)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Sq
import Data.Text qualified as T
import Streaming.Prelude qualified as S

data Definition = Definition
  { defSimp :: !T.Text,
    defTrad :: !T.Text,
    defPinyin :: !(Sq.Seq T.Text),
    defEng :: !(Sq.Seq T.Text)
  }
  deriving (Show)

dropCR :: T.Text -> T.Text
dropCR txt = fromMaybe txt $ T.stripSuffix "\r" txt

parseLines :: (MonadFail m) => S.Stream (S.Of T.Text) m () -> S.Stream (S.Of Definition) m ()
parseLines =
  S.map dropCR
    >>> S.zip (S.each [1 :: Int ..])
    >>> S.dropWhile (snd >>> T.isPrefixOf "#")
    >>> S.mapM (uncurry parseAndReport)

parseAndReport :: (MonadFail m) => Int -> T.Text -> m Definition
parseAndReport l =
  parseEntry
    >>> either
      (T.unpack >>> (("On line " <> show l <> ": ") <>) >>> fail)
      pure

parseEntry :: T.Text -> Either T.Text Definition
parseEntry = evalStateT $ do
  defSimp <- state (T.span (isSpace >>> not))
  when (T.null defSimp) $ lift $ Left "Could not parse simplified"
  oneSpace

  defTrad <- state (T.span (isSpace >>> not))
  when (T.null defSimp) $ lift $ Left "Could not parse traditional"
  oneSpace

  defPinyin <- parsePinyins
  oneSpace

  defEng <- parseEng
  rest <- get
  unless (T.null rest) $ lift $ Left "Expected end of line"

  pure Definition {..}

expect :: Char -> StateT T.Text (Either T.Text) ()
expect c =
  gets T.uncons >>= \case
    Just (c', rest) | c == c' -> put rest
    r ->
      lift $
        Left $
          ("Expected " <> T.pack (show c) <> ", got: ")
            <> maybe "<EOS>" (fst >>> show >>> T.pack) r

oneSpace :: StateT T.Text (Either T.Text) ()
oneSpace = expect ' '

parsePinyins :: StateT T.Text (Either T.Text) (Sq.Seq T.Text)
parsePinyins = do
  expect '['
  pinyinStr <- state (T.span (/= ']'))
  expect ']'
  pure $ Sq.fromList $ T.splitOn " " pinyinStr

parseEng :: StateT T.Text (Either T.Text) (Sq.Seq T.Text)
parseEng = expect '/' >> go
  where
    go =
      gets T.null >>= \case
        True -> pure mempty
        False -> do
          def <- state (T.span (/= '/'))
          expect '/'
          (def Sq.:<|) <$> go
