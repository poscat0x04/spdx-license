{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

-- | Parsing, pretty-printing and rendering of SPDX license templates
module Distribution.SPDX.Template
  ( Parser,
    License (..),
    Piece (..),
    SubstitutionError (..),
    license,
    prettyLicense,
    render,
  )
where

import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String.Interpolate
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Void
import GHC.Generics
import Text.Megaparsec
  ( MonadParsec (eof, notFollowedBy, takeWhileP, try),
    Parsec,
    anySingle,
    chunk,
    many,
    single,
    some,
    (<|>),
  )
import Text.Regex.TDFA ((=~))

type Parser = Parsec Void Text

data SubstitutionError = RegexNoMatch
  { name :: {-# UNPACK #-} Text,
    original :: {-# UNPACK #-} Text,
    match :: {-# UNPACK #-} Text,
    target :: {-# UNPACK #-} Text
  }
  deriving (Eq, Generic)

instance Show SubstitutionError where
  show RegexNoMatch {..} =
    [i|the substitution target "#{target}" does not match the regex "#{match}" required by the var "#{name}"|]

newtype License = License [Piece]
  deriving (Show, Eq, Generic)

data Piece
  = Substansive {-# UNPACK #-} Text
  | Optional [Piece]
  | Var
      { name :: {-# UNPACK #-} Text,
        original :: {-# UNPACK #-} Text,
        match :: {-# UNPACK #-} Text
      }
  deriving (Show, Eq, Generic)

prettyPiece :: Piece -> Text
prettyPiece (Substansive s) = s
prettyPiece (Optional o) = [i|<<beginOptional>>#{foldMap prettyPiece o}<<endOptional>>|]
prettyPiece Var {..} = [i|<<var;name="#{name}";original="#{original}";match="#{match}">>|]

prettyLicense :: License -> Text
prettyLicense (License ps) = foldMap prettyPiece ps

bra :: Parser ()
bra = chunk "<<" $> ()

ket :: Parser ()
ket = chunk ">>" $> ()

bracket :: Parser a -> Parser a
bracket p = bra *> p <* ket

semi :: Parser ()
semi = single ';' $> ()

quote :: Parser a -> Parser a
quote p = single '"' *> p <* single '"'

eq :: Parser ()
eq = single '=' $> ()

field :: Text -> Parser Text
field fieldName = do
  _ <- chunk fieldName
  eq
  quote $ takeWhileP Nothing (/= '"')

beginOptional :: Parser ()
beginOptional = bracket $ chunk "beginOptional" $> ()

endOptional :: Parser ()
endOptional = bracket $ chunk "endOptional" $> ()

substansive :: Parser Piece
substansive = do
  l <- some $ do
    notFollowedBy var
    notFollowedBy beginOptional
    notFollowedBy endOptional
    anySingle
  pure $ Substansive $ pack l

optional :: Parser Piece
optional = do
  beginOptional
  l <- many piece
  endOptional
  pure $ Optional l

var :: Parser Piece
var = bracket $ do
  _ <- chunk "var"
  semi
  name <- field "name"
  semi
  original <- field "original"
  semi
  match <- field "match"
  pure Var {..}

piece :: Parser Piece
piece = try var <|> try optional <|> try substansive

license :: Parser License
license = do
  pieces <- many piece
  eof
  pure $ License pieces

substitute :: Map Text Text -> Piece -> Either SubstitutionError Text
substitute _ (Substansive s) = pure s
substitute ctx (Optional ps) = substitute' ctx ps
substitute ctx Var {..} =
  let mTarget = M.lookup name ctx
   in maybe
        (pure original)
        ( \target ->
            if target =~ match
              then pure target
              else Left $ RegexNoMatch {..}
        )
        mTarget

substitute' :: Map Text Text -> [Piece] -> Either SubstitutionError Text
substitute' ctx ps = T.concat <$> traverse (substitute ctx) ps

render :: Map Text Text -> License -> Either SubstitutionError Text
render ctx (License ps) = T.concat <$> traverse (substitute ctx) ps
