{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Derivasjon.Main where

import Data.Aeson
import Data.FileEmbed
import Data.Fix (Fix)
import Data.Functor.Foldable
import Data.Functor.Foldable.TH (makeBaseFunctor)
import qualified Data.Text as Text
import Lucid
import Lucid.Htmx
import Network.Wai.Handler.Warp
import Servant
import Servant.API
import Servant.HTML.Lucid
import Servant.Htmx
import Servant.JS.Fiat
import System.Random
import Web.FormUrlEncoded (FromForm)

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | X
  | Constant Int

makeBaseFunctor ''Expr

data Polynomial = Polynomial {coeffs :: [Int], startExp :: Int} deriving (Show)

data Function = Function {fName :: Text, polynomial :: Polynomial} deriving (Show)

showPolynomial :: Polynomial -> Text
showPolynomial (Polynomial coeffs startExp) = (\case "" -> "0"; x -> x) . unwords . zipWith showTerm (True : repeat False) . reverse . filter ((/= 0) . fst) $ zip coeffs [startExp ..]
  where
    showTerm :: Bool -> (Int, Int) -> Text
    showTerm isFirst (x, e) =
      (if x > 0 then (if isFirst then "" else "+ ") else "- ") <> case (abs x, e) of
        (1, 1) -> "x"
        (c, 0) -> show c
        (c, 1) -> show c <> "x"
        (1, e) -> if e > 0 then "x^{" <> show e <> "}" else "\\frac{1}{x" <> (if e == (-1) then "" else "^{" <> show (negate e) <> "}") <> "}"
        (c, e) -> if e > 0 then show c <> "x^{" <> show e <> "}" else "\\frac{" <> show c <> "}{x" <> (if e == (-1) then "" else "^{" <> show (negate e) <> "}") <> "}"

instance ToHtml Polynomial where
  toHtml p = toHtml $ "\\[" <> showPolynomial p <> "\\]"

instance ToHtml Function where
  toHtml f = toHtml $ "\\[" <> fName f <> "(x) = " <> showPolynomial (polynomial f) <> "\\]"

makeSameLength :: ([Int], [Int]) -> ([Int], [Int])
makeSameLength (p1, p2) =
  let l1 = length p1
      l2 = length p2
      l = max l1 l2
   in (p1 <> replicate (l - l1) 0, p2 <> replicate (l - l2) 0)

makeSameLengthPolynomial :: (Polynomial, Polynomial) -> (Polynomial, Polynomial)
makeSameLengthPolynomial (Polynomial c1 s1, Polynomial c2 s2) =
  let s = min s1 s2
      (c1', c2') = makeSameLength (replicate (s1 - s) 0 <> c1, replicate (s2 - s) 0 <> c2)
   in (Polynomial c1' s, Polynomial c2' s)

showExp :: Expr -> Text
showExp = cata $ \case
  AddF a b -> a <> " + " <> b
  MulF a b -> a <> " * " <> b
  XF -> "x"
  ConstantF x -> show x

toPolynomial :: Expr -> Polynomial
toPolynomial = cata $ \case
  AddF p1 p2 -> let (Polynomial c1 s1, Polynomial c2 s2) = makeSameLengthPolynomial (p1, p2) in Polynomial (zipWith (+) c1 c2) s1
  MulF p1 p2 -> let (Polynomial c1 s1, Polynomial c2 s2) = makeSameLengthPolynomial (p1, p2) in Polynomial (foldr (\a p -> map (+ a) p) c1 c2) s2
  XF -> Polynomial [1] 1
  ConstantF x -> Polynomial [x] 0

differentiate :: Polynomial -> Polynomial
differentiate (Polynomial coeffs startExp) = Polynomial (zipWith (*) coeffs [startExp ..]) (startExp - 1)

generateRandomPolynomial :: Int -> Int -> Int -> Int -> Int -> Integer -> IO Polynomial
generateRandomPolynomial maxParts minDegree maxDegree minCoef maxCoef qid = do
  setStdGen (mkStdGen (fromIntegral qid))
  numParts <- randomRIO (1, maxParts :: Int)
  startExp <- randomRIO (minDegree, maxDegree - numParts + 1 :: Int)
  coeffs <- replicateM numParts (randomRIO (minCoef, maxCoef :: Int))
  return $ Polynomial coeffs startExp

newtype Question = Question {qid :: Integer} deriving (Generic)

instance FromJSON Question

instance FromForm Question

instance ToHtml Question where
  toHtml (Question qid) =
    h1_ [style_ "text-align:center"] ("Oppgave " <> show qid)
      <> p_ [style_ "text-align:center"] "Derivér:"
      <> div_ [style_ "text-align:center; height: 60px; box-sizing: border-box;", hxGet_ ("questionPolynomial/" <> show qid), hxTrigger_ "load"] ""
      <> div_ [style_ "text-align:center; height: 100px; box-sizing: border-box;"] (button_ [style_ "text-align:center", hxGet_ ("answer/" <> show qid), hxSwap_ "outerHTML"] "Vis fasit")
      <> div_ [style_ "text-align:center"] (button_ [style_ "text-align:center", hxGet_ ("question/" <> show (qid + 1)), hxTarget_ "#main-content"] "Neste oppgave")
      <> div_ [style_ "height: 50px;"] ""
      <> form_ [style_ "text-align:center", hxPost_ "questionPost", hxTarget_ "#main-content"] (input_ [type_ "text", name_ "qid", size_ "4"] <> button_ [type_ "submit"] "Gå til oppgave")

newtype Answer = Answer Integer

instance ToHtml Answer where
  toHtml (Answer qid) =
    div_ "Fasit:"
      <> div_ [hxGet_ ("answerPolynomial/" <> show qid), hxTrigger_ "load"] ""

type UserAPI =
  "htmx.min.js" :> Get '[JS] ByteString
    :<|> Get '[HTML] (Html ())
    :<|> "question" :> Capture "id" Integer :> Get '[HTML] Question
    :<|> "questionPost" :> ReqBody '[FormUrlEncoded] Question :> Post '[HTML] Question
    :<|> "questionPolynomial" :> Capture "id" Integer :> Get '[HTML] (Headers '[HXTriggerAfterSwap] Function)
    :<|> "answer" :> Capture "id" Integer :> Get '[HTML] Answer
    :<|> "answerPolynomial" :> Capture "id" Integer :> Get '[HTML] (Headers '[HXTriggerAfterSwap] Function)

userAPI :: Proxy UserAPI
userAPI = Proxy

server :: Server UserAPI
server =
  ( return $(embedFile "htmx.min.js")
      :<|> (return . toHtmlRaw $ $(embedFile "index.html"))
      :<|> (pure . Question)
      :<|> pure
      :<|> ( fmap (addHeader "myEvent") . liftIO
               . fmap (Function "f")
               . generateRandomPolynomial 3 (-5) 5 (-5) 5
           )
      :<|> (pure . Answer)
      :<|> ( fmap (addHeader "myEvent") . liftIO
               . fmap (Function "f'")
               . fmap differentiate
               . generateRandomPolynomial 3 (-5) 5 (-5) 5
           )
  )

app = serve userAPI server

main :: IO ()
main = run 8000 app
