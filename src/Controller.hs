{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Controller where
import Cribbage
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

type CribbageApi =
  -- localhost:3000/
  Get '[JSON] String
  -- localhost:3000/cribbage
  :<|> "cribbage" :> Get '[JSON] [Card]
  -- http://localhost:3000/cribbage/1001
  :<|> "cribbage" :> Capture "id" Integer :> Get '[JSON] Card

cribbageApi :: Proxy CribbageApi
cribbageApi = Proxy

apiService :: IO ()
apiService = do
  let port = 3000
  let settings =  setPort port $ setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = do
    shuffledDeck <- dealCards (shuffleNew newDeck)
    return $ serve cribbageApi (server shuffledDeck)

server ::  [Card] -> Server CribbageApi
server cards =
  getRoot
  :<|> getCard cards
  :<|> getCardById cards

getCard :: [Card] -> Handler [Card]
getCard = return

-- http://localhost:3000/cribbage/1001
getCardById :: [Card] -> Integer -> Handler Card
getCardById cards id = return (last cards)
--getCribbageById _ _ = throwError err404

getRoot :: Handler String
getRoot = return "{}"

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

--stringHandler = liftIO ioMaybeString >>= f
--    where f (Just str) = return str
--          f Nothing = throwError err404