-- secrethsare
-- haskell version of secretshare [1]
-- this program implements shamir's secret sharing algorithm [2]
--
-- [1] https://github.com/sellibitze/secretshare
-- [2] https://en.wikipedia.org/wiki/Shamir%27s_Secret_Sharing

{-# LANGUAGE OverloadedStrings #-}
import Options.Applicative (Parser, subparser, command, progDesc, info, (<>), strOption,
    argument,auto,long,short,metavar,value,help,switch,execParser,fullDesc,header, helper)
import Text.ParserCombinators.Parsec (parse,GenParser,ParseError,many1,
        oneOf,endBy,alphaNum,spaces,digit,char)
import Control.Applicative ((<*),(<$>),(<*>),(<|>),many,pure)
import Control.Monad (when, unless)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Data.List (intercalate, group, sort, transpose)
import Data.Char (chr, ord)
import System.Random as R

import GF256
import CRC24

--------------------------------------------------------------------------------
-- cli option parsing

data Command = Encrypt Int Int | Decrypt
    deriving (Show,Eq)

data Options = Options Command String Bool Bool
--data Options = Options
--  { cmd :: Command
--  , file :: String
--  , nocrc :: Bool
--  , verb :: Bool }

parseEncrypt :: Parser Command
parseEncrypt = Encrypt
        <$> argument auto (metavar "k")
        <*> argument auto (metavar "n")

parseCommand :: Parser Command
parseCommand = subparser (
       command "encrypt" (info (helper <*> parseEncrypt) 
                            (progDesc "split secret to n shares, k required for reconstruction"))
    <> command "e"       (info (helper <*> parseEncrypt) (progDesc "short for encrypt"))
    <> command "decrypt" (info (pure Decrypt) (progDesc "reconstruct secret from k shares"))
    <> command "d"       (info (pure Decrypt) (progDesc "short for decrypt"))
    )

parseOptions :: Parser Options
parseOptions = Options
  <$> parseCommand
  <*> strOption
      ( long "file" <> short 'f'
     <> metavar "FILE" <> value "-"
     <> help "input file (default is STDIN)" )
  <*> switch
      ( long "no-crc" <> short 'c'
     <> help "disable crc checking" )
  <*> switch
      ( long "verbose" <> short 'v'
     <> help "enable verbose output" )

main :: IO ()
main = execParser opts >>= runApp
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> progDesc "Implements Shamir's secret sharing\n" 
     <> header "secrethsare - a secret sharing system" )


runApp :: Options -> IO ()
runApp (Options (Encrypt k n) f _ v) = verbose v "encrypting stuff..." >> encryptSecret k n f v
runApp (Options Decrypt f c v) = verbose v "decrypting stuff..." >> decryptSecret f c v

--------------------------------------------------------------------------------
-- worker functions

encryptSecret :: Int -> Int -> String -> Bool -> IO ()
encryptSecret ki ni f v = do
    input <- getInput f
    case checkKN ki ni of
        Left e       -> putStrLn $ "E: " ++ e
        Right (k, n) -> do
            verbose v $ "sensible k, n found: k = " ++ show k ++ ", n = " ++ show n
            gen <- R.getStdGen
            let secs = encryptSecretWorker gen k n input
            mapM_ print secs

checkKN :: Int -> Int -> Either String (Int, Int)
checkKN k n = do
        when (k > n) $ Left "k greater than n."
        --when (k == n) $ Left "k equal n - are you serious?"
        return (k, n)

decryptSecret :: String -> Bool -> Bool -> IO ()
decryptSecret f c v = do
    input <- getInput f
    case parseSecretList input of
        Left e        -> print e
        Right secrets -> do
            verbose v $ "parsed secrets:\n" ++ intercalate "\n" (map show secrets)
            case sanitize secrets c of
                Left e     -> putStrLn $ "E: " ++ e
                Right secs -> print $ decryptSecretWorker secs
            return ()

sanitize :: [Secret] -> Bool -> Either String [Secret]
sanitize [] _ = Left "no shares."
sanitize ss c = do
        when (any ((k /=) . sk) ss) $ Left "Different k in input shares."
        when (any ((1 /=) . length) (group $ sort ns) ) $ Left "Inconsistent n in input shares."
        when (length ss < k) $ Left "Not enough shares given."
        unless (c || all crcValid ss) $ Left "Checksum mismatch."
        return ss
        where k = sk $ head ss
              ns = map sn ss

decryptSecretWorker :: [Secret] -> String
decryptSecretWorker secs = map (chr . fromEnum . lagInterpolation 0) ps
    where ps = map (zip sps) sds
          sds = transpose $ map (map (GF256 . fromIntegral) . BS.unpack . dat) secs
          sps = map (GF256 . fromIntegral . sn) secs

encryptSecretWorker :: StdGen -> Int -> Int -> String -> [Secret]
encryptSecretWorker g k n str = map (crcAdd . \(i,s) -> Secret k i s "") $ zip [1..n] ss
        where ss = map (BS.pack . map (toEnum . fromEnum)) ts
              ts = transpose $ map (encryptLetter g k n)  str

encryptLetter :: StdGen -> Int -> Int -> Char -> [GF256]
encryptLetter g k n l = map poly [1..(fromIntegral n)]
        where s = GF256 $ fromIntegral $ ord l
              poly x = s + sum (zipWith (*) as $ iterate (*x) x)
              as = map GF256 $ take (k-1) $ R.randomRs (0,255) g

-- interpolate polynomial at x0 by given supporting points xys
lagInterpolation :: (Fractional a, Eq a) => a -> [(a, a)] -> a
lagInterpolation x0 xys = sum $ map w xys
    where w (x,y) = y * product (map (p x) xys)
          p xi (xp,_) | xi == xp  = 1
                      | otherwise = (x0 - xp) / (xi - xp)

--------------------------------------------------------------------------------
-- helper functions

verbose :: Bool -> String -> IO ()
verbose v msg = when v $ putStrLn msg

getInput :: String -> IO String
getInput "-" = getContents
getInput x   = readFile x

data Secret = Secret { 
            sk :: Int,
            sn :: Int,
            dat :: BS.ByteString,
            crc :: BS.ByteString
    }

mkSecret :: Int -> Int -> String -> String -> Secret
mkSecret k n d c = Secret k n (B64.decodeLenient (BS8.pack d)) (B64.decodeLenient (BS8.pack c))

instance Show Secret where
    show (Secret k n d c) = intercalate "-" [show k,show n,toStr d,toStr c]
        where toStr x = filter (/= '=') (BS8.unpack (B64.encode x))

crcSecret :: Secret -> BS.ByteString
crcSecret (Secret k n d _) = BS.pack (crc24 (toEnum k:toEnum n:BS.unpack d))

crcAdd :: Secret -> Secret
crcAdd s@(Secret k n d _) = Secret k n d (crcSecret s)

crcValid :: Secret -> Bool
crcValid s = crcSecret s == crc s

parseSecretA :: GenParser Char st Secret
parseSecretA = mkSecret <$> (read <$> many1 digit)
                <* char '-' <*> (read <$> many1 digit)
                <* char '-' <*> many1 base64c
                <* char '-' <*> many base64c
            where base64c = alphaNum <|> oneOf "+/"

parseSecretList :: String -> Either ParseError [Secret]
parseSecretList = parse parseSecretL "" 
    where parseSecretL = parseSecretA `endBy` spaces
