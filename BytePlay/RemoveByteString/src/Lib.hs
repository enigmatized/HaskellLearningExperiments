{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE OverloadedStrings    #-}


module Lib
    ( someFunc,
      getShort0,
      someFunc2,
      my32LengthShortByteString,
      createIdunno,
      createIdunno',
      giveMeSize,
      whatIsThis',
      printBytes',
      getFoo,
      getByteArray, 
      putOutTest,
      printShortByteLength,
      bytesToWord64,
      bytesToWord8,
      bytesToWord8'B16,
      bytesToWord16,
      byteArrayToWords'      
    ) where



import qualified Data.ByteString.Base16     as B16
import qualified  Data.ByteString.Short as SBS
import Data.ByteString.Char8 hiding (putStrLn)
import Data.ByteString
import Data.Char(ord)
import Data.Word
import System.IO.Unsafe
import qualified Data.Primitive.ByteArray  as PBA
import qualified Data.ByteArray            as BA
import GHC.Num.BigNat
import           GHC.Exts
import           GHC.Integer.GMP.Internals
import           GHC.Word
import           GHC.Num.BigNat
import           GHC.Num.Integer
import           Numeric
import           Data.Vector.Generic.Mutable
import qualified Data.ByteString           as B
--import           System.Endian
import GHC.IO.Encoding.UTF8
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Foreign.Storable          as FS
import           System.Endian
import           Control.Monad
import           Data.Primitive.Types
import           Data.Bits
-- https://hackage.haskell.org/package/primitive-0.8.0.0/docs/Data-Primitive-Types.html#v:indexByteArray-35- This is how I might solve it
--


data ExprKeccek = Keccack256 ByteArray#
--myArray :: Addr# Doesn't work, you know why?
--myArray = "Hello, world!"#

--convertTOSee:: SBS.ShortByteString -> [Char]
--convertTOSee  (SBS.SBS x)  = utf8DecodeByteArray# x --Note you do note have access to this function via the library anymore

getWord8 :: SBS.ShortByteString -> Word
getWord8 !(SBS.SBS !x)  = do 
     let !w = W# (indexWordArray#  x 0#)
     w    


bytesToWord64 :: B.ByteString -> Word64
bytesToWord64 bytes | B.length bytes /= 8 = error $ "bytesToWord256 called with the wrong number of bytes: " ++ show bytes
                         | otherwise = unsafePerformIO $
  (BA.withByteArray bytes :: (Ptr Word64 -> IO Word64) -> IO Word64) $ \src -> do
    hh <-  FS.peekElemOff src 0
    let hhh = fromBE64 hh
    putStrLn $ show $ hh
    putStrLn $ show $ fromBE64 hhh
    pure $ hhh
    
bytesToWord8 :: B.ByteString -> Word8
bytesToWord8 bytes | B.length bytes /= 8 = error $ "bytesToWord256 called with the wrong number of bytes: " ++ show bytes
                         | otherwise = unsafePerformIO $
  (BA.withByteArray bytes :: (Ptr Word8 -> IO Word8) -> IO Word8) $ \src -> do
    hh <-  FS.peekElemOff src 0
    --let hhh = fromBE8 hh
    putStrLn $ show $ "ByteString to Word 8"
    putStrLn $ show $ hh
    --putStrLn $ show $ fromBE64 hhh
    pure $ hh

bytesToWord8'B16 :: B.ByteString -> Word8
bytesToWord8'B16 bytes | B.length bytes /= 8 = error $ "bytesToWord256 called with the wrong number of bytes: " ++ show bytes
                         | otherwise = unsafePerformIO $
  (BA.withByteArray (B16.encode bytes)  :: (Ptr Word8 -> IO Word8) -> IO Word8) $ \src -> do
    hh <-  FS.peekElemOff src 0
    --let hhh = fromBE8 hh
    putStrLn $ show $ "ByteString B.16 Encodeed to Word 8"
    putStrLn $ show $ hh
    
    --putStrLn $ show $ fromBE64 hhh
    pure $ hh


bytesToWord16 :: B.ByteString -> Word16
bytesToWord16 bytes | B.length bytes /= 8 = error $ "bytesToWord256 called with the wrong number of bytes: " ++ show bytes
                         | otherwise = unsafePerformIO $
  (BA.withByteArray bytes :: (Ptr Word16-> IO Word16) -> IO Word16) $ \src -> do
    hh <-  FS.peekElemOff src 0
    let hhh = fromBE16 hh
    putStrLn $ show $ hh
    putStrLn $ show $ hhh
    pure $ hhh


byteArrayToWords' :: SBS.ShortByteString -> IO () -- Word64 -- -> Bool-- ---> Word64
byteArrayToWords'  (SBS.SBS ba#) = do
    let (I# uno# ) = 0
        (I# dos# ) = 1
        (I# tres# ) = 2
        (I# four# ) = 3
        b1 = fromBE64 $ indexByteArray# ba# uno#  :: Word64
        b2 = fromBE64 $ indexByteArray# ba# uno#  :: Word64
        b3 = indexByteArray# ba# uno# :: Word8
        b4 = indexByteArray# ba# uno# :: Word16     
        --b4 = fromBE64 $ indexByteArray# ba# four# :: Word64
      --b5 = indexByteArray# ba# 4 :: Word64
      --b6 = indexByteArray# ba# 5 :: Word64
      --b7 = indexByteArray# ba# 6 :: Word64
      --b8 = indexByteArray# ba# 7 :: Word64
    putStrLn $ ("This is 64 from ByteArray# " ++) $ show  b1
    putStrLn $ ("This is 64 from ByteArray# b1 .|. b2 " ++) $ show $ b1 .|. b2 
    putStrLn $ ("This is 64 from ByteArray# b1 .&. b2 " ++) $ show $ b1 .&. b2
    putStrLn $ ("This is word8 from ByteArray# " ++) $ show b3

    putStrLn $ ("This is word16 from ByteArray# " ++) $ show b4




--testWords :: SBS.ShortByteString -> Word
--testWords = getLiftedWord' . getWord8  

getPtr' :: SBS.ShortByteString ->  Ptr a
getPtr' x  = wordPtrToPtr . WordPtr . getWord8 $ x


getPtr'' :: SBS.ShortByteString ->  WordPtr
getPtr'' x  = WordPtr . getWord8 $ x





getLiftedWord' :: Word# -> Word
getLiftedWord' w#  = W# ( w#)

getPtr :: SBS.ShortByteString -> Addr#
getPtr (SBS.SBS x)  = byteArrayContents# x 

--withByteArray' :: (Ptr p -> IO a) -> IO a
--withByteArray' = BA.withByteArray my32LengthShortByteString



someFunc :: IO ()
someFunc = (putStrLn $ show whatIsThis) >> putStrLn ( show $  createInt my32LengthShortByteString) --  >> putStrLn $ show $ getWord8 --  >> putStrLn $ show . getLiftedWord  $  getWord8 $ my32LengthShortByteString -- >> putStrLn ( show .  getPtr $ my32LengthShortByteString ) 

someFunc2 :: IO ()
someFunc2 = (putStrLn $ show $ getWord8 $ my32LengthShortByteString)  >> someFunc3

someFunc3 :: IO ()
someFunc3 = putStrLn $ show $  createIdunno $ my32LengthShortByteString


createIdunno :: SBS.ShortByteString -> Word64
createIdunno !(SBS.SBS !a) = unsafePerformIO $  do
    !hh <- fromBE64 <$!> (FS.peekElemOff  $ getPtr' $  my32LengthShortByteString) 2
    return $ hh


printShortByteLength :: SBS.ShortByteString -> IO ()
printShortByteLength x  = putStrLn $ show $ SBS.length x





whatIsThis' :: SBS.ShortByteString ->  Integer
whatIsThis' !(SBS.SBS x#) =  --unsafePerformIO $ do
     --newArr <- PBA.newByteArray 25
     --ll     <- fromLE64 
     --PBA.writeByteArray newArr 0 ll
     --dst' <- PBA.unsafeFreezeByteArray newArr --Convert a mutable byte array to an immutable one without copying. The array should not be modified after the conversion.
     --let !(PBA.ByteArray dst'#) = dst'
     IP (unBigNat (BN# x#))








createIdunno' :: SBS.ShortByteString -> Word8
createIdunno' !(SBS.SBS !a) = unsafePerformIO $  do
    !hh <- (FS.peekElemOff  $ getPtr' $  my32LengthShortByteString) 0
    return $ hh


printBytes' :: IO ()
printBytes' = do 
    let (SBS.SBS x# ) = my32LengthShortByteString
    printBytes x#


countOnes :: Word64 -> Int
countOnes w =
  let w' = w - ((w `shiftR` 1) .&. 0x5555555555555555)
      w'' = (w' .&. 0x3333333333333333) + ((w' `shiftR` 2) .&. 0x3333333333333333)
      w''' = (w'' + (w'' `shiftR` 4)) .&. 0x0F0F0F0F0F0F0F0F
  in fromIntegral $ (w''' * 0x0101010101010101) `shiftR` 56

byteArrayToWord64 :: SBS.ShortByteString -> Word64 -- -> Bool-- ---> Word64
byteArrayToWord64 (SBS.SBS ba#) = do
    let (I# uno# ) = 0
        (I# dos# ) = 1
        (I# tres# ) = 2
        (I# four# ) = 3
        b1 = fromBE64 $ indexByteArray# ba# uno#  :: Word64
        b2 = fromBE64 $ indexByteArray# ba# dos#  :: Word64 
        b3 = fromBE64 $ indexByteArray# ba# tres# :: Word64
        b4 = fromBE64 $ indexByteArray# ba# four# :: Word64
      --b5 = indexByteArray# ba# 4 :: Word64
      --b6 = indexByteArray# ba# 5 :: Word64
      --b7 = indexByteArray# ba# 6 :: Word64
      --b8 = indexByteArray# ba# 7 :: Word64
    b1 .|. b2  .|. b3  --countOnes (( b1 .|. b2) :: Word64) 
    --b1 .|. b1 == 0 
--b1 .|. (b2 `shiftL` 8) .|. (b3 `shiftL` 16) .|. (b4 `shiftL` 24) .|.
  --   (b5 `shiftL` 32) .|. (b6 `shiftL` 40) .|. (b7 `shiftL` 48) .|. (b8 `shiftL` 56)


putOutTest :: IO ()
putOutTest = putStrLn . show $  byteArrayToWord64 ("00000000" :: SBS.ShortByteString)-- $ getShort

--byteArrayToWord64 :: Word64 -> Integer


getByteArray :: SBS.ShortByteString -> ByteArray#
getByteArray (SBS.SBS s) = s


--getByteArray' :: SBS.ShortByteString -> IO ()
--getByteArray' (SBS.SBS bitArray#) = putStrLn $ show $  (SBS.SBS bitArray#)  .|.  (SBS.SBS bitArray#)


getShort0 :: SBS.ShortByteString
getShort0 = "00000000"


getShort32Zero :: SBS.ShortByteString
getShort32Zero = "00000000000000000000000000000000"



getShort :: SBS.ShortByteString
getShort = "12345678"

getFoo :: IO ()
getFoo = do 
    let (SBS.SBS x#) = getShort 
    let (I# index#) = 0 
    putStrLn $ show $ (indexByteArray# x# index# :: Word32)
   --byteSwap64 I tried this, but doesn't work like I wanted




-- isZero64 ::
printBytes'' :: ByteArray# -> IO ()
printBytes''    arr# = loop 0 51
  where
    len# = sizeofByteArray# arr#
    loop i b
      | i < (I# len#) && i < 32 = do
          let (I# ii ) = i
          let w = (indexByteArray# arr# ii :: Word8)
              --w = 
          putStrLn $ "Byte " ++ show i ++ ": " ++ (show w) ++ "  " ++  (show  (b  .|. w  ))
          loop (i + 1 ) (b  .|. w  )
      | otherwise = return ()


printBytes :: ByteArray# -> IO ()
printBytes arr# = loop 0
  where
    len# = sizeofByteArray# arr#
    loop i
      | i < (I# len#) && i < 32 = do
          let (I# ii ) = i 
          let w = (indexByteArray# arr# ii :: Word8)
              --w = `
          putStrLn $ "Byte " ++ show i ++ ": " ++ show w
          loop (i + 1)
      | otherwise = return ()


giveMeSize ::  SBS.ShortByteString -> Int
giveMeSize (SBS.SBS x) = do  
            let !sz1 = sizeofByteArray# x
            I# sz1

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . ord

-- createBArray :: PBA.ByteArray
-- createBArray (SBS.SBS a) = PBA.byteArrayFromListN 256  a

createInt :: SBS.ShortByteString -> Integer
createInt (SBS.SBS a) = unsafePerformIO $  do
     --let !(PBA.ByteArray dst'#) = a
     pure $ IP (unBigNat (BN# a))

my32LengthShortByteString :: SBS.ShortByteString
my32LengthShortByteString  = SBS.pack $  Prelude.map charToWord8 "00000000010111213141516171819200"


checkIfShortLength32 :: SBS.ShortByteString -> Bool
checkIfShortLength32  x  = SBS.length x == 32

-- new256ByteArray :: m (PBA.MutableByteArray (PrimState m))
-- new256ByteArray =  PBA.newByteArray 25

whatIsThis :: Integer
whatIsThis =  unsafePerformIO $ do
     newArr <- PBA.newByteArray 25
     --ll     <- fromLE64 
     --PBA.writeByteArray newArr 0 ll
     dst' <- PBA.unsafeFreezeByteArray newArr --Convert a mutable byte array to an immutable one without copying. The array should not be modified after the conversion.
     let !(PBA.ByteArray dst'#) = dst'
     pure $ IP (unBigNat (BN# dst'#))	
-- convertSBSToWord256 :: SBS.ShortByteString -> Word64
-- convertSBSToWord256 | SBS.length bytes /= 32 = error $ "bytesToWord256 called with the wrong number of bytes: " ++ show bytes
-- convertSBSToWord256
