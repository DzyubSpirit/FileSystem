module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS
import qualified Data.Map as M
import qualified Config as C

import Data.List

memory = fmap readMemory $ B.readFile "test.bin"

dirFiles = fmap (getDirFiles 0) memory

memory' = fmap (putFileInDir (3, "New.bin") 0) memory

dirFiles' = fmap (getDirFiles 0) memory'


main = do
    memory <- memory
    undefined    

type Filename = String
type Reference = Int
type DescIndex = Int
newtype Memory = Memory (M.Map Reference MemoryBlock) deriving (Show)
data MemoryBlock = MemoryBlock {
    getContent :: B.ByteString,
    isFree     :: Bool
} deriving (Show)
data DriverInfo = DriverInfo {
    getBlockSize :: Int,
    getFileRefCount :: Int,
    getMaxFilenameLength :: Int,
    getMaxDescriptorCount :: Int
}

type DescriptorIndex = Int
data FileType = File | Folder | None deriving (Eq, Show)
data Descriptor = Descriptor {
    getType :: FileType,
    getSize :: Int,
    getLinkCount :: Int,
    getRefs :: [Reference]
}

instance Show Descriptor where
    show (Descriptor fileType size linkCount refs) = 
        (show fileType) ++ ":" ++
        (show size) ++ ":" ++
        (intercalate ":" $ map show refs)

class Noneable a where
    isNone  :: a -> Bool
    notNone :: a -> Bool
    isFile  :: a -> Bool

    notNone = not . isNone
    isNone  = not . notNone

instance Noneable FileType where
    isNone None = True
    isNone _    = False
    isFile File = True
    isFile _    = False

instance Noneable Descriptor where
    isNone = isNone . getType 
    isFile = isFile . getType

class HasDescriptors a where
    readDescriptors :: a -> [Descriptor]
    takeDescriptor  :: Int -> a -> Descriptor

    takeDescriptor i =  (!! i) . readDescriptors

instance HasDescriptors B.ByteString where
    readDescriptors = map readDescriptor
                    . splitAtChunks C.descriptorLength

instance HasDescriptors MemoryBlock where
    readDescriptors = readDescriptors . getContent

instance HasDescriptors Memory where
    readDescriptors (Memory mem) = readDescriptors . (M.! 0) $ mem

splitAtChunks :: Int -> B.ByteString -> [B.ByteString]
splitAtChunks n = map (B.take n)
                . takeWhile ((>0).(B.length)) 
                . iterate (B.drop n)

modifyContent :: (B.ByteString -> B.ByteString) -> MemoryBlock -> MemoryBlock
modifyContent f (MemoryBlock cont free) = MemoryBlock (f cont) free

readFileType 0 = None
readFileType 1 = File
readFileType 2 = Folder

writeFileType None   = B.singleton 0
writeFileType File   = B.singleton 1
writeFileType Folder = B.singleton 2

insertBetween :: ([a] -> ([a], [a])) -> a -> [a] -> [a]
insertBetween f el xs = before ++ (el:after)
    where (before, _:after) = f xs          

insertAt :: Int -> a -> [a] -> [a]
insertAt i = insertBetween (splitAt i)

insertWhen :: (Descriptor -> Bool) -> Descriptor -> [Descriptor] -> [Descriptor]
insertWhen f = insertBetween (span f)

insertWhenNone :: Descriptor -> [Descriptor] -> [Descriptor]
insertWhenNone = insertWhen isNone

readDescriptor :: B.ByteString -> Descriptor
readDescriptor str = Descriptor {
        getType = readFileType fileType,
        getSize = size,
        getLinkCount = linkCount,
        getRefs = refs
    } where mapFst f (a, b) = (f a, b)
            (fileType, str')   = mapFst readNumber $ B.splitAt C.fileTypeLength str
            (size, str'')      = mapFst readNumber $ B.splitAt C.sizeLength str'
            (linkCount, str''') = mapFst readNumber $ B.splitAt C.linkCountLength str''
            refs = take C.maxRefCount .  map (readNumber.fst) . tail 
                 . iterate (B.splitAt C.refLength . snd) 
                 $ (B.empty, str''') 

showDescriptor :: Descriptor -> B.ByteString
showDescriptor (Descriptor fileType size linkCount refs) = mconcat $
            [writeFileType fileType
            ,writeNumberN C.sizeLength size
            ,writeNumberN C.linkCountLength linkCount] ++ 
            map (writeNumberN C.refLength) refs


showDescriptors :: [Descriptor] -> B.ByteString
showDescriptors = mconcat . map showDescriptor

writeNumberN :: Int -> Int -> B.ByteString
writeNumberN n = B.pack
               . reverse
               . map (fromIntegral . (`mod` 256))
               . take n
               . iterate (`div` 256)

readNumberN :: Int -> B.ByteString -> Int
readNumberN n = readNumber . B.take n

readNumbersN :: Int -> B.ByteString -> [Int]
readNumbersN n = map (readNumberN n) 
               . takeWhile ((>=n) . B.length)
               . iterate (B.drop n)

readNumber :: B.ByteString -> Int
readNumber = foldl1 (\acc b -> acc*256+b)
           . map fromIntegral 
           . B.unpack

toByteString :: String -> B.ByteString
toByteString = B.pack . map BS.c2w

fromByteString :: B.ByteString -> String
fromByteString = map BS.w2c . B.unpack

readFiles :: B.ByteString -> [(DescIndex, Filename)]
readFiles = 
    let pairLen = C.fileFieldLength 
        readPair = transformPair . B.splitAt C.refLength
        transformPair (s1, s2) = (readNumberN C.refLength s1, fromByteString s2) 
    in   map (readPair . B.take pairLen) 
       . iterate (B.drop pairLen)

readDriverInfo :: String -> DriverInfo
readDriverInfo str = 
    case map read . lines $ str of
        bs:frc:mfl:mdc:_ -> DriverInfo bs frc mfl mdc
        _ -> error $ "Not DriverInfo format "

takeBlock :: Int -> Memory -> MemoryBlock
takeBlock i (Memory mem) = mem M.! i

readMemory :: B.ByteString -> Memory
readMemory str = calcMemoryFreedom
               . map (B.take C.blockSize) 
               . takeWhile ((>0) . B.length) 
               . iterate (B.drop C.blockSize)
               . B.append (B.pack (replicate addByteCount 0))
               $ str 
    where len = B.length str
          addByteCount = mod (-(mod len C.blockSize)) C.blockSize 

calcMemoryFreedom :: [B.ByteString] -> Memory
calcMemoryFreedom blocks@(f:_) = Memory . snd
                               . M.mapAccum (\(x:xs) el -> (xs, (MemoryBlock x el))) blocks
                               . foldr (`M.insert` False) initMap 
                               . concat 
                               . map descriptorAllRefs 
                               . filter isFile
                               $ descriptors
    where descriptors = readDescriptors f
          blocksLen = length blocks
          initMap = M.fromList initList
          initList = zip [0..blocksLen-1] $ False:(repeat True)
          descriptorAllRefs d = neededRefs ++ (take size moreRefs)
            where size = getSize d
                  neededRefsCount = calcRefsCount size
                  neededRefs = take neededRefsCount refs
                  refs = getRefs d
                  moreRefs = concat
                           . map (readNumbersN C.refLength
                                 . (blocks!!)) 
                           $ refs

getDirFiles :: DescriptorIndex -> Memory -> [(DescIndex, Filename)]
getDirFiles folderId mem = files
    where folder = (!! folderId) . readDescriptors $ mem
          size = getSize folder
          refs = getRefs folder
          filesInBlock = div C.blockSize C.fileFieldLength
          files = take size
                . concat
                . map (readFiles
                      . getContent
                      . (`takeBlock` mem))  
                $ refs

putFileInDir :: (DescIndex, Filename) -> DescIndex -> Memory -> Memory
putFileInDir (descindex, filename) folderIndex mem@(Memory blocks) = Memory blocks'
    where fileField = mappend (writeNumberN 2 descindex) (toByteString filename)
          folderDesc = takeDescriptor folderIndex mem
          size = getSize folderDesc
          refs = getRefs folderDesc
          size' = size + 1
          refsCount  = calcFileFieldsCount size
          refsCount' = calcFileFieldsCount size'
          isNewBlock = refsCount' > refsCount
          (freeBlockIndex,_) = head 
                             . M.toList 
                             . M.filter isFree
                             $ blocks 
          folderDesc' = folderDesc {
            getSize = size',
            getRefs = if isNewBlock
                      then refs
                      else insertAt refsCount freeBlockIndex refs
          }
          folderDesc'String = showDescriptor folderDesc'
          fieldIndexToWrite = mod size C.fileFieldsInBlock
          addFileField = modifyContent $
                mconcat
                . insertAt fieldIndexToWrite fileField
                . splitAtChunks C.fileFieldLength
          makeBusy (MemoryBlock a _) = MemoryBlock a False
          addNewFolderDesc = modifyContent $
                mconcat
                . insertAt folderIndex folderDesc'String
                . splitAtChunks C.descriptorLength
          changedBlock = (getRefs folderDesc') !! refsCount'
          blocks' = M.adjust addNewFolderDesc 0
                  . ( if isNewBlock 
                      then M.adjust makeBusy freeBlockIndex
                      else id
                  )
                  . M.adjust addFileField changedBlock
                  $ blocks




writeDescriptor :: DescriptorIndex -> Descriptor -> Memory -> Memory
writeDescriptor i desc mem@(Memory memMap) = Memory $ M.insert 0 memoryBlock' memMap
    where descriptors = readDescriptors mem
          descriptors' = insertAt i desc descriptors
          memoryBlock' = MemoryBlock {
            getContent = showDescriptors descriptors',
            isFree = False
          } 

writeDescriptors :: [Descriptor] -> Memory -> Memory
writeDescriptors descs (Memory mem) = Memory $ M.insert 0 
    (MemoryBlock (mconcat (map showDescriptor descs)) False) mem

createFile :: Filename -> DescriptorIndex -> Memory -> Memory
createFile filename folderId (Memory mem) = Memory $ undefined 
    where descriptorsBlock = mem M.! 0
          descriptors = readDescriptors $ getContent descriptorsBlock
          descriptors' = insertWhenNone desc' descriptors
          desc' = Descriptor {
            getType = File,
            getSize = 0,
            getLinkCount = 0,
            getRefs = replicate 4 0
          }


calcCount objInBlock size = 1 + (div (size-1) C.refsInBlock)
calcRefsCount = calcCount C.refsInBlock 
calcFileFieldsCount = calcCount C.fileFieldsInBlock