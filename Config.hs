module Config where

blockSize, maxRefCount, maxFilenameLength, maxDescriptorCount :: Int
blockSize          = 48
maxRefCount        = 4
maxFilenameLength  = 14
maxDescriptorCount = 4
fileFieldLength    = refLength + maxFilenameLength
refsInBlock        = div blockSize refLength
fileFieldsInBlock  = div blockSize fileFieldLength

fileTypeLength, sizeLength, refLength, linkCountLength:: Int
fileTypeLength  = 1
sizeLength      = 1
linkCountLength = 2
refLength       = 2
descriptorLength = fileTypeLength
                 + sizeLength
                 + linkCountLength
                 + refLength*maxRefCount
