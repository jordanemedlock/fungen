{-# OPTIONS_HADDOCK hide #-}
{- |
This FunGEn module loads [bmp] files.
-}
{-

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Graphics.UI.Fungen.Loader (
        loadBitmap, loadBitmapList, FilePictureList
) where

import Graphics.Rendering.OpenGL
import System.IO
import Foreign
import Graphics.UI.Fungen.Types
import Graphics.UI.Fungen.Util

binAux :: String
binAux = "000000000000000000000000"

type BmpList = [(GLubyte, GLubyte, GLubyte, GLubyte)]
type FilePictureList = [(FilePath,InvList)]

-- | Loads a bitmap from a file.
loadBitmap :: FilePath -> Maybe (ColorList GLubyte) -> IO AwbfBitmap
loadBitmap bmName invList = do
        bmFile <- openBinaryFile bmName (ReadMode)
        bmString <- hGetContents bmFile
        (bmW,bmH) <- getWH (dropGLsizei 18 bmString)
        bmData <- getBmData (dropGLsizei 54 bmString) (bmW,bmH) invList
        hClose bmFile
        return (AwbfBitmap bmW bmH bmData)

-- | Loads n bitmaps from n files.
loadBitmapList :: [(FilePath, Maybe (ColorList GLubyte))] -> IO [AwbfBitmap]
loadBitmapList bmps = do
        bmList <- loadBmListAux bmps []
        return (reverse bmList)

loadBmListAux :: [(FilePath, Maybe (ColorList GLubyte))] -> [AwbfBitmap] -> IO [AwbfBitmap]
loadBmListAux [] bmList = return (bmList)
loadBmListAux ((n,l):as) bmList = do
        bm <- loadBitmap n l
        loadBmListAux as (bm:bmList)

getWH :: String -> IO (GLsizei,GLsizei)
getWH (a:b:c:d:e:f:g:h:_) = do
        return ( (op (bin a) 0) + (op (bin b) 8) + (op (bin c) 16) + (op (bin d) 24),
                 (op (bin e) 0) + (op (bin f) 8) + (op (bin g) 16) + (op (bin h) 24))
                 where bin x = toBinary(fromEnum x)
                       op x n = toDecimal(shiftLeft(binAux ++ (make0 (8 - (length x)) ++ x)) n)
getWH _ = error "Loader.getWH error: strange bitmap file"

getBmData :: String -> (GLsizei,GLsizei) -> Maybe (ColorList GLubyte) -> IO (PixelData GLubyte)
getBmData bmString (bmW,bmH) invList =
        let colorList = makeColorList bmString (bmW,bmH) in
        newArray [Color4 r g b a | (r,g,b,a) <- addInvisiblity colorList invList] >>= \bmData ->
        return (PixelData RGBA UnsignedByte (castPtr bmData))

addInvisiblity :: (ColorList GLubyte) -> Maybe (ColorList GLubyte) -> BmpList
addInvisiblity [] _ = []
addInvisiblity l Nothing = map (\(ColorRGB r g b) -> (r,g,b,255)) l
addInvisiblity (c@(ColorRGB r g b):as) i@(Just invList) | c `elem` invList = ((r,g,b,0):(addInvisiblity as i))
                                             | otherwise = ((r,g,b,255):(addInvisiblity as i))

makeColorList :: String -> (GLsizei,GLsizei) -> ColorList GLubyte
makeColorList bmString (bmW,bmH) = makeColorListAux (bmW `mod` 4) bmString (bmW*bmH) (bmW,bmW)

makeColorListAux :: GLsizei -> String -> GLsizei -> (GLsizei,GLsizei) -> ColorList GLubyte
makeColorListAux _ _ 0 _ = []
makeColorListAux x bmString totVert (0,bmW) = makeColorListAux x (dropGLsizei x bmString) totVert (bmW,bmW)
makeColorListAux x (b:g:r:bmString) totVert (n,bmW) = (ColorRGB (ord2 r) (ord2 g) (ord2 b)): (makeColorListAux x bmString (totVert - 1) (n - 1,bmW))
makeColorListAux _ _ _ _ = error "Loader.makeColorListAux error: strange bitmap file"
