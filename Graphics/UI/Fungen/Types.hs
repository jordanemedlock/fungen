{-# OPTIONS_HADDOCK hide #-}
{- | This FunGEn module contains the FunGEN basic types.
-}
{-

FunGEN - Functional Game Engine
http://www.cin.ufpe.br/~haskell/fungen
Copyright (C) 2002  Andre Furtado <awbf@cin.ufpe.br>

This code is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

-}

module Graphics.UI.Fungen.Types (
        WindowConfig(..),
        Point2D(..), origin,
        ColorRGB(..),
        ColorList,
        AwbfBitmap(..),
        InvList,
) where

import Graphics.Rendering.OpenGL

-- | position, size and name of the window
data WindowConfig = WindowConfig { wcPosition :: Point2D Int -- ^ Position of the Window
                                 , wcSize :: Point2D Int -- ^ Size of the Window
                                 , wcName :: String -- ^ Name of the Window
                                 } deriving (Show, Eq, Ord)

-- | a bidimensional point in space
data Point2D a = Point2D { xPos :: a -- ^ X position of the point
                         , yPos :: a -- ^ Y position of the point
                         } deriving (Show, Eq, Ord)
origin :: (Num a) => Point2D a
origin = Point2D 0 0

-- | A RGB color
data ColorRGB a = ColorRGB { red :: a -- ^ Red value
                           , green :: a -- ^ Green value
                           , blue :: a -- ^ Blue value
                           } deriving (Show, Eq, Ord)

-- | color in RGB format
type ColorList a = [ColorRGB a]

-- | width, height and data of bitmap
data AwbfBitmap = AwbfBitmap { awfbWidth :: GLsizei -- ^ Height of the data
                             , awfbHeight :: GLsizei -- ^ Width of the data
                             , awfbData ::  PixelData GLubyte -- ^ Data itself
                             } deriving (Show, Eq, Ord)

-- | invisible colors (in RGB) of bitmap
type InvList = Maybe (ColorList Int)
