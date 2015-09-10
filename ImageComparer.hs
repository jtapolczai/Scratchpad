import Control.Monad.State
import Data.Array as A
import qualified Data.Map as M
import Data.Maybe
import Data.Ratio
import System.FilePath

--needed: juicy pixels

-- todo: * GUI
--       * remove images from cache once they've been compared to all
--         other elems
--       * edge and texture histograms
--       * convert everything that's possible into RGBF colorspace
--       * option to delete one image in case of match
--       * create a redirect-file (i.e. if image X was deleted because
--         it matches Y, create an entry X -> Y somewhere for the PPP
--         sessions

data Cache = Cache {
   _cacheFiles :: M.Map FilePath ImageData,
   _cacheCurSize :: Int,
   _cacheMaxSize :: Int
   }

data Channel = Red | Green | Blue

type Pixel = (Int, Int, Int)
type Image = V.Vector PixelRGBF

data Histogram = ColorHistogram {
   _histogramBuckets :: M.Map Int Int 
   }
   --possibly other constructors for edges and such

data ImageData = ImageData {
   _imageDimensions :: (Width, Height),
   _imageHistograms :: M.Map Channel Histogram
   }

type Similarity = Rational

fromColorHistogram :: Histogram -> M.Map Int Int
fromColorHistogram (ColorHistogram h) = h
fromColorHistogram _ = error "fromColorHistogram: not a color histogram!"


emptyColorHistogram :: Histogram
emptyColorHistogram = ColorHistogram . M.fromList . zip (repeat 0)
                      $ [0.25, 0.5, 0.75, 1] 

getCache :: FilePath -> StateT IO Cache Image
getCache fp = do
   cache <- get
   case M.lookup fp cache of
      Nothing -> do img <- loadImage fp
                    put (M.insert fp img cache)
      Just x -> return x

loadImage :: MonadIO m => FilePath -> m Image
loadImage fp = do
   img <- readImage fp



   promoteImage 

generateHistogram :: Image -> ImageData
generateHistogram is = Image dims $ M.fromList [(Red, rh),
                                                (Green, gh),
                                                (Blue, bh)]
   where
      dims = case A.indices is of [] -> (0,0)
                                  xs -> last xs

      (rh,gh,bh) = foldl' addRGB (emptyColorHistogram,
                                  emptyColorHistogram,
                                  emptyColorHistogram)

      addRGB :: Pixel -> (Histogram, Histogram, Histogram) -> (Histogram, Histogram, Histogram)
      addRGB (r,g,b) (rA,gA,bA) = (add r rA, add g gA, add b bA)
      add :: Int -> Histogram -> Histogram
      add x xs = flip (M.adjust (+1)) xs . head . filter (x <) . M.keys $ xs

compareImageData :: ImageData -> ImageData -> Similarity
compareImageData i1 i2 =
   where
      numPixels1 = i1 ^. dimensions . to (uncurry (*))
      numPixels2 = i2 ^. dimensions . to (uncurry (*))

      hist c = view (histograms . at c . to fromJust . fromColorHistogram)

      colorsDiff = M.intersectionWith (\x y -> abs $ (x `div` numPixels1) - (y `div` numPixels1))
                                      (hist i1) (hist i2)

checkPair :: FilePath -> FilePath -> StateT IO Cache Similarity
checkPair i1 i2 = check <$> getCache i1 <*> getCache i2

removeFromCache :: Monad m => FilePath -> StateT m Cache ()
removeFromCache fp = modify f
   where
      f (files cur max) = Cache (M.delete fp files) (max 0 cur-1)
