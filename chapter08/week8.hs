import qualified Data.Map as Map

-- Add value - int - 20 and 50.
data NoteType = TwentyNote | FiftyNote  deriving (Ord, Eq)

-- Add value
data CoinType = FiveCoin | TenCoin | TwentyCoin | FiftyCoin deriving (Ord, Eq)

-- Add note above coin.
data CurrencyType = AUD | EUR | GBP deriving (Ord, Eq)

data Bundle = Bundle { numberOfNotes :: Int
    , noteType :: NoteType
	} deriving (Show, Ord, Eq)

data Roll = Roll { numberOfCoins :: Int
    , coinType :: CoinType
    } deriving (Show, Ord, Eq)

type Bundles = [Bundle]
type Rolls = [Roll]

data NotesAndCoins = NotesAndCoins { currency :: CurrencyType
    , bundles :: Bundles
    , rolls :: Rolls
    } deriving (Show, Ord, Eq)

data CurrencySupply k = CurrencySupply { currencies :: Map.Map k NotesAndCoins
    				} deriving (Show, Ord, Eq)

getWithKey :: CurrencySupply CurrencyType -> CurrencyType -> Maybe NotesAndCoins
getWithKey c t = Map.lookup t (currencies c)

findRollByType :: CoinType -> Rolls -> Maybe Roll
findRollByType ct rs = foldr(\r acc -> if ct == coinType r then Just r else acc) Nothing rs

findBundleByType :: NoteType -> Bundles -> Maybe Bundle
findBundleByType ct rs = foldr(\r acc -> if ct == noteType r then Just r else acc) Nothing rs

-- Functions based on http://en.literateprograms.org/Binary_Search_Tree_(Haskell) and
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
data Tree a = Node a (Tree a) (Tree a) | Leaf deriving (Show, Eq)
instance Functor Tree where
	fmap f Leaf = Leaf
	fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

leaf :: a -> Tree a
leaf a = Node a Leaf Leaf	

insert :: Ord b => a -> Tree a -> (a -> b) -> Tree a
insert n Leaf _ = leaf n
insert n (Node a left right) c
	| (c n) == (c a) = Node n left right
	| (c n) < (c a) = Node a (insert n left c) right
	| (c n) > (c a) = Node a left (insert n right c)

listToTree :: Ord b => [a] -> (a -> b) -> Tree a
listToTree [x] _ = leaf x 
listToTree (x:xs) c = insert x (listToTree xs c) c

findInTree :: Ord b => b -> Tree a -> (a -> b) -> Maybe a
findInTree x Leaf _ = Nothing
findInTree x (Node a left right) f
	| x == f a = Just a
	| x < f a = findInTree x left f
	| x > f a = findInTree x right f

combo :: [a] -> [a] -> [[a]]
combo a b = [ [x, y] | x <- a, y <- b]

bundleCombo :: Bundle -> Bundles
bundleCombo b 
	| numberOfNotes b == (-1) = []
	| otherwise = Bundle (numberOfNotes b) (noteType b) : bundleCombo (Bundle (numberOfNotes b - 1) (noteType b))

bundleToInt :: Bundle -> Int
bundleToInt a 
	 | noteType a == FiftyNote = numberOfNotes a * 50
	 | noteType a == TwentyNote = numberOfNotes a * 20
	 | otherwise = 0

bundlesToInt :: Bundles -> Int
bundlesToInt a = foldl1 (+) (map (bundleToInt) a)
	 
findInBundle :: Int -> Tree Bundles -> Maybe Bundles
findInBundle x n = findInTree x n bundlesToInt		
		
sn = Bundle 4 TwentyNote
sr = Roll 2 FiveCoin
wn = Bundle 1 FiftyNote
wr = Roll 1 FiveCoin
snc = NotesAndCoins AUD [sn] [sr]
wnc = NotesAndCoins AUD [wn] [wr]
tnc = NotesAndCoins AUD [sn, Bundle 5 FiftyNote] [sr, Roll 5 FiftyCoin]
l = Map.fromList [(AUD, snc)]
cs = CurrencySupply l
tree = Node 10 Leaf Leaf
cashMachine = listToTree (combo (bundleCombo sn) (bundleCombo wn)) bundlesToInt
						
instance Show CurrencyType where  
   show AUD = "$"  
   show EUR = "€"  
   show GBP = "₤"  

instance Show NoteType where  
   show FiftyNote = "50"  
   show TwentyNote = "20"

instance Show CoinType where  
   show FiftyCoin = "50"  
   show TwentyCoin = "20"
   show TenCoin = "10"
   show FiveCoin = "5"
