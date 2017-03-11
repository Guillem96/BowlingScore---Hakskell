module Bowling where
        data Frame =     Open      { pins1::Int
                                    , pins2::Int
                                    }
                        | Spare     { pins1::Int
                                    , bonus::Int
                                    }
                        | Strike    { bonus1::Int
                                    , bonus2::Int
                                    }
                        deriving(Eq, Show)

        toFrames::[Int]->Maybe [Frame]
        toFrames pins = go 1 pins
            where
                go 10 [x, y]
                    | x + y < 10    = Just [Open x y]
                    | otherwise     = Nothing
                go 10 [x, y, z]
                    | x == 10       = Just [Strike y z]
                    | x + y == 10   = Just [Spare x z]
                    | otherwise     = Nothing
                go n (x:y:z:zs)
                    | x ==  10      = fmap(Strike y z:) $ go (n + 1) (y:z:zs)
                    | x + y == 10   = fmap(Spare x z:)  $ go (n + 1) (z:zs)
                    | x + y < 10    = fmap(Open x y:)   $ go (n + 1)  (z:zs)
                    | otherwise     = Nothing
                go _ _ = Nothing


        frameScore::Frame->Int
        frameScore (Open x y) = x + y
        frameScore (Strike x y) = 10 + x + y
        frameScore (Spare x y) = 10 + y

        mySumatori::[Int]->Int
        mySumatori (x:xs) = x + mySumatori(xs)
        mySumatori [] = 0

        score::[Frame] -> Int
        score = mySumatori.map frameScore
