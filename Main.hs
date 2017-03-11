module Main where
    import Bowling

    join:: Maybe a -> a
    join (Just x) = x

    main::IO()
    main = do

        print $ join $ fmap score $ toFrames (take 12 $ repeat 10)
