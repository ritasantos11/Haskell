--65
module Main where
	main:: IO()
	main = do { x <- getContents
                putStrLn (show (length (lines x)));
                putStrLn (show (length (words x)));
                putStrLn (show (length (x)));
            }