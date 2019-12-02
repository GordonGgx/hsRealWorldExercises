module Main where

import System.Environment

interactWith function inputFile outputFile=do
    content<-readFile inputFile
    writeFile outputFile $ function content

-- main :: IO ()
-- main = mainWith myFunction
--     where mainWith function =do
--             args<-getArgs 
--             case args of 
--                 [input,output]->interactWith function input output
--                 _->putStrLn "期望能有两个参数"
--           myFunction=fixLines
main :: IO()
main=do
    args<-getArgs 
    case args of
        [input]->do
            content<-readFile input
            printFirstWorld $ lines content
            where printFirstWorld xs=case xs of []->return ()                                            
                                                (x:xs')->do
                                                    print (head $ words x)
                                                    printFirstWorld xs'
