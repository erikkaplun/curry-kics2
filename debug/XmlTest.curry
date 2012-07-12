import XML

main = do
  putStrLn "Read test.xml..."
  xml <- readXmlFile "test.xml"
  putStrLn "Write test1.xml..."
  writeXmlFile "test1.xml" xml
