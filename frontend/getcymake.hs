import Files.CymakePath (getCymake)

main = getCymake >>= putStrLn