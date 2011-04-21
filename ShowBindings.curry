--- Internal library with auxiliary operations to show
--- the main goal with the bindings of free variables:

module ShowBindings where

show1 (result,[n1],v1) =
  show result ++ " where {" ++
  concat [n1,"=",show v1] ++ "}"

show2 (result,[n1,n2],v1,v2) =
  show result ++ " where {" ++
  concat [n1,"=",show v1,"; ",n2,"=",show v2] ++ "}"

show3 (result,[n1,n2,n3],v1,v2,v3) =
  show result ++ " where {" ++
  concat [n1,"=",show v1,"; ",n2,"=",show v2,"; ",n3,"=",show v3] ++ "}"

show4 (result,[n1,n2,n3,n4],v1,v2,v3,v4) =
  show result ++ " where {" ++
  concat [n1,"=",show v1,"; ",n2,"=",show v2,"; ",n3,"=",show v3,
          "; ",n4,"=",show v4] ++ "}"
