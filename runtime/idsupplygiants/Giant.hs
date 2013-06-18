{-# LANGUAGE NoMonomorphismRestriction #-}
-- needed to define toplevel isomorphisms
module Giant where
-- cabal install data-ordlist is required before importing this
--import Data.List.Ordered
--import Data.List hiding (unionBy)
--import System.CPUTime

--import Visuals

class Eq n => N n where

  e :: n
  o,o',i,i' :: n -> n
  o_ :: n -> Bool

  e_,i_ :: n -> Bool
  e_ x = x == e
  i_ x = not (e_ x || o_ x)

  s,s' :: n -> n
  
  s x | e_ x = o x
  s x | o_ x = i (o' x)
  s x | i_ x = o (s (i' x))
  
  s' x | x == o e = e
  s' x | i_ x = o (i' x)
  s' x | o_ x = i (s' (o' x))

  allFrom :: n -> [n]
  allFrom x = iterate s x

  add :: n -> n -> n
  add x y | e_ x = y
  add x y | e_ y  = x
  add x y | o_ x && o_ y = i (add (o' x) (o' y))
  add x y | o_ x && i_ y = o (s (add (o' x) (i' y)))
  add x y | i_ x && o_ y = o (s (add (i' x) (o' y)))
  add x y | i_ x && i_ y = i (s (add (i' x) (i' y)))
  
  sub :: n -> n -> n
  sub x y | e_ y = x
  sub y x | o_ y && o_ x = s' (o (sub (o' y) (o' x))) 
  sub y x | o_ y && i_ x = s' (s' (o (sub (o' y) (i' x))))
  sub y x | i_ y && o_ x = o (sub (i' y) (o' x))  
  sub y x | i_ y && i_ x = s' (o (sub (i' y) (i' x))) 

  mul :: n -> n -> n
  mul x _ | e_ x = e
  mul _ y | e_ y = e
  mul x y = s (m (s' x) (s' y)) where
    m x y | e_ x = y
    m x y | o_ x = o (m (o' x) y)
    m x y | i_ x = s (add y  (o (m (i' x) y)))

  db,hf :: n -> n 
  db = s' . o
  hf = o' .s 

instance N Integer where
  e = 0
  
  o_ x = odd x
  
  o  x = 2*x+1
  o' x | odd x && x >  0 = (x-1) `div` 2
  
  i  x = 2*x+2
  i' x | even x && x > 0 = (x-2) `div` 2
{-
data B = B | O B | I B deriving (Show, Read, Eq)

instance N B where
  e = B
  o = O
  i = I
  
  o' (O x) = x
  i' (I x) = x
  
  o_ (O _) = True
  o_ _ = False
-}
view :: (N a,N b) => a -> b
view x | e_ x = e
view x | o_ x = o (view (o' x))
view x | i_ x = i (view (i' x))

--t :: (N n) => n -> T
--t = view

--b :: (N n) => n -> B
--b = view

n :: (N n) => n -> Integer
n = view

class N n => OrdN n where
  cmp :: n -> n -> Ordering
  cmp x y | e_ x && e_ y = EQ
  cmp x _ | e_ x = LT
  cmp _ y | e_ y = GT
  cmp x y | o_ x && o_ y = cmp (o' x) (o' y)
  cmp x y | i_ x && i_ y = cmp (i' x) (i' y)
  cmp x y | o_ x && i_ y = down (cmp (o' x) (i' y)) where
    down EQ = LT
    down r = r
  cmp x y | i_ x && o_ y = up (cmp (i' x) (o' y)) where
    up EQ = GT
    up r = r

  min2,max2 :: n -> n -> n
  min2 x y = if LT==cmp x y then x else y
  max2 x y = if LT==cmp x y then y else x

--instance OrdN Integer
--instance OrdN B
instance OrdN T
{-
class OrdN n => IntegralN n where
  pow :: n->n->n
  pow _ y | e_ y = o e
  pow x y | o_ y = mul x (pow (mul x x) (o' y))
  pow x y | i_ y = mul (mul x x) (pow (mul x x) (i' y)) 

  exp2 :: n->n
  exp2 x | e_ x = o e
  exp2 x = db (exp2 (s' x)) 
  
  leftshiftBy :: n->n->n
  leftshiftBy x y = mul y (exp2 x)

  div_and_rem :: n->n->(n,n)
  
  div_and_rem x y | LT == cmp x y = (e,x)
  div_and_rem x y | not (e_ y) = (q,r) where 
    (qt,rm) = divstep x y
    (z,r) = div_and_rem rm y
    q = add (exp2 qt) z
    
    divstep :: IntegralN n => n->n->(n,n)
    divstep n m = (q, sub n p) where
      q = try_to_double n m e
      p = mul (exp2 q) m    
    
      try_to_double x y k = 
        if (LT==cmp x y) 
          then s' k
          else try_to_double x (db y) (s k)   
          
  divide,remainder :: n->n->n
  
  divide n m = fst (div_and_rem n m)
  remainder n m = snd (div_and_rem n m)
-}
data T = T | V T [T] | W T [T] deriving (Eq,Show,Read)

instance N T where
  e = T
  
  o T = V T []
  o (V x xs) = V (s x) xs
  o (W x xs) = V T (x:xs)

  i T = W T []
  i (V x xs) = W T (x:xs)
  i (W x xs) = W (s x) xs
  
  o' (V T []) = T
  o' (V T (x:xs)) = W x xs
  o' (V x xs) = V (s' x) xs

  i' (W T []) = T
  i' (W T (x:xs)) = V x xs
  i' (W x xs) = W (s' x) xs
  
  o_ (V _ _ ) = True
  o_ _ = False
{-
class IntegralN n => HeredN n where
  v :: (n,[n]) -> n
  v (x,[]) = otimes (s x) e
  v (x,y:xs) = otimes (s x) (w (y,xs))

  w :: (n,[n]) -> n
  w (x,[]) = itimes (s x) e
  w (x,y:xs) = itimes (s x) (v (y,xs))

  v' :: n-> (n,[n])
  v' z =  (s' (ocount z), f (otrim z)) where
    f y | e_ y = []
    f y =  (fst x:snd x) where x = w' y

  w' :: n-> (n,[n])
  w' z =  (s' (icount z), f (itrim z)) where
    f y | e_ y = []
    f y = fst x:snd x where x = v' y
  
  v_ :: n -> Bool
  v_ z = e /= ocount z
  w_ :: n -> Bool
  w_ z = e /= icount z

  ocount,icount,otrim,itrim :: n->n
  
  ocount x | o_ x = s (ocount (o' x))
  ocount _ = e
  
  icount x | i_ x = s (icount (i' x))
  icount _ = e
  
  otrim x | o_ x = otrim (o' x)
  otrim x = x
  
  itrim x | i_ x = itrim (i' x)
  itrim x = x

  otimes,itimes :: n->n->n
  
  otimes x y | e_ x = y
  otimes x y = otimes (s' x) (o y)
   
  itimes x y | e_ x = y
  itimes x y = itimes (s' x) (i y)

instance HeredN Integer
instance HeredN B
instance HeredN T where
  ocount (V x _) = s x
  ocount _ = T

  icount (W x _) = s x
  icount _ = T
  
  otrim (V _ []) = T
  otrim (V _ (x:xs)) = W x xs
  otrim x = x
  
  itrim (W _ []) = T
  itrim (W _ (x:xs)) = V x xs
  itrim x = x
  
  otimes T y = y
  otimes n T = V (s' n) []
  otimes n (V y ys) = V (add n y) ys
  otimes n (W y ys) = V (s' n) (y:ys)
  
  itimes T y = y
  itimes n T = W (s' n) []
  itimes n (W y ys) = W (add n y) ys
  itimes n (V y ys) = W (s' n) (y:ys)    

instance IntegralN Integer
instance IntegralN B
instance IntegralN T where
  exp2 T = V T []
  exp2 x = s (V (s' x) [])

  leftshiftBy _ T = T
  leftshiftBy n k = s (otimes n (s' k))

leftshiftBy' _ T = T
leftshiftBy' T k = k
leftshiftBy' n (V T []) = s (V (s' n) [])
leftshiftBy' n k = s (s (itimes n (s' (s' k))))

instance Ord B where
  compare = cmp
instance Num B where
  (+) = add
  (-) = sub
  (*) = mul
  fromInteger = b
  abs = id
  signum B = B
  signum _ = O B
instance Integral B where
  quot = divide
  div = divide
  rem = remainder
  mod = remainder
  quotRem = div_and_rem
  divMod = div_and_rem
  toInteger = n  
instance Real B where
  toRational = toRational . n
instance Enum B where
  fromEnum = fromEnum . n 
  toEnum = b . f where
    f:: Int->Integer
    f = toEnum
  succ = s
  pred = s'
-}  

instance Ord T where
  compare = cmp

{-
instance Num T where
  (+) = add
  (-) = sub
  (*) = mul
  fromInteger = t
  abs = id
  signum T = T
  signum _ = V T []
instance Integral T where
  quot = divide
  div = divide
  rem = remainder
  mod = remainder
  quotRem = div_and_rem
  divMod = div_and_rem
  toInteger = n  
instance Real T where
  toRational = toRational . n
instance Enum T where
  fromEnum = fromEnum . n 
  toEnum = t . f where
    f:: Int->Integer
    f = toEnum
  succ = s
  pred = s'


fermat n = s (exp2 (exp2 n))

mersenne p = s' (exp2 p)

perfect p = s (V q [q]) where q = s' (s' p)

-- its exponent
prime48 = 57885161 :: Integer
prime45 = 43112609 :: Integer

-- the actual Mersenne primes
mersenne48 = s' (exp2 (t p)) where 
  p = prime48::Integer
  
mersenne45 = s' (exp2 (t p)) where 
  p = prime45::Integer

perfect48 = perfect (t prime48)
perfect45 = perfect (t prime45)

genFermatPrime = s (leftshiftBy n k) where 
  n = t (9167433::Integer)
  k = t (27653::Integer)

cullenPrime = s (leftshiftBy n n) where n = t (6679881::Integer)

woodallPrime = s' (leftshiftBy n n) where n = t (3752948::Integer)

prothPrime = s (leftshiftBy n k) where 
  n = t (13018586::Integer)
  k = t (19249::Integer)

sophieGermainePrime = s' (leftshiftBy n k) where 
  n = t (666667::Integer)
  k = t (18543637900515::Integer)

twinPrimes = (s' m,s m) where 
  n = t (666669::Integer)
  k = t (3756801695685::Integer)
  m = leftshiftBy n k

class HeredN n => Collections n where
  c :: n->n->n
  c',c'' :: n->n
 
  c x y = mul (exp2 x) (o y)
  
  c' x | not (e_ x) = if o_ x then e else s (c'  (hf x))
  c'' x | not (e_ x) = if o_ x then o' x else c'' (hf x)

  to_list :: n->[n]
  to_list x | e_ x = []
  to_list x = (c' x) : (to_list (c'' x))

  from_list:: [n]->n
  from_list [] = e
  from_list (x:xs) = c x (from_list xs)

  list2mset, mset2list, list2set, set2list :: [n]->[n]
 
  list2mset ns = tail (scanl add e ns)
  mset2list ms =  zipWith sub ms (e:ms)
    
  list2set = (map s') . list2mset . (map s)
  set2list = (map s') . mset2list . (map s) 

  to_mset, to_set :: n->[n]
  from_mset, from_set :: [n]->n

  to_mset = list2mset . to_list
  from_mset = from_list . mset2list
  
  to_set = list2set . to_list
  from_set = from_list . set2list

instance Collections B
instance Collections Integer
instance Collections T where
  c = cons where
    cons n y = s (otimes n (s' (o y)))
    
  c' = hd where  
    hd z | o_ z = T
    hd z = s x where
      V x _ = s' z
      
  c'' = tl where
    tl z | o_ z = o' z 
    tl z = f xs where
      V _ xs = s' z
 
      f [] = T
      f (y:ys) = s (i' (W y ys)) 

class Collections n => BitwiseOperations n where 
  l_op :: ([n]->[n]->[n])->n->n->n
  l_op op x y = from_set (op (to_set x) (to_set y))

  l_and,l_or,l_xor,l_dif:: n->n->n
  l_and = l_op (isectBy cmp)
  l_or = l_op (unionBy cmp)
  l_xor = l_op (xunionBy cmp)
  l_dif = l_op (minusBy cmp)

  l_ite :: n->n->n->n
  l_ite x y z = from_set (ite (to_set x) (to_set y) (to_set z)) where
    ite d a b =  xunionBy cmp e b where
      c = xunionBy cmp a b
      e = isectBy cmp c d

  l_not :: Integer->n->n
  l_not l x |xl<=l = from_set (minusBy cmp ms xs) where
    xs = to_set x
    xl = genericLength xs
    ms = genericTake l (allFrom e)

  neg :: n->n
  neg x | e_ x = e
  neg x | o_ x = i (neg (o' x))
  neg x | i_ x = o (neg (i' x)) 

  conj :: n->n->n
  conj x _ | e_ x = e
  conj _ y | e_ y = e
  conj x y | o_ x && o_ y = o (conj (o' x) (o' y))
  conj x y | o_ x && i_ y = o (conj (o' x) (i' y))
  conj x y | i_ x && o_ y = o (conj (i' x) (o' y))
  conj x y | i_ x && i_ y = i (conj (i' x) (i' y))

  disj :: n->n->n
  disj x y = neg (conj (neg x) (neg y))

  leq :: n->n->n
  leq x y = neg (conj x (neg y)) 

  eq :: n->n->n
  eq x y = conj (leq x y) (leq y x)

instance BitwiseOperations B
instance BitwiseOperations Integer
instance BitwiseOperations T

data Iso a b = Iso (a->b) (b->a)

from (Iso f _) = f
to (Iso _ f') = f'

as that this x = to that (from this x)

lend1 op1 (Iso f f') x = f' (op1 (f x))
lend2 op2 (Iso f f') x y = f' (op2 (f x) (f y))

nat = Iso id id
list = Iso from_list to_list
mset = Iso from_mset to_mset
set = Iso from_set to_set

class Collections n => SpecialComputations n where
  dual :: n->n
  dual x | e_ x = e
  dual x | o_ x = i (dual (o' x))
  dual x | i_ x = o (dual (i' x))

  bitsize :: n->n
  bitsize x | e_ x = e
  bitsize x | o_ x = s (bitsize (o' x))
  bitsize x | i_ x = s (bitsize (i' x))

  -- representation size - defaults to bitsize
  repsize :: n->n
  repsize = bitsize

  decons ::n->(n,n)
  cons :: (n,n)->n
   
  decons z | o_ z = (x,y) where
    x0 = s' (ocount z)
    y = otrim z
    x = if e_ y then (s'.o) x0 else x0 
  decons z | i_ z = (x,y) where
    x0 = s' (icount z)
    y = itrim z
    x = if e_ y then (s'.i) x0 else x0 
    
  cons (x,y) | e_ x && e_ y = s e
  cons (x,y) | o_ x && e_ y = itimes (s (i' (s x))) e
  cons (x,y) | i_ x && e_ y = otimes (s (o' (s x))) e
  cons (x,y) | o_ y = itimes (s x) y
  cons (x,y) | i_ y = otimes (s x) y

  to_list' :: n->[n]
  to_list' x | e_ x = []
  to_list' x = hd : (to_list' tl) where (hd,tl)=decons x

  from_list' :: [n]->n
  from_list' [] = e
  from_list' (x:xs) = cons (x,from_list' xs)

  to_mset', to_set' :: n->[n]
  from_mset', from_set' :: [n]->n

  to_mset' = list2mset . to_list'
  from_mset' = from_list' . mset2list
  
  to_set' = list2set . to_list'
  from_set' = from_list' . set2list

instance SpecialComputations Integer
instance SpecialComputations B
instance SpecialComputations T where
  bitsize = tbitsize where
    tbitsize T = T
    tbitsize (V x xs) = s (foldr add1 x xs)
    tbitsize (W x xs) = s (foldr add1 x xs)
    
    add1 x y = s (add x y)
    
  dual = tdual where
    tdual T = T
    tdual (V x xs) = W x xs
    tdual (W x xs) = V x xs 
    
  repsize = tsize where
    tsize T = T
    tsize (V x xs) = s (foldr add T (map tsize (x:xs)))
    tsize (W x xs) = s (foldr add T (map tsize (x:xs)))

  decons (V x []) = ((s'.o) x,T)
  decons (V x (y:ys)) = (x,W y ys)
  decons (W x []) = ((s'.i) x,T)
  decons (W x (y:ys)) = (x,V y ys)

  cons (T,T) = V T []
  cons (x,T) | o_ x =  W (i' (s x)) []
  cons (x,T) | i_ x = V (o' (s x)) []
  cons (x,V y ys) = W x (y:ys)
  cons (x,W y ys) = V x (y:ys)

class SpecialComputations n => D n where
  enc :: (n,n) -> n
  dec :: n ->(n,n)
  
  enc (x,y) | e_ x && e_ y = e 
  enc (x,y) = s' (cons (x,y))
  
  dec n | e_ n = (e,e)
  dec n  = decons (s n)

instance D Integer
instance D B
instance D T where

class SpecialComputations n => Benchmarks n where
  primes :: [n]

  primes = s (s e) : filter is_prime (odds_from (s (s (s e)))) where
    odds_from x = x : odds_from (s (s x))
    
    is_prime p = [p]==to_primes p
    
    to_primes n = to_factors n p ps where
       (p:ps) = primes
    
    to_factors n p ps | cmp (mul p p) n == GT = [n]
    to_factors n p ps | e_ r =  p : to_factors q p ps where
       (q,r) = div_and_rem n p
    to_factors n p (hd:tl) = to_factors n hd tl

  lucas_lehmer :: n -> Bool
  lucas_lehmer p = e_ y where
    p_2 = s' (s' p)
    four = i (o e)
    m  = exp2 p
    m' = s' m
    y = f p_2 four
 
    f k n | e_ k = n
    f k n = r where 
      x = f (s' k) n
      y = s' (s' (mul x x))
      --r = remainder y m'
      r = fastmod y m 

    -- fast computation of k mod 2^p-1  
    fastmod k m | k == s' m = e
    fastmod k m | LT == cmp k m = k
    fastmod k m = fastmod (add q r) m where
       (q,r) = div_and_rem k m
  
  -- exponents leading to Mersenne primes
  mersenne_prime_exps :: [n]
  mersenne_prime_exps = filter lucas_lehmer primes
  
  -- actual Mersenne primes
  mersenne_primes :: [n]
  mersenne_primes = map f mersenne_prime_exps where
    f p = s' (exp2 p)   

  ack :: n->n->n
  ack x n | e_ x = s n
  ack m1 x | e_ x = ack (s' m1) (s e)
  ack m1 n1 = ack (s' m1) (ack m1 (s' n1))

  syracuse  :: n->n
  -- n->c'' (3n+2)
  syracuse n = c'' (add n (i n))

  nsyr :: n->[n]
  nsyr n | e_ n = [e]
  nsyr n = n : nsyr (syracuse n)

instance Benchmarks Integer
instance Benchmarks B
instance Benchmarks T


benchmark mes f = do
  x<-getCPUTime
  print f
  y<-getCPUTime
  let time=(y-x) `div` 1000000000
  return (mes++" :time="++(show time))

bm1t = benchmark "ack 3 7 on t" (ack (t (toInteger 3)) (t (toInteger 7)))
bm1b = benchmark "ack 3 7 on b" (ack (b (toInteger 3)) (b (toInteger 7)))
bm1n = benchmark "ack 3 7 on n" (ack (n (toInteger 3)) (n (toInteger 7)))

bm2t = benchmark "exp2 t" (exp2 (exp2 (t (toInteger 14))))
bm2b = benchmark "exp2 b" (exp2 (exp2 (b (toInteger 14))))
bm2n = benchmark "exp2 n" (exp2 (exp2 (n (toInteger 14))))

bm3 tvar = benchmark "sparse_set on a type" (n (bitsize (from_set ps)))
  where ps = map tvar [101,2002..100000]

bm4t =benchmark "bitsize of Mersenne 45"  (n (bitsize mersenne45))
bm5t = benchmark "bitsize of Perfect 45" (n (bitsize perfect45))

bm6t = benchmark "large leftshiftBy" (leftshiftBy n n) where
  n = t prime45

bm3' tvar m = benchmark "to/from list on a type" 
              (n (bitsize (from_list (to_list (from_list ps)))))
  where ps = map tvar [101,2002..3000+m]

bm3'' tvar m = benchmark "to/from list on a type" 
              (n (bitsize (from_list (to_list (from_list ps)))))
  where ps = map (dual.tvar) [101,2002..3000+m]

bm7t = benchmark "primes on t" 
  (last (take 100 ps)) where ps = primes :: [T]
bm7b = benchmark "primes on b" 
  (last (take 100 ps)) where ps = primes :: [B]
bm7n = benchmark "primes on n" 
  (last (take 100 ps)) where ps = primes :: [Integer]

bm8t = benchmark "mersenne on t" 
  (last (take 7 ps)) where ps = mersenne_primes :: [T]
bm8b = benchmark "mersenne on b" 
  (last (take 7 ps)) where ps = mersenne_primes :: [B]
bm8n = benchmark "mersenne on n" 
  (last (take 7 ps)) where ps = mersenne_primes :: [Integer]

test_syr tvar m = maximum (map length (map (nsyr . tvar) [0..m]))

compress_syr tvar m = r where
  nss = map (nsyr . tvar) [0..m]
  r = maximum (map (n.bitsize) (map from_list nss))

-- overflows for m>2 except for tvar=t
compress_syr_twice tvar m = r  where
  nss = map (nsyr . tvar) [0..m]
  r = (n.bitsize) (from_list (map from_list nss))
  
bm9 tvar = benchmark "test syracuse" (test_syr tvar 2000)

bm10 tvar = benchmark "compress syracuse" (compress_syr tvar 100)

bm11 tvar = benchmark "compress syracuse_twice" r where
            r = compress_syr_twice tvar 20

tsize T = 1
tsize (V x xs) = 1+ sum (map tsize (x:xs))
tsize (W x xs) = 1+ sum (map tsize (x:xs))

kth _ k x | e_ k  = x
kth f k x  = f (kth f (s' k) x)

-- relation between iterations of o,i and power of 2 
a1 k = pow (i e) k == s (kth o k e) 
a2 k = pow (i e) k == s (s (kth i (s'  k) e))

-- relations between power operations and multiplication
a3 n b = (u==v,u,v) where
 m = pow (i e) n
 u = kth o n b
 v = s' (mul m (s b))
 
a4 x y = (a==b,a,b) where 
  a = mul (pow (i e) x) y
  b = s (kth o x (s' y))

-- experiments
syracuse' x = c'' (syradd x) 
syradd x | e_ x  = i x
syradd x | o_ x = o (s (add x (o' x)))
syradd x | i_ x = i (s (add x (i' x)))
-}
