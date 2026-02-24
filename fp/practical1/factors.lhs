Practical 1: Factoring Numbers

Here is a simple method for finding the smallest prime factor of a positive
integer:

> factor :: Integer -> (Integer, Integer)
> factor n = factorFrom 2 n

> factorFrom :: Integer -> Integer -> (Integer, Integer)
> factorFrom m n | r == 0    = (m,q)
>                | otherwise = factorFrom (m+1) n
>    where (q,r) = n `divMod` m

for example

*Main> factor 7654321
(19,402859)

because 

*Main> 19 * 402859
7654321

Repeatedly extracting the smallest factor will return a list
of prime factors:

> factors :: Integer -> [Integer]
> factors n = factorsFrom 2 n

> factorsFrom :: Integer -> Integer -> [Integer]
> factorsFrom m n | n == 1    = []
>                 | otherwise = p:factorsFrom p q
>    where (p,q) = factorFrom m n

for example

*Main> factor 123456789
(3,41152263)
*Main> factors 123456789
[3,3,3607,3803]

Exercise 1: factor 0 should return the value (2,0) because the remainder 
and quotient upon diving 0 by 2 are 0 and 0 respectively. factor 1 on 
the other hand, should result in an infinite loop because 1 `divMod` 2 
gives (0,1) (i.e. r /= 0) which would then go on to determine factorFrom 3 1,
which also gives the remainder 1 and so on, resulting in an infinite loop 
because none of the numbers can divide 1. 

Exercise 2: On running factor 0 and factor 1 in GHCi, we get (2,0) and an 
infinite loop respectively.

Exercise 3: To show that: the smallest factor of n (say, f) cannot be in the range
sqrt(n) < f < n, we can use proof by contradiction. Let us assume that there exists
a smallest factor of f such that sqrt (n) < f < n. This means that n can be expressed
as n = f * k, where k is another integer factor of n. Since f > sqrt(n), it follows 
that k must be less than sqrt(n) (because if both f and k were greater than sqrt(n) 
then their product would be greater than the square of sqrt(n) or n). This contradicts 
our assumption because this makes k the smallest factor of n instead of f. Therefore,
our initial assumption is false, and we can conclude that there does not exist a
smallest factor f of n, such that sqrt(n) < f < n. Hence, the smallest factor of n 
must be less than or equal to sqrt(n).

> factor1 :: Integer -> (Integer, Integer)
> factor1 n = factorFrom1 2 n 

> factorFrom1 :: Integer -> Integer -> (Integer, Integer)
> factorFrom1 m n | r == 0     = (m,q)
>                 | n <= m*m   = (n,1)
>                 | otherwise  = factorFrom1 (m+1) n
>    where (q,r) = n `divMod` m

Yes, the order of the guards matters because if we check for n <= m*m after 
the otherwise guard, we may end up in an infinite loop for cases where n is 
a prime number. Meanwhile, it doesn't matter if we check for r == 0 first or 
n <= m*m first, because if either condition is true, we would want to return 
the corresponding result. 

In the worst case scenario, the maximum number of recursive calls made by 
factorFrom1 to determine the smallest factor of a number n would be approximately 
sqrt(n) - 1. This is because n cannot have any smallest factors greater than 
sqrt(n) (as shown above). 

Exercise 4: Replacing n <= m*m with q < m defines the same function and is more 
efficient because in the original version, we have to compute m*m which is a 
multiplication operation. In the revised version, we only need to compare q with m,
which is a simpler operation, thus, more efficient.

> factor2 :: Integer -> (Integer, Integer)
> factor2 n = factorFrom2 2 n 

> factorFrom2 :: Integer -> Integer -> (Integer, Integer)
> factorFrom2 m n | r == 0    = (m,q)
>                | q < m     = (n,1)
>                | otherwise = factorFrom2 (m+1) n
>    where (q,r) = n `divMod` m

Exercise 5: If we treated 2 as a special case, and the odd numbers 3, 5, 7... as 
trial divisors, we could halve the number of trial divisors we need to check. This is
because if a number n is even, we can directly return 2 as its smallest factor without
checking any other divisors. If n is odd, we can skip all even trial divisors
(since they cannot divide an odd number) and only check odd trial divisors starting
from 3. This would effectively reduce the number of trial divisors we need to check
by half, leading to a more efficient factorization process.

> factor3 :: Integer -> (Integer, Integer)
> factor3 n | even n = (2, n `div` 2)
>           | otherwise = factorFrom3 3 n

> factorFrom3 :: Integer -> Integer -> (Integer, Integer)
> factorFrom3 m n | r == 0    = (m,q)
>                 | q < m     = (n,1)
>                 | otherwise = factorFrom3 (m+2) n
>    where (q,r) = n `divMod` m 

Exercise 6: Testing factor3:
ghci> factor3 1
(1,1)
(0.00 secs, 69,760 bytes)

ghci> factor3 13
(13,1)
(0.00 secs, 70,928 bytes)

ghci> factor3 99999999192818231907
(3,33333333064272743969)
(0.01 secs, 84,704 bytes)

ghci> factor3 1827837990917261619829187918738120997882637
(241,7584390003806064812569244476091788372957)
(0.01 secs, 160,760 bytes)

ghci> factor3 972797453539
(972797453539,1)
(0.18 secs, 201,283,728 bytes)

Exercise 7: 

> factor4 :: Integer -> (Integer, Integer)
> factor4 n  | n `mod` 2 == 0 = (2, n `div` 2)
>            | n `mod` 3 == 0 = (3, n `div` 3)
>            | otherwise      = factorFrom4 5 n 2

> factorFrom4 :: Integer -> Integer -> Integer -> (Integer, Integer)
> factorFrom4 m n s | r == 0    = (m,q)
>                   | q < m     = (n,1)
>                   | otherwise = factorFrom4 (m + s) n (6 - s)
>    where (q,r) = n `divMod` m

Testing factor4: 
ghci> factor4 1
(1,1)
(0.00 secs, 69,888 bytes)

ghci> factor4 13
(13,1)
(0.00 secs, 70,648 bytes)

ghci> factor4 99999999192818231907
(3,33333333064272743969)
(0.00 secs, 84,408 bytes)

ghci> factor4 1827837990917261619829187918738120997882637
(241,7584390003806064812569244476091788372957)
(0.00 secs, 146,704 bytes)

(We can see that this is more efficient than factor3 for larger numbers.)

ghci> factor4 972797453539
(972797453539,1)
(0.15 secs, 157,886,384 bytes)

Exercise 8: The problem with the idea of using only prime numbers as trial divisors
is that generating a list of prime numbers (or having to check all primes to get to 
a final prime) takes a lot of time and space/ memory. 

Optional Exercises:
Exercise 9: 

> factors2 :: Integer -> [Integer]
> factors2 n | n == 1   = []
>            | n `mod` 2 == 0 = 2: factors2 (n `div` 2)
>            | n `mod` 3 == 0 = 3: factors2 (n `div` 3)
>            | otherwise = factorsFrom2 5 n 2


> factorsFrom2 :: Integer -> Integer -> Integer -> [Integer]
> factorsFrom2 m n s | n == 1   = []
>                    | otherwise = p : factorsFrom2 p q s'
>   where (p, q) = factorFrom4 m n s
>         s' = if p `mod` 6 == 5 then 2 else 4

Exercise 10:
Testing factors2 against factors: 

ghci> factors 123456789
[3,3,3607,3803]
(0.01 secs, 1,448,000 bytes)
ghci> factors2 123456789
[3,3,3607,3803]
(0.00 secs, 656,456 bytes)

(Here, we can see that factors2 is more efficient, both in terms of time and
memory usage.)

ghci> factors 912818231907
[3,10847,28051327]
(6.55 secs, 10,098,558,240 bytes)
ghci> factors2 912818231907
[3,10847,28051327]
(0.01 secs, 1,816,432 bytes)

(Again, factors2 is much more efficient for larger numbers, when compared to factors.)

Testing on Jevons' number:
ghci> factors 8616460799
[89681,96079]
(0.05 secs, 34,664,496 bytes)
ghci> factors2 8616460799
[89681,96079]
(0.03 secs, 14,425,496 bytes)

(Here, again, factors2 shows better efficiency in both time and memory usage 
compared to factors.)

