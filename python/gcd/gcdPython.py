# Built-in
# Works with: Python version 3.7
# (Note that fractions.gcd is now deprecated in Python 3)
from math import gcd

#Iterative Euclid algorithm
def gcd_iter(u, v):
    while v:
        u, v = v, u % v
    return abs(u)

#Recursive Euclid algorithm
#Interpreter: Python 2.5
def gcd(u, v):
    return gcd(v, u % v) if v else abs(u)

#Iterative binary algorithm
#See The Art of Computer Programming by Knuth (Vol.2)   

def gcd_bin(u, v):
    u, v = abs(u), abs(v) # u >= 0, v >= 0
    if u < v:
        u, v = v, u # u >= v >= 0
    if v == 0:
        return u
   
    # u >= v > 0
    k = 1
    while u & 1 == 0 and v & 1 == 0: # u, v - even
        u >>= 1; v >>= 1
        k <<= 1
       
    t = -v if u & 1 else u
    while t:
        while t & 1 == 0:
            t >>= 1
        if t > 0:
            u = t
        else:
            v = -t
        t = u - v
    return u * k



#Tests
print("*** Tests gcd(), gcd_iter(), gcd_bin() ***")
#>>> gcd(0,0)
print("gcd(0,0): ",gcd(0,0));
#0
#>>> gcd(0, 10) == gcd(10, 0) == gcd(-10, 0) == gcd(0, -10) == 10
#True
print("gcd(0,10): ",gcd(0,10));
#>>> gcd(9, 6) == gcd(6, 9) == gcd(-6, 9) == gcd(9, -6) == gcd(6, -9) == gcd(-9, 6) == 3
print("gcd(9,6): ",gcd(9,6));
print("gcd_iter(9,6): ",gcd_iter(6,9));
#True
#>>> gcd(8, 45) == gcd(45, 8) == gcd(-45, 8) == gcd(8, -45) == gcd(-8, 45) == gcd(45, -8) == 1
print("gcd_iter(8,45): ",gcd_iter(8,45));
#True
#>>> gcd(40902, 24140) # check Knuth :)
print("Knuth gcd_bin(40902, 24140): ",gcd_bin(40902, 24140));
#34
