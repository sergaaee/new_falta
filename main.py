from constants import *

def limit(p1, p2, x):
    return p1 if x <= p1 else x if x < p2 else p2

timer = Timer()
print(timer.tas)
