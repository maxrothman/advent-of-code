# Part 2

import re
from z3 import Real, Solver

test_raw = """19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
"""

def flatten(d):
    return [x for xs in d for x in xs]

def parse(line):
    return dict(zip(
        ['x', 'y', 'z', 'vx', 'vy', 'vz'],
        map(int, re.findall(r'-?\d+', line))))

rock = dict(zip(
    terms:=['xi', 'yi', 'zi', 'vx', 'vy', 'vz'],
    map(Real, terms)
))

hails = map(parse, test_raw.splitlines())

def sol(hails):
    equations = flatten([
        [
            rock['xi'] + rock['vx']*(t:=Real(f't{i}')) == h['x'] + h['vx']*t,
            rock['yi'] + rock['vy']*(t:=Real(f't{i}')) == h['y'] + h['vy']*t,
            rock['zi'] + rock['vz']*(t:=Real(f't{i}')) == h['z'] + h['vz']*t,
        ]
        for i, h in enumerate(hails)
    ])
    s = Solver()
    s.add(*equations)
    s.check()
    return sum([s.model()[rock[term]].as_long() for term in ['xi', 'yi', 'zi']])

sol(hails)
# 47

with open('../clj2023/resources/day24.txt') as f:
    sol(map(parse, f))