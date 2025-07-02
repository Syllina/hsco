import re, math

def verify(k, b):
    for i in range(len(x)):
        # print(f"sample {i}: {k * x[i] + b}")
        if not(k * x[i] + b - 1 < y[i] and y[i] <= k * x[i] + b):
            return False
    return True

def interpolate(p, q):
    x1, y1 = p
    x2, y2 = q
    k = (y1 - y2) / (x1 - x2)
    b = y1 - x1 * k
    return k, b

atks = [
    (36, 269, 408, 683, 1019, 1199),
    (35, 266, 404, 677, 1009, 1186),
    (35, 264, 400, 671, 1000, 1176),
    (34, 261, 396, 664, 989, 1163),
    (34, 259, 393, 658, 980, 1153),
    (34, 256, 389, 652, 972, 1143),
    (34, 254, 385, 646, 963, 1133),
    (33, 251, 381, 639, 952, 1120),
    (33, 249, 378, 633, 944, 1110),
    (32, 246, 374, 626, 933, 1097),
    (32, 241, 367, 614, 916, 1077),
    (31, 232, 352, 589, 879, 1034),
    (28, 212, 322, 539, 803, 945)
]

def gen(base):
    base = base * 11
    a = math.floor(base * 0.224 + 0.5)
    b = math.floor(base * 0.34 + 0.5)
    c = math.floor(base * 0.54 + 0.5)
    d = math.floor(base * 0.79 + 0.5)
    e = math.floor(base * 0.91 + 0.5)
    p = math.floor(a * 0.134)
    return (p, a, b, c + p, d + 2 * p, e + 3 * p)

for base in range(100, 110):
    print(gen(base))
