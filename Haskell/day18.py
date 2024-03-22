import numpy
import imageio
import matplotlib.pyplot as plt


# Map movement/walk direction to a unit vector
directions = {
    'U': (0, 1),
    'R': (1, 0),
    'D': (0, -1),
    'L': (-1, 0),
}

def sign(x):
    """The mathematical sign operation.

    :param x: The number to evaluate
    :return: -1 if x < 0, else +1
    """
    if x < 0: return -1
    else: return 1

def dir_to_coords(cc, dir, dist):
    """Perform a walk from the current coordinates in a given direction
    for the given amount of distance. Produce a list of walked coordinates
    in order.
    
    The given direction must be one of the following:
        UP='U', RIGHT='R', DOWN='D', LEFT='L'
    i.e. if cc=(0,0), dir='U', dist=3 then the result is
        [(0,0), (0,1), (0,2), (0,3)]

    :param cc: The current coordinates in the walk
    :param dir: The direction to walk in
    :param dist: The distance to walk
    """
    dist += 1
    # The direction offset vector
    do = directions[dir]
    # The target coordinates
    tc = (cc[0] + do[0] * dist, cc[1] + do[1] * dist)
    # A bool to decide which of the xs or ys lists should be
    # extended/padded to match the other in length
    isXMove: bool = do[0] != 0

    xCoords = [x for x in range(cc[0], tc[0], sign(tc[0] - cc[0]))]
    if not isXMove: xCoords = [cc[0]] * dist
    yCoords = [y for y in range(cc[1], tc[1], sign(tc[1] - cc[1]))]
    if isXMove: yCoords = [cc[1]] * dist
    return [i for i in zip(xCoords, yCoords)]


f = open("./day182.txt")
coords = []
currPos = (0, 0)
for line in f:
    elems = line.strip('\n').split(' ')
    dir, dist, col = elems[0], int(elems[1]), elems[2][2:][:-1]

    newCoords = dir_to_coords(currPos, dir, dist)
    coords.extend([(*c, col) for c in newCoords])
    currPos = newCoords[-1]

xs = numpy.array([x for x, _, _ in coords])
ys = numpy.array([y for _, y, _ in coords])
# y-coordinates are flipped; y=0 is at the top of the image
ys *= -1
colors = numpy.array([[
    int(col[0:2], 16), int(col[2:4], 16), int(col[4:], 16)]
    for _, _, col in coords
])
maxx, minx = max(xs), min(xs)
maxy, miny = max(ys), min(ys)

xs -= minx
ys -= miny

X,Y = maxx-minx+1, maxy-miny+1
image = numpy.zeros((Y, X, 3), dtype=numpy.uint8)
for idx, col in zip(zip(ys, xs), colors):
    image[idx] = col
imageio.imwrite('day18.png', image)

#plt.scatter(xs, ys)
#plt.savefig('day18.png')