"""Test the BFS reachability representation of the AoC day 21 problem.
"""

import math
import copy
s = """
.#...#.
.......
..#.#..
...S...
..#.#..
.......
.#...#.
"""
# Choose width and height each >= 2 for sane visualizations
width, height = 5, 5

init_val = 0
nr_steps = 3 + 2 * 7 #65 + 1 * 131
min_val = int(math.floor((init_val - nr_steps) / 6))
frame_interval = 100
output_path = "day21/day21.gif"
output_path_diff = "day21/day21_difference_lists.png"
print_to_terminal = True
display_animation = True


s_new = ""
rows = s.split("\n")[1:-1]
start = None
# Tile the input grid into a larger input grid.
for h in range(height):
    for row_idx, row in enumerate(rows):
        for w in range(width):
            row_str = copy.copy(row)
            if not (w == math.floor((width - 1) / 2.0) and h == math.ceil((height - 1) / 2.0)):
                row_str = row_str.replace("S", ".")
            if "S" in row_str:
                start = (w * len(row) + row_str.find("S"), h * len(rows) + row_idx)
            s_new += row_str
        s_new += """
"""
s = s_new
rows = s.split("\n")[:-1]

strict_fringe = { start }   # The fringe for a grid where the edges are hard walls
cyclic_fringe = { start }   # The fringe for a grid where the edges loop around to the other side
width, height = len(rows[0]), len(rows)

def coord_is_wall(coord):
    return rows[coord[1]][coord[0]] == '#'

def coord_is_start(coord):
    return rows[coord[1]][coord[0]] == 'S'


def neighbours(coords, strict_boundaries=True):
    res = [
        (coords[0] + 1,  coords[1]),
        (coords[0] + -1, coords[1]),
        (coords[0],      coords[1] + 1),
        (coords[0],      coords[1] - 1),
    ]
    if strict_boundaries:
        res = [e for e in res if e[0] >= 0 and e[0] < width and e[1] >= 0 and e[1] < height ]
    else:
        res = [(e[0] % width, e[1] % height) for e in res]
    res = [e for e in res if not coord_is_wall(e)]
    return res

def dfs_update(fringe, strict_boundaries=True):
    updated = []
    for e in fringe:
        updated.extend(neighbours(e, strict_boundaries=strict_boundaries))
    return set(updated)

def display_row(row, fringe):
    res = ""
    for col in range(width):
        coord = (col, row)
        if coord_is_start(coord):
            if coord in fringe:
                res += "X"
            else:
                res += "S"
        elif coord_is_wall(coord):
            res += "#"
        else:
            res += "O" if coord in fringe else "."
    return res

def display(strict_fringe, cyclic_fringe):
    res = ""
    for row in range(height):
        res += display_row(row, strict_fringe)
        res += " "*4
        res += display_row(row, cyclic_fringe)
        res += "\n"
    res += f"strict = {len(strict_fringe)}  |  cyclic = {len(cyclic_fringe)}" + "\n"
    print(res)



############
# PLOTTING #
############

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as anim

fringe_sizes = []

data = np.full((width, height), init_val)
for h, row in enumerate(rows):
    for w, c in enumerate(row):
        if c == "#": data[(w, h)] = min_val

# Plotting the grid
plt.ion()
fig, ax = plt.subplots()
# Displaying the plot
# Adjust the subplot layout to fill the entire figure
plt.tight_layout()


# Adding grid lines
ax.set_xticks(np.arange(data.shape[1]+1)-0.5, minor=True)
ax.set_yticks(np.arange(data.shape[0]+1)-0.5, minor=True)
ax.grid(which="minor", color='black', linewidth=0)
ax.tick_params(which="minor", size=0)

cax = ax.imshow(data, cmap='viridis', origin='lower', vmin=min_val, vmax=nr_steps)
# Adding a colorbar to show the value scale
cbar = fig.colorbar(cax)
cbar.set_label('Values', rotation=270, labelpad=20)




def init():
    cax.set_data(data)

def animate(i):
    global strict_fringe
    global cyclic_fringe
    global fringe_sizes

    strict_fringe = dfs_update(strict_fringe)
    cyclic_fringe = dfs_update(cyclic_fringe, strict_boundaries=False)
    if print_to_terminal:
        display(strict_fringe, cyclic_fringe)
    for coord in strict_fringe:
        if data[coord] == init_val:
            data[coord] = i


    # UPDATE PLOT
    cax.set_data(data)
    if display_animation:
        fig.canvas.flush_events()

    fringe_sizes.append((i+1, len(strict_fringe)))
    
    return cax

animation = anim.FuncAnimation(fig, animate, init_func=init, frames=nr_steps, interval=frame_interval)
animation.save(output_path)


# Print results
def difference_list(l: list[int]):
    return [l[i+1] - l[i] for i in range(len(l) - 1)]

diff_lst_count = 4
fig, axes = plt.subplots(diff_lst_count)

lst = [fs[1] for fs in fringe_sizes]
for i in range(diff_lst_count):
    ax = axes[i]
    lst = difference_list(lst)
    x = list(range(len(lst)))
    ax.plot(x, lst)

    plt.savefig(output_path_diff)
