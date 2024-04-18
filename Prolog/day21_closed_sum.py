import math
import matplotlib.pyplot as plt


def guess(steps):
    S = steps * (steps + 1) / 2
    S_even = 2 * math.floor(steps / 2) * (math.floor(steps / 2) + 1) / 2
    return (
        4 * int(S_even) + 1,
        4 * int(S - S_even)
    )

def true(length):
    return (
        int(sum(range(0, length+1, 2))),
        int(sum(range(1, length+1, 2)))
    )

length = 20
x = list(range(length))
yeven_guess = []
yuneven_guess = []
yeven_true = []
yuneven_true = []

for i in x:
    ye, yu = guess(i)
    yeven_guess.append(ye)
    yuneven_guess.append(yu)

    ye, yu = true(i)
    yeven_true.append(ye)
    yuneven_true.append(yu)

print("x              ", x)
print("guess uneven   ", yuneven_guess)
print("guess even     ", yeven_guess)
print("true  uneven   ", yuneven_true)
print("true  even     ", yeven_true)
print("evens match:   ", yeven_guess == yeven_true)
print("unevens match: ", yuneven_guess == yuneven_true)

for i in range(20):
    print((i, i // 2, (i+1) // 2))

fig, axes = plt.subplots(2)
axes[0].plot(x, yeven_guess)
axes[0].plot(x, yuneven_guess)
axes[1].plot(x, yeven_true)
axes[1].plot(x, yuneven_true)
plt.show()

