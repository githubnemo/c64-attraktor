import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation

a = 10
b = 28
c = 8/3

# X = a(Y-X)
# Y = X(b-Z)-Y
# Z = XY - cZ

X_cur = 2#0.001
Y_cur = 1#0.001
Z_cur = 1#0.001
n_steps = 10_000
dt = 0.005

points = []
points_2d = []
norms = []
norms_approx = []
norm_scale = 32
grads = []

def plot(x, y, z):
    global points, points_2d
    points.append((x, y, z))
    #points_2d.append((x * (320/2)/20 + 160, (y + z*10) * (200/512)))
    points_2d.append((int((x * (320/2)/20 + 160)), int((y + z*10) * (200/512))))
    #points_2d.append((int(x), int(y + z*10)))


    #points_2d.append((int((x * (320/2)/20 + 160)), int((y + z*10) * (200/512))))

    u = (y + z * 10)
    points_2d.append((
        int(x * 8 + 160),
        int( (u * 25)) >> 6,

    ))

    x_proj = int(x*8 + 160)
    y_proj = int(u * 25) >> 6

    norms.append(
        #np.sqrt(x_proj**2 + y_proj**2) / norm_scale
        #np.sqrt(grads[-1][0]**2 + grads[-1][1]**2),
        np.sqrt(grads[-1][0]**2 + (grads[-1][1] + 2*grads[-1][2])**2),
    )

    def approx_sq(n):
        n = int(abs(n))
        if n == 0:
            return n
        f = int(np.floor(np.log2(n)))
        return (int(n) << f) + (n << 1)

    def approx_l2(x, y):
        return max(x, y) + 1/2*min(x, y)

    norms_approx.append(
        #np.sqrt(approx_sq(x_proj) + approx_sq(y_proj))
        #approx_l2(x_proj, y_proj) / norm_scale
        #approx_l2(grads[-1][0], grads[-1][1])
        approx_l2(
            abs(grads[-1][0]) // 4,
            abs(grads[-1][1] + 2*grads[-1][2]) // 4,
        )
    )



for step in range(n_steps):
    X_new = a * (Y_cur - X_cur)
    X_cur += X_new * dt

    Y_new = X_cur * (b - Z_cur) - Y_cur
    Y_cur += Y_new * dt

    Z_new = X_cur * Y_cur - c * Z_cur
    Z_cur += Z_new * dt

    grads.append((int(X_new/8), int(Y_new/8), int(Z_new/8)))

    plot(X_cur, Y_cur, Z_cur)


f, axs = plt.subplots(nrows=4)
axs[0].plot(norms)
axs[1].plot(norms_approx)
axs[2].plot(abs(np.array(norms_approx) - np.array(norms)))

print(np.array(grads).max(axis=0))

x_p, y_p = zip(*points_2d)
axs[3].plot(x_p, y_p)
plt.show()




"""
fig, ax = plt.subplots()
line, = ax.plot([], [], lw=2)
ax.grid()
xdata, ydata = [], []

def init():
    del xdata[:]
    del ydata[:]
    line.set_data(xdata, ydata)
    return line,

def data_gen():
    for i in range(int(2*np.pi*100)):
        yield 10*np.sin(i/10)

def run(z):
    XYZ = np.array(points)
    proj = np.array([
        [1, 0],
        [0, 1],
        [0, z]
    ])

    foo = XYZ @ proj
    x_p = foo[:, 0]
    y_p = foo[:, 1]

    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()
    if max(x_p) >= xmax:
        ax.set_xlim(xmin, 2*xmax)
        ax.figure.canvas.draw()
    if max(y_p) >= ymax:
        ax.set_ylim(ymin, 2*ymax)
        ax.figure.canvas.draw()
    line.set_data(x_p, y_p)

ani = animation.FuncAnimation(fig, run, data_gen, interval=100, init_func=init,
                              save_count=100)
plt.show()
"""

"""
z = 10

XYZ = np.array(points)
proj = np.array([
    [1, 0],
    [0, 1],
    [0, z]
])

foo = XYZ @ proj
x_p = foo[:, 0]
y_p = foo[:, 1]
plt.plot(x_p, y_p)
plt.show()
"""

"""
p_new = []
for x, y, z in points:
    x_n = x*1 + y*0 + z*0
    y_n = x*0 + y*1 + z*10
    p_new.append((x_n, y_n))

x_p, y_p = zip(*p_new)
plt.plot(x_p, y_p)
plt.show()
"""


"""
ax = plt.figure().add_subplot(projection='3d')
x, y, z = zip(*points)
ax.plot(x, y, z)
plt.show()
"""
