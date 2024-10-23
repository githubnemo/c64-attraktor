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
dt = 0.01

points = []
points_2d = []


def plot(x, y, z):
    global points, points_2d
    points.append((x, y, z))
    points_2d.append((x * (320/2)/20 + 160, (y + z*10) * (200/512)))
    #points_2d.append((int((x * (320/2)/20 + 160)), int((y + z*10) * (200/512))))


for step in range(n_steps):
    X_new = a * (Y_cur - X_cur)
    X_cur += X_new * dt

    Y_new = X_cur * (b - Z_cur) - Y_cur
    Y_cur += Y_new * dt

    Z_new = X_cur * Y_cur - c * Z_cur
    Z_cur += Z_new * dt

    plot(X_cur, Y_cur, Z_cur)

FLOAD(A_ADDR, a)
FLOAD(B_ADDR, b)
FLOAD(C_ADDR, c)
FLOAD(DT_ADDR, dt)

FA1 = Y_cur
FSUB(FA1, A=X_cur.LB, Y=X_cur.HB)
FMULT(FA1, A=#< A_ADDR, Y=#> A_ADDR)
# now FA1 = X_new
FMULT(FA1, A=#< DT_ADDR, Y=#> DT_ADDR)
FADD(FA1, A=#< XCUR_ADDR, Y=#> XCUR_ADDR)
# FA1=X_cur + X_new
MOVEMF(FAC, X=#< XCUR_ADDR, Y=#> XCUR_ADDR)
# X_cur = X_cur'




x_p, y_p = zip(*points_2d)
plt.plot(x_p, y_p)
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
