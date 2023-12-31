import pygame
import math
import queue

class point:
    def __init__(self, x, y, old=None):
        self.x = x
        self.y = y
        self.G = 0
        self.H = 0
        self.old = old

    def __lt__(self, other):
        return (self.G + self.H) < (other.G + other.H)

class smallbox:
    basecolor = (255,255,255)
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def draw(self,win):
        pygame.draw.rect(win, (0,0,0),(self.x, self.y,5,5))
        pygame.draw.rect(win, self.basecolor, (self.x+1, self.y+1,3,3))
    def change(self, state): #Khoi tao cac mau cho bai toan me cung
        if state=="obs":
            self.basecolor = (34, 59, 245) #mau cua chuong ngai vat xanh blue
        elif state=="sou":
            self.basecolor = (162, 50, 168) #mau cua diem xuat phat tim
        elif state=="dis":
            self.basecolor = (255, 0, 0) #mau cua cac o da duoc duyet vang
        elif state=="check":
            self.basecolor = (245, 245, 34) #mau cua tuyen duong tot nhat da cam
        elif state=="road":
            self.basecolor = (224, 123, 57) #mau cua may o dang cho duyet xanh la cay
        elif state=="uncheck":
            self.basecolor = (0, 255, 0)

class makeMatrix: #thu tuc de tao ra 1 mang ma tran ban dau
    def __init__(self, size):
        self.matrix = []
        self.size = size
        self.start = point(0,0)
        self.end = None
        self.n = size[0]//5
        self.m = size[1]//5
        self.E = [[0 for i in range(200)] for i in range(200)]
        self.sets = queue.PriorityQueue()
        self.isFind = True
        for i in range (0, size[0]+5,5):
            b=[]
            for j in range (0, size[1]+5,5):
                b.append(smallbox(i,j))
            self.matrix.append(b) #A*

    def drawEnd(self, mousepos): #Ham nay de ve ra cac diem ket thuc tuc diem chung ta can tim
        x = mousepos[0] // 5
        y = mousepos[1] // 5
        self.matrix[x][y].change("dis")
        self.matrix[x][y].draw(win)
        self.end = point(x, y)
        self.E[x][y] = 0
        t = [1, 0, -1]
        for i in range(0, 3):
           for j in range(0, 3):
               self.E[x + t[i]][y + t[j]] = 0
               self.matrix[x + t[i]][y + t[j]].change("dis")
               self.matrix[x + t[i]][y + t[j]].change(win)
    def drawStart(self, mousepos): #Ham de ve ra diem bat dau xuat phat trong me cung
        x = mousepos[0] // 5
        y = mousepos[1] // 5
        self.matrix[x][y].change("sou")
        self.matrix[x][y].draw(win)
        self.start = point(x, y)
        self.E[x][y] = 0
        t = [1, 0, -1]
        for i in range(0, 3):
           for j in range(0, 3):
               self.E[x + t[i]][y + t[j]] = 0
               self.matrix[x + t[i]][y + t[j]].change("sou")
               self.matrix[x + t[i]][y + t[j]].draw(win)
        self.sets.put(self.start)
    def drawSE(self): #Ham ve ra diem dau tien xuat phat cua me cung
        self.matrix[self.start.x][self.start.y].draw(win)
        x = self.start.x
        y = self.start.y
        t = [1, 0, -1]
        for i in range(0, 3):
            for j in range(0, 3):
                self.matrix[x + t[i]][y + t[j]].change("sou")
                self.matrix[x + t[i]][y + t[j]].draw(win)
    def draw(self, win, mouse, mousePos): #Ham nay de ve ra chuong ngai vat cho me cung thong qua chuot trai
        if mouse[0]:
            x = mousePos[0] // 5
            y = mousePos[1] // 5
            self.matrix[x][y].change("obs")
            self.matrix[x][y].draw(win)
            self.E[x][y] = -1
            t = [1, 0, -1]
            for i in range(0, 3):
                for j in range(0, 3):
                    self.E[x + t[i]][y + t[j]] = -1
                    self.matrix[x + t[i]][y + t[j]].change("obs")
                    self.matrix[x + t[i]][y + t[j]].draw(win)
    def drawF(self,win):
        for i in range(0, self.size[0]//5+1):
            for j in range(0, self.size[1]//5 + 1):
                self.matrix[i][j].draw(win)
    def solve(self, win): #Noi chua thuat toan Dijkstra
        self.drawSE()
        t = [1, 0, -1]
        for i in range(0, 3):
            for j in range(0, 3):
                self.E[self.end.x + t[i]][self.end.y + t[j]] = 0
                self.matrix[self.end.x + t[i]][self.end.y + t[j]].change("dis")
                self.matrix[self.end.x + t[i]][self.end.y + t[j]].draw(win)
                self.E[self.end.x][self.end.y] = 0

        if self.isFind == False:
            return

        p1 = [0, 0, 1, -1]
        p2 = [1, -1, 0, 0]
        visited = set()
        pq = queue.PriorityQueue()

        pq.put(self.start)

        while not pq.empty():
            k = pq.get()

            if (k.x, k.y) in visited:
                continue

            visited.add((k.x, k.y))

            self.matrix[k.x][k.y].change("check")
            self.matrix[k.x][k.y].draw(win)

            for i in range(4):
                x = k.x + p1[i]
                y = k.y + p2[i]

                if x < 0 or x >= self.n:
                    continue
                if y < 0 or y >= self.m:
                    continue
                if self.E[x][y] == -1:
                    continue

                temp = point(x, y, k)
                temp.G = k.G + 1
                temp.H = math.sqrt(pow(x - self.end.x, 2) + pow(y - self.end.y, 2))

                if (x, y) not in visited:
                    pq.put(temp)
                    self.matrix[x][y].change("uncheck")
                    self.matrix[x][y].draw(win)

                if temp.H == 0:
                    self.isFind = False
                    while temp is not None:
                        self.matrix[temp.x][temp.y].change('road')  #Ve ra tuyen duong tot nhat
                        self.matrix[temp.x][temp.y].draw(win)
                        temp = temp.old

                    self.matrix[self.start.x][self.start.y].draw(win)
                    self.matrix[x][y].change("dis") #Toa do cua diem can tim
                    self.matrix[x][y].draw(win)
size = (1000, 1000) #Khai bao giao dien va chay chuong trinh
win = pygame.display.set_mode(size)
isRun = True
clock = pygame.time.Clock()
M = makeMatrix(size)
M.drawF(win)

bf = False
while isRun:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            isRun = False

    keys = pygame.key.get_pressed()
    if keys[pygame.K_SPACE]: #gan nut cach cho phep chay thuat toan
        bf = True

    mouse = pygame.mouse.get_pressed()
    mousePos = pygame.mouse.get_pos()
    if keys[pygame.K_s]: #Gan nut S cho phep ve ra vi tri bat dau can tim theo vi tri chuot
        M.drawStart(mousePos)
    if keys[pygame.K_d]: #Gan  nut D cho phep ve ra vi tri ket thuc can tim theo vi tri chuot
        M.drawEnd(mousePos)
    M.draw(win, mouse, mousePos)
    if bf:
        M.solve(win)

    pygame.display.flip()
    clock.tick(60)