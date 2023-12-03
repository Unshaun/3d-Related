DECLARE SUB placeitem (ix, iy, iz, ixa, iya, iza, i)
DECLARE SUB make2d (tx, ty, tz, tx2, ty2, tz2, sx, sy)
DECLARE FUNCTION turn360 (numbr)
DECLARE SUB rotate (tx, ty, tz, txa, tya, tza)
DECLARE FUNCTION numbr (x)
DECLARE SUB drawguy ()
COMMON SHARED depth, camdisty
COMMON SHARED tx, ty, tz, txa, tya, tza
'concider making ITEMS as obj3d which can be anything
'
'


'                                     +Z
'X = LEFT  RIGHT                       |   -Y
'Y = UP    DOWN                        |  /
'Z = IN    OUT                -X_______|/_______+X
'V = # of verticies                   /|
'nl = # or lines                    /  |
'dir = direction                 +Y    |
'rx() = recent x                      -Z
'rx2() = recent x2
'pt() = verticy that line x starts at
'pt2() = verticy that line x goes to

SCREEN 7, , 2, 0: RANDOMIZE TIMER
up$ = CHR$(0) + CHR$(72): dwn$ = CHR$(0) + CHR$(80): lft$ = CHR$(0) + CHR$(75): rght$ = CHR$(0) + CHR$(77)
CONST pi = 3.141593265358979#
DIM map$(25, 15, 3)
nl = 12: REDIM pt(nl), pt2(nl)
FOR z = 1 TO 3: FOR y = 1 TO 15: FOR x = 1 TO 25: READ map$(x, y, z): IF map$(x, y, z) = "Û" THEN c = z + 6 ELSE c = 0
IF c THEN PSET (x + z, y - z + 5), c
NEXT x: NEXT y: NEXT z

TYPE obj
  p AS INTEGER  'number of points
  t AS INTEGER  'type (for graphics tweaks)
  x AS SINGLE   'x center location
  y AS SINGLE   'y center location
  z AS SINGLE   'z center location
  xa AS SINGLE  'x angle
  ya AS SINGLE  'y angle
  za AS SINGLE  'z angle
END TYPE
TYPE obj3d
  px AS SINGLE    'points perminant x
  py AS SINGLE    'points perminant y
  pz  AS SINGLE   'points perminant z
  tx AS SINGLE    'points temporary x
  ty AS SINGLE    'points temporary y
  tz AS SINGLE    'points temporary z
  lp1 AS INTEGER  'line from point to lp1
  lp2 AS INTEGER  'line from point to lp2
  lc1 AS INTEGER  'line from point to lp1
  lc2 AS INTEGER  'line from point to lp2
  sx AS SINGLE    'points screen location x
  sy AS SINGLE    'points screen location y
END TYPE
TYPE tempobj3d
  tx AS SINGLE    'points temporary x
  ty AS SINGLE    'points temporary y
  tz AS SINGLE    'points temporary z
  sx AS SINGLE    'points screen location x
  sy AS SINGLE    'points screen location y
END TYPE
TYPE camera
  px AS SINGLE    'points perminant x
  py AS SINGLE    'points perminant y
  pz AS SINGLE   'points perminant z
  tx AS SINGLE    'points temporary x
  ty AS SINGLE    'points temporary y
  tz AS SINGLE    'points temporary z
  sx AS SINGLE    'points temporary y DELETE ONCE CAMERA IS OPERATIVE
  sy AS SINGLE    'points temporary z DELETE ONCE CAMERA IS OPERATIVE
  c AS SINGLE    'points color
END TYPE

DIM SHARED boxp(8) AS obj3d
DIM SHARED tilep(4) AS obj3d
DIM SHARED item(99) AS tempobj3d

   DIM SHARED box AS obj
     box.x = 0: box.y = 0: box.z = 0
     box.xa = 0: box.ya = 0: box.za = 0
     box.p = 8
   DIM SHARED tile AS obj
     tile.x = 100: tile.y = 100: tile.z = 100
     tile.xa = 0: tile.ya = 0: tile.za = 0
     tile.p = 4






'/-------------CHARACTER SET UP
'    (11)
'    /1\
'   3 | 5
'  /  |  \
' 4   2   6
'    / \
'   7   9
'  /     \
' 8      10
   v = 11
DIM SHARED bx(v), by(v), bz(v), sx(v), sy(v)
bx(1) = 0: by(1) = 0: bz(1) = -34
bx(2) = 0: by(2) = 0: bz(2) = -20
bx(3) = -3:  by(3) = 0: bz(3) = -25
bx(4) = -6:  by(4) = 0: bz(4) = -18
bx(5) = 3:   by(5) = 0: bz(5) = -25
bx(6) = 6:  by(6) = 0: bz(6) = -18
bx(7) = -3:   by(7) = 0: bz(7) = -10
bx(8) = -6:    by(8) = 0:  bz(8) = 0
bx(9) = 3:    by(9) = 0: bz(9) = -10
bx(10) = 6: by(10) = 0: bz(10) = 0
bx(11) = 0:   by(11) = 0:  bz(11) = -37
pt(1) = 1: pt2(1) = 2: pt(2) = 2: pt2(2) = 4: pt(3) = 4: pt2(3) = 3
pt(4) = 3: pt2(4) = 1: pt(5) = 1: pt2(5) = 5: pt(6) = 5: pt2(6) = 6
pt(7) = 6: pt2(7) = 8: pt(8) = 8: pt2(8) = 7: pt(9) = 7: pt2(9) = 5
pt(10) = 7: pt2(10) = 3: pt(11) = 8: pt2(11) = 4: pt(12) = 6: pt2(12) = 2


'/-----------BOX SET UP
'      2_______3
'     /|      /|             Z+
'   /  |    /  |             |  /-
' 1----|---4   |         -___|/___X+
' |    |___|___|            /|
' |  / 6   |  /7         +Y  |
' |/_______|/                -
' 5        8

boxsize = 25
boxp(1).px = -boxsize: boxp(1).py = boxsize:  boxp(1).pz = boxsize * 2
boxp(2).px = -boxsize: boxp(2).py = -boxsize: boxp(2).pz = boxsize * 2
boxp(3).px = boxsize:  boxp(3).py = -boxsize: boxp(3).pz = boxsize * 2
boxp(4).px = boxsize:  boxp(4).py = boxsize:  boxp(4).pz = boxsize * 2
boxp(5).px = -boxsize: boxp(5).py = boxsize:  boxp(5).pz = 0
boxp(6).px = -boxsize: boxp(6).py = -boxsize: boxp(6).pz = 0
boxp(7).px = boxsize:  boxp(7).py = -boxsize: boxp(7).pz = 0
boxp(8).px = boxsize:  boxp(8).py = boxsize:  boxp(8).pz = 0
boxp(1).lp1 = 2: boxp(1).lp2 = 5: boxp(2).lp1 = 3: boxp(2).lp2 = 6: boxp(3).lp1 = 4: boxp(3).lp2 = 7: boxp(4).lp1 = 1: boxp(4).lp2 = 8
boxp(5).lp1 = 6: boxp(5).lp2 = 0: boxp(6).lp1 = 7: boxp(6).lp2 = 0: boxp(7).lp1 = 8: boxp(7).lp2 = 0: boxp(8).lp1 = 5: boxp(8).lp2 = 0

'/-----------TILE SET UP
'
'      2_______3
'     /       /              Z+
'   /       /                |  /-
' 1--------4             -___|/___X+
'                           /|
'                        +Y  |
'                            -
tilesize = 25   'tile size will actually be 50
tilep(1).px = -tilesize: tilep(1).py = tilesize:  tilep(1).pz = 0
tilep(2).px = -tilesize: tilep(2).py = -tilesize: tilep(2).pz = 0
tilep(3).px = tilesize:  tilep(3).py = -tilesize: tilep(3).pz = 0
tilep(4).px = tilesize:  tilep(4).py = tilesize:  tilep(4).pz = 0
tilep(1).lp1 = 2
tilep(2).lp1 = 3
tilep(3).lp1 = 4
tilep(4).lp1 = 1





depth = 250
camdisty = 200


camx = 450   'each block is 50 X 50
camy = 350
camz = 0
camxa = 0
camya = 1
camza = 1

SCREEN 7, , 1, 0
DO: PCOPY 2, 1: '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
stepx = 20: stepy = 0: rz = 0
rotate stepx, stepy, rz, camxa, camya, camza
IF a$ = up$ THEN camx = camx + stepx: camy = camy + stepy
IF a$ = dwn$ THEN camx = camx - stepx: camy = camy - stepy
IF a$ = rght$ THEN camza = camza + 10
IF a$ = lft$ THEN camza = camza - 10

'HANDLE RAY CASTING

'tx = 0: ty = 0: tz = 1: txa = 0: tya = camya: tza = 0
'rotate tx, ty, tz, txa, tya, tza
'LOCATE 1: PRINT tz; camya: PSET (160 + tx * 10, 100 + tz * 10), 12: PSET (160, 100), 10: camya = turn360(camya + 1)

 
'camza = turn360(camza + 5)  'rotate cam
'FOR ray = 1 TO 5
'FOR vsy = -ray TO ray
'FOR vsx = -ray TO ray
'
'NEXT vsx
'NEXT vsy
'NEXT ray

  
  

  

  
  
 

'/--------------POSSIBLE RAY CASTING
  vspan = 7
  vrange = 10
 FOR ray = 0 TO vspan
  cx = 0: cy = 0
  stepx = 1: stepy = 0: stepz = 0
  vstepx = 50: vstepy = 0: vstepz = 0
  rayza = turn360(camza + ((ray - 4) * vspan))
  rotate stepx, stepy, stepz, camxa, camya, rayza
  rotate vstepx, vstepy, vstepz, camxa, camya, (rayza) MOD 90
  FOR rstep = 1 TO vrange
   cx = cx + stepx
   cy = cy + stepy
   mx = camx * .02 + cx: IF mx > 25 THEN mx = 25 ELSE IF mx < 1 THEN mx = 1
   my = camy * .02 + cy: IF my > 15 THEN my = 15 ELSE IF my < 1 THEN my = 1
   mz = 1
   scx = (ray - vspan * .5) * 50
   scy = rstep * 50 + 120
   scz = -60
   rotate scx, scy, scz, camxa, camya, (camza) MOD 45
  
   PSET (mx, my), 13
 
   IF map$(mx, my, mz) = "Û" THEN
   i = 2
   placeitem scx, scy, scz, camxa, camya, camza, i: EXIT FOR
  'placeitem mx * 50 - 160, my * 50, mz * 50 - 80, i: EXIT FOR
   ELSE

   placeitem scx, scy, scz, camxa, camya, (camza) MOD 90, 1
   'PSET (scx, scy), 10
   END IF
  NEXT rstep
 NEXT ray

 PSET (camx / 50, camy / 50), 11





'HANDLE BOX
IF 5 = 5 THEN
'box.za = box.za + 1
box.x = 0
box.y = 0
box.z = 0
FOR p = 1 TO box.p
  boxp(p).tx = boxp(p).px
  boxp(p).ty = boxp(p).py
  boxp(p).tz = boxp(p).pz
  rotate boxp(p).tx, boxp(p).ty, boxp(p).tz, box.xa, box.ya, box.za
  make2d boxp(p).tx, boxp(p).ty, boxp(p).tz, box.x, box.y, box.z, boxp(p).sx, boxp(p).sy
NEXT p
     'DRAW BOX

'FOR l = 1 TO 8: LINE (boxp(l).sx, boxp(l).sy)-(boxp(boxp(l).lp1).sx, boxp(boxp(l).lp1).sy), 1: FOR q = 1 TO SGN(boxp(l).lp2): LINE (boxp(l).sx, boxp(l).sy)-(boxp(boxp(l).lp2).sx, boxp(boxp(l).lp2).sy), 1: NEXT q: NEXT l
END IF


       'HANDLE tile
'tile.za = tile.za + 1
tile.x = 0
tile.y = 0
tile.z = -5
FOR p = 1 TO tile.p
  tilep(p).tx = tilep(p).px
  tilep(p).ty = tilep(p).py
  tilep(p).tz = tilep(p).pz
  rotate tilep(p).tx, tilep(p).ty, tilep(p).tz, tile.xa, tile.ya, tile.za
  make2d tilep(p).tx, tilep(p).ty, tilep(p).tz, tile.x, tile.y, tile.z, tilep(p).sx, tilep(p).sy
NEXT p
'     'DRAW tile
'FOR l = 1 TO 4: LINE (tilep(l).sx, tilep(l).sy)-(tilep(tilep(l).lp1).sx, tilep(tilep(l).lp1).sy), 1: FOR q = 1 TO SGN(tilep(l).lp2): LINE (tilep(l).sx, tilep(l).sy)-(tilep(tilep(l).lp2).sx, tilep(tilep(l).lp2).sy), 2: NEXT q: NEXT l
   
  
   

    'DRAW CHARACTER
LINE (sx(1), sy(1))-(sx(2), sy(2)), 10: LINE (sx(1), sy(1))-(sx(3), sy(3)), 10
LINE (sx(1), sy(1))-(sx(5), sy(5)), 10: LINE (sx(2), sy(2))-(sx(7), sy(7)), 10
LINE (sx(2), sy(2))-(sx(9), sy(9)), 10: LINE (sx(3), sy(3))-(sx(4), sy(4)), 1
LINE (sx(5), sy(5))-(sx(6), sy(6)), 1: LINE (sx(7), sy(7))-(sx(8), sy(8)), 1
LINE (sx(9), sy(9))-(sx(10), sy(10)), 1: CIRCLE (sx(11), sy(11)), 11, 10

                   
                    'ROTATION LOOP------------------------------------------
             FOR p = 1 TO v
               tx = bx(p): ty = by(p): tz = bz(p)
               ttz = tz * COS(xang * .01745) + ty * SIN(xang * .01745)
               tty = ty * COS(xang * .01745) - tz * SIN(xang * .01745)
               tz = ttz: ty = tty
               ttx = tx * COS(zang * .01745) - ty * SIN(zang * .01745)
               tty = ty * COS(zang * .01745) + tx * SIN(zang * .01745)
               tx = ttx: ty = tty
               ttx = tx * COS(yang * .01745) + tz * SIN(yang * .01745)
               ttz = tz * COS(yang * .01745) - tx * SIN(yang * .01745)
               tz = ttz: tx = ttx
               tx = tx + yx: ty = ty + yy: tz = tz + yz
               sx(p) = 160 - (depth * (tx / (ty - camdisty)))
               sy(p) = 100 - (depth * (tz / (ty - camdisty)))
             NEXT p'------------------------------------------------------------------------


PCOPY 1, 0: a$ = UCASE$(INKEY$): LOOP UNTIL a$ = CHR$(27)












END


SCREEN 7, , 1, 0
DO: PCOPY 2, 1: '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        'WHICH DIRECTION KEY PUSHED
  r = 7
  angle = (zang - 135) MOD 360
  stx = r * (COS(angle * .01745) - SIN(angle * .01745))
  sty = r * (COS(angle * .01745) + SIN(angle * .01745))

LOCATE 2, 2: PRINT yx; yy
IF a$ = up$ THEN yx = yx + stx: yy = yy + sty
IF a$ = dwn$ THEN yx = yx - stx: yy = yy - sty
IF a$ = rght$ THEN zang = zang + 10
IF a$ = lft$ THEN zang = zang - 10
IF a$ = " " THEN
  yx = yx + 1
END IF



PCOPY 1, 0: a$ = UCASE$(INKEY$): LOOP UNTIL a$ = CHR$(27)

DATA Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û,Û,Û,Û,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û

DATA Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û,Û,Û,Û,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û

DATA Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û,Û,Û,Û,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û

SUB drawguy
size = 1
spin = 1
bodyc = 1
headc = 1
CIRCLE ((sx(6) + sx(4)) / 2, 180), 25, 8, , , .15
'arm 2

LINE (sx(1) + ((sx(2) - sx(1)) / 3) - 1, sy(1) + ((sy(2) - sy(1)) / 3) - 1)-(sx(9), sy(9)), bodyc
LINE (sx(1) + ((sx(2) - sx(1)) / 3) + 1, sy(1) + ((sy(2) - sy(1)) / 3) + 1)-(sx(9), sy(9)), bodyc

'IF bigarm  < 2 AND fistc1  >= 0 THEN
'IF fistc2  < 0 THEN fistc2  = (-fistc2 ) + .1
LINE (sx(9) - ((spin * size) / 2), sy(9) - (size / 2))-(sx(10), sy(10)), fistc1
LINE (sx(9) + ((spin * size) / 2), sy(9) + (size / 2))-(sx(10), sy(10)), fistc2
CIRCLE (sx(10), sy(10)), 2 * qqq, fistc1: IF INSTR(1, STR$(fistc2), ".1") THEN fistc2 = (-fistc2) + .1
'END IF
'body
'IF bodyc  >= 0 THEN
LINE (sx(1) + ((sx(2) - sx(1)) / 3) - (spin * size), sy(1) + ((sy(2) - sy(1)) / 3) - size)-(sx(2), sy(2)), bodyc
LINE (sx(1) + ((sx(2) - sx(1)) / 3) + (spin * size), sy(1) + ((sy(2) - sy(1)) / 3) + size)-(sx(2), sy(2)), bodyc
'END IF
'IF qrq <> 0 THEN SWAP size , biglegs
'upper legs
'IF legc  >= 0 THEN
LINE (sx(2) - (spin * size), sy(2) - size)-(sx(3), sy(3)), legc
LINE (sx(2) + (spin * size), sy(2) + size)-(sx(3), sy(3)), legc
LINE (sx(2) - (spin * size), sy(2) - size)-(sx(5), sy(5)), legc
LINE (sx(2) + (spin * size), sy(2) + size)-(sx(5), sy(5)), legc
'END IF
'lower legs
'IF bootc  >= 0 THEN
LINE (sx(3) - ((spin * size) / 2), sy(3) - (size / 2))-(sx(4), sy(4)), bootc
LINE (sx(3) + ((spin * size) / 2), sy(3) + (size / 2))-(sx(4), sy(4)), bootc
CIRCLE (sx(4), sy(4)), 4 * qqq, bootc, , , .2
LINE (sx(5) - ((spin * size) / 2), sy(5) - (size / 2))-(sx(6), sy(6)), bootc
LINE (sx(5) + ((spin * size) / 2), sy(5) + (size / 2))-(sx(6), sy(6)), bootc
CIRCLE (sx(6), sy(6)), 4 * qqq, bootc, , , .2
'END IF
IF qrq <> 0 THEN SWAP size, biglegs
'arm 1
IF bodyc >= 0 THEN
LINE (sx(1) + ((sx(2) - sx(1)) / 3) - 1, sy(1) + ((sy(2) - sy(1)) / 3) - 1)-(sx(7), sy(7)), bodyc
LINE (sx(1) + ((sx(2) - sx(1)) / 3) + 1, sy(1) + ((sy(2) - sy(1)) / 3) + 1)-(sx(7), sy(7)), bodyc
END IF
IF bigarm = 0 AND fistc2 >= 0 THEN
IF fistc1 < 0 THEN fistc1 = (-fistc1) + .1
LINE (sx(7) - ((spin * size) / 2), sy(7) - (size / 2))-(sx(8), sy(8)), fistc1
LINE (sx(7) + ((spin * size) / 2), sy(7) + (size / 2))-(sx(8), sy(8)), fistc2
CIRCLE (sx(8), sy(8)), 2 * qqq, fistc1: IF INSTR(1, STR$(fistc1), ".1") THEN fistc1 = (-fistc1) + .1
END IF
'head
IF headc >= 0 THEN
CIRCLE (sx(1), sy(1)), 11 * qqq, headc
IF headc >= 8 THEN CIRCLE (sx(1), sy(1)), 11 * qqq, headc - 8, 4, 0
IF headc = 7 THEN CIRCLE (sx(1), sy(1)), 11 * qqq, 8, 4, 0
EXIT SUB
END IF

END SUB

SUB make2d (tx, ty, tz, tx2, ty2, tz2, sx, sy)
  camdisty = 1 'temporary variable
  tx = tx + tx2
  ty = ty + ty2
  tz = tz + tz2
'  camdisty = camdisty + SGN(-ty - camdisty = 0) * .0001
  sx = 160 - (depth * (tx / (-ty - camdisty)))
  sy = 100 - (depth * (-tz / (-ty - camdisty)))


END SUB

SUB placeitem (ix, iy, iz, ixa, iya, iza, i)
IF i = 1 THEN


     

       'HANDLE tile
tile.xa = ixa
tile.ya = iya
tile.za = iza
tile.x = ix
tile.y = iy
tile.z = iz
FOR p = 1 TO tile.p
  tilep(p).tx = tilep(p).px
  tilep(p).ty = tilep(p).py
  tilep(p).tz = tilep(p).pz
  rotate tilep(p).tx, tilep(p).ty, tilep(p).tz, tile.xa, tile.ya, tile.za
  make2d tilep(p).tx, tilep(p).ty, tilep(p).tz, tile.x, tile.y, tile.z, tilep(p).sx, tilep(p).sy
NEXT p
'     'DRAW tile
FOR l = 1 TO 4: LINE (tilep(l).sx, tilep(l).sy)-(tilep(tilep(l).lp1).sx, tilep(tilep(l).lp1).sy), 1: FOR q = 1 TO SGN(tilep(l).lp2): LINE (tilep(l).sx, tilep(l).sy)-(tilep(tilep(l).lp2).sx, tilep(tilep(l).lp2).sy), 2: NEXT q: NEXT l

ELSEIF i = 2 THEN



box.xa = ixa
box.ya = iya
box.za = iza

box.x = ix
box.y = iy
box.z = iz
FOR p = 1 TO box.p
  boxp(p).tx = boxp(p).px
  boxp(p).ty = boxp(p).py
  boxp(p).tz = boxp(p).pz
  rotate boxp(p).tx, boxp(p).ty, boxp(p).tz, box.xa, box.ya, box.za
  make2d boxp(p).tx, boxp(p).ty, boxp(p).tz, box.x, box.y, box.z, boxp(p).sx, boxp(p).sy
NEXT p
     'DRAW BOX
FOR l = 1 TO 8: LINE (boxp(l).sx, boxp(l).sy)-(boxp(boxp(l).lp1).sx, boxp(boxp(l).lp1).sy), 1: FOR q = 1 TO SGN(boxp(l).lp2): LINE (boxp(l).sx, boxp(l).sy)-(boxp(boxp(l).lp2).sx, boxp(boxp(l).lp2).sy), 1: NEXT q: NEXT l

END IF


END SUB

SUB rotate (tx, ty, tz, txa, tya, tza)
  tx2 = tx * COS(tza * .01745) - ty * SIN(tza * .01745): ty2 = tx * SIN(tza * .01745) + ty * COS(tza * .01745): tz2 = tz
  tx = tx2 * COS(tya * .01745) - tz2 * SIN(tya * .01745): tz = tx2 * SIN(tya * .01745) + tz2 * COS(tya * .01745): ty = ty2
  tz2 = tz * COS(txa * .01745) - ty * SIN(txa * .01745): ty2 = tz * SIN(txa * .01745) + ty * COS(txa * .01745): tx2 = tx
  tx = tx2: ty = ty2: tz = tz2
END SUB

SUB shit
'                         +Z
'                          |   -Y
'                          |  /
'                 -X_______|/_______+X
'                         /|
'                       /  |
'                    +Y    |
'                         -Z
    
    ' origonal rotation patterns
     '        FOR p = 1 TO v
     '          nx = bx(p): ny = by(p): nz = bz(p)
     '          tz = bz(p) * COS(xang * .01745) + by(p) * SIN(xang * .01745)
     '          ty = by(p) * COS(xang * .01745) - bz(p) * SIN(xang * .01745)
     '          nz(p) = tz
     '          ny(p) = ty
     '          z(p) = nz(p)
     '          y(p) = ny(p)
     '        NEXT p
     '        FOR p = 1 TO v
     '          tx = bx(p) * COS(yang * .01745) - by(p) * SIN(yang * .01745)
     '          ty = by(p) * COS(yang * .01745) + bx(p) * SIN(yang * .01745)
     '          nx(p) = tx
     '          ny(p) = ty
     '          x(p) = nx(p)
     '          y(p) = ny(p)
    ''        NEXT p
    ''        FOR p = 1 TO v
     '          tx = bx(p) * COS(zang * .01745) + bz(p) * SIN(zang * .01745)
     '          tz = bz(p) * COS(zang * .01745) - bx(p) * SIN(zang * .01745)
     '          nz(p) = tz
     '          nx(p) = tx
     '          z(p) = nz(p)
     '          x(p) = nx(p)
     '        NEXT p

' essentials for a cube
'   bbx(1) = -boxsize: bby(1) = -boxsize: bbz(1) = -boxsize
'   bbx(2) = boxsize:  bby(2) = -boxsize: bbz(2) = -boxsize
'   bbx(3) = -boxsize: bby(3) = boxsize:  bbz(3) = -boxsize
'   bbx(4) = boxsize:  bby(4) = boxsize:  bbz(4) = -boxsize
'   bbx(5) = -boxsize: bby(5) = -boxsize: bbz(5) = boxsize
'   bbx(6) = boxsize:  bby(6) = -boxsize: bbz(6) = boxsize
'   bbx(7) = -boxsize: bby(7) = boxsize:  bbz(7) = boxsize
'   bbx(8) = boxsize:  bby(8) = boxsize:  bbz(8) = boxsize
'   FOR p = 1 TO v: nx(p) = bx(p): ny(p) = by(p): nz(p) = bz(p): NEXT p
'   nl = 12
'   DIM pt(nl), pt2(nl)
'   pt(1) = 1: pt2(1) = 2: pt(2) = 2: pt2(2) = 4: pt(3) = 4: pt2(3) = 3
'   pt(4) = 3: pt2(4) = 1: pt(5) = 1: pt2(5) = 5: pt(6) = 5: pt2(6) = 6
'   pt(7) = 6: pt2(7) = 8: pt(8) = 8: pt2(8) = 7: pt(9) = 7: pt2(9) = 5
'   pt(10) = 7: pt2(10) = 3: pt(11) = 8: pt2(11) = 4: pt(12) = 6: pt2(12) = 2
'   FOR l = 1 TO nl: LINE (bsx(pt(l)), bsy(pt(l)))-(bsx(pt2(l)), bsy(pt2(l))), 1: NEXT l'
'
'clean guy
'   bx(1) = 0: by(1) = -14: bz(1) = 0
'   bx(2) = 0: by(2) = 0: bz(2) = 0
'   bx(3) = -3:  by(3) = -5: bz(3) = 0
'   bx(4) = -6:  by(4) = 2: bz(4) = 0
'   bx(5) = 3:   by(5) = -5: bz(5) = 0
'   bx(6) = 6:  by(6) = 2: bz(6) = 0
'   bx(7) = -3:   by(7) = 10: bz(7) = 0
'   bx(8) = -6:    by(8) = 20:  bz(8) = 0
'   bx(9) = 3:    by(9) = 10: bz(6) = 0
'   bx(10) = 6: by(10) = 20: bz(10) = 0
'   bx(11) = 0:   by(11) = -17:  bz(11) = 0

'/--------------POSSIBLE RAY CASTING
'  vspan = 5
'  vrange = 15
'  FOR ray = 0 TO vspan
'  cx = 0: cy = 0
'  stepx = 50: stepy = 0: stepz = 0
'  vstepx = 50: vstepy = 0: vstepz = 0
'  rayza = turn360(camza + ((ray - 4) * vspan))
'  rotate stepx, stepy, stepz, camxa, camya, rayza
'  rotate vstepx, vstepy, vstepz, camxa, camya, (rayza) MOD 45
'  LOCATE 1
'  'PRINT stepx, stepy
'  'PRINT vstepx, vstepy
'  FOR rstep = 1 TO vrange
'  cx = cx + stepx
'  cy = cy + stepy
'  mx = (camx + cx) * .02: IF mx > 25 THEN mx = 25 ELSE IF mx < 1 THEN mx = 1
'  my = (camy + cy) * .02: IF my > 15 THEN my = 15 ELSE IF my < 1 THEN my = 1
'  scx = (ray - vspan * .5)
'  scy = rstep
'  scz = -60
'  PSET (mx, my), 13
'
'  IF map$(mx, my,mz) = "Û" THEN
'  i = 20
'  placeitem scx, scy, scz, i: EXIT FOR
'  'placeitem mx * 50 - 160, my * 50, mz * 50 - 80, i: EXIT FOR
'  ELSE
'  'placeitem scx, scy, scz, 1
'  PSET (scx, scy), 10
'  END IF
'  NEXT rstep
'  NEXT ray
'  PSET (camx / 50, camy / 50), 3
'

'/----POSSIBLE CAM ENGINE
'/-------------SET UP CAMERA
'p = 0: range = 5
'DIM camp(285) AS camera
'FOR z = 1 TO 5
'FOR y = -z TO z
'FOR x = -z TO z
'p = p + 1
'camp(p).px = x
'camp(p).py = y
'camp(p).pz = z
'camp(p).c = z
'NEXT x
'NEXT y
'NEXT z
'
'IF a$ = " " THEN camya = (camya + 4)' MOD 90 + 90
'camx = 0
'camy = 1
'camz = 0
'FOR p = 1 TO 280
'  camp(p).tx = camp(p).px
'  camp(p).ty = camp(p).py
'  camp(p).tz = camp(p).pz
'  rotate camp(p).tx, camp(p).ty, camp(p).tz, camxa, camya, camza
'  make2d camp(p).tx, camp(p).ty, camp(p).tz, camx, camy, camz, camp(p).sx, camp(p).sy
'NEXT p
'     'DRAW CAM
'FOR l = 1 TO 285
'CIRCLE (camp(l).sx, camp(l).sy), 10 - camp(l).c * 2, camp(l).c

END SUB

FUNCTION turn360 (numbr)
IF numbr >= 360 THEN numbr = numbr - 360
IF numbr < 0 THEN numbr = numbr + 360
turn360 = numbr
END FUNCTION

