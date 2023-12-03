
DECLARE SUB drawguy ()
'X = LEFT  RIGHT                       |
'Y = UP    DOWN                        |  /
'Z = IN    OUT                  _______|/_______X
'V = # of verticies                   /|
'nl = # or lines                    /  |
'dir = direction                   Y   |
'rx() = recent x                       Z
'rx2() = recent x2
'pt() = verticy that line x starts at
'pt2() = verticy that line x goes to
'zf = how fucked up the z range is

SCREEN 7
RANDOMIZE TIMER
constmove = 1 'If 0, nothing.  If 1, the object constantly moves on Z.
clr = 256 '256=Green, 65536=Blue, 1=Red, 65793=White, 65792=Cyan, 257=Yellow
       '65537=Purple
delay = 500
CONST pi = 3.141593265358979#
IF constmove = 1 THEN dir = INT(RND * 8) + 1
SCREEN 7, , 2, 0
DIM mapspot$(25, 14)
FOR y = 1 TO 14
FOR x = 1 TO 25
READ mapspot$(x, y)
IF mapspot$(x, y) = "Û" THEN c = 15 ELSE c = 0
PSET (x, y), c
NEXT x
NEXT y




sh = 12
shf = .3
cx = 160
cy = 100
zf = 200
dist = 100  ' default 100!
ta = 7

vsrng = 8

up$ = CHR$(0) + CHR$(72)
dwn$ = CHR$(0) + CHR$(80)
lft$ = CHR$(0) + CHR$(75)
rght$ = CHR$(0) + CHR$(77)

   v = 11
   DIM SHARED bx(v), by(v), bz(v), Sx(v), Sy(v)
   DIM bbx(8), bby(8), bbz(8), bsx(8), bsy(8)



boxsize = 20
bbx(1) = -boxsize: bby(1) = -boxsize: bbz(1) = -boxsize
bbx(2) = boxsize:  bby(2) = -boxsize: bbz(2) = -boxsize
bbx(3) = -boxsize: bby(3) = boxsize:  bbz(3) = -boxsize
bbx(4) = boxsize:  bby(4) = boxsize:  bbz(4) = -boxsize
bbx(5) = -boxsize: bby(5) = -boxsize: bbz(5) = boxsize
bbx(6) = boxsize:  bby(6) = -boxsize: bbz(6) = boxsize
bbx(7) = -boxsize: bby(7) = boxsize:  bbz(7) = boxsize
bbx(8) = boxsize:  bby(8) = boxsize:  bbz(8) = boxsize
nl = 12
DIM pt(nl), pt2(nl)
pt(1) = 1: pt2(1) = 2: pt(2) = 2: pt2(2) = 4: pt(3) = 4: pt2(3) = 3
pt(4) = 3: pt2(4) = 1: pt(5) = 1: pt2(5) = 5: pt(6) = 5: pt2(6) = 6
pt(7) = 6: pt2(7) = 8: pt(8) = 8: pt2(8) = 7: pt(9) = 7: pt2(9) = 5
pt(10) = 7: pt2(10) = 3: pt(11) = 8: pt2(11) = 4: pt(12) = 6: pt2(12) = 2
FOR l = 1 TO nl: LINE (bsx(pt(l)), bsy(pt(l)))-(bsx(pt2(l)), bsy(pt2(l))), 1: NEXT l'
yx = 10
yy = 5

 
 
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



  FOR p = 1 TO 8
    tx = bbx(p) + yx: ty = bby(p) + yy: tz = bbz(p)
        'rotate around x
    ttz = tz * COS(xang * .01745) + ty * SIN(xang * .01745)
    tty = ty * COS(xang * .01745) - tz * SIN(xang * .01745)
    tz = ttz: ty = tty
                'rotate around y
    ttx = tx * COS(yang * .01745) - ty * SIN(yang * .01745)
    tty = ty * COS(yang * .01745) + tx * SIN(yang * .01745)
    tx = ttx: ty = tty
                ' rotate around z
    ttx = tx * COS(zang * .01745) + tz * SIN(zang * .01745)
    ttz = tz * COS(zang * .01745) - tx * SIN(zang * .01745)
    tz = ttz: tx = ttx
   
    'tx = tx + yx
    'ty = ty + yy
    'tz = tz + yz
   
    bsx(p) = 160 - (zf * (tx / (tz - dist)))
    bsy(p) = 100 - (zf * (ty / (tz - dist)))
  NEXT p'------------------------------------------------------------------------



FOR l = 1 TO nl: LINE (bsx(pt(l)), bsy(pt(l)))-(bsx(pt2(l)), bsy(pt2(l))), 1: NEXT l

PCOPY 1, 0: a$ = UCASE$(INKEY$): LOOP UNTIL a$ = CHR$(27)

DATA Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,-,Û
DATA Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û,Û

SUB drawguy
size = 1
spin = 1
bodyc = 1
headc = 1
CIRCLE ((Sx(6) + Sx(4)) / 2, 180), 25, 8, , , .15
'arm 2

LINE (Sx(1) + ((Sx(2) - Sx(1)) / 3) - 1, Sy(1) + ((Sy(2) - Sy(1)) / 3) - 1)-(Sx(9), Sy(9)), bodyc
LINE (Sx(1) + ((Sx(2) - Sx(1)) / 3) + 1, Sy(1) + ((Sy(2) - Sy(1)) / 3) + 1)-(Sx(9), Sy(9)), bodyc

'IF bigarm  < 2 AND fistc1  >= 0 THEN
'IF fistc2  < 0 THEN fistc2  = (-fistc2 ) + .1
LINE (Sx(9) - ((spin * size) / 2), Sy(9) - (size / 2))-(Sx(10), Sy(10)), fistc1
LINE (Sx(9) + ((spin * size) / 2), Sy(9) + (size / 2))-(Sx(10), Sy(10)), fistc2
CIRCLE (Sx(10), Sy(10)), 2 * qqq, fistc1: IF INSTR(1, STR$(fistc2), ".1") THEN fistc2 = (-fistc2) + .1
'END IF
'body
'IF bodyc  >= 0 THEN
LINE (Sx(1) + ((Sx(2) - Sx(1)) / 3) - (spin * size), Sy(1) + ((Sy(2) - Sy(1)) / 3) - size)-(Sx(2), Sy(2)), bodyc
LINE (Sx(1) + ((Sx(2) - Sx(1)) / 3) + (spin * size), Sy(1) + ((Sy(2) - Sy(1)) / 3) + size)-(Sx(2), Sy(2)), bodyc
'END IF
'IF qrq <> 0 THEN SWAP size , biglegs
'upper legs
'IF legc  >= 0 THEN
LINE (Sx(2) - (spin * size), Sy(2) - size)-(Sx(3), Sy(3)), legc
LINE (Sx(2) + (spin * size), Sy(2) + size)-(Sx(3), Sy(3)), legc
LINE (Sx(2) - (spin * size), Sy(2) - size)-(Sx(5), Sy(5)), legc
LINE (Sx(2) + (spin * size), Sy(2) + size)-(Sx(5), Sy(5)), legc
'END IF
'lower legs
'IF bootc  >= 0 THEN
LINE (Sx(3) - ((spin * size) / 2), Sy(3) - (size / 2))-(Sx(4), Sy(4)), bootc
LINE (Sx(3) + ((spin * size) / 2), Sy(3) + (size / 2))-(Sx(4), Sy(4)), bootc
CIRCLE (Sx(4), Sy(4)), 4 * qqq, bootc, , , .2
LINE (Sx(5) - ((spin * size) / 2), Sy(5) - (size / 2))-(Sx(6), Sy(6)), bootc
LINE (Sx(5) + ((spin * size) / 2), Sy(5) + (size / 2))-(Sx(6), Sy(6)), bootc
CIRCLE (Sx(6), Sy(6)), 4 * qqq, bootc, , , .2
'END IF
IF qrq <> 0 THEN SWAP size, biglegs
'arm 1
IF bodyc >= 0 THEN
LINE (Sx(1) + ((Sx(2) - Sx(1)) / 3) - 1, Sy(1) + ((Sy(2) - Sy(1)) / 3) - 1)-(Sx(7), Sy(7)), bodyc
LINE (Sx(1) + ((Sx(2) - Sx(1)) / 3) + 1, Sy(1) + ((Sy(2) - Sy(1)) / 3) + 1)-(Sx(7), Sy(7)), bodyc
END IF
IF bigarm = 0 AND fistc2 >= 0 THEN
IF fistc1 < 0 THEN fistc1 = (-fistc1) + .1
LINE (Sx(7) - ((spin * size) / 2), Sy(7) - (size / 2))-(Sx(8), Sy(8)), fistc1
LINE (Sx(7) + ((spin * size) / 2), Sy(7) + (size / 2))-(Sx(8), Sy(8)), fistc2
CIRCLE (Sx(8), Sy(8)), 2 * qqq, fistc1: IF INSTR(1, STR$(fistc1), ".1") THEN fistc1 = (-fistc1) + .1
END IF
'head
IF headc >= 0 THEN
CIRCLE (Sx(1), Sy(1)), 11 * qqq, headc
IF headc >= 8 THEN CIRCLE (Sx(1), Sy(1)), 11 * qqq, headc - 8, 4, 0
IF headc = 7 THEN CIRCLE (Sx(1), Sy(1)), 11 * qqq, 8, 4, 0
EXIT SUB
END IF

END SUB

 SUB shit
    
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


'                    'ROTATION LOOP------------------------------------------
'             FOR p = 1 TO v
'               tx = bx(p): ty = by(p): tz = bz(p)
'               ttz = tz * COS(xang * .01745) + ty * SIN(xang * .01745)
'               tty = ty * COS(xang * .01745) - tz * SIN(xang * .01745)
'               tz = ttz: ty = tty
'               ttx = tx * COS(yang * .01745) - ty * SIN(yang * .01745)
'               tty = ty * COS(yang * .01745) + tx * SIN(yang * .01745)
'               tx = ttx: ty = tty
'               ttx = tx * COS(zang * .01745) + tz * SIN(zang * .01745)
'               ttz = tz * COS(zang * .01745) - tx * SIN(zang * .01745)
'               tz = ttz: tx = ttx
'               tx = tx + yx: ty = ty + yy: tz = tz + yz
'               sx(p) = 160 - (zf * (tx / (tz - dist)))
'               sy(p) = 100 - (zf * (ty / (tz - dist)))
'             NEXT p'------------------------------------------------------------------------


END SUB

