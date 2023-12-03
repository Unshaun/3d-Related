'X = LEFT  RIGHT
'Y = UP    DOWN
'Z = IN    OUT
'V = # of verticies
'nl = # or lines
'dir = direction
'rx() = recent x
'rx2() = recent x2
'pt() = verticy that line x starts at
'pt2() = verticy that line x goes to

DEFINT A, L, P, R, T  'to get strange effects.
RANDOMIZE TIMER
constmove = 1 'If 0, nothing.  If 1, the object constantly moves on Z.
clr = 256 '256=Green, 65536=Blue, 1=Red, 65793=White, 65792=Cyan, 257=Yellow
       '65537=Purple

delay = 500
CONST pi = 3.141593265358979#
'IF constmove = 1 THEN m = 8
IF constmove = 1 THEN dir = INT(RND * 8) + 1
SCREEN 7
sh = 12: shf = .3: cx = 160: cy = 100

zf = 200: dist = 50: ta = 7

   v = 8
   DIM rx(v), rx2(v), ry(v), ry2(v), x(v), y(v), z(v)
   DIM nx(v), ny(v), nz(v)
   x(1) = -12: y(1) = -12: z(1) = -12
   x(2) = 12: y(2) = -12: z(2) = -12
   x(3) = -12: y(3) = 12: z(3) = -12
   x(4) = 12: y(4) = 12: z(4) = -12
   x(5) = -12: y(5) = -12: z(5) = 12
   x(6) = 12: y(6) = -12: z(6) = 12
   x(7) = -12: y(7) = 12: z(7) = 12
   x(8) = 12: y(8) = 12: z(8) = 12
   FOR p = 1 TO v: nx(p) = x(p): ny(p) = y(p): nz(p) = z(p): NEXT p
   nl = 12
   DIM pt(nl), pt2(nl)
   pt(1) = 1: pt2(1) = 2: pt(2) = 2: pt2(2) = 4: pt(3) = 4: pt2(3) = 3
   pt(4) = 3: pt2(4) = 1: pt(5) = 1: pt2(5) = 5: pt(6) = 5: pt2(6) = 6
   pt(7) = 6: pt2(7) = 8: pt(8) = 8: pt2(8) = 7: pt(9) = 7: pt2(9) = 5
   pt(10) = 7: pt2(10) = 3: pt(11) = 8: pt2(11) = 4: pt(12) = 6: pt2(12) = 2


DO'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        'WHICH DIRECTION KEY PUSHED
            
'if a$ =         
            
            
             FOR p = 1 TO v
               nz(p) = z(p) * COS(xang * .01745) + y(p) * SIN(xang * .01745)
               ny(p) = y(p) * COS(xang * .01745) - z(p) * SIN(xang * .01745)
               z(p) = nz(p)
               y(p) = ny(p)
             NEXT p
             FOR p = 1 TO v
               nx(p) = x(p) * COS(yang * .01745) - y(p) * SIN(yang * .01745)
               ny(p) = y(p) * COS(yang * .01745) + x(p) * SIN(yang * .01745)
               x(p) = nx(p)
               y(p) = ny(p)
             NEXT p
             FOR p = 1 TO v
               nx(p) = x(p) * COS(zang * .01745) + z(p) * SIN(zang * .01745)
               nz(p) = z(p) * COS(zang * .01745) - x(p) * SIN(zang * .01745)
               z(p) = nz(p)
               x(p) = nx(p)
             NEXT p

IF constmove = 1 THEN
   SELECT CASE dir
      CASE 1: cx = cx - m: cy = cy + m
      CASE 2: cx = cx - m
      CASE 3: cx = cx - m: cy = cy - m
      CASE 4: cy = cy - m
      CASE 5: cy = cy - m: cx = cx + m
      CASE 6: cx = cx + m
      CASE 7: cx = cx + m: cy = cy + m
      CASE 8: cy = cy + m
      CASE 9: cx = cx - m: cy = cy + m
   END SELECT
END IF
FOR p = 1 TO v
   rx2(p) = rx(p): ry2(p) = ry(p)
NEXT p
DO: cdelay = cdelay + .1: LOOP UNTIL cdelay >= delay: cdelay = 0

'i$ = INKEY$: IF i$ = "" THEN i$ = recent$

FOR p = 1 TO v
   rx(p) = zf * (nx(p) / (nz(p) - dist)) + cx
   ry(p) = zf * (ny(p) / (nz(p) - dist)) + cy
NEXT p


FOR l = 1 TO nl
   LINE (rx2(pt(l)), ry2(pt(l)))-(rx2(pt2(l)), ry2(pt2(l))), 0
NEXT l
FOR l = 1 TO nl
   LINE (rx(pt(l)), ry(pt(l)))-(rx(pt2(l)), ry(pt2(l))), sh + (z(pt(l)) * shf)
NEXT l
recent$ = i$
IF constmove = 1 THEN
      IF cx >= 640 THEN dir = INT(RND * 3) + 1
      IF cx <= 0 THEN dir = INT(RND * 3) + 5
      IF cy <= 0 THEN dir = INT(RND * 3) + 7
      IF cy >= 480 THEN dir = INT(RND * 3) + 3
END IF
IF i$ = "6" OR i$ = "7" OR i$ = "8" THEN ang = 360 - ta
IF i$ = "4" OR i$ = "2" OR i$ = "9" THEN ang = ta
IF i$ = "3" THEN zf = zf * 1.07
IF i$ = "1" THEN zf = zf * .93

IF a$ = "A" THEN xang = xang + 1
a$ = UCASE$(INKEY$): LOOP UNTIL a$ = CHR$(27)



