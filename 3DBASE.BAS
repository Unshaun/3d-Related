
'-----------------------------USE MOUSE

DECLARE SUB drawguy ()
DECLARE SUB MouseStatus (lb%, rb%, xMouse%, yMouse%)
DECLARE SUB MouseHide ()
DECLARE SUB MouseDriver (AX%, BX%, CX%, DX%)
DECLARE SUB MouseShow ()
DECLARE FUNCTION mouseInit% ()
DIM SHARED MOUSE$
MOUSE$ = SPACE$(57)
FOR i% = 1 TO 57
  READ A$
  H$ = CHR$(VAL("&H" + A$))
  MID$(MOUSE$, i%, 1) = H$
NEXT i%
DATA 55,89,E5,8B,5E,0C,8B,07,50,8B,5E,0A,8B,07,50,8B
DATA 5E,08,8B,0F,8B,5E,06,8B,17,5B,58,1E,07,CD,33,53
DATA 8B,5E,0C,89,07,58,8B,5E,0A,89,07,8B,5E,08,89,0F
DATA 8B,5E,06,89,17,5D,CA,08,00            : CLS
ms% = mouseInit%
MouseShow



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
'zf = how fucked up the z range is

SCREEN 7
RANDOMIZE TIMER
constmove = 1 'If 0, nothing.  If 1, the object constantly moves on Z.
clr = 256 '256=Green, 65536=Blue, 1=Red, 65793=White, 65792=Cyan, 257=Yellow
       '65537=Purple
delay = 500
CONST pi = 3.141593265358979#

IF constmove = 1 THEN dir = INT(RND * 8) + 1

sh = 12
shf = .3
CX = 160
cy = 100
zf = 200
dist = 50
ta = 7

up$ = CHR$(0) + CHR$(72)
dwn$ = CHR$(0) + CHR$(80)
lft$ = CHR$(0) + CHR$(75)
rght$ = CHR$(0) + CHR$(77)

   v = 8
   DIM SHARED BX(v), by(v), bz(v), sx(v), sy(v)
   BX(1) = -12: by(1) = -12: bz(1) = -12
   BX(2) = 12: by(2) = -12: bz(2) = -12
   BX(3) = -12: by(3) = 12:  bz(3) = -12
   BX(4) = 12:  by(4) = 12:  bz(4) = -12
   BX(5) = -12: by(5) = -12: bz(5) = 12
   BX(6) = 12:  by(6) = -12: bz(6) = 12
   BX(7) = -12: by(7) = 12:  bz(7) = 12
   BX(8) = 12:  by(8) = 12:  bz(8) = 12
 
 
   FOR p = 1 TO v: nx(p) = BX(p): ny(p) = by(p): nz(p) = bz(p): NEXT p
   nl = 12
   DIM pt(nl), pt2(nl)
   pt(1) = 1: pt2(1) = 2: pt(2) = 2: pt2(2) = 4: pt(3) = 4: pt2(3) = 3
   pt(4) = 3: pt2(4) = 1: pt(5) = 1: pt2(5) = 5: pt(6) = 5: pt2(6) = 6
   pt(7) = 6: pt2(7) = 8: pt(8) = 8: pt2(8) = 7: pt(9) = 7: pt2(9) = 5
   pt(10) = 7: pt2(10) = 3: pt(11) = 8: pt2(11) = 4: pt(12) = 6: pt2(12) = 2
 
SCREEN 7, , 1, 0
DO: PCOPY 2, 1: '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  LOCATE 2, 1: PRINT x%; y%, lb%; rb%
  MouseStatus lb%, rb%, x%, y%
  zang = -x%
  xang = -y%


        'WHICH DIRECTION KEY PUSHED
            
IF A$ = up$ THEN xang = xang + 10: x = x + 1
IF A$ = dwn$ THEN yang = yang + 10
IF A$ = rght$ THEN zang = zang + 10
IF A$ = " " THEN
'cx = cx - 10
'yx = yx - 2
END IF
LOCATE 1, 1: PRINT CX; yx

                    'ROTATION LOOP------------------------------------------
             FOR p = 1 TO v
               tx = BX(p): ty = by(p): tz = bz(p)
               ttz = tz * COS(xang * .01745) + ty * SIN(xang * .01745)
               tty = ty * COS(xang * .01745) - tz * SIN(xang * .01745)
               tz = ttz: ty = tty
               ttx = tx * COS(yang * .01745) - ty * SIN(yang * .01745)
               tty = ty * COS(yang * .01745) + tx * SIN(yang * .01745)
               tx = ttx: ty = tty
               ttx = tx * COS(zang * .01745) + tz * SIN(zang * .01745)
               ttz = tz * COS(zang * .01745) - tx * SIN(zang * .01745)
               tz = ttz: tx = ttx
               tx = tx + yx: ty = ty + yy: tz = tz + yz
               sx(p) = zf * (tx / (tz - dist)) + 160
               sy(p) = zf * (ty / (tz - dist)) + 100
             NEXT p'------------------------------------------------------------------------

FOR l = 1 TO nl
   LINE (sx(pt(l)), sy(pt(l)))-(sx(pt2(l)), sy(pt2(l))), 10
NEXT l

PCOPY 1, 0: A$ = UCASE$(INKEY$): LOOP UNTIL A$ = CHR$(27)

SUB MouseDriver (AX%, BX%, CX%, DX%)
  DEF SEG = VARSEG(MOUSE$)
  MOUSE% = SADD(MOUSE$)
  CALL Absolute(AX%, BX%, CX%, DX%, MOUSE%)
END SUB

SUB MouseHide
 AX% = 2
 MouseDriver AX%, 0, 0, 0

END SUB

FUNCTION mouseInit%
  AX% = 0
  MouseDriver AX%, 0, 0, 0
  mouseInit% = AX%

END FUNCTION

SUB MouseShow
  AX% = 1
  MouseDriver AX%, 0, 0, 0

END SUB

SUB MouseStatus (lb%, rb%, xMouse%, yMouse%)
  AX% = 3
  MouseDriver AX%, BX%, CX%, DX%
  lb% = ((BX% AND 1) <> 0)
  rb% = ((BX% AND 2) <> 0)
  xMouse% = CX% * .565
  yMouse% = DX% * 1.81

END SUB

 SUB shit
     '        FOR p = 1 TO v
     '          nx = bx(p): ny = by(p): nz = bz(p)
     '
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
     '           '


'   bx(1) = -12: by(1) = -12: bz(1) = -12
'   bx(2) = 12:  by(2) = -12: bz(2) = -12
'   bx(3) = -12: by(3) = 12:  bz(3) = -12
'   bx(4) = 12:  by(4) = 12:  bz(4) = -12
'   bx(5) = -12: by(5) = -12: bz(5) = 12
'   bx(6) = 12:  by(6) = -12: bz(6) = 12
'   bx(7) = -12: by(7) = 12:  bz(7) = 12
'   bx(8) = 12:  by(8) = 12:  bz(8) = 12
'
'
'   FOR p = 1 TO v: nx(p) = bx(p): ny(p) = by(p): nz(p) = bz(p): NEXT p
'   nl = 12
'   DIM pt(nl), pt2(nl)
'   pt(1) = 1: pt2(1) = 2: pt(2) = 2: pt2(2) = 4: pt(3) = 4: pt2(3) = 3
'   pt(4) = 3: pt2(4) = 1: pt(5) = 1: pt2(5) = 5: pt(6) = 5: pt2(6) = 6
'   pt(7) = 6: pt2(7) = 8: pt(8) = 8: pt2(8) = 7: pt(9) = 7: pt2(9) = 5
'   pt(10) = 7: pt2(10) = 3: pt(11) = 8: pt2(11) = 4: pt(12) = 6: pt2(12) = 2
'
'
END SUB

