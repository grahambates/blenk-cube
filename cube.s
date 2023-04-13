                incdir "include"
                include "hw.i"
                include "PhotonsMiniWrapper1.04.i"
                include     "macros.i"

********************************************************************************
* Constants:
********************************************************************************

DOT_SHIFT = 8                                   ; Dot product for maximum brightness (15<<DOT_SHIFT)
CUBE_SIZE = 500
MAX_VERTS = 14                                  ; Maximum number of vertices per object before clipping
MAX_FACE_VERTS = 16                             ; Maximum number of vertices per face after clipping - used for size of vertex buffers
MAX_RESTORES = 6
ARR_END = $8000
FILL = 1
DIST_SHIFT = 8
ZOOM = 600

X_SPEED = 1                                     ; rotation speeds
Y_SPEED = 2
Z_SPEED = 3

; Display window:
DIW_W = 320
DIW_H = 256
BPLS = 3
SCROLL = 0                                      ; enable playfield scroll
INTERLEAVED = 0

; Screen buffer:
SCREEN_W = DIW_W
SCREEN_H = DIW_H

DMA_SET = DMAF_SETCLR!DMAF_MASTER!DMAF_RASTER!DMAF_COPPER!DMAF_BLITTER
INTSET = INTF_SETCLR!INTF_INTEN!INTF_VERTB

;-------------------------------------------------------------------------------
; Derived

COLORS = 1<<BPLS

SCREEN_BW = SCREEN_W/16*2                       ; byte-width of 1 bitplane line
                ifne        INTERLEAVED
SCREEN_MOD = SCREEN_BW*(BPLS-1)                 ; modulo (interleaved)
SCREEN_BPL = SCREEN_BW                          ; bitplane offset (interleaved)
                else
SCREEN_MOD = 0                                  ; modulo (non-interleaved)
SCREEN_BPL = SCREEN_BW*SCREEN_H                 ; bitplane offset (non-interleaved)
                endc
SCREEN_SIZE = SCREEN_BW*SCREEN_H*BPLS           ; byte size of screen buffer

DIW_BW = DIW_W/16*2
DIW_MOD = SCREEN_BW-DIW_BW+SCREEN_MOD-SCROLL*2
DIW_SIZE = DIW_BW*DIW_H*BPLS
DIW_XSTRT = ($242-DIW_W)/2
DIW_YSTRT = ($158-DIW_H)/2
DIW_XSTOP = DIW_XSTRT+DIW_W
DIW_YSTOP = DIW_YSTRT+DIW_H
DIW_STRT = (DIW_YSTRT<<8)!DIW_XSTRT
DIW_STOP = ((DIW_YSTOP-256)<<8)!(DIW_XSTOP-256)
DDF_STRT = ((DIW_XSTRT-17)>>1)&$00fc-SCROLL*8
DDF_STOP = ((DIW_XSTRT-17+(((DIW_W>>4)-1)<<4))>>1)&$00fc


********************************************************************************
* Entry points:
********************************************************************************

********************************************************************************
Demo:
;-------------------------------------------------------------------------------
                lea         custom,a6
                bsr         WaitEOF
                move.l      #Cop,cop1lc(a6)
                move.l      #Interrupt,$6c(a4)
                move.w      #DMA_SET,dmacon(a6)
                move.w      #INTSET,intena(a6)

.mainLoop:
                bsr         SwapBuffers
                bsr         Restore

                bsr         Update
                bsr         BuildMatrix
                lea         CubeObj(pc),a0
                bsr         Transform
                lea         CubeObj(pc),a0
                bsr         Draw

                bsr         WaitEOF
                bra.b       .mainLoop
                rts


********************************************************************************
Interrupt:
;-------------------------------------------------------------------------------
        movem.l d0-a6,-(sp)
        lea     custom,a6
        btst    #5,intreqr+1(a6)
        beq.s   .notvb

; Increment frame counter:
        lea     VBlank(pc),a0
        addq.l  #1,(a0)

        move.l      ViewBuffer,a1
        lea         bpl0pt+custom,a2
        rept        BPLS
        move.l      a1,(a2)+
        lea         SCREEN_BPL(a1),a1
        endr

        moveq   #INTF_VERTB,d0
        move.w  d0,intreq(a6)
        move.w  d0,intreq(a6)
.notvb: movem.l (sp)+,d0-a6
        rte

********************************************************************************
* Routines:
********************************************************************************

********************************************************************************
SwapBuffers:
; Swap per-frame buffers
;-------------------------------------------------------------------------------
                movem.l     DrawBuffer(pc),a0-a5
                exg         a0,a1
                exg         a2,a3
                exg         a4,a5
                movem.l     a0-a5,DrawBuffer
                move.l      a5,cop2lc(a6)
                rts


********************************************************************************
Restore:
; Restore previous bounding boxes using blitter
;
; We store the blt regs for the last blit fills applied to the current screen
; buffer. When it comes back round again we can reuse these for a clear
; operation before drawing.
;-------------------------------------------------------------------------------
                move.l      DrawRestore(pc),a1
.l:
                move.w      (a1)+,d0            ; d0 = bltsize
                beq         .done               ; zero marks end of array
                WAIT_BLIT
                move.l      #$01000002,bltcon0(a6)
                move.w      (a1)+,bltdmod(a6)
                move.l      (a1)+,bltdpt(a6)
                move.w      d0,bltsize(a6)
                bra.b       .l
.done           rts


********************************************************************************
Update:
; Update object transform for this frame
;-------------------------------------------------------------------------------
                lea         CubeObj(pc),a0
; Rotate:
                movem.w     Obj_XRot(a0),d0-d2
                add.w       #X_SPEED*2,d0       ; pre-multiply for use as long-word offsets
                add.w       #Y_SPEED*2,d1
                add.w       #Z_SPEED*2,d2
                move.w      #$200-1,d3          ; clamp to 256*2
                and.w       d3,d0
                and.w       d3,d1
                and.w       d3,d2
                movem.w     d0-d2,Obj_XRot(a0)
; Translate
                lea         Sin(pc),a1
                move.l      VBlank,d2

                move.w      d2,d0
                add.w       d0,d0
                add.w       d0,d0
                and.w       #$fe,d0
                add.w       d0,d0
                move.w      (a1,d0),d1
                asr.w       #7,d1
                move.w      d1,Obj_X(a0)

                move.w      d2,d0
                and.w       #$fe,d0
                add.w       d0,d0
                move.w      (a1,d0),d1
                asr.w       #8,d1
                move.w      d1,Obj_Y(a0)

                rts


********************************************************************************
BuildMatrix:
; Build rotation matrix
;-------------------------------------------------------------------------------
; a0 - Object
;-------------------------------------------------------------------------------
                movem.w     Obj_XRot(a0),d0-d2  ; d0-d2 = rotation x/y/z
                lea         Cos,a0
                lea         Sin,a1

                move.w      (a1,d2.w),d5        ; sinZ
                move.w      (a0,d2.w),d4        ; cosZ
                move.w      (a1,d1.w),d3        ; sinY
                move.w      (a0,d1.w),d2        ; cosY
                move.w      (a1,d0.w),d1        ; sinX
                move.w      (a0,d0.w),d0        ; cosX

                lea         Matrix(pc),a0

                move.w      d2,d6
                muls        d4,d6               ; cos(Y) * cos(Z)
                swap        d6
                move.w      d6,Mat_A(a0)

                move.w      d2,d7
                muls        d5,d7               ; cos(Y) * sin(Z)
                swap        d7
                move.w      d7,Mat_B(a0)

                muls        d7,d6               ; A*B
                move.l      d6,Mat_AB(a0)

                move.w      d3,d6
                neg.w       d6
                asr.w       #1,d6               ; we lose a bit cause of the swap
                move.w      d6,Mat_C(a0)        ; -sin(Y)

                move.w      d2,d6
                muls        d1,d6               ; cos(Y) * sin(X)
                swap        d6
                move.w      d6,Mat_F(a0)

                move.w      d2,d6
                muls        d0,d6               ; cos(Y) * cos(X)
                swap        d6
                move.w      d6,Mat_I(a0)

                move.w      d1,d6
                muls        d3,d6               ; sin(X) * sin(Y)
                swap        d6
                rol.l       #1,d6
                move.w      d6,a1

                muls        d4,d6               ; sin(X)*sin(Y)*cos(Z)
                move.w      d0,d7
                muls        d5,d7               ; cos(X) * sin(Z)
                sub.l       d7,d6
                swap        d6
                move.w      d6,Mat_D(a0)

                move.w      a1,d6
                muls        d5,d6               ; sin(X)*sin(Y)*sin(Z)
                move.w      d0,d7
                muls        d4,d7               ; cos(X) * cos(Z)
                add.l       d7,d6
                swap        d6
                move.w      d6,Mat_E(a0)

                muls        Mat_D(a0),d6        ; D*E
                move.l      d6,Mat_DE(a0)

                move.w      d0,d6
                muls        d3,d6               ; cos(X) * sin(Y)
                swap        d6
                rol.l       #1,d6
                move.w      d6,a1

                muls        d4,d6               ; cos(X)*sin(Y)*cos(Z)
                move.w      d1,d7
                muls        d5,d7               ; sin(X) * sin(Z)
                add.l       d7,d6
                swap        d6
                move.w      d6,Mat_G(a0)

                move.w      a1,d6
                muls        d5,d6               ; cos(X)*sin(Y)*sin(Z)
                move.w      d1,d7
                muls        d4,d7               ; sin(X) * cos(Z)
                sub.l       d7,d6
                swap        d6
                move.w      d6,Mat_H(a0)

                muls        Mat_G(a0),d6        ; G*H
                move.l      d6,Mat_GH(a0)

                rts


********************************************************************************
Matrix:
; 3x3 rotation matrix
;-------------------------------------------------------------------------------
                dc.w        0,0,0               ; A  B  C
                dc.w        0,0,0               ; D  E  F
                dc.w        0,0,0               ; G  H  I
                dc.l        0,0,0               ; AB DE GH

                rsreset
Mat_A           rs.w        1
Mat_B           rs.w        1
Mat_C           rs.w        1
Mat_D           rs.w        1
Mat_E           rs.w        1
Mat_F           rs.w        1
Mat_G           rs.w        1
Mat_H           rs.w        1
Mat_I           rs.w        1
; Pre-multiply pairs for later optimisation
Mat_AB          rs.l        1
Mat_DE          rs.l        1
Mat_GH          rs.l        1
Mat_SIZEOF      rs.b        0


********************************************************************************
Transform:
; Combined operation to rotate, translate and apply perspective to all the
; vertices in an object. The resulting 2d vertices are written back to the
; temporary 'Transformed' buffer.
;
; Uses optimised matrix multiplication algorithm:
; https://amycoders.org/opt/avoidmuls.html
;
; Usual algorithm:
; x'=A*x+B*y+C*z
; y'=D*x+E*y+F*z
; z'=G*x+H*y+I*z

; Reduce to two unique multiplications per row:
; (A+y) * (B+x) +C*z -A*B -y*x = A*x +B*y +C*z
; x*y is constant per vertex
; A*B is constant per matrix
;
; x' = (A+y) * (B+x) +C*z -A*B -y*x
; y' = (D+y) * (E+x) +F*z -D*E -y*x
; z' = (G+y) * (H+x) +I*z -G*H -y*x
;-------------------------------------------------------------------------------
; a0 - Object
;-------------------------------------------------------------------------------
                lea         Transformed,a1      ; Destination for transformed 2D vertices
                movem.l     Matrix+Mat_AB,a2-a4 ; a2-a4 = matrix pre-multiplied pairs
                move.l      Obj_Verts(a0),a5    ; Original 3D vertices
                move.w      Obj_VertCnt(a0),d0  ; Vertex counts
.l
                move.l      (a5)+,d4            ; d4 = x*y
                movem.w     (a5)+,d5-d7         ; d5-d7 = x/y/x
; x' = (A+y) * (B+x) +C*z -A*B -y*x
                move.w      d5,d1               ; x
                move.w      d6,d2               ; y
                add.w       Matrix+Mat_A(pc),d2 ; A+y
                add.w       Matrix+Mat_B(pc),d1 ; B+x
                muls        d2,d1               ; (A+y) * (B+x)
                move.w      d7,d2               ; z
                muls        Matrix+Mat_C(pc),d2
                add.l       d2,d1               ; -C*z
                sub.l       a2,d1               ; -A*B
                sub.l       d4,d1               ; -x*y
                swap        d1                  ; d1 = x'
; y' = (D+y) * (E+x) +F*z -D*E -y*x
                move.w      d5,d2               ; x
                move.w      d6,d3               ; y
                add.w       Matrix+Mat_D(pc),d3 ; D+y
                add.w       Matrix+Mat_E(pc),d2 ; E+x
                muls        d3,d2               ; (D+y) * (E+x)
                move.w      d7,d3               ; z
                muls        Matrix+Mat_F(pc),d3
                add.l       d3,d2               ; -F*z
                sub.l       a3,d2               ; -D*E
                sub.l       d4,d2               ; -x*y
                swap        d2                  ; d2 = y'
; z' = (G+y) * (H+x) +I*z -G*H -y*x
                add.w       Matrix+Mat_G(pc),d6 ; G*y
                add.w       Matrix+Mat_H(pc),d5 ; H*x
                muls        d6,d5               ; (G+y) * (H+x)
                muls        Matrix+Mat_I(pc),d7
                add.l       d7,d5               ; +I*z
                sub.l       a4,d5               ; -G*H
                sub.l       d4,d5               ; -x*y
                swap        d5                  ; d5 = z'

; Translate
                add.w       Obj_X(a0),d1        ; x
                ext.l       d1
                add.w       Obj_Y(a0),d2        ; y
                ext.l       d2
                add.w       Obj_Z(a0),d5        ; z

; Apply perspective:
                add.w       #ZOOM,d5            ; apply zoom
                beq.s       .noDivs             ; Don't divide by zero
                lsl.l       #DIST_SHIFT,d1      ; x=x*d
                lsl.l       #DIST_SHIFT,d2      ; y=y*d
                divs        d5,d1               ; x'=x*d/z
                divs        d5,d2               ; y'=y*d/z
.noDivs
                add.w       #SCREEN_W/2,d1      ; center in screen
                add.w       #SCREEN_H/2,d2
                move.w      d1,(a1)+            ; write 2d vertex to output
                move.w      d2,(a1)+

                dbf         d0,.l
                rts


********************************************************************************
Draw:
; Draw object to screen buffer
;-------------------------------------------------------------------------------
; a0 - Object
;-------------------------------------------------------------------------------
                move.l      DrawRestore(pc),RestorePos
                move.l      DrawPal(pc),a3
                addq        #6,a3
                move.l      Obj_Faces(a0),a4
                lea         Transformed,a5
                lea         custom,a6

                move.l      DrawBuffer(pc),DrawDest

                move.w      Obj_FaceCnt(a0),d7
;-------------------------------------------------------------------------------
; Process each face:
.face:
                bsr.w       DotProd             ; Skip backfaces
                ble         .skipFace

                ; Fade color based on dot product
                cmp.w       #15<<DOT_SHIFT,d0   ; clamp to max
                ble.b       .dotOk
                move.w      #15<<DOT_SHIFT,d0
.dotOk
                move.w      #DOT_SHIFT,d2
                asr.w       d2,d0

                move.w      (a4)+,d1            ; Set color
                add.w       d0,d1
                add.w       d1,d1
                lea         Colors,a0
                move.w      (a0,d1),d1
                move.w      d1,(a3)
                move.w      d1,4(a3)
                lea         8(a3),a3

;-------------------------------------------------------------------------------
; Lookup vertices and build polygon array
                move.w      (a4)+,d6            ; d6 = vertex count
                moveq       #0,d0
                moveq       #0,d2               ; d2 = clipping required?
                lea         Poly,a2
.vert:
                move.w      (a4)+,d0            ; d0 = vertex index
                move.l      (a5,d0.w),d1        ; d1 = vertex x/y : high/low word
                move.l      d1,(a2)+            ; add to polygon

                ; Check if vertex is out of clipping bounds
                tst.w       d1
                blt.b       .clipped
                cmp.w       #SCREEN_H-1,d1
                bgt.b       .clipped
                swap        d1
                tst.w       d1
                blt.b       .clipped
                cmp.w       #SCREEN_W-1,d1
                bgt.b       .clipped

                bra         .nextVert           ; no clipping
.clipped        moveq       #1,d2               ; Flag clipping required for this face

.nextVert       dbf         d6,.vert
                move.w      #ARR_END,(a2)+      ; end array
; /vert ------------------------------------------------------------------------

; Clip polygon if required
                cmp.w       #0,d2               ; clipping required?
                beq         .noClip
                lea         Poly,a0
                bsr         Clip
; Skip if polygon array is now empty i.e. all points off-screen
                lea         Poly,a1
                cmp.w       #ARR_END,(a1)
                beq         .skipDraw
.noClip:

; Draw lines:
                move.l      DrawDest(pc),a0
                lea         Poly,a1
                WAIT_BLIT                       ; Prepare common blit regs for line draw
                move.w      #SCREEN_BW,bltcmod(a6)
                move.l      #-$8000,bltbdat(a6)
                move.l      #-1,bltafwm(a6)
;-------------------------------------------------------------------------------
; Blit each line in polygon
.line:
                movem.w     (a1),d0-d3          ; Next two points
                cmp.w       #ARR_END,d2
                beq.b       .endLines
                bsr         DrawLine
                addq        #4,a1               ; Increment one point
                bra.b       .line
.endLines:
; /line-------------------------------------------------------------------------

; Calculate and fill bounding box
                lea         Poly,a1
                bsr         GetBounds
                bsr         FillBounds

                add.l       #SCREEN_BPL,DrawDest ; next bpl
.skipDraw
                dbf         d7,.face

; /face ------------------------------------------------------------------------

                move.l      RestorePos(pc),a1   ; End restore array
                move.l      #0,(a1)
                rts

.skipFace:
; Use vertex count to move to next face struct
                move.w      Face_VertCnt(a4),d6
                add.w       d6,d6
                addq        #6,d6
                lea         (a4,d6.w),a4
                dbf         d7,.face


********************************************************************************
DotProd:
; Calculate dot product
; If positive the face is visible
;-------------------------------------------------------------------------------
; a5 = Transformed vertices
; a4 = Face
; returns:
; d0 = (y1-y2)*(x2-x3)-(y2-y3)*(x1-2)
; trashes:
; d1-d5
;-------------------------------------------------------------------------------
                moveq       #0,d5
                move.w      Face_Verts(a4),d5   ; 1st vertex idx
                movem.w     (a5,d5.w),d0-d1     ; d0 = x1, d1 = y1
                move.w      Face_Verts+2(a4),d5 ; 2nd vertex idx
                movem.w     (a5,d5.w),d2-d3     ; d2 = x2, d3 = y2
                move.w      Face_Verts+4(a4),d5 ; 3rd vertex idx
                movem.w     (a5,d5.w),d4-d5     ; d4 = x3, d5 = y3

                sub.w       d2,d0               ; d0 = x1-x2
                sub.w       d4,d2               ; d2 = x2-x3
                sub.w       d3,d1               ; d1 = y1-y2
                sub.w       d5,d3               ; d5 = y2-y3

; Keep within range
                asr.w       d0
                asr.w       d1
                asr.w       d2
                asr.w       d3

                muls        d1,d2               ; d2 = (y1-y2)*(x2-x3)
                muls        d3,d0               ; d0 = (y2-y3)*(x1-x2)

                sub.w       d2,d0               ; d0 = (y1-y2)*(x2-x3)-(y2-y3)*(x1-2)
                rts

********************************************************************************
GetBounds:
; Get max/min values for vertex array
;-------------------------------------------------------------------------------
; a1 - Vertices
; returns:
; d0.w - x1
; d1.w - y1
; d2.w - x2
; d3.w - y2
;-------------------------------------------------------------------------------
; Initial min/max values
                move.w      #$4000,d0           ; d0 = x1
                move.w      #$4000,d1           ; d1 = y1
                moveq       #0,d2               ; d2 = x2
                moveq       #0,d3               ; d3 = y2

;-------------------------------------------------------------------------------
.vert:
                move.w      (a1)+,d4            ; d4 = x
                cmp.w       #ARR_END,d4         ; end of array?
                beq.b       .endVerts
                move.w      (a1)+,d5            ; d5 = y
; Compare with current min/max
                cmp.w       d4,d0
                blt.b       .notX1
                move.w      d4,d0
.notX1          cmp.w       d5,d1
                blt.b       .notY1
                move.w      d5,d1
.notY1          cmp.w       d4,d2
                bgt.b       .notX2
                move.w      d4,d2
.notX2          cmp.w       d5,d3
                bgt.b       .notY2
                move.w      d5,d3
.notY2
                bra.b       .vert
.endVerts:
; /vert ------------------------------------------------------------------------
                rts


********************************************************************************
FillBounds:
; Fill bounding box using blitter
;-------------------------------------------------------------------------------
; a0 - destination
; d0.w - x1
; d1.w - y1
; d2.w - x2
; d3.w - y2
;-------------------------------------------------------------------------------
; Calculate blit start and size from bounding box
                asr.w       #4,d0               ; conv x to words
                asr.w       #4,d2
                addq        #1,d2
; Offset pointer for blit start: reverse blit so start from last word
                move.w      d3,d4               ; d4 = (y2-1) * screen byte width
                subq        #1,d4
                lea         ScreenMuls(pc),a1
                add.w       d4,d4
                move.w      (a1,d4.w),d4
                add.w       d2,d4               ; add x offset bytes
                add.w       d2,d4               ; add twice as value is in words
                subq        #2,d4               ; -1 word: start *on* last word, not after
                lea         (a0,d4.w),a2        ; apply offset to dest

                sub.w       d1,d3               ; d3 = dy
                ble.b       .empty
                sub.w       d0,d2               ; d2 = dx (words)
                ble.b       .empty

                lsl.w       #6,d3               ; d3 = bltsize
                add.w       d2,d3

                move.w      #SCREEN_BW,d4       ; d4 = bltamod
                sub.w       d2,d4
                sub.w       d2,d4

                ifne        FILL
                WAIT_BLIT_NASTY
                ; move.w             #-1,bltadat(a6)
                ; move.l             #$01f00002,bltcon0(a6)
                move.l      #$09f00012,bltcon0(a6)
                move.w      d4,bltamod(a6)
                move.w      d4,bltdmod(a6)
                move.l      a2,bltapt(a6)
                move.l      a2,bltdpt(a6)
                move.w      d3,bltsize(a6)
                endc

                move.l      RestorePos(pc),a1
                move.w      d3,(a1)+
                move.w      d4,(a1)+
                move.l      a2,(a1)+
                move.l      a1,RestorePos

.empty          rts


********************************************************************************
DrawLine:
; Draw a line for filling using the blitter
; Based on TEC, but with muls LUT
;-------------------------------------------------------------------------------
; d0.w - x1
; d1.w - y1
; d2.w - x2
; d3.w - y2
; a0 - Draw buffer
; a6 - Custom
;-------------------------------------------------------------------------------
                cmp.w       d1,d3
                bgt.s       .l0
                beq.s       .done
                exg         d0,d2
                exg         d1,d3
.l0             moveq       #0,d4
                move.w      d1,d4
                add.w       d4,d4
                move.w      ScreenMuls(pc,d4.w),d4
                move.w      d0,d5
                add.l       a0,d4
                asr.w       #3,d5
                add.w       d5,d4
                moveq       #0,d5
                sub.w       d1,d3
                sub.w       d0,d2
                bpl.s       .l1
                moveq       #1,d5
                neg.w       d2
.l1             move.w      d3,d1
                add.w       d1,d1
                cmp.w       d2,d1
                dbhi        d3,.l2
.l2             move.w      d3,d1
                sub.w       d2,d1
                bpl.s       .l3
                exg         d2,d3
.l3             addx.w      d5,d5
                add.w       d2,d2
                move.w      d2,d1
                sub.w       d3,d2
                addx.w      d5,d5
                and.w       #15,d0
                ror.w       #4,d0
                or.w        #$a4a,d0
                WAIT_BLIT
                move.w      d2,bltaptl(a6)
                sub.w       d3,d2
                lsl.w       #6,d3
                addq.w      #2,d3
                move.w      d0,bltcon0(a6)
                move.b      .oct(pc,d5.w),bltcon1+1(a6)
                move.l      d4,bltcpt(a6)
                move.l      d4,bltdpt(a6)
                movem.w     d1/d2,bltbmod(a6)
                move.w      d3,bltsize(a6)
.done           rts
.oct            dc.b        3,3+64,19,19+64,11,11+64,23,23+64

; Multiplication LUT for screen byte width
ScreenMuls:
                rept        SCREEN_H
                dc.w        SCREEN_BW*REPTN
                endr


********************************************************************************
Clip:
; Sutherland-Hodgman clipping algorithm
; Based on Planet-Rocklobster
;-------------------------------------------------------------------------------
; a0 - source polygon (overwritten)
;-------------------------------------------------------------------------------
                move.l      a0,-(sp)            ; keep reference to source
                move.w      #SCREEN_W-1,d5      ; d5 = MAX_X
                move.w      #SCREEN_H-1,d6      ; d6 = MAX_Y

;-------------------------------------------------------------------------------
; Right:
                lea         ClipRight,a1        ; a1 = dest
                move.l      a1,a2               ; keep reference for first point
.lRight:
                movem.w     (a0),d0-d3          ; get 2 points from source - current and next
                addq        #4,a0               ; increment 1 point
                cmpi.w      #ARR_END,d2         ; end of array?
                beq.b       .endRight

                moveq       #0,d4               ; d4 = inside state
; Check p1:
                cmp.w       d5,d0               ; p1 > MAX_X?
                bgt.b       .p1Right
                bset        #0,d4               ; p1 is inside - set bit
                move.w      d0,(a1)+            ; add to output
                move.w      d1,(a1)+
.p1Right:

; Check p2:
                cmp.w       d5,d2               ; p2 > MAX_X?
                bgt.b       .p2Right
                bchg        #0,d4               ; toggle inside state - this will tell us if state for both points is the same
.p2Right:

                tst.w       d4                  ; clipping needed?
; If states for p1 and p2 are the same the bit will cancel out to zero
; Nothing to do in either case - either both visible (p1 was added) or both offscreen (no points added)
                beq.b       .noClipRight

; Do clipping:
; Find intersection point
                sub.w       d0,d2               ; x2-x1
                sub.w       d1,d3               ; y2-y1
                move.w      d5,d4
                sub.w       d0,d4               ; MAX_X-x1
                muls.w      d4,d3               ; (y2-y1)*(MAX_X-x1)
                divs        d2,d3               ; (y2-y1)*(MAX_X-x1)/(x2-x1)
                add.w       d3,d1               ; y1+(y2-y1)*(MAX_X-x1)/(x2-x1)
; Add clippoint:
                move.w      d5,(a1)+            ; x = MAX_X
                move.w      d1,(a1)+            ; y = intersection point
.noClipRight:

                bra.b       .lRight
.endRight:
                move.l      (a2)+,(a1)+         ; add first point to close loop
                move.w      #ARR_END,(a1)+

;-------------------------------------------------------------------------------
; Left:
                lea         ClipRight,a0
                lea         ClipLeft,a1
                move.l      a1,a2
.lLeft:
                movem.w     (a0),d0-d3
                addq        #4,a0
                cmpi.w      #ARR_END,d2
                beq.b       .endLeft
                moveq       #0,d4
                tst.w       d0
                blt.b       .p1Left
                bset        #0,d4
                move.w      d0,(a1)+
                move.w      d1,(a1)+
.p1Left:
                tst.w       d2
                blt.b       .p2Left
                bchg        #0,d4
.p2Left:
                tst.w       d4
                beq.b       .noClipLeft
                sub.w       d0,d2               ; x2-x1
                sub.w       d1,d3               ; y2-y1
                neg.w       d0                  ; MIN_X-x1
                muls.w      d0,d3               ; (y2-y1)*(MIN_X-x1)
                divs        d2,d3            ; (y2-y1)*(MIN_X-x1)/(x2-x1)
                add.w       d3,d1               ; y1+(y2-y1)*(MIN_X-x1)/(x2-x1)
                clr.w       (a1)+               ; x = 0
                move.w      d1,(a1)+            ; y = intersection point
.noClipLeft:
                bra.b       .lLeft
.endLeft:
                move.w      (a2)+,(a1)+
                move.w      (a2)+,(a1)+
                move.w      #ARR_END,(a1)+

;-------------------------------------------------------------------------------
; Bottom:
                lea         ClipLeft,a0
                lea         ClipBottom,a1
                move.l      a1,a2
.lBottom:
                movem.w     (a0),d0-d3
                addq        #4,a0
                cmpi.w      #ARR_END,d2
                beq.b       .endBottom
                moveq       #0,d4
                cmp.w       d6,d1
                bgt.b       .p1Bottom
                bset        #0,d4
                move.w      d0,(a1)+
                move.w      d1,(a1)+
.p1Bottom:
                cmp.w       d6,d3
                bgt.b       .p2Bottom
                bchg        #0,d4
.p2Bottom:
                tst.w       d4
                beq.b       .noClipBottom
                sub.w       d1,d3               ; y2-y1
                sub.w       d0,d2               ; x2-x1
                move.w      d6,d4
                sub.w       d1,d4               ; MAX_Y-y1
                muls.w      d4,d2               ; (x2-x1)*(MAX_Y-y1)
                divs        d3,d2            ; (x2-x1)*(MAX_Y-y1)/(y2-y1)
                add.w       d2,d0               ; x1+(x2-x1)*(MAX_Y-y1)/(y2-y1)
                move.w      d0,(a1)+            ; x = intersection point
                move.w      d6,(a1)+            ; y = MAX_Y
.noClipBottom:
                bra.b       .lBottom
.endBottom:
                move.l      (a2)+,(a1)+
                move.w      #ARR_END,(a1)+

;-------------------------------------------------------------------------------
; Top:
                lea         ClipBottom,a0
                move.l      (sp)+,a1
                move.l      a1,a2
.lTop:
                movem.w     (a0),d0-d3
                addq        #4,a0
                cmpi.w      #ARR_END,d2
                beq.b       .endTop
                moveq       #0,d4
                tst.w       d1
                blt.b       .p1Top
                bset        #0,d4
                move.w      d0,(a1)+
                move.w      d1,(a1)+
.p1Top:
                tst.w       d3
                blt.b       .p2Top
                bchg        #0,d4
.p2Top:
                tst.w       d4
                beq.b       .noClipTop
                sub.w       d1,d3               ; y2-y1
                sub.w       d0,d2               ; x2-x1
                neg.w       d1                  ; MIN_Y-y1
                muls.w      d1,d2               ; (x2-x1)*(MIN_Y-y1)
                divs        d3,d2            ; (x2-x1)*(MAX_Y-y1)/(y2-y1)
                add.w       d2,d0               ; x1+(x2-x1)*(MIN_Y-y1)/(y2-y1)
                move.w      d0,(a1)+            ; x = intersection point
                clr.w       (a1)+               ; y = MIN_Y
.noClipTop:
                bra.b       .lTop
.endTop:
                move.l      (a2)+,(a1)+         ; add first point as last to make loop perfect
                move.w      #ARR_END,(a1)+
                rts


********************************************************************************
* Variables
********************************************************************************

VBlank:         dc.l        0
DrawBuffer:     dc.l        Screen2             ; TODO: group into struct
ViewBuffer:     dc.l        Screen1
DrawRestore:    dc.l        Restore2
ViewRestore:    dc.l        Restore1
DrawPal:        dc.l        CopPal2
ViewPal:        dc.l        CopPal1
RestorePos:     dc.l        0                   ; ptr to current position in restore list
DrawDest:       dc.l        0                   ; ptr to current draw bitplane


********************************************************************************
* Data
********************************************************************************

                rsreset
Obj_X           rs.w        1
Obj_Y           rs.w        1
Obj_Z           rs.w        1
Obj_XRot        rs.w        1
Obj_YRot        rs.w        1
Obj_ZRot        rs.w        1
Obj_FaceCnt     rs.w        1
Obj_Faces       rs.l        1
Obj_VertCnt     rs.w        1
Obj_Verts       rs.l        1
Obj_SIZEOF      rs.b        0

                rsreset
Face_Col        rs.w        1
Face_VertCnt    rs.w        1
Face_Verts      rs.w        0

                rsreset
Vert_XY         rs.l        1
Vert_X          rs.w        1
Vert_Y          rs.w        1
Vert_Z          rs.w        1
Vert_SIZEOF     rs.b        0


********************************************************************************
FACE            macro
; Generate Face struct
;-------------------------------------------------------------------------------
; \1 - color index
; \2+ - vertex indeces
;-------------------------------------------------------------------------------
                dc.w        \1*16               ; color index
                dc.w        4                   ; vertex count
                dc.w        \2*4,\3*4,\4*4,\5*4,\2*4 ; vertex indeces pre-multiplied for use as offsets
                endm

********************************************************************************
VERT            macro
; Generate Vert struct
;-------------------------------------------------------------------------------
; \1 - x
; \2 - y
; \3 - z
;-------------------------------------------------------------------------------
                dc.l        \1*\2               ; x*y
                dc.w        \1,\2,\3            ; x/y/z
                endm

CubeObj:
                dc.w        0,0,0               ; Translation x/y/z
                dc.w        64,0,0              ; Rotation x/y/z
                dc.w        6-1                 ; Face count -1
                dc.l        CubeFaces           ; Ptr to face polygon array
                dc.w        8-1                 ; Vertex count - 1
                dc.l        CubeVerts           ; Ptr to vertex array

CubeVerts:
                VERT        CUBE_SIZE,CUBE_SIZE,CUBE_SIZE
                VERT        CUBE_SIZE,-CUBE_SIZE,CUBE_SIZE
                VERT        -CUBE_SIZE,-CUBE_SIZE,CUBE_SIZE
                VERT        -CUBE_SIZE,CUBE_SIZE,CUBE_SIZE
                VERT        CUBE_SIZE,CUBE_SIZE,-CUBE_SIZE
                VERT        CUBE_SIZE,-CUBE_SIZE,-CUBE_SIZE
                VERT        -CUBE_SIZE,-CUBE_SIZE,-CUBE_SIZE
                VERT        -CUBE_SIZE,CUBE_SIZE,-CUBE_SIZE

CubeFaces:
                FACE        2,1,2,3,0
                FACE        1,3,2,6,7
                FACE        2,6,5,4,7
                FACE        1,4,5,1,0
                FACE        0,1,5,6,2
                FACE        0,4,0,3,7

                include     "data/sin.i"

Colors:
; Color 0
                dc.w        $000
                dc.w        $100
                dc.w        $200
                dc.w        $300
                dc.w        $400
                dc.w        $500
                dc.w        $600
                dc.w        $700
                dc.w        $800
                dc.w        $900
                dc.w        $a00
                dc.w        $b00
                dc.w        $c00
                dc.w        $d00
                dc.w        $e00
                dc.w        $f00
; Color 1
                dc.w        $00
                dc.w        $10
                dc.w        $20
                dc.w        $30
                dc.w        $40
                dc.w        $50
                dc.w        $60
                dc.w        $70
                dc.w        $80
                dc.w        $90
                dc.w        $a0
                dc.w        $b0
                dc.w        $c0
                dc.w        $d0
                dc.w        $e0
                dc.w        $f0
; Color 2
                dc.w        $0
                dc.w        $1
                dc.w        $2
                dc.w        $3
                dc.w        $4
                dc.w        $5
                dc.w        $6
                dc.w        $7
                dc.w        $8
                dc.w        $9
                dc.w        $a
                dc.w        $b
                dc.w        $c
                dc.w        $d
                dc.w        $e
                dc.w        $f

*******************************************************************************
                bss
*******************************************************************************

; Working buffer for transformed vertex coordinates
Transformed     ds.l        MAX_VERTS*Vert_SIZEOF
; Polygon for current face
Poly            ds.l        MAX_FACE_VERTS

; Buffers for Clip routine
ClipRight       ds.l        MAX_FACE_VERTS
ClipLeft        ds.l        MAX_FACE_VERTS
ClipBottom      ds.l        MAX_FACE_VERTS

; Stores details of previous blit fills for easy restoration
; bltsize/bltmod/bltdpt(l)
Restore1        ds.w        4*MAX_RESTORES
Restore2        ds.w        4*MAX_RESTORES


*******************************************************************************
                data_c
*******************************************************************************

;--------------------------------------------------------------------------------
; Main copper list:
Cop:
                dc.w        fmode,0
                dc.w        diwstrt,DIW_STRT
                dc.w        diwstop,DIW_STOP
                dc.w        ddfstrt,DDF_STRT
                dc.w        ddfstop,DDF_STOP
CopBplCon:
                dc.w        bplcon0,BPLS<<12+$100
                dc.w        bplcon1,0
                dc.w        bplcon2,0
                dc.w        bplcon3,0
CopBplMod:
                dc.w        bpl1mod,DIW_MOD
                dc.w        bpl2mod,DIW_MOD

                dc.w        copjmp2,0           ; Jump to current palette

;--------------------------------------------------------------------------------
; Palette copper lists:
CopPal1:
                dc.w        color00,0
                dc.w        color01,0
                dc.w        color05,0
                dc.w        color02,0
                dc.w        color06,0
                dc.w        color04,0
                dc.w        color07,0
                dc.w        color03,0
                dc.l        -2
CopPal2:
                dc.w        color00,0
                dc.w        color01,0
                dc.w        color05,0
                dc.w        color02,0
                dc.w        color06,0
                dc.w        color04,0
                dc.w        color07,0
                dc.w        color03,0
                dc.l        -2


*******************************************************************************
                bss_c
*******************************************************************************

; Screen buffers
Screen1:        ds.b        SCREEN_SIZE
Screen2:        ds.b        SCREEN_SIZE

                end
