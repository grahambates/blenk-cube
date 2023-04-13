; Fixed point reciprocal division:
; Replaces DIVS operations with multiply by reciprocal in FP

                ifnd        RECIPDIV_EN
RECIPDIV_EN = 1                                 ; Enable reciprocal division - allow reverting back to divs for comparison
                endc
                ifnd        RECIPDIV_RANGE
RECIPDIV_RANGE = 2000                           ; Range to determine size of lookup table
                endc

********************************************************************************
; Divide using reciprocal LUT
; \1 - Divisor (dn)
; \2 - Dividend (dn)
; \3 - Lookup table ptr (an)
; trashes divisor
;-------------------------------------------------------------------------------
RECIPDIV        macro
                ifeq        RECIPDIV_EN
                divs.w      \1,\2
                else
                add.w       \1,\1
                move.w      (\3,\1),\1
                muls.w      \1,\2
                add.l       \2,\2
                swap        \2
                endc
                endm


********************************************************************************
; Generate lookup table for reciprocal
;-------------------------------------------------------------------------------
RECIPDIV_LUT    macro
                ifne        RECIPDIV_EN
                rept        RECIPDIV_RANGE
                dc.w        -$8000/(RECIPDIV_RANGE-REPTN+3)
                endr
                dc.w        $8000
RecipDiv:       dc.w        0
                dc.w        $7fff               ; Keep in positive range
                rept        RECIPDIV_RANGE
                dc.w        $8000/(REPTN+3)
                endr
                endc
                endm

********************************************************************************
; Blitter
********************************************************************************

WAIT_BLIT:      macro
.l\@:           btst        #DMAB_BLITTER,dmaconr(a6)
                bne.b       .l\@
                endm

WAIT_BLIT_NASTY: macro
                HOG_BLIT
.l\@:           btst        #DMAB_BLITTER,dmaconr(a6)
                bne.b       .l\@
                RELEASE_BLIT
                endm

HOG_BLIT:       macro
                move.w      #DMAF_SETCLR!DMAF_BLITHOG,dmacon(a6)
                endm

RELEASE_BLIT:   macro
                move.w      #DMAF_BLITHOG,dmacon(a6)
                endm