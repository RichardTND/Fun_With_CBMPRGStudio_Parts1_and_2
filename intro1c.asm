;PETSCII Intro by Richard
;Scene World fun with CBMPRGSTUDIO

TGT_C64 ;Set target as C64

; Setup variables

Screen = $0400 ;Assign screen RAM start pos
Colour = $d800 ;Assign colour RAM start pos

; 10 SYS2064

*=$0801

        BYTE    $0B, $08, $0A, $00, $9E, $32, $30, $36, $34, $00, $00, $00

*=$0810
        sei
        jsr SetupMain
        jsr SetupIRQ
        jmp MainLoop


;Main code to Setup and Draw PETSCII to screen
SetupMain        
        lda #0 ;Border and background colour
        sta $d020 
        sta $d021

        lda #$14        
        sta $d018 ;Default char

        ; Draw main PETSCII logo

        ldx #$00
DrawLoop
        lda ScreenData,x        ;Read screen data from logo
        sta Screen,x            ;Places to screen
        lda ScreenData+$100,x
        sta Screen+$100,x
        lda ScreenData+$200,x
        sta Screen+$200,x
        lda ScreenData+$2e8,x
        sta Screen+$2e8,x

        lda ColourData,x        ;Read colour data from logo
        sta Colour,x            ;Places to colour RAM
        lda ColourData+$100,x
        sta Colour+$100,x
        lda ColourData+$200,x
        sta Colour+$200,x
        lda ColourData+$2e8,x
        sta Colour+$2e8,x
        inx
        bne DrawLoop
        rts                     ;End of subroutine.

;Setup Kernal base IRQ Raster interrupt

SetupIRQ
        ldx #<irq1
        ldy #>irq1
        lda #$7f
        stx $0314
        sty $0315
        sta $dc0d
        sta $dd0d
        lda #$32
        sta $d012
        lda #$1b
        sta $d011
        lda #$01
        sta $d01a
        cli

        ;Reset scrolltext 

        lda #<ScrollText
        sta MessRead+1
        lda #>ScrollText
        sta MessRead+2

        rts

;IRQ Raster interrupt 1 - Controlling the smooth scrolling message

irq1    inc $d019
        lda $dc0d
        sta $dd0d
        lda #$2a
        sta $d012

        ; Make colour split for bottom raster

        lda BarColour2 ;Assign bar colour
        sta $d020      
        nop           ;Trigger Raster timing 
        ldx #$0a
        dex
        bne *-1
        lda #6        ;Assign blue
        sta $d020

        lda xpos  ;Assign smooth scrolling
        sta $d016

        lda #1
        sta rt
        
        ldx #<irq2
        ldy #>irq2
        stx $0314
        sty $0315
        jmp $ea7e

;IRQ Raster interrupt 2 - Controlling the static screen

irq2    inc $d019
        lda #$f0
        sta $d012
        lda #8    ;Set still screen 
        sta $d016

; Make colour split for top raster

        lda BarColour1 ;Assign bar colour
        sta $d020
        ldx #$0b       ;Raster timing 
        dex
        bne *-1
        lda #0         ;Assign black
        sta $d020
        ldx #<irq3
        ldy #>irq3
        stx $0314
        sty $0315
        jmp $ea7e

; IRQ Raster interrupt 3 - Scrolling again

irq3    inc $d019
        lda #$fa
        sta $d012
        lda xpos        ;Assign smooth scrolling
        sta $d016
        ldx #<irq1
        ldy #>irq1
        stx $0314
        sty $0315
        jmp $ea7e


;Main loop

MainLoop
        jsr SyncTimer   ;Synchronize timer
        jsr Scroller    ;Scrolling message
        jsr ColourWash  ;Colour cycling 
        ;Check for spacebar 

        lda #16
        bit $dc01
        bne SpaceNotPressed
        jmp ExitIntro
        
SpaceNotPressed
        jmp MainLoop    ;Code loops

;Synchronize timer (rt) outside IRQ raster interrupt

SyncTimer
        lda #0  ;Reset raster timer pointer
        sta rt  
        cmp rt  ;Check that it matches itself
        beq *-3 ;Make the delay
        rts

;Scrolling text routine

Scroller
        lda xpos
        sec
        sbc #2 ;Speed of scrolling text (can be changed)
        and #7 ;Only use 8 values in hires char mode for scrolling
        sta xpos
        bcs ExitScroll

        ;Shifting the characters by each column to form the scrolling effect

        ldx #$00
ScrollLoop
        lda Screen+961,x
        sta Screen+960,x

        ;Colour scroll white (not needed now)

       ; lda #1
       ; sta Colour+960,x
        inx
        cpx #40
        bne ScrollLoop

        ;Check message for @ character. If detected, reset the scroll

MessRead
        lda ScrollText
        bne StoreChar

        lda #<ScrollText
        sta MessRead+1
        lda #>ScrollText
        sta MessRead+2
        jmp MessRead ;After resetting, jump to read next character

        ;Store last character in scroll text to screen

StoreChar
        sta Screen+999
        
        ;Update to next character in scroll text 

        inc MessRead+1 ;Move to next Low byte of self-modifying MessRead
        bne ExitScroll ;Skip if not reached the 256th character on message
        inc MessRead+2 ;Move to next Hi byte of self-modifying MessRead 
ExitScroll
        rts ; End of subroutine

;Colour wash subroutine

ColourWash
        lda WashDelay ;Read pointer WashDelay
        cmp #1 ;Speed of wash
        beq DoWash
        inc WashDelay
        rts

DoWash ;Main colour wash
        lda #0
        sta WashDelay
        ldx WashPointer
        lda ScrollColourTable,x
        sta Colour+999
        lda BarColourTable1,x
        sta BarColour1
        lda BarColourTable2,x
        sta BarColour2
        inx
        cpx #20 ;20 chars colour
        beq LoopWash
        inc WashPointer
        jmp ScrollColours
LoopWash
        lda #0
        sta WashPointer

        ;Wash colour across scroll text
ScrollColours
        ldx #$00
LoopWash2
        lda Colour+961,x
        sta Colour+960,x
        inx
        cpx #39
        bne LoopWash2
        rts
        


;Exit the intro and end all subroutines and exit the program ...

ExitIntro
        sei
        ldx #$31
        ldy #$ea
        lda #$81
        stx $0314
        sty $0315
        sta $dc0d
        lda #$00
        sta $d01a
        sta $d019
        jsr $ff81       ;Default C64 screen.
        cli
        rts

rt      byte 0 ;Raster sync timer, for running features outside IRQ
xpos    byte 0 ;X-Scroll position and delay for scrolling message

WashDelay byte 0 ;Colour wash delay pointer
WashPointer byte 0 ;Colour wash cycle pointer
BarColour1 byte 0 ;Raster bar colour 1
BarColour2 byte 0 ;Raster bar colour 2

;Main colour table for the scrolling message

ScrollColourTable
        byte $06,$04,$0e,$03,$01
        byte $03,$0e,$04,$06,$0b
        byte $02,$08,$0a,$07,$01
        byte $07,$0a,$08,$02,$09
       
;Main colour table for raster bar 1

BarColourTable1
        byte $06,$04,$0e,$03,$01
        byte $03,$0e,$04,$06,$0b
        byte $02,$08,$0a,$07,$01
        byte $07,$0a,$08,$02,$09

;Main colour table for raster bar 2

BarColourTable2
        byte $06,$04,$0e,$03,$01
        byte $03,$0e,$04,$06,$0b
        byte $02,$08,$0a,$07,$01
        byte $07,$0a,$08,$02,$09
        
;Import PETSCII logo video RAM data

*=$2000
ScreenData
        incbin "c64/petscr.bin",2

;Import PETSCII logo colour RAM data

*=$2400
ColourData
        incbin "c64/petcol.bin",2

*=$2800
ScrollText
        text ' ... welcome to the first step of the '
        text 'petscii intro, written as part of fun '
        text 'with cbmprgstudio ...   this example d'
        text 'isplays a petscii logo with a nice scr'
        text 'olling message ...   written as part o'
        text 'f scene world issue 33 ...   please fe'
        text 'el free to mess around with this code '
        text 'and experiment with it if you wish to '
        text ' ...   richard 21st may 2023 ...      '
        text '                                      '
        byte 0

