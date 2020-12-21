
inlin    =$a560
axout    =$bdcd
resetvic =$e518
cls      =$e544
clrln    =$e9ff
dollar   =$ebcd ; dollar sign in ROM to save one byte
lsn      =$ffb1
tlk      =$ffb4
stlk     =$ff96
slsn     =$ff93
acptr    =$ffa5
untlk    =$ffab
unlsn    =$ffae
in       =$ffa5
out      =$ffa8
setfls   =$ffba
setnam   =$ffbd
open     =$ffc0
close    =$ffc3
bsout    =$ffd2
chkin    =$ffc6
clrch    =$ffcc
stop     =$ffe1
getin    =$ffe4
plot     =$fff0

trkbuf   =$1d00
secbuf   =trkbuf+$d00
allocb   =$a000
cx       =211
cy       =214
v1       =$fb
v2       =$45
v3       =$47
b1       =$fd
b2       =$fe
bx       =$02
sectbuf  =$06e8
r0       =v2
r1       =v2+1
r2       =v3
r3       =v3+1
r4       =b1
r5       =b2

          .word $080b
          bcc :+
          .byte $9e,"2049"
          brk
:

          lda #$00
          sta bx
          sta $9d

          lda #$80
          sta 650    ; keyboard repeat
          lda #$04
          sta $ba

:         jsr c1581
          cmp #$ff
          beq menu
          inc $ba
          lda $ba
          cmp #$1f
          bcc :-
          jsr clsprint
          .byte "no 1581 connected...",$0d
          brk
          rts

menu:     jsr vicrespr
          .byte $9f,$0d," ",$12,$be,$a1,$a2,$a1,$bb,$a1,$1d,$1d,$bb,$92
          .byte $be,$12,$ac,$92,$a1,$12,$ac,$92,$a1,$a1," ",$12,$ac,$92
          .byte $be,$0d," ",$12,$a1,$92,$bc,$12,$bb,$a1,$bb,$a1,$92,$bc
          .byte $be,$12,$a1,$92," ",$a1,$a1,$a1,$a1,$a1," ",$12,$a2,$92
          .byte $a1,$0d," ",$bc,$bc,$12,$a2,$92,$bc,$12,$a2,$92,$bc,"  "
          .byte $bc," ",$12,$a2,$92,$be,$12,$a2,$92,$be,$12,$a2,$92,$be
          .byte $12,$a2,$92,$be,$9a,$91,$91,"   1581 device:"
          brk
          lda #$00
          ldx $ba
          jsr axout

          jsr print
          .byte $0d,$01,$1d,$15,$75,$60,$60,"(w) svolli",$60,$60,$69,$0d
          .byte $01,$1d,$15,$6a,$01,$60,$0e
          .byte $6b,$0d,$0d,$12,$01," ("
          .byte $92," ",$a4,$0d,$1d,$12,"d",$92,"irectory"
          .byte $0d," ",$a4,$0d,$1d,$12,"c",$92,"ommand"
          .byte $0d," ",$a4,$0d,$1d,$12,"m",$92,"kdir"
          .byte $0d," ",$a4,$0d,$1d,$12,"s",$92,"ector editor"
          .byte $0d," ",$a4,$0d,$1d,$12,"l",$92,"ocate files"
          .byte $0d," ",$a4,$0d,$1d,$12,"f",$92,"ree 20 blocks"
          .byte $0d," ",$a4,$0d,$1d,$12,"e",$92,"rror scan"
          .byte $0d,"  ",$a4,$0d,$1d,"e",$12,"x",$92,"it"
          .byte $0d,$0d,$12,$01," ",$28,$92
          brk

          lda bx
          beq :+
          sta $07c0
          ldx #$01
          stx $dbc0
          .byte $2c
:         ldx #$00
          lda $ba
          jsr tlk
          lda #$6f
          jsr stlk
errloop:  jsr in
          cmp #$0d
          beq :+
          and #$3f
          sta $07c0,x
          lda #$01
          sta $dbc0,x
          inx
          cpx #$28
          bne errloop
:         jsr untlk
          lda $b8
          jsr close
          lda #$00
          sta bx

key:      jsr getin
          beq key
          ldx #jtab-keys-1
kloop:    cmp keys,x
          beq jump
          dex
          bpl kloop
          bmi key

keys:    .byte "xcdmslfe"
jtab:    .word exit,sendcom,dir,mkdir
         .word secedit,locfile,f20b,scan

jump:     txa
          asl
          tax
          lda jtab+0,x
          sta v1+0
          lda jtab+1,x
          sta v1+1
          jsr jsub
          jmp menu
jsub:     cpx #$03
          bcc nocd
          jsr chkdsk
nocd:     jmp (v1)

exit:     pla
          pla
          jmp $e518

dir:      jsr cls
          lda #$0e
          ldx $ba
          ldy #$60
          jsr setfls
          lda #$01
          ldx #<dollar
          ldy #>dollar
          jsr setnam
          jsr open
          lda $ba
          jsr tlk
          lda #$60
          jsr stlk
          lda #$00
          sta $90
          ldy #$03
l002a:    sty $b7
          jsr acptr
          sta $c1
          ldy $90
          bne l007b
          jsr acptr
          sta $c2
          ldy $90
          bne l007b
          ldy $b7
          dey
          bne l002a
          ldx $c1
          lda $c2
          stx b1
          sta b2
          jsr axout
          lsr b2
          ror b1
          lsr b2
          ror b1
          lda b2
          bne l004f
          lda b1
          cmp #$fa
          bcs l004f
          lda #$20
          jsr bsout
l004f:    jsr acptr
          ldx $90
          bne l007b
          cmp #$00
          beq l0072
          jsr bsout
          jsr stop
          beq l007b
          jsr getin
          beq l004f
          cmp #$20
          bne l004f
          jsr waitkey
          bne l004f
l0072:    lda #$0d
          jsr bsout
          ldy #$02
          bne l002a
l007b:    jsr clrch
          lda #$0e
          jsr close
          jmp waitkey

sendcom:  jsr cls
          jsr inlin
          ldx #$00
chkloop:  lda $0200,x
          beq null
          inx
          bne chkloop
null:     cpx #$01
          bne com
          lda $0200
          cmp #$20
          bne com
          rts
com:      txa
          ldx #$00
          ldy #$02
          jsr setnam
          lda #$01
          ldx $ba
          ldy #$6f
          jsr setfls
          jmp open
waitkey:  jsr getin
          beq waitkey
          rts

vicrespr: jsr resetvic
clsprint: jsr cls
print:    pla
          sta v1
          pla
          sta v1+1
          tay
          pha
          ldy #$00
ploop:    inc v1
          bne nh1
          inc v1+1
nh1:      lda (v1),y
          beq end
          cmp #$01
          beq mp
          jsr bsout
          bcc ploop

mp:       inc v1
          bne nh2
          inc v1+1
nh2:      lda (v1),y
          pha
          inc v1
          bne nh3
          inc v1+1
nh3:      lda (v1),y
          tay
          pla
l01:      jsr bsout
          dey
          bne l01
          beq ploop

end:      pla
          tay
          lda v1+1
          pha
          lda v1
          pha
          rts

hex2:     pha
          lsr
          lsr
          lsr
          lsr
          jsr :+
          tax
          pla
          and #$0f
:         ora #$30
          cmp #$3a
          bcc :+
          adc #$06
:         and #$3f
          tay
          rts

c1581:    lda #$01
          ldx $ba
          ldy #$6f
          jsr setfls
          lda #$06
          ldx #<fchk
          ldy #>fchk
          jsr setnam
          jsr open
          lda $90
          bpl drvcon
          lda #$01
          jmp close

getbyt:   jsr open
drvcon:   lda $ba
          jsr tlk
          lda #$6f
          jsr stlk
          jsr in
          pha
          jsr untlk
          lda #$01
          jsr close
          pla
          rts

send:     jsr open
          lda #$01
          jmp close

chkdsk:   lda #$01
          ldx $ba
          ldy #$6f
          jsr setfls

          lda #$92
          jsr runjob
          and #$fe
          beq diskin
          jsr clsprint
          .byte $0d,$1d,"no disk in drive"
          brk
          pla
          pla
          jmp waitkey

diskin:   lda #$06
          ldx #<dchk
          ldy #>dchk
          jsr setnam
          jsr getbyt
          beq noi

sendi:    lda #$01
          ldx #<i
          ldy #>i
          jsr setnam
          jsr send

noi:      rts

cal:      lda b1
          ldx #$00
          ldy #$00
:         cmp #$0a
          bcc :+
          sec
          sbc #$0a
          inx
          bne :-

:         clc
          adc #$04
          tay
          txa
          clc
          adc #$0d
          tax
          jsr plot

cur:      ldy cx
          lda ($d1),y
          eor #$80
          sta ($d1),y
          rts

numout:   clc
          pha
          jsr plot
          pla
          cmp #$64
          bcs num
          pha
          lda #$20
          jsr bsout
          pla
          cmp #$0a
          bcs num
          pha
          lda #$20
          jsr bsout
          pla
num:      tax
          lda #$00
          jmp axout

ptrklst:  jsr print
          .byte $1d,$1d,$1d,"1234567890"
          .byte $01,$9d,$0b,$11
          .byte $b0,$01,$60,$0a,$ae
          .byte $01,$9d,$0e,$11
          .byte " 1",$7d,$01,".",$0a,$7d,"10"
          .byte $01,$9d,$10,$11
          .byte "11",$7d,$01,".",$0a,$7d,"20"
          .byte $01,$9d,$10,$11
          .byte "21",$7d,$01,".",$0a,$7d,"30"
          .byte $01,$9d,$10,$11
          .byte "31",$7d,$01,".",$0a,$7d,"40"
          .byte $01,$9d,$10,$11
          .byte "41",$7d,$01,".",$0a,$7d,"50"
          .byte $01,$9d,$10,$11
          .byte "51",$7d,$01,".",$0a,$7d,"60"
          .byte $01,$9d,$10,$11
          .byte "61",$7d,$01,".",$0a,$7d,"70"
          .byte $01,$9d,$10,$11
          .byte "71",$7d,$01,".",$0a,$7d,"80"
          .byte $01,$9d,$10,$11
          .byte $1d,$1d,$ad,$01,$60,$0a,$bd
          .byte $01,$9d,$0b,$11
          .byte "1234567890"
          brk
          rts

mkdir:    jsr clsprint
          .byte $0d,$1d,"make directory",$0d
          .byte $0d,$1d,"name of directory:",$0d,$1d,"/",$0d
          .byte $0d,$1d,"first track:"
          .byte $0d,$1d,"last track :"
          .byte $0d,$1d,"blocks used:"
          .byte $0d,$1d,"blocks free:",$0d,$0d,$1d
          brk
          jsr ptrklst

          lda $d021
          ldy #$27
l0b51:    sta $d800,y
          sta $db98,y
          sta $dbc0,y
          dey
          bpl l0b51
          ldx #$04
          ldy #$02
          clc
          jsr plot
          lda #$00
          sta $0400
l0b6a:    jsr print
          .byte $12,$20,$92,$9d
          brk
          jsr getin
          beq l0b6a
          cmp #$0d
          beq l0bab
          cmp #$14
          bne l0b91
          lda $0400
          beq l0b6a
          dec $0400
          jsr print
          .byte " ",$9d,$9d
          brk
          jmp l0b6a
l0b91:    cmp #$20
          bcc l0b6a
          cmp #$60
          bcs l0b6a
          ldx $0400
          cpx #$10
          bcs l0b6a
          sta $0403,x
          jsr bsout
          inc $0400
          bne l0b6a
l0bab:    lda #$20
          jsr bsout
          lda $0400
          bne l0bb6
          rts
l0bb6:    lda #$06
          ldx #<bam
          ldy #>bam
          jsr setnam
          ldx #$00
          stx b1
          lda #$10
          sta bam+3
l0bc8:    lda #$0a
          sta bam+4
          jsr getbyt
          ldx b1
          sta $0798,x
          lda #$7e
          sta $0590,x
          lda #$0b
          sta bam+4
          jsr getbyt
          ldx b1
          sta $07c0,x
          lda #$e2
          sta $0590,x
          inx
          stx b1
          lda bam+3
          clc
          adc #$06
          sta bam+3
          bne l0bc8
          ldx #$0a
          jsr clrln
          lda #$00
          sta l0c42+1
l0c04:    ldy #$00
          ldx #$00
          lda #$0c
          sta $fb
          lda #$06
          sta $fc
l0c10:    lda $0798,x
          stx b1
          tax
          lda #$2f
          cpx #$00
          beq l0c23
          cpx #$28
          bne l0c25
          lda #$2e
          .byte $2c
l0c23:    lda #$2a
l0c25:    sta ($fb),y
          iny
          cpy #$0a
          bne l0c3b
          ldy #$00
          lda $fb
          clc
          adc #$28
          sta $fb
          lda $fc
          adc #$00
          sta $fc
l0c3b:    ldx b1
          inx
          cpx #$50
          bne l0c10
l0c42:    lda #$00
          sta b1
          lda #$20
          ldx #$03
l0c4a:    sta $0525,x
          sta $054d,x
          sta $0575,x
          dex
          bpl l0c4a
l0c56:    ldx b1
          inx
          txa
          ldx #$06
          ldy #$0e
          jsr numout
          jsr cal
l0c64:    jsr getin
          beq l0c64
          pha
          jsr cur
          pla
          cmp #$0d
          beq l0c89
          cmp #$03
          bne l0c77
          rts
l0c77:    cmp #$1d
          beq l0ca2
          cmp #$9d
          beq l0cb6
          cmp #$11
          beq l0ca5
          cmp #$91
          beq l0cb9
          bne l0c56
l0c89:    ldx b1
          cpx #$4e
          bcs l0c56
          lda #$28
          cmp $0798,x
          bne l0c56
          cmp $0799,x
          bne l0c56
          cmp $079a,x
          bne l0c56
          beq l0cc8
l0ca2:    lda #$01
         .byte $2c
l0ca5:    lda #$0a
          clc
          adc b1
          cmp #$50
          bcc l0cb1
          sec
          sbc #$50
l0cb1:    sta b1
          jmp l0c56
l0cb6:    lda #$ff
          .byte $2c
l0cb9:    lda #$f6;
          clc
          adc b1
          bcs l0cc3
          clc
          adc #$50
l0cc3:    sta b1
          jmp l0c56
l0cc8:    lda b1
          sta b2
          jsr cal
          inc b1
          jsr cal
          inc b1
          jsr cal
          lda b1
          sta l0d3d+1
l0cde:    ldx b1
          inx
          txa
          ldx #$07
          ldy #$0e
          jsr numout
          lda b1
          sec
          sbc b2
          pha
          asl
          asl
          ldx #$09
          ldy #$0d
          jsr numout
          lda #$30
          jsr bsout
          pla
          clc
          adc #$01
          asl
          asl
          ldx #$08
          ldy #$0d
          jsr numout
          lda #$30
          jsr bsout
l0d0f:    jsr getin
          beq l0d0f
          ldx b1
          cmp #$03
          bne l0d22
          lda b2
          sta l0c42+1
          jmp l0c04
l0d22:    cmp #$1d
          bne l0d39
          cpx #$4f
          beq l0cde
          lda $0799,x
          cmp #$28
          bne l0cde
          inc b1
          jsr cal
          jmp l0cde
l0d39:    cmp #$9d
          bne l0d48
l0d3d:    cpx #$00
          beq l0cde
          jsr cal
          dec b1
          bne l0cde
l0d48:    cmp #$0d
          bne l0cde
          ldx $0400
          lda #$2f
          sta $0402
          lda #$2c
          sta $0403,x
          lda b2
          clc
          adc #$01
          sta $0404,x
          lda #$00
          sta $0405,x
          sta $0406,x
          sta $0407,x
          lda #$2c
          sta $0408,x
          lda #$43
          sta $0409,x
          lda b1
          sec
          sbc b2
          tay
l0d7c:    lda $0406,x
          clc
          adc #$28
          sta $0406,x
          lda $0407,x
          adc #$00
          sta $0407,x
          dey
          bpl l0d7c
          lda $0400
          clc
          adc #$08
          ldx #$02
          ldy #$04
          jsr setnam
          jsr getbyt
          cmp #$30
          beq l0da7
          sta bx
          rts
l0da7:    lda $b7
          sec
          sbc #$07
          sta $b7
          jsr send
          ldx $0400
          lda #$4e
          sta $0401
          lda #$3a
          sta $0402
          lda #$a0
          sta $0404,x
          sta $0405,x
          lda $b7
          clc
          adc #$04
          sta $b7
          dec $bb
          jsr send
          lda #$2f
          sta $0401
          lda #$01
          sta $b7
          jmp send

secedit:  ldx #$3f
:         sta $02c0,x
          dex
          bpl :-
          lda #$0f
          sta $02c1
          sta $02c4
          sta $02c7
          sta $02ca
          jsr clsprint
          .byte "sector editor:"
          .byte $01,$0d,$0a,$01," ",$07
          .byte "normal",$01," ",$06
          .byte "shift",$01," ",$04
          .byte "commodore",$0d
          .byte $0d,"  f1 "
          .byte " inc. byte "
          .byte " dec. byte "
          .byte " null byte"
          .byte $0d,"  f3"
          .byte $01," ",$04,"read",$01," ",$07
          .byte "write",$01," ",$04
          .byte "jump link"
          .byte $0d,"  f5 "
          .byte " inc.track "
          .byte " dec.track "
          .byte "  restart"
          .byte $0d,"  f7 "
          .byte " inc.sectr "
          .byte " dec.sectr "
          .byte " auto-read"
          brk
          lda #$01
          sta $0427
          lda #$28
          sta track
          lda #$00
          sta sector
seread:   lda #$08
          ldx #<setts
          ldy #>setts
          jsr setnam
          jsr send
          lda #$80
          jsr runjob
          jsr hex2
          sty $040e
          lda #$52
          sta rwmode
          lda #$06
          ldx #<rdwrt
          ldy #>rdwrt
          jsr setnam
          lda #$00
          sta rwlo
          lda #$05
          sta rwhi
l0e48:    jsr open
l0e4b:    ldy #$00
          lda $ba
          jsr tlk
          lda #$6f
          jsr stlk
l0e57:    jsr acptr
          sta sectbuf,y
          iny
          tya
          and #$1f
          bne l0e57
          jsr untlk
          lda #$01
          jsr close
          lda rwlo
          clc
          adc #$20
          sta rwlo
          sta l0e4b+1
          bne l0e48
          lda #$01
          sta $d015
          sta $d017
          sta $d01b
          sta $d01d
          sta $d027
          lda #$0b
          sta $07f8
          lda #$00
          sta b1
          sta b2
          sta bx
setcsr:   lda track
          jsr hex2
          stx $0410
          sty $0411
          lda sector
          jsr hex2
          stx $0413
          sty $0414
          jsr conv
          lda b1
          asl
          asl
          asl
          sta $d000
          lda b2
          asl
          asl
          asl
          clc
          adc #$3a
          sta $d001
          lda b2
          asl
          asl
          asl
          asl
          asl
          ora b1
          sta bx
          jsr hex2
          stx $0417
          sty $0418
          ldx bx
          lda sectbuf,x
          jsr hex2
          stx $041b
          sty $041c
          ldx bx
          inx
          lda sectbuf,x
          jsr hex2
          stx $041e
          sty $041f
:         jsr getin
          beq :-
          tax
          lda $28d
          and #$04
          beq noctrl
          jmp ctrl
noctrl:   txa
          ldx $28d
          cmp #$03
          bne nostp
          lda #$00
          sta bx
          lda dirty
          beq nodirty
          jsr vicrespr
          .byte $0d,$1d,"sector has been modified"
          .byte " but not saved",$0d,$0d,$1d,"save?"
          brk
:         jsr getin
          beq :-
          cmp #'y'
          bne nodirty
          jsr sesave

nodirty:  rts

nostp:    ldy #sejtab-sekeys-1
:         cmp sekeys,y
          beq sejump
          dey
          bpl :-
          bmi nocomm

sekeys:  .byte $1d,$9d,$11,$91,$85,$86,$87,$88,$89,$8a,$8b,$8c
sejtab:  .word seri,sele,sedn,seup
         .word sebi,seread,seti,sesi
         .word sebd,sewrite,setd,sesd
         .word senull,sejl,sedrvi,searead

sejump:   tya
          cpy #$08
          bcc nocom
          cpx #$02
          bne nocom
          clc
          adc #$04
nocom:    asl
          tay
          lda sejtab,y
          sta v1
          lda sejtab+1,y
          sta v1+1
          jmp (v1)

ctrl:     txa
nocomm:   ldx bx
xnull:    sta sectbuf,x
          lda #$01
          sta dirty
seri:     ldx b1
          inx
          cpx #$20
          beq :+
          stx b1
          jmp setcsr
:         ldx #$00
          stx b1

sedn:     ldx b2
          inx
          cpx #$08
          bne :+
          ldx #$00
:         stx b2
          jmp setcsr

sele:     ldx b1
          dex
          cpx #$ff
          beq :+
          stx b1
          jmp setcsr
:         ldx #$1f
          stx b1
seup:     ldx b2
          dex
          cpx #$ff
          bne :+
          ldx #$07
:         stx b2
          jmp setcsr

senull:   ldx bx
          lda #$00
          beq xnull

sebi:     ldx bx
          inc sectbuf,x
          lda #$01
          sta dirty
          jmp setcsr

seti:     inc track
          jmp caread

sesi:     inc sector
          jmp caread

sebd:     ldx bx
          dec sectbuf,x
          lda #$01
          sta dirty
          jmp setcsr

sesd:     dec sector
          jmp caread

setd:     dec track
          jmp caread

sejl:     ldx bx
          lda sectbuf,x
          sta track
          inx
          lda sectbuf,x
          sta sector
          jmp seread

sewrite:  jsr sesave
          jmp setcsr

sesave:   lda #$26
          ldx #<rdwrt
          ldy #>rdwrt
          jsr setnam
          lda #'w'
          sta rwmode
          lda #$00
          sta rwlo
wroffs   =wrloop1+1
wrloop1:  ldx #$00
          ldy #$00
wrloop2:  lda sectbuf,x
          sta rwdat,y
          inx
          iny
          tya
          and #$1f
          bne wrloop2
          stx wroffs
          jsr send
          lda rwlo
          clc
          adc #$20
          sta rwlo
          bne wrloop1
          lda #'r'
          sta rwmode

          lda #$08
          ldx #<setts
          ldy #>setts
          jsr setnam
          jsr send
          lda #$90
          jsr runjob
          jsr hex2
          sty $040e
          lda #$00
          sta dirty
          rts

caread:   lda #$00
          beq jread
          jmp setcsr
jread:    jmp seread

searead:  lda caread+1
          eor #$01
          sta caread+1
          beq :+
          lda #$20
          .byte $2c
:         lda #$01
          sta $0427
          cmp #$01
          beq jread
          jmp key

sedrvi:   jsr sendi
          jmp secedit

conv:     ldy #$00
          ldx #$00
          lda #$28
          sta v1
          lda #$04
          sta v1+1
convloop: lda sectbuf,x
          pha
          bmi cv2
          and #$7f
          cmp #$60
          bcs cv1
          and #$3f
          bcc cv3
cv1:      and #$5f
          bne cv3
cv2:      and #$7f
          cmp #$60
          bcs cv3
          ora #$40
          bne cv3
cv3:      sta bx
          pla
          and #$7f
          cmp #$20
          bcs noinv
          lda #$80
          .byte $2c
noinv:    lda #$00
          ora bx
          cmp #$60
          bne noshsp
          lda #228
noshsp:   sta (v1),y
          inx
          iny
          tya
          and #$1f
          bne convloop
          clc
          lda v1
          adc #$28
          sta v1
          lda v1+1
          adc #$00
          sta v1+1
          ldy #$00
          txa
          bne convloop
          rts
dirty:    .byte $00

locfile:  jsr clsprint
          .byte $11,$1d,"locate files on disk"
          .byte $0d,$0d,$1d,"scanning track:"
          brk
          lda #$01
          sta bx
          jsr setpnt
          lda #$00
          sta v1
          tay
          ldx #$0e
:         sta (v1),y
          sta (v2),y
          sta (v3),y
          iny
          bne :-
          inc v1+1
          inc v2+1
          inc v3+1
          dex
          bne :-
          beq oversub

setpnt:   ldy #$00
          lda #>trkbuf
          sty v2
          sta v2+1
          lda #>secbuf
          sty v3
          sta v3+1
          lda #>allocb
          sty v1
          sta v1+1
          rts

incpnt:   inc v1
          inc v2
          inc v3
          bne :+
          inc v1+1
          inc v2+1
          inc v3+1
:         lda v2+1
          cmp #>trkbuf+$d
          rts

oversub:  jsr setpnt
          lda #$01
          ldx $ba
          ldy #$6f
          jsr setfls
          lda #gtlk2-gtlk1
          ldx #<gtlk1
          ldy #>gtlk1
          jsr setnam
          jsr send
          lda #gtlk3-gtlk2
          ldx #<gtlk2
          ldy #>gtlk2
          jsr setnam
          jsr send

nexttrk:  lda $dc01
          bmi :+
          rts

:         lda bx
          jsr hex2
          stx $0488
          sty $0489

          lda #$02
          ldx #<uc
          ldy #>uc
          jsr setnam
          jsr send
          lda #$06
          ldx #<linker
          ldy #>linker
          jsr setnam
          jsr open
          lda $ba
          ldx #$01
          jsr chkin
          ldy #$00
:         jsr getin
          sta (v2),y
          jsr getin
          sta (v3),y
          iny
          cpy #$28
          bne :-
          jsr clrch
          lda #$01
          jsr close
          lda #$28
          clc
          adc v2
          sta v2
          sta v3
          php
          lda #$00
          adc v2+1
          sta v2+1
          plp
          lda #$00
          adc v3+1
          sta v3+1
          inc bx
          lda bx
          cmp #$51
          bne nexttrk

          jsr print
          .byte $0d,$0d,$1d,"analysing linker"
          brk
          jsr setpnt
loop1:    lda (v2),y
          cmp #$51
          bcc ok1
          lda v2
          sta v1
          lda v2+1
          clc
          adc #>allocb->trkbuf
          sta v1+1
          lda #$03
          sta (v1),y
ok1:      jsr incpnt
          bne loop1

          jsr setpnt
loop2:    lda (v2),y
          cmp (v3),y
          bne ok2
          lda v2
          sta v1
          lda v2+1
          clc
          adc #>allocb->trkbuf
          sta v1+1
          lda #$02
          sta (v1),y
ok2:      jsr incpnt
          bne loop2

          jsr setpnt
loop3:    lda (v2),y
          beq ok3
          tax
          lda #$00
          sta v1
          sta v1+1
addtrk:   dex
          beq addsec
          clc
          lda v1
          adc #$28
          sta v1
          lda v1+1
          adc #$00
          sta v1+1
          jmp addtrk
addsec:   clc
          lda v1
          adc (v3),y
          sta v1
          lda v1+1
          adc #>allocb
          sta v1+1
          lda #$01
          sta (v1),y
ok3:      jsr incpnt
          bne loop3
          jsr print
          .byte $0d,$0d,$1d,"start blocks:",$0d,$0d,$9d
          brk

          dec $01
          jsr setpnt
floop:    ldy #$00
          lda v1+1
          and #$0f
          sta v2+1
          lda v1
          sta v2
          lda (v1),y
          bne next

found:    sec
          jsr plot
          tya
          sec
tab1:     sbc #$08
          bcs tab1
          eor #$ff
          adc #$01
          tax
          inx
tab2:     dex
          beq endtab
          lda #$1d
          jsr bsout
          bne tab2

endtab:   lda #$1d
          jsr bsout
          ldy #$00
subloop:  iny
          sec
          lda v2
          sbc #$28
          sta v2
          lda v2+1
          sbc #$00
          sta v2+1
          bcs subloop
          lda v2
          clc
          adc #$28
          pha
          tya
          jsr hex2
          txa
          cmp #$30
          bcs hok1
          ora #$40
hok1:     jsr bsout
          tya
          cmp #$30
          bcs hok2
          ora #$40
hok2:     jsr bsout

          lda #$20
          jsr bsout

          pla
          jsr hex2
          txa
          cmp #$30
          bcs hok3
          ora #$40
hok3:     jsr bsout
          tya
          cmp #$30
          bcs hok4
          ora #$40
hok4:     jsr bsout

next:     lda v1
          cmp #$7f
          bne nohichk
          lda v1+1
          cmp #>allocb+$c
          beq endthis

nohichk:  inc v1
          bne jfloop
          inc v1+1
jfloop:   jmp floop

endthis:  inc $01
          lda #$00
          sta bx
          jmp waitkey

f20b:     jsr clsprint
          .byte $11,$1d,"free 20 blocks"
          .byte $0d,$0d,$1d,"warning:",$0d
          .byte $1d,"on false use it"
          .byte " may destroy data!"
          .byte $0d,$0d,$1d,"are you sure? (y/n) ",$12," ",$92,$9d
          brk
:         jsr getin
          beq :-
          cmp #'y'
          bne :+
          jsr print
          .byte "yes"
          brk
          lda #$01
          ldx $ba
          ldy #$6f
          jsr setfls
          lda #f20b2-f20b1
          ldx #<f20b1
          ldy #>f20b1
          jsr setnam
          jsr send
          lda #f20b3-f20b2
          ldx #<f20b2
          ldy #>f20b2
          jsr setnam
          jsr send
          lda #f20b4-f20b3
          ldx #<f20b3
          ldy #>f20b3
          jsr setnam
          jsr send
          lda #$02
          ldx #<uc
          ldy #>uc
          jsr setnam
          jsr send
          lda #f20i2-f20i1
          ldx #<f20i1
          ldy #>f20i1
          jsr setnam
          jsr send
:         rts

scan:     jsr clsprint
          .byte $0d,$01,$1d,$0f,"error scan"
          .byte $0d,$0d,$01,$1d,$0e,"error status"
          .byte $0d,$01,$1d,$07,"side 0",$01,$1d,$0e,"side 1",$0d,$0d
          .byte $1d,$1d
          brk
          jsr ptrklst
          jsr print
          .byte $01,$91,$0b,$01,$1d,$07
          brk
          jsr ptrklst
          jsr print
          .byte $0d,$0d,$01,$1d,$0a,"write protect is o"
          brk
          lda #$b6
          jsr runjob
          beq nowp
          lda #'n'
          bne wp
nowp:     lda #'f'
          jsr bsout
wp:       jsr bsout

          lda #<$0545
          sta v1
          lda #>$0545
          sta v1+1

          lda #$01
          sta track

ri:       ldy #$00
          sty bx

nri:      lda #$00
          sta sector
          jsr getst
          clc
          adc #20
          sta bx

          lda #$14
          sta sector
          jsr getst
          sec
          sbc #19
          sta bx
          inc track
          cmp #$0a
          bne nri
          clc
          lda v1
          adc #$28
          sta v1
          lda v1+1
          adc #$00
          sta v1+1
          lda track
          cmp #$50
          bcc ri

          lda #$00
          sta bx
          jsr print
          .byte $0d,$0d,$01,$1d,$12,"done"
          brk
          jmp waitkey

getst:    lda $dc01
          bmi :+
          pla
          pla
          lda #$00
          sta bx
          rts

:         lda #$08
          ldx #<setts
          ldy #>setts
          jsr setnam
          jsr send
          lda #$80
          jsr runjob

          ldy bx
          cmp #$02
          bcc nocolch

          pha
          clc
          lda v1+1
          adc #$d4
          sta v1+1
          lda #$03
          sta (v1),y
          sec
          lda v1+1
          sbc #$d4
          sta v1+1
          pla

nocolch:  and #$0f
          ora #$30
          cmp #$3a
          bcc noaf
          adc #$06
noaf:     and #$3f

          sta (v1),y
          tya
          rts

runjob:   sta job
          lda #$0e
          ldx #<setjob
          ldy #>setjob
          jsr setnam
          jsr send
          lda #$06
          ldx #<jobrc
          ldy #>jobrc
          jsr setnam
          jmp getbyt

gtlk1:    .byte "m-w",$00,$05,$1f
          lda #$01
          sta $0b       ; start of track cache
          ldx #$00
          stx $0c
          stx $00
l14e2:    lda #$80
          sta $02
l14e6:    lda $02
l14e8:    bmi l14e6
          lda #$0c
          sta $01
l14ee:    ldy #$00
          lda ($00),y
          sta $0400,x
          inx
          iny
gtlk2:    .byte "m-w",$1f,$05,$1d
          lda ($00),y
          sta $0400,x
          inx
          ldy $01
          iny
          sty $01
          cpy #$20      ; end of track cache
          bne l14ee+6   ; compensate for "m-w"...
          lda #$14
          cmp $0c
          beq l1516
          sta $0c
          bne l14e2+6   ; compensate for "m-w"...
l1516:    inc $0501
          rts
gtlk3:

f20b1:    .byte "m-w",$00,$05,$22
          ldx $022b
          inx
          stx $0b
          lda #$27
          sta $0c
          lda #$80
          sta $02
l152e:    lda $02
          bmi l152e
          ldy #$00
          ldx #$14
l1536:    lda ($8b),y
          cmp $0b
          bne :+
          sbc #$01
          sta ($8b),y
:         inc $8c
f20b2:    .byte "m-w",$22,$05,$1d
          dex
          bne l1536+6 ; +6 compensates the "m-w" in between...
          lda #$0c
          sta $8c
          dec $0b
          dec $95
          lda #$90
          sta $02
l1557:    lda $02
          bmi l1557
          lda #$13
          sta $0c
          inc $0b
          lda #$80
          sta $02
f20b3:    .byte "m-w",$3f,$05,$1b
l1569:    lda $02
          bmi l1569
          ldx $0300
          dex
          cpx $022b
          bne l1581
          stx $0300
          lda #$90
          sta $02
l157d:    lda $02
          bmi l157d
l1581:    jmp $ff09
f20b4:

f20i1:    .byte "m-e",$05,$02
          lda $022b
          sta $0f
          sty $10
          lda #$80
          sta $04
:         lda $04
          bmi :-
          lda #'+'
          sta $0518
          lda #'2'
          sta $0519
          lda #'0'
          sta $051a
          lda #$90
          sta $04
          rts
f20i2:

i:        .byte "i"
uc:       .byte "uc"

fchk:     .byte "m-r",$ff,$ff,$01
dchk:     .byte "m-r",$25,$00,$01
bam:      .byte "m-r",$10,$0a,$01
setts:    .byte "m-w",$0f,$00,$02,$00,$00
track     =6+setts
sector    =7+setts
setjob:   .byte "m-e",$05,$02
job       =6+setjob
          lda #$80
          sta $04
:         lda $04
          bmi :-
          rts
jobrc:    .byte "m-r",$04,$00,$01

linker:   .byte "m-r",$00,$04,$50
rdwrt:    .byte "m-w",$00,$05,$20
          .res $20,0
rwmode    =2+rdwrt
rwlo      =3+rdwrt
rwhi      =4+rdwrt
rwdat     =6+rdwrt
