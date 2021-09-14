ldx #$aa
ldy #$bb

; we can't assign value x with y difreclty
tya
pha
txa
tay
pla
tax
