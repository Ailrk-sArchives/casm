  ldx #$08

decrement:
  dex
  stx $0200
  cpx #$03
  bne decrement
  stx $0201
  brk
