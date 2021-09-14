  jsr init
  jsr loop
  jmp there
back:
  jsr end

init:
  ldx #$00
  rts

loop:
  inx
  cpx #$05
  bne loop
  rts

end:
  brk

there:    ; jump is the only way to do global jumping
  jmp back
