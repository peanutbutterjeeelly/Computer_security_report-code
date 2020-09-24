; returns a sum of an array
    global  sum
    section .text
sum:
    xorpd   xmm0, xmm0    ; initialize the sum to 0
    cmp     rsi, 0        ; special case for length = 0
    je      done
next:
    addsd   xmm0, [rdi]   ; add in the current array element
    add     rdi, 8        ; move to next array element
    dec     rsi           ; count down
    jnz     next          ; if not done counting, continue
done:
    ret                   ; return value already in xmm0
