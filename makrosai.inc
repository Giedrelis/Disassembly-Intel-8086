;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro writeln   1
          push ax
          push dx
          mov ah, 09
          mov dx, %1
          int 21h
          mov ah, 09
          mov dx, newLine
          int 21h
          pop dx
          pop ax

%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro readln    1
          mov ah, 0Ah
          mov dx, %1
          int 21h
          mov ah, 09
          mov dx, newLine
          int 21h

%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro write 1
          push ax
          push dx
          mov ah, 09
          mov dx, %1
          int 21h
          mov ah, 09
          pop dx
          pop ax
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro writeChar 1

        push ax
        push dx
        mov dl, %1
        mov ah, 02h
        int 21h
        pop dx
        pop ax

%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro checkReadByteError 0
jnc .byteReadOk
; IF FAILED READING BYTE - PRINTS ERROR AND ENDS PROGRAM
writeln errorReadingFile
call closeProgram

.byteReadOk
%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro check 1

        push ax
        mov ax, %1
        call writeHexNumber
        pop ax

%endmacro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro op 5

    op%1:
        dw %2
        db %3
        db %4
        db %5

%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro incCounter 0

    push bx
    mov bx, [counter]
    inc bx
    mov [counter], bx
    pop bx

%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro decCounter 0

    push bx
    mov bx, [counter]
    dec bx
    mov [counter], bx
    pop bx

%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro readOpCode 0 ; iš al įdeda į opwholecode + bx baitą. bx - perskaitytų baitų skaičius
             push bx
             mov bx, [opWholeCodeLength]
             mov byte [opWholeCode + bx], al
             add byte [opWholeCodeLength], 1
             incCounter
             pop bx
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro writeWidth 2 ; Nusiunčiam kokio norim pločio ir kiek simbolių užima parašytas dalykas.
    push ax
    push bx

    mov ax, %1
    sub ax, %2

    call writeWidthLoop

    push bx
    push ax
%endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%macro findLength 2 ;
    push di
    mov di, %1
    call findLengthF
    mov %2, di
    pop di
%endmacro
