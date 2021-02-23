;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printString:
   ; spausdina asciiz eilutę į stdout
   ; DX - asciiz eilutė;
   push bx
   push dx
   push ax
   mov bx, dx
   .loopBySymbols:
      mov dl, [bx]
      cmp dl, 0
      je .return
        mov ah, 0x02
        int 0x21
        inc bx
      jmp .loopBySymbols

   .return:
      pop ax
      pop dx
      pop bx
      ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
convertToHex:

     cmp al, 10
     JGE .ifHex
     add al, '0'
     jmp .end

     .ifHex:

         sub al, 10
         add al, 'A'

     .end

     ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
writeHexByte:
     push bx
     push dx

     mov dx, 0
     mov ah, 0
     mov bx, 16
     div bx

     call convertToHex
     writeChar al
     mov ax, dx

     call convertToHex
     writeChar al

     pop dx
     pop bx
     ret
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
writeHexWord: ; al - skai2ius

     push bx
     push dx
     mov dx, 0

     mov bx, 4096  ; 16^3
     div bx

     call convertToHex
     writeChar al
     mov ax, dx

     mov dx, 0
     mov bx, 256
     div bx

     call convertToHex
     writeChar al
     mov ax, dx

     mov dx, 0
     mov bx, 16
     div bx

     call convertToHex
     writeChar al
     mov ax, dx

     call convertToHex
     writeChar al

     pop dx
     pop bx
     ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  openFile:
        ; dx - failo vardo adresas
        ; CF yra 1 jeigu klaida
        push ax
        push dx

        mov ah, 3Dh
        mov al, 00h       ; skaitymui
        int 21h

        jc .pab
        mov [readingFile], ax

        .pab:
        pop dx
        pop ax
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  closeFile:
        ; bx - failo vardo adresas
        ; CF yra 1 jeigu klaida
        push ax
        push bx

        mov ah, 3Eh
        int 21h

        .pab:
        pop dx
        pop ax
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

printNewLine:
   ; prints \n
   jmp .begin
   .localData:
   db 0x0D, 0x0A, 0x00

   .begin:

   push dx
   push ax
   mov dx, .localData
   call printString
   pop ax
   pop dx
   ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    writeSymbol:
        ; al - simbolis
        push ax
        push dx
        mov dl, al
        mov ah, 02h
        int 21h
        pop dx
        pop ax
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    writeDecNumber: ; 0 - 99
        ; al - simbolis
        push ax
        push bx
        push dx

        aam                ; -> AH is quotient (1) , AL is remainder (2)
        add ax, 3030h      ; -> AH is "1", AL is "2"
        mov bx, ax

        mov dl, bh         ; First we print the tens
        mov ah, 02h        ; DOS.PrintChar
        int 21h
        mov dl, bl
        mov ah, 02h        ; DOS.PrintChar
        int 21h

        pop dx
        pop bx
        pop ax
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    writeASCIIZ:
         ; spausdina eilute su nuline pabaiga, dx - jos adresas
         ;

         push si
         push ax
         push dx

         mov  si, dx

         .pagalVisus:
         mov dl, [si]  ; krauname simboli
         cmp dl, 00             ; gal jau eilutes pabaiga?
         je .pab

         mov ah, 02
         int 21h
         inc si
         jmp .pagalVisus

         .pab:
         writeln newLine

         pop dx
         pop ax
         pop si
         ret



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
readByte:

        push dx
        push bx
        push cx
        push si

        xor ax,ax ; Užnulina ax

        mov bx, [readingFile]
        mov dx, dataBuffer
        mov si, dx

        mov cx, 01 ; skaitome vieną baitą
        mov ah, 3Fh ; komanda failo skaitymo
        int 21h
        jc .isejimas           ; skaitymo klaida
        cmp ax, 00 ; ax - 0 jeigu nebėra ką skaityt
        je .EOF         ; skaitymo pabaiga

        mov al,[si]

        .isejimas:
        pop si
        pop cx
        pop bx
        pop dx

        ret

        .EOF
        pop si
        pop cx
        pop bx
        pop dx
        call closeProgram



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    readConsoleArg:

 ; nuskaito ir paruosia argumenta
         ; jeigu jo nerasta, tai CF <- 1, prisingu atveju - 0

         push bx
         push di
         push si
         push ax

         xor bx, bx
         xor si, si
         xor di, di

         mov bl, [80h]
         mov [cmdArgSize], bl ; 80h rodo ilgį parametro
         mov si, 0081h  ; programos paleidimo parametrai (argumentas) rašomi nuo 81h
         mov di, cmdArg ; buferis
         push cx
         mov cx, bx
         mov ah,00
         cmp cx, 0000 ; jeigu ilgis - 0, baigia.
         jne .pagalVisus ;
         stc
         jmp .pab

         .pagalVisus:
         mov al, [si]     ;
         cmp al, ' ' ; jeigu tarpas - praleidžia
         je .toliau
         cmp al, 0Dh ;jeigu pabaiga - praleidžia
         je .toliau
         cmp al, 0Ah ; jeigu pabaiga - praleidžia
         je .toliau
         mov [di],al ; perkelia į komEilutesArgumentą perskaitytą cmd charą. di incrementina loop pabaigoj
         ; call rasykSimboli
         mov ah, 01                  ; ah - pozymis, kad buvo bent vienas "netarpas" (bent vienas simbolis, kuris nėra tarpas ar pabaiga per visą string)
         inc di
         jmp .kitasZingsnis
         ; jeigu buvo tarpas ar pabaiga eina .toliau.
         .toliau:
         cmp ah, 01
         je .isejimas                ; jeigu buvo bent vienas "netarpas" - eina į išėjimą
         .kitasZingsnis:
         inc si

         loop .pagalVisus
         .isejimas:
         cmp ah, 01                  ; ar buvo "netarpu"?
         je .pridetCOM
         stc                         ; klaida!
         jmp .pab
         .pridetCOM:
         mov [di], byte '.'
         mov [di+1], byte 'c'
         mov [di+2], byte 'o'
         mov [di+3], byte 'm'
         clc                         ; klaidos nerasta
         .pab:
         pop cx
         pop ax
         pop si
         pop di
         pop dx
         ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
closeProgram:
           ; CLOSE THE FILE
           mov bx, [readingFile] ; bx - file handle cf - 1 if error
           call closeFile
           jnc .end
           writeln errorClosingFile ; jeigu yra error - rašo error ir baigia
           jmp .end

           .end ; END PROGRAM
           mov ah, 4ch
           int 21h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
writeWidthLoop: ; works with writeWidth macro. ax - kiek kartų kartoti
    mov bx, 0

    .loop
    writeChar ' '
    inc bx
    cmp bx, ax
    jb .loop
    ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
findLengthF:
  push ax
  push bx
  push cx

  mov   bx, di
  mov   al, '$'
  mov   cx, 0xffff

  repne scasb               ; REPeat while Not Equal [edi] != al

  sub   di, bx            ; length = offset of (edi - ebx)

  pop cx
  pop bx
  pop ax
  ret