;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%include 'makrosai.inc'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
org 100h
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
section .text
    main:
       ; perskaito konsolėje esantį argumentą. jeigu argumento nėra - užbaigia
       call readConsoleArg
       jnc .writeArg
       writeln errorReadingArgument ; Jeigu nėra argumento konsolėje (cf = 1) - pereina į .end
       jmp .end

    ; parašo failo pavadinimą į stdout.
    .writeArg:
       mov dx, cmdArg
       call writeASCIIZ

       ;Atidarome faila
       mov dx, cmdArg  ; funkcija skaito iš dx
       call openFile
       jnc .readFile
       writeln errorOpeningFile ; jeigu yra error - rašo error ir baigia
       jmp .end

     ; Skaito failą
     .readFile:
       mov bx, [readingFile]
       call readFile
       jmp .end

       .end
       call closeProgram

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    readFile: ; skaito po vieną komandą ir daro veiksmus
        mov si, dx

        .analyzeOp

            ; CLEAR INFO ON OPERATIONS
            call clearOpInfo

            ; READS OPHEX, CHECK IF ERROR READING A BYTE
            call readByte
            checkReadByteError

            call recognizeOp ; perkelia į [opName] [opType] [opArg1] [opArg2] [opHex] informaciją

            ; IS BYTE A PREFIX ?
            mov al, [opType]
            cmp al, type_Prefix
            jae .isPrefix ; Jei baitas yra prefixas
            jmp .isNotPrefix ; jeigu nėra prefixas - šoka toliau

                    ; IF BYTE IS PREFIX:
                    .isPrefix ; dar netikrinau
                    push bx
                    mov bx, ax
                    mov bx, [opName]
                    mov [prefix], bx
                    mov bl, [opType]
                    mov [prefixType], bl
                    mov bl, [opHex]
                    mov [prefixHex], bl
                    mov ax, bx
                    pop bx

                    jmp .analyzeOp

            .isNotPrefix
                    ; PRINTS CS:IP
                    mov ax, cs ; CS
                    call writeHexWord
                    writeChar ':'
                    mov ax, [counter]
                    call writeHexWord
                    writeChar ' '

                    ; CHECKS IS OPERATION KNOWN
                    mov al, [opType]
                    cmp al, type_Unknown
                    jne .isKnown

                        ;IF OPERATION NOT KNOWN
                        write [opName] ; išspausdina "Unknown command"
                        incCounter
                        call printNewLine
                        jmp .analyzeOp ; grįžta analizuoti toliau

            ; IF OPERATION IS KNOWN: CHECKS IF OP HAS PREFIX
            .isKnown
            ; IF THERE IS PREFIX - ADDS IT TO OPHEXCODE and continues the program
            cmp byte [prefixHex], 0
            je .skipAddingPrefixHex
                mov al, [prefixHex]
                readOpCode

            .skipAddingPrefixHex
            mov al, [opHex]
            readOpCode

             ; CHECKS IF IS EXTRA OP
             mov al, [opType]
             cmp al, type_ExtraOp
             je .isExtraType
             jmp .isNormalType

                    ; IF OPERATION IS TYPE EXTRA
                    .isExtraType
                    call analyzeExtraOpk

             ; IF OPERATION IS NORMAL TYP
             .isNormalType
            ; ANALYZE FIRST ARGUMENT - 1) IF ADDRESSIGN BYTE - READS IT AND PUTS TO DATA, FINDS SEEK 2) IF IMMEDIATE - FINDS HOW MANY BYTE
            mov al, [opArg1]
            mov [arg], al
            call analyzeArg

            ; ANALYZE SECOND ARGUMENT - 1) IF ADDRESSIGN BYTE - READS IT AND PUTS TO DATA, FINDS SEEK 2) IF IMMEDIATE - FINDS HOW MANY BYTE
            ; JEIGU YRA SEEK, TAI KITAME ARG NEBUS SEEK, BUS IMMEDIATE ARBA TSG REGISTRAS. IR ATVIRKŠČIAI
            mov al, [opArg2]
            mov [arg], al
            call analyzeArg

            ; READS SEEK OR IMMEDIATE IF THERE IS ONE
            call setupSeekAndImm

            ; --------------------------------------------------------------------------- PRINTS EVERYTHING ELSE
            call printOpHexCode
            mov al, [opWholeCodeLength]
            add [opWholeCodeLength], al ; Gali būti bugs. Length is by bytes, not by symbols
            writeWidth 18, [opWholeCodeLength] ;

            mov al, [prefixType]
            cmp al, type_PrefixOp ; tikrina, ar prefixą reikia rašyti prieš pagr komandą
            jne .skipPrefix
                    ; IF PREFIX SHOULD BE WRITTEN BEFORE OPERATION - PRINTS PREFIX
                    write [prefix]
                    findLength [prefix], [prefixLength]
                    writeWidth 9, [prefixLength]
                    call clearPrefixInfo
             ; IF THE PREFIX SHOUDLN'T BE WRITTEN BEFORE OPERATION - PRINTS OPERATION NAME
             .skipPrefix
            write [opName]

            cmp byte [opArg1], 0 ; Jeigu pirmas arg = 0, antras irgi bus 0. Tdl jeigu pirmas = 0, tai neprintins nieko
            je .endLine

            findLength [opName], [opNameLength]
            writeWidth 9, [opNameLength]

            ; PRINTS FIRST ARGUMENTS BYTE
            mov al, [opArg1]
            mov [arg], al
            call printArg

            ; CHECK IF THERE IS A SECOND ARG, IF THERE IS, SKIPS WRITING IT
            mov al, [opArg2]
            cmp al, arg_None
            je .endLine

            ; IF THERE IS - PRINTS IT
            writeChar ','
            mov [arg], al
            call printArg

            .endLine:
            call printNewLine
            jmp .analyzeOp

            .end
            ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    recognizeOp: ; į opAdress įrašo komandos adresą duomenyse

        mov [opHex], al

        mov dl, 05h ; nes 5 baitus užima vienos operacijos informacija
        mul dl ; ax dabar yra offset, kelinto baito mums reikia

        mov bx, op00 ; pirmas baitas sąrašo
        add bx, ax ; pridedamas offset nuo pirmo baito sąraše (ax)

        mov ax, [bx]
        mov [opName], ax

        mov ax, [bx+2]
        mov [opType], ax

        mov ax, [bx+3]
        mov [opArg1], ax

        mov ax, [bx+4]
        mov [opArg2], ax

        ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printOpHexCode: ; REIKIA KAD BŪTŲ PADARYTA SETUPSEEKANDIMM IR ANALYZEOP
    push ax
    push bx
    push cx

    mov bx, 0

    .loop
    mov al, [opWholeCode + bx]
    call writeHexByte
    inc bx
    cmp bx, [opWholeCodeLength]
    jb .loop

    pop cx
    pop bx
    pop ax
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
clearOpInfo:

     mov ax, 0h
     mov [opHex], ax
     mov [opName], ax
     mov [opType], ax
     mov [opArg1], ax
     mov [opArg2], ax
     mov [mod], al
     mov [rm], al
     mov [reg], al
     mov [isAdressed], al
     mov [arg], al
     mov [hasImm], al
     mov [hasSeek], al
     mov word [imm], ax
     mov word [imm + 2], ax
     mov word [seek], ax
     mov [ptrUsed], ax
     mov word [opWholeCode], ax
     mov word [opWholeCode + 2], ax
     mov word [opWholeCode + 4], ax
     mov [hasHex], al
     mov [argHex], al
     mov [opWholeCodeLength], al
     mov [opNameLength], al
     mov [isFar], al

     ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
clearPrefixInfo:
     mov ax, 0
     mov [prefix], ax
     mov [prefixType], al
     mov [prefixHex], al
     mov [prefixLength], al
     ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printOpInfo:

     mov  ax, [opHex]
     call writeHexByte

     write [opName]

     mov  ax, [opType]
     call writeDecNumber

     mov  ax, [opArg1]
     call writeDecNumber

     mov  ax, [opArg2]
     call writeDecNumber

     call printNewLine

     ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    analyzeAdressingByte: ; perskaito ir išanalizuoja addresavimo baitą, nusiunčia į data [mod] [r/m] [reg]

             mov ax, 0h

             ; Skip analysis if already analyzed the command
             mov al, [isAdressed]
             cmp al, 1
             je .end

             ; Reads Adressing Byte
             call readByte
             checkReadByteError
             readOpCode


             ; isAdressed = true
             mov dl, 1
             mov [isAdressed], dl

             ; Takes first two bites (mod) of adressing byte, puts it to [mod]
             mov dl, 01000000b
             div dl
             mov [mod], al

             ; Takes 3 bites (reg) from the remaining 6 bites, places the remaining 3 bites to r/m
             mov al, ah
             mov ah, 0
             mov dl, 00001000b
             div dl
             mov [reg], al
             mov [rm], ah

             .end

              ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
    analyzeArg: ; gali reikti push/pop
         ; Visi argumentai iki 22 argumentai nereikalauja papildomos analizės
         mov al, [arg]

         cmp al, 36 ; jeigu 36 arg - hex
         je .hasJustHex

         cmp al, 22 ; jeigu iki 22 argumento - nereikia papildomų baitų
         jbe .end

         ; Visi argumentai virš 31 reikalauja adresavimo baito
         cmp al, 31
         jge .hasAdressingByte

         ; Visi argumentai tarp 23 - 30 yra immediate adresssing type, juos reikia analizuoti
         jmp .hasImmAdressing ; jeigu 23 - 30 yra imm, kurį analizuoja (jumpuose, loopuose, t.t.)



         ; Jeigu turi adresavimo baitą - perskaito ir išanalizuoja sekantį baitą
         .hasAdressingByte
         call analyzeAdressingByte
         jmp .analyzeSeek


        ; Analizuoja immediate
        .hasImmAdressing
        ; Jeigu 23-25 1 baitas
        cmp al, 25
        jbe .imm1byte

        ; Jeigu  26-29 2 baitai
        cmp al, 29
        jbe .imm2byte
        ; Jeigu 30 - 4 baitai
        jmp .imm4byte

        ; Parašo, kiek immediate užims baitų
            .imm1byte
            mov al, 1
            mov [hasImm], al
            jmp .end

            .imm2byte
            mov al, 2
            mov [hasImm], al
            jmp .end

            .imm4byte
            mov al, 3
            mov [hasImm], al
            jmp .end

        ; ANALYZE HOW MANY BYTES ARE THERE GONNA BE
        .analyzeSeek:
         mov al, [mod]
         cmp al, 00b
         je .analyze00mod
         cmp al, 01b
         je .analyze01mod
         cmp al, 10b
         je .analyze10mod
         jmp .analyze11mod

         .analyze11mod: ; abu operandai - registrai
             jmp .end

         .analyze00mod: ; nėra papildomų poslinkio baitų (su exception, jeigu r/m 110)
            mov al, [rm]
             cmp al, 110b
             je .exceptionMod
             jmp .end

         .exceptionMod: ; if mod 00 and r/m 110, then EA = disp-high; disp-low ( same as 10 mod). turės 2 baitų poslinkį
             mov al, 2
             mov [hasSeek], al
             jmp .end

         .analyze01mod: ; bet koks r/m turės 1 baito poslinkį (signed) -128 - 127
             mov al, 1
             mov [hasSeek], al
             jmp .end

         .analyze10mod: ; bet koks r/m turės 2 baitŲ poslinkį (unsigned) t.y. nuo 0000 iki FFFF: [disp-low][disp-high]
             mov al, 2
             mov [hasSeek], al
             jmp .end

         .hasJustHex
            mov byte [hasHex], 1
            jmp .end

            .end
            ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
setupSeekAndImm:
         cmp byte [hasHex], 1
         je .setupHex

         mov al, [hasSeek]
         cmp al, 1 ; vieno baito poslinkis
         je .setup1ByteSeek

         mov al, [hasSeek]
         cmp al, 2 ; dviejų baitų poslinkis
         je .setup2ByteSeek
         jmp .setupImm ; jei neturi poslinkio - ieško imm

     .setup1ByteSeek:

         call readByte
         readOpCode
         mov [seek + 1], al
         jmp .setupImm

     .setup2ByteSeek:
         call readByte
         readOpCode
         mov bl, al
         call readByte
         readOpCode
         mov bh, al
         mov word [seek], bx

     .setupImm:
         mov al, [hasImm]
         cmp al, 1
         je .setup1ByteImm
         cmp al, 2
         jge .setup2ByteImm
         ret

     .setup1ByteImm:
         call readByte
         readOpCode
         mov [imm + 1], al
         ret

     .setup2ByteImm:

         call readByte
         readOpCode
         mov bl, al
         call readByte
         readOpCode
         mov bh, al
         mov word [imm], bx
         mov al, [hasImm]
         cmp al, 2
         jg .setup4ByteImm
         ret

     .setup4ByteImm:
         call readByte
         readOpCode
         mov bl, al
         call readByte
         readOpCode
         mov bh, al
         mov word [imm + 2], bx
         ret

      .setupHex
         cmp byte [opHex], 0x0F
         je .opDBHex
         ;Jeigu ne DB, tai arg bus sekantis baitas
         call readByte
         readOpCode
         mov [argHex], al
         ret

         ;Jeigu DB, tai arg bus pats komandos opHex
         .opDBHex
         mov al, [opHex]
         mov [argHex], al
         ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
insertPtr:
         push bx

         mov bl, [ptrUsed]
         cmp bl, 1
         je .end

         ; CHECKS IF FF JMPF OR CALLF
         mov bl, 1
         cmp [isFar], bl
         je .writeFar

         ; CHECKS IF POINTER IS NEEDED
         mov bl, [arg]
         cmp bl, [opArg1]
         jne .isArg2
         ;isArg1
         mov bl, [opArg2]
         jmp .checkIsPtrNeeded

         .isArg2
         mov bl, [opArg1]
         jmp .checkIsPtrNeeded


         .checkIsPtrNeeded
         cmp bl, arg_SegReg
         je .writePtr
         cmp bl, arg_Offs8
         je .writePtr
         cmp bl, arg_Offs16
         je .writePtr
         cmp bl, arg_None
         je .writePtr
         cmp bl, arg_Const1
         je .writePtr
         cmp bl, arg_Const3
         je .writePtr
         cmp bl, arg_CL
         je .writePtr
         cmp bl, arg_Imm8
         je .writePtr
         cmp bl, arg_Imm16
         je .writePtr
         cmp bl, arg_EImm8
         je .writePtr
         jmp .end


        ; IF callf or jumpf -> far

         .writePtr
         ; IF NEEDED - WRITES PTR
         mov bl, [arg]

         cmp bl, arg_RegMem16
         je .wordptr

         cmp bl, arg_RegMem8
         je .byteptr

         jmp .end

         .byteptr:
             write byte_str
             writeChar ' '
             mov bx, 1
             mov [ptrUsed], bx
             jmp .end

         .wordptr:
             write word_str
             writeChar ' '
             mov bx, 1
             mov [ptrUsed], bx
             jmp .end

         .writeFar
            write far_str
            writeChar ' '
            mov bx, 1
            mov [ptrUsed], bx
            jmp .end

         .end
         pop bx
         ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
checkAndWritePrefix:
   ; Ar Argumentas tures prefix'a
         push ax
         mov ax, 0
         cmp [prefix], ax
         je .end

         write [prefix]
         writeChar ':'
         mov [prefix], ax
         jmp .end

         .end
         pop ax
         ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printRegMem:
          mov bh, 0
          mov bl, [mod]
          cmp bl, 11b ; Jeigu mod = 11
          je .11mod ; abu yra registrai
          cmp bl, 01b ; Jeigu mod = 01
          jge .01mod ;
          jmp .00mod ; Jeigu mod = 00 ir exception - du baitai

          .00mod:
              call insertPtr
              call checkAndWritePrefix

              writeChar '['

              mov bl, [rm]
              cmp bl, 110b ; Patikrina ar yra išimtinis variantas 00 mod 110 rm. Tada skaito du baitus
              je .mod2ByteSeek

              add bl, bl ; Padaugina iš dviejų adresą, kad praleistų word elements.
              write [rm_array + bx]
              writeChar ']'
              ret

          .01mod:

              call insertPtr
              call checkAndWritePrefix
              writeChar '['
              mov bl, [rm]
              add bl, bl
              mov bx, [rm_array + bx]
              write bx
              jmp .seek

          .seek:

              writeChar '+'
              mov bl, [hasSeek]
              cmp bl, 2
              je .mod2ByteSeek
              mov al, [seek + 1]
              cbw
              call writeHexByte ;---------------------------------------------------------------------------------------------------------------------
              writeChar ']'
              ret

          .mod2ByteSeek:

              mov ax, word [seek]
              call writeHexWord
              writeChar ']'
              ret

          .11mod:
              mov bl, [arg]
              cmp bl, arg_RegMem8
              je .print_rArray

          .print_rwArray: ; w = 1 registrai (word dydžio)
              mov bl, [rm] ; R/M yra registro skaičius
              add bl, bl
              write [rw_array + bx]
              ret

          .print_rArray: ; byte dydžio registrai
              mov bl, [rm]
              add bl, bl
              write [r_array + bx]
              ret

          ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printImm:
         mov bl, al
         cmp bl, arg_Imm8
         je .Imm8
         cmp bl, arg_Imm16
         je .Imm16

         ; PRINTS 8-bit extended to 16-bit Imm
         .extendedImm:
             writeChar '+'
             mov al, [imm + 1]
             call writeHexByte
             ret

         ;PRINTS 8-bit IMM
         .Imm8:
             mov al, [imm + 1]
             call writeHexByte
             ret

         ;PRINTS 16-bit IMM
         .Imm16:
             mov ax, word [imm]
             call writeHexWord
             ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printOffset:
         mov bl, al
         mov ax, word [imm]
         cmp bl, arg_Offs8
         je .offset8bit
         jmp .offset16bit

         .offset8bit:

             call insertPtr
             call checkAndWritePrefix
             writeChar '['
             call writeHexWord
             writeChar ']'
             ret

         .offset16bit:

             call insertPtr
             call checkAndWritePrefix
             writeChar '['
             ;mov ah, [seek]
             call writeHexWord
             writeChar ']'
             ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
printRelative:
            mov bl, al
            cmp bl, arg_Short
            je .relShort
            cmp bl, arg_Near
            je .relNear
            jmp .relFar

         .relShort:
             mov al, [imm + 1]
             mov ah, 00h
             ; Jeigu daugiau už 200 - atima 100.
             add ax, [counter]
             cmp ax, 200h
             jb .skipMinus100 ; nes near tik iki 200
             sub ax, 100h

             .skipMinus100:
             call writeHexWord
             ret

         .relNear:
             mov ax, word [imm]
             ; Jeigu daugiau už 200 - atima 100.
             add ax, [counter]


             call writeHexWord
             ret

         .relFar:
             mov ax, word [imm + 2]
             call writeHexWord
             writeChar ':'
             mov ax, word [imm]
             call writeHexWord
             ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

printArg: ; CHECKS WHICH ARGUMENT AND PRINTS IT

    ; IF NO ARG - ENDS
    mov al, [arg]
    mov dl, 02h ; EVERY ELEMENT OF STRING ARRAY SIZE
    cmp al, arg_None
    je .end

    ; -------------------- IF THERE IS ARG - CHECKS WHICH ARG

    ; IF 1 - 8 (included), ITS HALF REGISTER (al... dl)
    cmp al, 8
    jbe .arg_halfReg

    ; IF 9 - 16, ITS FULL REGISTER (ax.. dx)
    cmp al, 16
    jbe .arg_fullReg

    ; IF 17 - 20, ITS ONE OF THE SEGMENT REGISTERS
    cmp al, 20
    jbe .arg_simpleSegReg

      cmp al, arg_Const1
      je .arg_Const1

      cmp al, arg_Const3
      je .arg_Const3

      cmp al, arg_EImm8
      jbe .arg_Imm

      cmp al, arg_Imm16
      je .arg_Imm

      cmp al, arg_Short
      je .arg_Relative

      cmp al, arg_Near
      je .arg_Relative

      cmp al, arg_Far
      je .arg_Relative

      cmp al, arg_Offs8
      je .arg_Offs8

      cmp al, arg_Offs16
      je .arg_Offs16

      cmp al, arg_RegMem8
      je .arg_RegMem

      cmp al, arg_RegMem16
      je .arg_RegMem

      cmp al, arg_Reg8
      je .arg_Reg8

      cmp al, arg_Reg16
      je .arg_Reg16

      cmp al, arg_SegReg
      je .arg_SegReg

      cmp al, arg_Hex
      je .arg_Hex

      jmp .end

;  PRINTS THE ARGUMENTS
      ; PRINTS HALF REG (WITH NO ADDRESS BYTE)
      .arg_halfReg: ;AL - argumento numeris sąraše ; 4 - praleidžia vieną žodį
          sub al, 1 ; tada nuo 0 - 7 registrų skaičiukai
          mul dl ; kiek baitų vienas array elementas turi.
          mov bx, ax ; bx tik leidžia
          write [r_array + bx]
          jmp .end

        ; PRINTS FULL REG (WITH NO ADDRESS BYTE)
      .arg_fullReg:
          sub al, 9 ; tada nuo 0 - 8 registrų skaičiukai
          mul dl
          ; AX - offset nuo registro pradžios
          mov bx, ax ; bx tik leidžia
          write [rw_array + bx]
          jmp .end

      ; PRINTS SEG REG (WITH NO ADDRESS BYTE)
      .arg_simpleSegReg:
          sub al, 17
          mul dl
          ; AX - offset nuo registro pradžios
          mov bx, ax ; bx tik leidžia
          write [sr_array + bx]
          jmp .end

      ; PRINTS HALF REGISTER
      .arg_Reg8:
          mov al, [reg]
          mul dl
          mov bx, ax ; bx tik leidžia
          write [r_array + bx]
          jmp .end

      ; PRINTS FULL REGISTER
      .arg_Reg16:
          mov al, [reg]
          mul dl
          mov bx, ax ; bx tik leidžia
          write [rw_array + bx]
          jmp .end

       ; PRINTS HALF REGISTER OR MEMORY (OF 1 BYTE) ------------------------------------------------------------------------------------------------------------------------------------------------------------
      .arg_RegMem:
          call printRegMem ;
          jmp .end

      ; PRINTS SEGMENTED REGISTER OR MEMORY (OF 1 BYTE)
      .arg_SegReg:
          mov al, [reg]
          mul dl
          mov bx, ax ; bx tik leidžia
          write [sr_array + bx]
          jmp .end

      .arg_Const1:
          writeChar '1'
          jmp .end

      .arg_Const3:
          writeChar '3'
          jmp .end

      .arg_Imm: ;------------------------------------------------------------------------------------------------------------------------------------------------------------
          call printImm;
          jmp .end

       .arg_Offs8 ;------------------------------------------------------------------------------------------------------------------------------------------------------------
          call printOffset ;
          jmp .end

      .arg_Offs16 ;------------------------------------------------------------------------------------------------------------------------------------------------------------
          call printOffset ;
          jmp .end


      .arg_Relative: ;------------------------------------------------------------------------------------------------------------------------------------------------------------
          call printRelative ;
          jmp .end

      .arg_Hex:
          mov al, [argHex]
          call writeHexByte
          jmp .end

; ------------------------------------------------------------------------------------------------------------------------------------------------------------

    .end
    ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
analyzeExtraOpk:
          mov bh, 0
          call analyzeAdressingByte

          mov al, [opHex]  ; komandos koda
          mov bl, [reg]
          add bl, bl ; padaugina iš 2

          cmp al, 0xFF
          je .FFextra
          cmp al, 0xFE
          je .FEextra
          cmp al, 0xF6
          je .F6
          cmp al, 0xF7
          je .F7
          cmp al, 0xD0
          jge .D0D3Extra

      .8083Extra: ; 80 - 83

          mov bx, [opGroup1 + bx]
          mov [opName], bx
          jmp .end

      .D0D3Extra: ; D0 - D3

          mov bx, [opGroup2 + bx]
          mov [opName], bx
          jmp .end

      .F6: ; Jeigu komanda yra TEST, ji turi imm8/16 kaip arg2
        cmp bl, 2
        jbe .isTestB
        jmp .F6F7Extraprint

            .isTestB
            mov byte [opArg2], arg_Imm8
            jmp .F6F7Extraprint

      .F7
        cmp bl, 2
        jbe .isTestW
        jmp .F6F7Extraprint

             .isTestW
             mov byte [opArg2], arg_Imm16
             jmp .F6F7Extraprint


      .F6F7Extraprint
        mov bx, [opGroup3 + bx]
        mov [opName], bx
        jmp .end

      .FEextra: ; FE

          mov bx, [opGroup4 + bx]
          mov [opName], bx
          jmp .end

      .FFextra: ; FF

          cmp bl, 6 ; Call far
          je .isFar
          cmp bl, 10 ; Jump far
          je .isFar
          jmp .isNotFar

          .isFar
          mov al, 1
          mov [isFar], al

          .isNotFar
          mov bx, [opGroup5 + bx]
          mov [opName], bx


      .end
      ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
%include 'komandos.asm' ; Pagalbinės komandos
%include 'opInfo.inc'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


section .data                   ; duomenys


    dataBuffer:
       db 00, 00, 'h$'

    errorReadingArgument:
       db 'Error: Cant read the argument from console $'

    errorOpeningFile:
       db 'Error: Cant open the file $'

    errorReadingFile:
       db 'Error: Cant read the file $'
    errorClosingFile:
       db 'Error: Cant close the file $'

    info:

       db 'fuck'

    newLine:
       db 0x0D, 0x0A, '$'  ; tekstas ant ekrano
 
    cmdArgSize:
       db 00
    cmdArg:
       times 255 db 00
    readingFile:
       dw 0FFFh 

    opHex:
        dw 0

    opName:
        dw 0
    opType:
        dw 0
    opArg1:
        dw 0
    opArg2:
        dw 0
    opWholeCode:
        db 00, 00, 00, 00, 00, 00
    opWholeCodeLength:
        db 0
    opNameLength:
        db 0


    prefix:
        dw 0
    prefixType:
        db 0
    prefixHex:
        db 0
    prefixLength:
        db 0

    mod:
        db 0
    rm:
        db 0
    reg:
        db 0

    isAdressed:
        db 0

    imm:
        db 0, 0, 0, 0
    hasImm:
        db 0
    seek:
        db 0, 0
    hasSeek:
        db 0
    ptrUsed:
        db 0
    isFar:
        db 0
    hasHex:
        db 0
    argHex:
        db 0

    arg: ; laikinas arg laikiklis
        db 0

    counter:
        dw 0100h ;(IP - 1, nes pirmas baitas - 0, ne 1)

