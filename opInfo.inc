;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Argumentų string'ai
    r_array:  ; w-0 registrai
         dw  r_000, r_001, r_010, r_011
         dw  r_100, r_101, r_110, r_111

    rw_array: ; w-1 registrai
         dw  rw_000, rw_001, rw_010, rw_011
         dw  rw_100, rw_101, rw_110, rw_111

    sr_array:
         dw opRegES, opRegCS, opRegSS, opRegDS

    rm_array: ; lauko r/m reiksmes
         dw rm_000, rm_001, rm_010, rm_011
         dw rm_100, rm_101, rm_110, rm_111

        ; w - 0registrai

        r_000:     db "AL$"
        r_001:     db "CL$"
        r_010:     db "DL$"
        r_011:     db "BL$"
        r_100:     db "AH$"
        r_101:     db "CH$"
        r_110:     db "DH$"
        r_111:     db "BH$"

        ; w - 1 registrai

        rw_000:    db "AX$"
        rw_001:    db "CX$"
        rw_010:    db "DX$"
        rw_011:    db "BX$"
        rw_100:    db "SP$"
        rw_101:    db "BP$"
        rw_110:    db "SI$"
        rw_111:    db "DI$"

        ; segmento registrai

        opRegES:   db "ES$"
        opRegCS:   db "CS$"
        opRegSS:   db "SS$"
        opRegDS:   db "DS$"

        ; r/m adresavimo budai
        rm_000:    db "BX+SI$"
        rm_001:    db "BX+DI$"
        rm_010:    db "BP+SI$"
        rm_011:    db "BP+DI$"
        rm_100:    db "SI$"
        rm_101:    db "DI$"
        rm_110:    db "BP$"
        rm_111:    db "BX$"

        word_str:  db "WORD PTR$"
        byte_str:  db "BYTE PTR$"
        far_str:   db "FAR$"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; komandų tipai

type_Normal       equ 1
type_ExtraOp     equ 2  ; reikia ziuret i OPK kad nustatyt komanda
type_Unknown      equ 3
type_Prefix       equ 4
type_PrefixOp     equ 5
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; komandu argumentu tipai
arg_None       equ 0
arg_AL         equ 1
arg_CL         equ 2
arg_DL         equ 3
arg_BL         equ 4
arg_AH         equ 5
arg_CH         equ 6
arg_DH         equ 7
arg_BH         equ 8
arg_AX         equ 9
arg_CX         equ 10
arg_DX         equ 11
arg_BX         equ 12
arg_SP         equ 13
arg_BP         equ 14
arg_SI         equ 15
arg_DI         equ 16

arg_ES         equ 17
arg_CS         equ 18
arg_SS         equ 19
arg_DS         equ 20

arg_Const1     equ 21 ; '1'
arg_Const3     equ 22 ; '3'
; argumentai nereikalaujantys addresavimo baito (naudoja loopai, jumpai, t.t.
arg_Imm8       equ 23
arg_EImm8      equ 24 ; 8 bit praplestas iki 16 bitu
arg_Short      equ 25
arg_Offs8      equ 26
arg_Imm16      equ 27
arg_Offs16     equ 28
arg_Near       equ 29
arg_Far        equ 30
;argumentai reikalaujantys addresavimo baito - reg adresavimo tipas
arg_RegMem8    equ 31
arg_RegMem16   equ 32
arg_Reg8       equ 33
arg_Reg16      equ 34
arg_SegReg     equ 35
; argumentas tiesioginis pagal hex code
arg_Hex        equ 36

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
opUnknown     db    "Unknown command$"
opFILD        db    "FILD$"
opFISTP       db    "FISTP$"
opAAA         db    "AAA$",
opAAD         db    "AAD$",
opAAM         db    "AAM$",
opAAS         db    "AAS$"
opADC         db    "ADC$"
opADD         db    "ADD$"
opAND         db    "AND$"
opCALL        db    "CALL$"
opCALLF       db    "CALL$"
opCBW         db    "CBW$"
opCLC         db    "CLC$"
opCLD         db    "CLD$"
opCLI         db    "CLI$"
opCMC         db    "CMC$"
opCMP         db    "CMP$"
opCMPSB       db    "CMPSB$"
opCMPSW       db    "CMPSW$"
opCWD         db    "CWD$"
opDAA         db    "DAA$"
opDAS         db    "DAS$"
opDEC         db    "DEC$"
opDIV         db    "DIV$"
opDBb         db    "DB$"
opESC         db    "ESC$"
opHLT         db    "HLT$"
opIDIV        db    "IDIV$"
opIMUL        db    "IMUL$"
opIN          db    "IN$"
opINC         db    "INC$"
opINT         db    "INT$"
opINTO        db    "INTO$"
opIRET        db    "IRET$"
opJA          db    "JA$"
opJAE         db    "JAE$"
opJB          db    "JB$"
opJBE         db    "JBE$"
opJCXZ        db    "JCXZ$"
opJE          db    "JE$"
opJG          db    "JG$"
opJGE         db    "JGE$"
opJNE         db    "JNE$"
opJL          db    "JL$"
opJLE         db    "JLE$"
opJMP         db    "JMP$"
opJMPF        db    "JMP$"
opJNO         db    "JNO$"
opJNL         db    "JNL$"
opJNLE        db    "JNLE$"
opJNP         db    "JNP$"
opJNS         db    "JNS$"
opJNZ         db    "JNZ$"
opJO          db    "JO$"
opJPE         db    "JPE$"
opJP          db    "JP$"
opJPO         db    "JPO$"
opJS          db    "JS$"
opJZ          db    "JZ$"
opLAHF        db    "LAHF$"
opLDS         db    "LDS$"
opLEA         db    "LEA$"
opLES         db    "LES$"
opLOCK        db    "LOCK$"
opLODSB       db    "LODSB$"
opLODSW       db    "LODSW$"
opLOOPW       db    "LOOPW$"
opLOOPZW      db    "LOOPZW$"
opLOOPNZW     db    "LOOPNZW$"
opMOV         db    "MOV$"
opMOVS        db    "MOVS$"
opMOVSB       db    "MOVSB$"
opMOVSW       db    "MOVSW$"
opMUL         db    "MUL$"
opNEG         db    "NEG$"
opNOP         db    "NOP$"
opNOT         db    "NOT$"
opOR          db    "OR$"
opOUT         db    "OUT$"
opPOP         db    "POP$"
opPOPF        db    "POPF$"
opPUSH        db    "PUSH$"
opPUSHF       db    "PUSHF$"
opRCL         db    "RCL$"
opRCR         db    "RCR$"
opREPE        db    "REPE$"
opREPNE       db    "REPNE$"
opRET         db    "RET$"
opRETF        db    "RETF$"
opRETN        db    "RETN$"
opREPZ        db    "REPZ$"
opROL         db    "ROL$"
opROR         db    "ROR$"
opSAHF        db    "SAHF$"
opSAL         db    "SAL$"
opSAR         db    "SAR$"
opSBB         db    "SBB$"
opSCASB       db    "SCASB$"
opSCASW       db    "SCASW$"
opSEGCS       db    "SEGCS$"
opSEGDS       db    "SEGDS$"
opSEGES       db    "SEGES$"
opSEGSS       db    "SEGSS$"
opSHL         db    "SHL$"
opSHR         db    "SHR$"
opSTC         db    "STC$"
opSTD         db    "STD$"
opSTI         db    "STI$"
opSTOS        db    "STOS$"
opSTOSB       db    "STOSB$"
opSTOSW       db    "STOSW$"
opSUB         db    "SUB$"
opTEST        db    "TEST$"
opWAIT        db    "WAIT$"
opXCHG        db    "XCHG$"
opXLATB       db    "XLATB$"
opXOR         db    "XOR$"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

op 00, opADD, type_Normal, arg_RegMem8, arg_Reg8
op 01, opADD, type_Normal, arg_RegMem16, arg_Reg16
op 02, opADD, type_Normal, arg_Reg8, arg_RegMem8
op 03, opADD, type_Normal, arg_Reg16, arg_RegMem16
op 04, opADD, type_Normal, arg_AL, arg_Imm8
op 05, opADD, type_Normal, arg_AX, arg_Imm16

op 06, opPUSH, type_Normal, arg_ES, arg_None
op 07, opPOP, type_Normal, arg_ES, arg_None

op 08, opOR, type_Normal, arg_RegMem8, arg_Reg8
op 09, opOR, type_Normal, arg_RegMem16, arg_Reg16
op 0A, opOR, type_Normal, arg_Reg8, arg_RegMem8
op 0B, opOR, type_Normal, arg_Reg16, arg_RegMem16
op 0C, opOR, type_Normal, arg_AL, arg_Imm8
op 0D, opOR, type_Normal, arg_AX, arg_Imm16

op 0E, opPUSH, type_Normal, arg_CS, arg_None
op 0F, opDBb, type_Normal, arg_Hex, arg_None

op 10, opADC, type_Normal, arg_RegMem8, arg_Reg8
op 11, opADC, type_Normal, arg_RegMem16, arg_Reg16
op 12, opADC, type_Normal, arg_Reg8, arg_RegMem8
op 13, opADC, type_Normal, arg_Reg16, arg_RegMem16
op 14, opADC, type_Normal, arg_AL, arg_Imm8
op 15, opADC, type_Normal, arg_AX, arg_Imm16

op 16, opPUSH, type_Normal, arg_SS, arg_None
op 17, opPOP, type_Normal, arg_SS, arg_None

op 18, opSBB, type_Normal, arg_RegMem8, arg_Reg8
op 19, opSBB, type_Normal, arg_RegMem16, arg_Reg16
op 1A, opSBB, type_Normal, arg_Reg8, arg_RegMem8
op 1B, opSBB, type_Normal, arg_Reg16, arg_RegMem16
op 1C, opSBB, type_Normal, arg_AL, arg_Imm8
op 1D, opSBB, type_Normal, arg_AX, arg_Imm16

op 1E, opPUSH, type_Normal, arg_DS, arg_None
op 1F, opPOP, type_Normal, arg_DS, arg_None

op 20, opAND, type_Normal, arg_RegMem8, arg_Reg8
op 21, opAND, type_Normal, arg_RegMem16, arg_Reg16
op 22, opAND, type_Normal, arg_Reg8, arg_RegMem8
op 23, opAND, type_Normal, arg_Reg16, arg_RegMem16
op 24, opAND, type_Normal, arg_AL, arg_Imm8
op 25, opAND, type_Normal, arg_AX, arg_Imm16

op 26, opRegES, type_Prefix, arg_None, arg_None ; Prefix
op 27, opDAA, type_Normal, arg_None, arg_None

op 28, opSUB, type_Normal, arg_RegMem8, arg_Reg8
op 29, opSUB, type_Normal, arg_RegMem16, arg_Reg16
op 2A, opSUB, type_Normal, arg_Reg8, arg_RegMem8
op 2B, opSUB, type_Normal, arg_Reg16, arg_RegMem16
op 2C, opSUB, type_Normal, arg_AL, arg_Imm8
op 2D, opSUB, type_Normal, arg_AX, arg_Imm16

op 2E, opRegCS, type_Prefix, arg_None, arg_None ; prefix
op 2F, opDAS, type_Normal, arg_None, arg_None

op 30, opXOR, type_Normal, arg_RegMem8, arg_Reg8
op 31, opXOR, type_Normal, arg_RegMem16, arg_Reg16
op 32, opXOR, type_Normal, arg_Reg8, arg_RegMem8
op 33, opXOR, type_Normal, arg_Reg16, arg_RegMem16
op 34, opXOR, type_Normal, arg_AL, arg_Imm8
op 35, opXOR, type_Normal, arg_AX, arg_Imm16

op 36, opRegSS, type_Prefix, arg_None, arg_None ; prefix
op 37, opAAA, type_Normal, arg_None, arg_None

op 38, opCMP, type_Normal, arg_RegMem8, arg_Reg8
op 39, opCMP, type_Normal, arg_RegMem16, arg_Reg16
op 3A, opCMP, type_Normal, arg_Reg8, arg_RegMem8
op 3B, opCMP, type_Normal, arg_Reg16, arg_RegMem16
op 3C, opCMP, type_Normal, arg_AL, arg_Imm8
op 3D, opCMP, type_Normal, arg_AX, arg_Imm16

op 3E, opRegDS, type_Prefix, arg_None, arg_None ; prefix
op 3F, opAAS, type_Normal, arg_None, arg_None

op 40, opINC, type_Normal, arg_AX, arg_None
op 41, opINC, type_Normal, arg_CX, arg_None
op 42, opINC, type_Normal, arg_DX, arg_None
op 43, opINC, type_Normal, arg_BX, arg_None
op 44, opINC, type_Normal, arg_SP, arg_None
op 45, opINC, type_Normal, arg_BP, arg_None
op 46, opINC, type_Normal, arg_SI, arg_None
op 47, opINC, type_Normal, arg_DI, arg_None

op 48, opDEC, type_Normal, arg_AX, arg_None
op 49, opDEC, type_Normal, arg_CX, arg_None
op 4A, opDEC, type_Normal, arg_DX, arg_None
op 4B, opDEC, type_Normal, arg_BX, arg_None
op 4C, opDEC, type_Normal, arg_SP, arg_None
op 4D, opDEC, type_Normal, arg_BP, arg_None
op 4E, opDEC, type_Normal, arg_SI, arg_None
op 4F, opDEC, type_Normal, arg_DI, arg_None

op 50, opPUSH, type_Normal, arg_AX, arg_None
op 51, opPUSH, type_Normal, arg_CX, arg_None
op 52, opPUSH, type_Normal, arg_DX, arg_None
op 53, opPUSH, type_Normal, arg_BX, arg_None
op 54, opPUSH, type_Normal, arg_SP, arg_None
op 55, opPUSH, type_Normal, arg_BP, arg_None
op 56, opPUSH, type_Normal, arg_SI, arg_None
op 57, opPUSH, type_Normal, arg_DI, arg_None

op 58, opPOP, type_Normal, arg_AX, arg_None
op 59, opPOP, type_Normal, arg_CX, arg_None
op 5A, opPOP, type_Normal, arg_DX, arg_None
op 5B, opPOP, type_Normal, arg_BX, arg_None
op 5C, opPOP, type_Normal, arg_SP, arg_None
op 5D, opPOP, type_Normal, arg_BP, arg_None
op 5E, opPOP, type_Normal, arg_SI, arg_None
op 5F, opPOP, type_Normal, arg_DI, arg_None

op 60, opUnknown, type_Unknown, arg_None, arg_None
op 61, opUnknown, type_Unknown, arg_None, arg_None
op 62, opUnknown, type_Unknown, arg_None, arg_None
op 63, opUnknown, type_Unknown, arg_None, arg_None
op 64, opUnknown, type_Unknown, arg_None, arg_None
op 65, opUnknown, type_Unknown, arg_None, arg_None
op 66, opUnknown, type_Unknown, arg_None, arg_None
op 67, opUnknown, type_Unknown, arg_None, arg_None
op 68, opUnknown, type_Unknown, arg_None, arg_None
op 69, opUnknown, type_Unknown, arg_None, arg_None
op 6A, opUnknown, type_Unknown, arg_None, arg_None
op 6B, opUnknown, type_Unknown, arg_None, arg_None
op 6C, opUnknown, type_Unknown, arg_None, arg_None
op 6D, opUnknown, type_Unknown, arg_None, arg_None
op 6E, opUnknown, type_Unknown, arg_None, arg_None
op 6F, opUnknown, type_Unknown, arg_None, arg_None
;-------------------------------------------------------------- Short jumps. Dar pažiūrėti
op 70, opJO, type_Normal, arg_Short, arg_None
op 71, opJNO, type_Normal, arg_Short, arg_None
op 72, opJB, type_Normal, arg_Short, arg_None ; JB/JNAE/JC
op 73, opJAE, type_Normal, arg_Short, arg_None ; JNB/JAE/JNC
op 74, opJZ, type_Normal, arg_Short, arg_None ;JE/JZ
op 75, opJNZ, type_Normal, arg_Short, arg_None ; JNE/JNZ
op 76, opJBE, type_Normal, arg_Short, arg_None ;JBE/JNA
op 77, opJA, type_Normal, arg_Short, arg_None ; JNBE/JA
op 78, opJS, type_Normal, arg_Short, arg_None ; JS
op 79, opJNS, type_Normal, arg_Short, arg_None ; JNS
op 7A, opJPE, type_Normal, arg_Short, arg_None ; JP/JPE
op 7B, opJPO, type_Normal, arg_Short, arg_None ; JNP/JPO
op 7C, opJL, type_Normal, arg_Short, arg_None ; JL/JNGE
op 7D, opJGE, type_Normal, arg_Short, arg_None ; JNL/JGE
op 7E, opJLE, type_Normal, arg_Short, arg_None ; JLE/JNG
op 7F, opJG, type_Normal, arg_Short, arg_None ; JNLE/JG
;-------------------------------------------------------------- EXTRA OPK THINGS, REIKS PAKEISTI
op 80, opUnknown, type_ExtraOp, arg_RegMem8, arg_Imm8
op 81, opUnknown, type_ExtraOp, arg_RegMem16, arg_Imm16
op 82, opUnknown, type_ExtraOp, arg_RegMem8, arg_Imm8
op 83, opUnknown, type_ExtraOp, arg_RegMem16, arg_EImm8
;--------------------------------------------------------------
op 84, opTEST, type_Normal, arg_RegMem8, arg_Reg8
op 85, opTEST, type_Normal, arg_RegMem16, arg_Reg16
op 86, opXCHG, type_Normal, arg_Reg8, arg_RegMem8
op 87, opXCHG, type_Normal, arg_Reg16, arg_RegMem16

op 88, opMOV, type_Normal, arg_RegMem8, arg_Reg8
op 89, opMOV, type_Normal, arg_RegMem16, arg_Reg16
op 8A, opMOV, type_Normal, arg_Reg8, arg_RegMem8
op 8B, opMOV, type_Normal, arg_Reg16, arg_RegMem16
op 8C, opMOV, type_Normal, arg_RegMem16, arg_SegReg

op 8D, opLEA, type_Normal, arg_Reg16, arg_RegMem16 ; Gali būti klaida
op 8E, opMOV, type_Normal, arg_SegReg, arg_RegMem16 ; Gali būti klaida
op 8F, opPOP, type_Normal, arg_RegMem16, arg_None ; Gali būti klaida
op 90, opNOP, type_Normal, arg_None, arg_None

op 91, opXCHG, type_Normal, arg_AX, arg_CX
op 92, opXCHG, type_Normal, arg_AX, arg_DX
op 93, opXCHG, type_Normal, arg_AX, arg_BX
op 94, opXCHG, type_Normal, arg_AX, arg_SP
op 95, opXCHG, type_Normal, arg_AX, arg_BP
op 96, opXCHG, type_Normal, arg_AX, arg_SI
op 97, opXCHG, type_Normal, arg_AX, arg_DI

op 98, opCBW, type_Normal, arg_None, arg_None
op 99, opCWD, type_Normal, arg_None, arg_None
op 9A, opCALL, type_Normal, arg_Far, arg_None
op 9B, opWAIT, type_Normal, arg_None, arg_None
op 9C, opPUSHF, type_Normal, arg_None, arg_None
op 9D, opPOPF, type_Normal, arg_None, arg_None
op 9E, opSAHF, type_Normal, arg_None, arg_None
op 9F, opLAHF, type_Normal, arg_None, arg_None

op A0, opMOV, type_Normal, arg_AL, arg_Offs8
op A1, opMOV, type_Normal, arg_AX, arg_Offs16
op A2, opMOV, type_Normal, arg_Offs8, arg_AL
op A3, opMOV, type_Normal, arg_Offs16, arg_AX

op A4, opMOVSB, type_Normal, arg_None, arg_None ; Moves strings. Strings are the arguments (In al)
op A5, opMOVSW, type_Normal, arg_None, arg_None ;Moves strings. Strings are the arguments (In ax)
op A6, opCMPSB, type_Normal, arg_None, arg_None ; Compare strings. Strings are the arguments (in al)
op A7, opCMPSW, type_Normal, arg_None, arg_None ; Compare strings. Strings are the arguments. (In ax)

op A8, opTEST, type_Normal, arg_AL, arg_Imm8
op A9, opTEST, type_Normal, arg_AX, arg_Imm16

op AA, opSTOSB, type_Normal, arg_None, arg_None ; Store string from AL
op AB, opSTOSW, type_Normal, arg_None, arg_None  ; Store string from AX
op AC, opLODSB, type_Normal, arg_None, arg_None ; Load string from AL
op AD, opLODSW, type_Normal, arg_None, arg_None ; Load string from AX
op AE, opSCASB, type_Normal, arg_None, arg_None ; SCAN string from AL
op AF, opSCASW, type_Normal, arg_None, arg_None ; Scan string from AX

op B0, opMOV, type_Normal, arg_AL, arg_Imm8
op B1, opMOV, type_Normal, arg_CL, arg_Imm8
op B2, opMOV, type_Normal, arg_DL, arg_Imm8
op B3, opMOV, type_Normal, arg_BL, arg_Imm8
op B4, opMOV, type_Normal, arg_AH, arg_Imm8
op B5, opMOV, type_Normal, arg_CH, arg_Imm8
op B6, opMOV, type_Normal, arg_DH, arg_Imm8
op B7, opMOV, type_Normal, arg_BH, arg_Imm8
op B8, opMOV, type_Normal, arg_AX, arg_Imm16
op B9, opMOV, type_Normal, arg_CX, arg_Imm16
op BA, opMOV, type_Normal, arg_DX, arg_Imm16
op BB, opMOV, type_Normal, arg_BX, arg_Imm16
op BC, opMOV, type_Normal, arg_SP, arg_Imm16
op BD, opMOV, type_Normal, arg_BP, arg_Imm16
op BE, opMOV, type_Normal, arg_SI, arg_Imm16
op BF, opMOV, type_Normal, arg_DI, arg_Imm16

op C0, opUnknown, type_Unknown, arg_None, arg_None
op C1, opUnknown, type_Unknown, arg_None, arg_None

op C2, opRET, type_Normal, arg_Imm16, arg_None ; Gali būti klaida
op C3, opRET, type_Normal, arg_None, arg_None ; Gali būti klaida

op C4, opLES, type_Normal, arg_Reg16, arg_RegMem16
op C5, opLDS, type_Normal, arg_Reg16, arg_RegMem16
op C6, opMOV, type_Normal, arg_RegMem8, arg_Imm8 ; Gali būti klaida
op C7, opMOV, type_Normal, arg_RegMem16, arg_Imm16 ; Gali būti klaida (REG gali reikti sukurt naują funkciją, kurioje naudoja tik 000 reg)

op C8, opUnknown, type_Unknown, arg_None, arg_None
op C9, opUnknown, type_Unknown, arg_None, arg_None

op CA, opRETF, type_Normal, arg_Imm16, arg_None ; Gali būti klaida
op CB, opRETF, type_Normal, arg_None, arg_None ; Gali būti klaida

op CC, opINT, type_Normal, arg_Const3, arg_None
op CD, opINT, type_Normal, arg_Imm8, arg_None
op CE, opINTO, type_Normal, arg_None, arg_None
op CF, opIRET, type_Normal, arg_None, arg_None
;---------------------------------------------------------------- Reikės papildyti extra opk
op D0, opUnknown, type_ExtraOp, arg_RegMem8, arg_Const1
op D1, opUnknown, type_ExtraOp, arg_RegMem16, arg_Const1
op D2, opUnknown, type_ExtraOp, arg_RegMem8, arg_CL
op D3, opUnknown, type_ExtraOp, arg_RegMem16, arg_CL
;----------------------------------------------------------------
op D4, opAAM, type_Normal, arg_Hex, arg_None
op D5, opAAD, type_Normal, arg_Hex, arg_None

op D6, opUnknown, type_Unknown, arg_None, arg_None ; not used

op D7, opXLATB, type_Normal, arg_None, arg_None
;---------------------------------------------------------------- SUTVARKYTI SU ESC
op D8, opESC, type_Normal, arg_RegMem8, arg_None
op D9, opESC, type_Normal, arg_RegMem8, arg_None
op DA, opESC, type_Normal, arg_RegMem8, arg_None
op DB, opESC, type_Normal, arg_RegMem8, arg_None
op DC, opESC, type_Normal, arg_RegMem8, arg_None
op DD, opESC, type_Normal, arg_RegMem8, arg_None
op DE, opESC, type_Normal, arg_RegMem8, arg_None
op DF, opFILD, type_Normal, arg_RegMem16, arg_None
;----------------------------------------------------------------
op E0, opLOOPNZW, type_Normal, arg_Short, arg_None ; LOOPNE/LOOPNZW
op E1, opLOOPZW, type_Normal, arg_Short, arg_None ; LOOPZW
op E2, opLOOPW, type_Normal, arg_Short, arg_None ; LOOPW

op E3, opJCXZ, type_Normal, arg_Short, arg_None
op E4, opIN, type_Normal, arg_AL, arg_Imm8
op E5, opIN, type_Normal, arg_AX, arg_Imm8
op E6, opOUT, type_Normal, arg_Imm8, arg_AL
op E7, opOUT, type_Normal, arg_Imm8, arg_AX
op E8, opCALL, type_Normal, arg_Near, arg_None
op E9, opJMP, type_Normal, arg_Near, arg_None
op EA, opJMP, type_Normal, arg_Far, arg_None
op EB, opJMP, type_Normal, arg_Short, arg_None
op EC, opIN, type_Normal, arg_AL, arg_DX
op ED, opIN, type_Normal, arg_AX, arg_DX
op EE, opOUT, type_Normal, arg_DX, arg_AL
op EF, opOUT, type_Normal, arg_DX, arg_AX

op F0, opLOCK, type_PrefixOp, arg_None, arg_None
op F1, opUnknown, type_Unknown, arg_None, arg_None
op F2, opREPNE, type_PrefixOp, arg_None, arg_None ; REPNE/REPNZ
op F3, opREPE, type_PrefixOp, arg_None, arg_None ; REP/REPE/REPZ
op F4, opHLT, type_Normal, arg_None, arg_None
op F5, opCMC, type_Normal, arg_None, arg_None
;-------------------------------------------------------------- Sutvarkyti extra opk
op F6, opUnknown, type_ExtraOp, arg_RegMem8, arg_None
op F7, opUnknown, type_ExtraOp, arg_RegMem16, arg_None
;--------------------------------------------------------------
op F8, opCLC, type_Normal, arg_None, arg_None
op F9, opSTC, type_Normal, arg_None, arg_None
op FA, opCLI, type_Normal, arg_None, arg_None
op FB, opSTI, type_Normal, arg_None, arg_None
op FC, opCLD, type_Normal, arg_None, arg_None
op FD, opSTD, type_Normal, arg_None, arg_None
;-------------------------------------------------------------- Sutvarkyti extra opk
op FE, opUnknown, type_ExtraOp, arg_RegMem8, arg_None
op FF, opUnknown, type_ExtraOp, arg_RegMem16, arg_None
;--------------------------------------------------------------

opGroup1: ;80-83
    dw opADD, opOR, opADC, opSBB
    dw opAND, opSUB, opXOR, opCMP

opGroup2: ;D0-D3
    dw opROL, opROR, opRCL, opRCR
    dw opSHL, opSHR, opSAL, opSAR

opGroup3: ; F6-F7
    dw opTEST, opTEST, opNOT, opNEG
    dw opMUL, opIMUL, opDIV, opIDIV

opGroup4: ;FE
    dw opINC, opDEC, opUnknown, opUnknown
    dw opUnknown, opUnknown, opUnknown, opUnknown

opGroup5: ; FF
    dw opINC, opDEC, opCALL, opCALLF ;FF
    dw opJMP, opJMPF, opPUSH, opUnknown