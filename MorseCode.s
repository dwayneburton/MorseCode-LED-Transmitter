;------------------------------------------------------------------------------
; Purpose: Convert text to Morse code and blink it via LED on LPC1768 using ARMv7 Assembly
;------------------------------------------------------------------------------
        THUMB                       ; Use the Thumb instruction set
        AREA    My_code, CODE, READONLY
        EXPORT  __MAIN              ; Entry label exported for linker
        ENTRY

__MAIN                              ; Do not rename - matches startup_LPC1768.s

;------------------------------------------------------------------------------
; Section 1: Initialize GPIO base address and disable all LEDs
;------------------------------------------------------------------------------
        MOV     R10, #0xC000        ; Offset to reach GPIO register base
        MOV     R4, #0x0            ; Clear R4
        MOVT    R4, #0x2009         ; Set upper half to GPIO base
        ADD     R4, R4, R10         ; R4 = 0x2009C000 (GPIO base address)

        MOV     R3, #0x7C           ; Bitmask to disable Port 2 LEDs
        STR     R3, [R4, #0x40]     ; Write to Port 2 (offset 0x40)

        MOV     R3, #0xB0000000     ; Bitmask to disable Port 1 LEDs (bits 28–30)
        STR     R3, [R4, #0x20]     ; Write to Port 1 (offset 0x20)

        MOV     R6, #0x2C2A         ; Lower 16 bits of delay value
        MOVT    R6, #0xA            ; Upper 16 bits (R6 = 0x000A2C2A)

;------------------------------------------------------------------------------
; Section 2: Load text and begin Morse code processing
;------------------------------------------------------------------------------
ResetLUT
        LDR     R5, =InputLUT       ; R5 points to beginning of string

NextChar
        LDRB    R0, [R5]            ; Load next character from string
        ADD     R5, #1              ; Move to next character
        TEQ     R0, #0              ; Check if null terminator reached
        BNE     ProcessChar         ; If not null, convert and process

        MOV     R0, #4              ; Delay 4 units (total 7 for word gap)
        BL      DELAY
        BEQ     ResetLUT            ; Restart string

;------------------------------------------------------------------------------
; Section 3: Convert ASCII to Morse and loop through pattern bits
;------------------------------------------------------------------------------
ProcessChar
        BL      CHAR2MORSE          ; Convert char to Morse code in R1
        CLZ     R9, R1              ; Count leading zeros in R1
        LSL     R1, R9              ; Remove leading zeros to align bits

SET_LED
        LSLS    R1, #1              ; Shift bit into carry flag
        BCC     SET_OFF             ; If 0: LED OFF
        BL      LED_ON              ; If 1: LED ON
        B       NEXT

SET_OFF
        BL      LED_OFF

NEXT
        MOV     R0, #1              ; Delay 1 unit between bits
        BL      DELAY
        CMP     R1, #0              ; Check if more bits remain
        BNE     SET_LED             ; Loop if yes

        BL      LED_OFF             ; Inter-character gap
        MOV     R0, #3              ; Delay 3 units
        BL      DELAY
        BL      NextChar            ; Process next character

;------------------------------------------------------------------------------
; Subroutine: CHAR2MORSE - Convert ASCII to Morse code pattern
; Input:  R0 = ASCII character
; Output: R1 = Morse code pattern (16-bit)
;------------------------------------------------------------------------------
CHAR2MORSE
        STMFD   R13!, {R14}         ; Save return address
        SUB     R0, #0x41           ; Convert ASCII to 0-based index
        LDR     R2, =MorseLUT       ; Load base of Morse LUT
        MOV     R9, #0x2            ; Word offset = 2 bytes per entry
        MUL     R0, R9              ; Multiply index by 2
        LDRH    R1, [R2, R0]        ; Load Morse pattern
        LDMFD   R13!, {R15}         ; Return

;------------------------------------------------------------------------------
; Subroutine: LED_ON - Turn LED ON (clears bit 28)
;------------------------------------------------------------------------------
LED_ON
        PUSH    {R3-R4}             ; Save R3, R4
        AND     R3, R3, #0xEFFFFFFF ; Clear bit 28
        STR     R3, [R4, #0x20]     ; Write to GPIO Port 1
        POP     {R3-R4}             ; Restore
        BX      LR                  ; Return

;------------------------------------------------------------------------------
; Subroutine: LED_OFF - Turn LED OFF (sets bit 28)
;------------------------------------------------------------------------------
LED_OFF
        STMFD   R13!, {R3, R14}     ; Save R3 and return address
        ORR     R3, R3, #0x10000000 ; Set bit 28
        STR     R3, [R4, #0x20]     ; Write to GPIO Port 1
        LDMFD   R13!, {R3, R15}     ; Return

;------------------------------------------------------------------------------
; Subroutine: DELAY - Software delay (0.5s * R0)
;------------------------------------------------------------------------------
DELAY
        STMFD   R13!, {R2, R14}     ; Save R2 and return address
MultipleDelay
        TEQ     R0, #0              ; Check if loop complete
        SUBS    R6, #1              ; Decrement delay counter
        BGT     MultipleDelay       ; Inner loop
        MOV     R6, #0x2C2A         ; Reload delay
        MOVT    R6, #0xA
        SUBS    R0, #1              ; Decrement repeat counter
        BGT     MultipleDelay       ; Outer loop
exitDelay
        LDMFD   R13!, {R2, R15}     ; Return

;------------------------------------------------------------------------------
; Section 4: Data segment - input string and Morse LUT
;------------------------------------------------------------------------------
        ALIGN
InputLUT
        DCB     "BADJU", 0         ; Message to encode (null-terminated)
        ALIGN
MorseLUT
        DCW     0x17,   0x1D5,  0x75D, 0x75     ; A, B, C, D
        DCW     0x1,    0x15D,  0x1DD, 0x55     ; E, F, G, H
        DCW     0x5,    0x1777, 0x1D7, 0x175    ; I, J, K, L
        DCW     0x77,   0x1D,   0x777, 0x5DD    ; M, N, O, P
        DCW     0x1DD7, 0x5D,   0x15,  0x7      ; Q, R, S, T
        DCW     0x57,   0x157,  0x177, 0x757    ; U, V, W, X
        DCW     0x1D77, 0x775                   ; Y, Z

LED_PORT_ADR   EQU     0x2009C000         ; Optional named constant for clarity

        END