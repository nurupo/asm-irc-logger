; Copyright (c) 2012 Maxim Biro
;
; Permission is hereby granted, free of charge, to any person obtaining a copy of
; this software and associated documentation files (the "Software"), to deal in
; the Software without restriction, including without limitation the rights to
; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
; of the Software, and to permit persons to whom the Software is furnished to do
; so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.


; =============================================================================
    include \masm32\include\masm32rt.inc
    include \masm32\include\ws2_32.inc
    include \masm32\include\shlwapi.inc
    include \masm32\macros\macros.asm
    includelib \masm32\lib\ws2_32.lib
    includelib \masm32\lib\masm32.lib
    includelib \masm32\lib\shlwapi.lib
; =============================================================================


    .data?
      sin                   sockaddr_in <>
      wsaData               WSADATA     <>
      sock                  DWORD       ?

    .data
      errWSAS               BYTE        "An error occured while calling WSAStartup",0
      errSocket             BYTE        "An error occured while creating a socket",0
      errConnect            BYTE        "An error occured while connecting",0
      errChannels           BYTE        "Too many channels",0

      logDirPath            BYTE        1024 dup (0)
      fSettings             BYTE        "\settings.ini",0
            ;doNick and ident <=40 chars
      user                  BYTE        "USER %s 8 * :%s",13,10,0 ;USER cirnog 8 * :cirnog
      pNick                 DWORD       0
      pNickLength           DWORD       0
      logTimestampFormat    BYTE        "%04d-%02d-%02dT%02d:%02d:%02d  ",0
      doNick                BYTE        "NICK %s",13,10,0
      doJoin                BYTE        "JOIN %s",13,10,0
      ping                  BYTE        "PING ",0
      pong                  BYTE        "PONG %s",0
      privmsg               BYTE        "PRIVMSG",0
      logPrivmsgFormat      BYTE        "<%s> %s",10,0 ;nick, msg
      action                BYTE        ":",1,"ACTION ",0
      logActionFormat       BYTE        "* %s %s",10,0 ;nick, msg
      part                  BYTE        "PART",0
      logPartFormat         BYTE        "*** %s has left %s",10,0
      join                  BYTE        "JOIN",0
      logJoinFormat         BYTE        "*** %s has joined %s",10,0
      topic                 BYTE        "TOPIC",0
      logTopicFormat        BYTE        "*** %s changes topic to ",34,"%s",34,10,0
      kick                  BYTE        "KICK",0
      logKickFormat         BYTE        "*** %s was kicked by %s",10,0
      mode                  BYTE        "MODE",0
      logModeFormat         BYTE        "*** %s sets mode: %s",10,0
      quit                  BYTE        "QUIT",0
      logQuitFormat         BYTE        "*** %s has quit IRC",10,0
      nick                  BYTE        "NICK",0
      logNickFormat         BYTE        "*** %s is now known as %s",10,0

      heap                  DWORD       0

      UTCtime               SYSTEMTIME  <0>
      day                   WORD        0

      chanNameMaxLength     equ         81
      nickMaxLength         equ         41
      maxServerLineLength   equ         600

      inBuff                BYTE        maxServerLineLength dup (0)
      inBuffLength          DWORD       0
      inCharBuff            BYTE        0

      tmpBuff               BYTE        maxServerLineLength dup (0)

      chanFHandl STRUC
            chan            BYTE        chanNameMaxLength dup (0)
            fOpen           BYTE        0
            fHandl          DWORD       0
            nickHead        DWORD       0 ; pointer to nickNode STRUCT
            nickTail        DWORD       0 ; pointer to nickNode STRUCT
      chanFHandl ENDS

      spacing STRUC
            pos             DWORD       6 dup (0)
      spacing ENDS

      pChanFHandlList       DWORD       0 ; pointer to chanFHandl STRUC
      chanFHandlCount       DWORD       0

      nameListBeg           BYTE        "353",0

      nickNode STRUCT
            nick            BYTE        nickMaxLength dup (0)
            next            DWORD       0
            prev            DWORD       0
     nickNode ENDS

     info                   BYTE        "IRC Logger",10,"Author: Maxim Biro, nurupo.contributions@gmail.com",10,"Date: 2012.11.24",10,10,0


    .code

start:

; =============================================================================

    call main
    exit

; =============================================================================

receive PROC
    push eax
    push ebx

    invoke RtlZeroMemory, addr inBuff, sizeof inBuff

    mov ebx, 0
    mov inCharBuff, 0

    .while inCharBuff != 10d

        invoke recv, sock, addr inCharBuff, sizeof inCharBuff, 0

        movzx ax, inCharBuff
        mov BYTE PTR [inBuff+ebx], al
        inc ebx

    .endw

    mov inBuffLength, ebx

    pop ebx
    pop eax

    ret
receive ENDP


onPing PROC
    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff ;zero out memory
    fn wsprintf, offset tmpBuff, addr pong, offset inBuff + ((lengthof ping)-1)
    ;invoke lstrcat, offset tmpBuff, addr pong ;put there pong message
    ;invoke lstrcat, offset tmpBuff + ((lengthof pong)-1), offset inBuff + ((lengthof ping)-1) ;append it to the pong string
    invoke send, sock, offset tmpBuff, eax, 0
    invoke StdOut, offset tmpBuff

    ret
onPing ENDP


getPChanFHandlListOffset PROC,
    chan:DWORD,
    chanLength:DWORD

    mov ebx, chanFHandlCount


    mov eax, pChanFHandlList
    .while ebx != 0
        cld
        mov ecx, chanLength
        lea esi, (chanFHandl PTR [eax]).chan
        mov edi, chan
        repe cmpsb
        jne notMatch

        ret
        notMatch:
        add eax, type chanFHandl
        dec ebx
    .endw
    mov eax, 0
ret
getPChanFHandlListOffset ENDP


createDir PROC,
    path: DWORD

    mov esi, path
    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, 1256
    mov edi, eax
    invoke lstrlen, path
    mov ecx, eax
    mov ebx, 0
    top:
        mov al,[esi+ebx]
        mov [edi+ebx],al
        .if BYTE PTR [edi+ebx] == '\'
            push ecx
            invoke CreateDirectory, edi, 0
            pop ecx
        .endif
        inc ebx
    loop top

    invoke HeapFree, heap, HEAP_NO_SERIALIZE, edi

    ret
createDir ENDP

createLogFileIfNeeded PROC,
    pChanFHandlListOffset:DWORD

    invoke GetSystemTime, addr UTCtime
    mov edi, pChanFHandlListOffset
    .if BYTE PTR ((chanFHandl PTR [edi]).fOpen) == 0
        invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, 1256
        mov esi, eax
        fn wsprintf, esi, "%s%s\", addr logDirPath, addr (chanFHandl PTR [edi]).chan
        ;invoke StdOut, esi
        push edi
        invoke createDir, esi
        pop edi
        ;invoke SHCreateDirectoryEx, 0, esi, 0
        invoke lstrlen, esi
        add eax, esi
        movzx ebx, UTCtime.wYear
        movzx ecx, UTCtime.wMonth
        movzx edx, UTCtime.wDay
        fn wsprintf, eax, "%04d-%02d-%02d.log", ebx, ecx, edx
        ;invoke StdOut, esi
        invoke CreateFile, esi, FILE_APPEND_DATA, FILE_SHARE_READ, 0, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0
        mov (chanFHandl PTR[edi]).fHandl, eax
        mov (chanFHandl PTR[edi]).fOpen, 1
        invoke HeapFree, heap, HEAP_NO_SERIALIZE, esi
    .endif
    mov eax, (chanFHandl PTR[edi]).fHandl
    ret
createLogFileIfNeeded ENDP

logTimestamp PROC,
    pChanFHandlListOffset:DWORD

    LOCAL dummy:DWORD

    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff

    invoke GetSystemTime, addr UTCtime
    movzx eax, UTCtime.wYear
    movzx ebx, UTCtime.wMonth
    movzx ecx, UTCtime.wDay
    movzx edx, UTCtime.wHour
    movzx esi, UTCtime.wMinute
    movzx edi, UTCtime.wSecond

    invoke wsprintf, offset tmpBuff, addr logTimestampFormat, eax, ebx, ecx, edx, esi, edi
    mov edi, pChanFHandlListOffset
    lea ebx, dummy
    invoke WriteFile, (chanFHandl PTR[edi]).fHandl, offset tmpBuff, eax, ebx, 0
    ret
logTimestamp ENDP

logAction PROC,
    usrNick:DWORD,
    spaces:spacing,
    pChanFHandlListOffset:DWORD

    invoke createLogFileIfNeeded, pChanFHandlListOffset
    invoke logTimestamp, pChanFHandlListOffset

    lea ebx, spaces
    mov esi, DWORD PTR[ebx+(2*4)]
    add esi, offset inBuff
    add esi, lengthof action

    mov edi, inBuffLength
    add edi, offset inBuff
    sub edi, esi
    sub edi, 2

    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, maxServerLineLength
    mov ebx, eax
    invoke lstrcpyn, eax, esi, edi

    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
    fn wsprintf, offset tmpBuff, addr logActionFormat, usrNick, ebx
    mov edi, pChanFHandlListOffset
    invoke WriteFile, (chanFHandl PTR[edi]).fHandl, offset tmpBuff, eax, ebx, 0
    invoke HeapFree, heap, HEAP_NO_SERIALIZE, ebx
    ret

logAction ENDP


logPrivmsg PROC,
    usrNick:DWORD,
    spaces:spacing,
    pChanFHandlListOffset:DWORD

    invoke createLogFileIfNeeded, pChanFHandlListOffset
    invoke logTimestamp, pChanFHandlListOffset

    lea ebx, spaces
    mov esi, DWORD PTR[ebx+(2*4)]
    add esi, offset inBuff
    add esi, 2

    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, maxServerLineLength
    mov ebx, eax
    mov ecx, offset inBuff
    add ecx, inBuffLength
    sub ecx, esi
    dec ecx
    invoke lstrcpyn, eax, esi, ecx

    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
    fn wsprintf, offset tmpBuff, addr logPrivmsgFormat, usrNick, ebx
    mov edi, pChanFHandlListOffset
    ;invoke StdOut, offset tmpBuff
    invoke WriteFile, (chanFHandl PTR[edi]).fHandl, offset tmpBuff, eax, ebx, 0
    invoke HeapFree, heap, HEAP_NO_SERIALIZE, ebx
    ret

logPrivmsg ENDP

logJoin PROC,
    usrNick:DWORD,
    chan:DWORD,
    pChanFHandlListOffset:DWORD

    LOCAL dummy:DWORD

    invoke createLogFileIfNeeded, pChanFHandlListOffset
    invoke logTimestamp, pChanFHandlListOffset


    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
    fn wsprintf, offset tmpBuff, addr logJoinFormat, usrNick, chan
    mov edi, pChanFHandlListOffset
    lea ebx, dummy
    invoke WriteFile, (chanFHandl PTR[edi]).fHandl, offset tmpBuff, eax, ebx, 0

    ret

logJoin ENDP



logPart PROC,
    usrNick:DWORD,
    chan:DWORD,
    pChanFHandlListOffset:DWORD

    LOCAL dummy:DWORD

    invoke createLogFileIfNeeded, pChanFHandlListOffset
    invoke logTimestamp, pChanFHandlListOffset

    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
    fn wsprintf, offset tmpBuff, addr logPartFormat, usrNick, chan
    mov edi, pChanFHandlListOffset
    lea ebx, dummy
    invoke WriteFile, (chanFHandl PTR[edi]).fHandl, offset tmpBuff, eax, ebx, 0

    ret

logPart ENDP

logMode PROC,
    usrNick:DWORD,
    spaces:spacing,
    pChanFHandlListOffset:DWORD

    invoke createLogFileIfNeeded, pChanFHandlListOffset
    invoke logTimestamp, pChanFHandlListOffset

    lea ebx, spaces
    mov esi, DWORD PTR[ebx+(2*4)]
    add esi, offset inBuff
    inc esi

    mov edi, inBuffLength
    add edi, offset inBuff
    sub edi, esi
    dec edi

    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, maxServerLineLength
    mov ebx, eax
    invoke lstrcpyn, eax, esi, edi

    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
    fn wsprintf, offset tmpBuff, addr logModeFormat, usrNick, ebx
    mov edi, pChanFHandlListOffset
    invoke WriteFile, (chanFHandl PTR[edi]).fHandl, offset tmpBuff, eax, ebx, 0
    invoke HeapFree, heap, HEAP_NO_SERIALIZE, ebx
    ret

logMode ENDP

logTopic PROC,
    usrNick:DWORD,
    spaces:spacing,
    pChanFHandlListOffset:DWORD

    invoke createLogFileIfNeeded, pChanFHandlListOffset
    invoke logTimestamp, pChanFHandlListOffset

    lea ebx, spaces
    mov esi, DWORD PTR[ebx+(2*4)]
    add esi, offset inBuff
    add esi, 2

    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, maxServerLineLength
    mov ebx, eax
    mov ecx, offset inBuff
    add ecx, inBuffLength
    sub ecx, esi
    dec ecx
    invoke lstrcpyn, eax, esi, ecx

    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
    fn wsprintf, offset tmpBuff, addr logTopicFormat, usrNick, ebx
    mov edi, pChanFHandlListOffset
    ;invoke StdOut, offset tmpBuff
    invoke WriteFile, (chanFHandl PTR[edi]).fHandl, offset tmpBuff, eax, ebx, 0
    invoke HeapFree, heap, HEAP_NO_SERIALIZE, ebx
    ret

logTopic ENDP


logKick PROC,
    usrNick:DWORD,
    spaces:spacing,
    pChanFHandlListOffset:DWORD

    invoke createLogFileIfNeeded, pChanFHandlListOffset
    invoke logTimestamp, pChanFHandlListOffset

    lea ebx, spaces
    mov esi, DWORD PTR[ebx+(2*4)]
    add esi, offset inBuff
    inc esi

    mov edi, DWORD PTR[ebx+(3*4)]
    add edi, offset inBuff
    sub edi, esi
    inc edi

    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, maxServerLineLength
    mov ebx, eax
    invoke lstrcpyn, eax, esi, edi

    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
    fn wsprintf, offset tmpBuff, addr logKickFormat, ebx, usrNick
    mov edi, pChanFHandlListOffset
    invoke WriteFile, (chanFHandl PTR[edi]).fHandl, offset tmpBuff, eax, ebx, 0
    invoke HeapFree, heap, HEAP_NO_SERIALIZE, ebx
    ret

logKick ENDP


secondWordMatches PROC,
    testWord:DWORD,
    spaces:spacing

    cld
    invoke lstrlen, testWord
    mov ecx, eax
    lea ebx, spaces
    mov esi, DWORD PTR [ebx]
    add esi, offset inBuff
    inc esi
    mov edi, testWord

    repe cmpsb
    jne notMatch
        mov eax, 1
        ret
    notMatch:
    mov eax, 0
    ret

secondWordMatches ENDP


findNickNode PROC,
    tmpNick:DWORD,
    pChanFHandlListOffset:DWORD,

    mov edx, pChanFHandlListOffset
    mov edx, (chanFHandl PTR [edx]).nickHead

    .while edx != 0

        cld
        push edx
        invoke lstrlen, tmpNick
        pop edx

        mov ecx, eax
        inc ecx
        lea esi, (nickNode PTR [edx]).nick
        mov edi, tmpNick
        repe cmpsb
        jne notMatch

        mov eax, edx
        ret

        notMatch:

        mov edx, (nickNode PTR [edx]).next
    .endw

    mov eax, 0

    ret

findNickNode ENDP


removeNickNode PROC,
    node:DWORD,
    pChanFHandlListOffset:DWORD

    mov ecx, pChanFHandlListOffset

    mov edx, node

    mov esi, (nickNode PTR [edx]).prev
    .if esi == 0
        ;head
        mov ebx, (nickNode PTR [edx]).next
        mov (chanFHandl PTR [ecx]).nickHead, ebx
        invoke HeapFree, heap, HEAP_NO_SERIALIZE, (nickNode PTR [ebx]).prev
        mov (nickNode PTR [ebx]).prev, 0
        ret
    .endif


    mov esi, (nickNode PTR [edx]).next
    .if esi == 0
        ;tail
        mov ebx, (nickNode PTR [edx]).prev
        mov (chanFHandl PTR [ecx]).nickTail, ebx
        invoke HeapFree, heap, HEAP_NO_SERIALIZE, (nickNode PTR [ebx]).next
        mov (nickNode PTR [ebx]).next, 0
        ret
    .endif

    ;if we got here, we are in the middle of the list

    mov eax, (nickNode PTR [edx]).prev ;prev node
    mov ebx, (nickNode PTR [edx]).next ;next node
    mov (nickNode PTR [eax]).next, ebx ;prev points to next instead of us
    push eax
    invoke HeapFree, heap, HEAP_NO_SERIALIZE, (nickNode PTR [ebx]).prev
    pop eax
    mov (nickNode PTR [ebx]).prev, eax ;next points to prev instead of us
    ret

removeNickNode ENDP



addNickNode PROC,
    tmpNick:DWORD,
    pChanFHandlListOffset:DWORD

    LOCAL newNickNodePTR:DWORD

    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, sizeof nickNode
    mov newNickNodePTR, eax
    lea ebx, (nickNode PTR [eax]).nick
    invoke lstrcpyn, ebx, tmpNick, 40

;invoke StdOut, ebx

    mov eax, newNickNodePTR
    mov ecx, pChanFHandlListOffset
    .if DWORD PTR (chanFHandl PTR [ecx]).nickTail == 0
        mov (chanFHandl PTR [ecx]).nickHead, eax
        mov (chanFHandl PTR [ecx]).nickTail, eax
    .else
        mov ebx, (chanFHandl PTR [ecx]).nickTail
        mov (nickNode PTR [ebx]).next, eax
        mov (nickNode PTR [eax]).prev, ebx
        mov (chanFHandl PTR [ecx]).nickTail, eax
    .endif

    ret

addNickNode ENDP

onNamesList PROC,
    spaces:spacing,
    pChanFHandlListOffset:DWORD

    LOCAL nickList:DWORD
    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff

    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, maxServerLineLength
    mov nickList, eax
    lea ecx, spaces
    mov edi, DWORD PTR [ecx+(4*4)]
    add edi, offset inBuff
    add edi, 2

    mov esi, inBuffLength
    add esi, offset inBuff
    sub esi, edi
    dec esi

    invoke lstrcpyn, nickList, edi, esi
    ;invoke StdOut, nickList

    mov ebx, nickList
    mov eax, rv(InString, 1, nickList, " ")
    .while eax > 0
        mov esi, eax
        invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
        .if BYTE PTR [ebx] == '@'
            inc ebx
            dec esi
        .elseif BYTE PTR [ebx] == '+'
            inc ebx
            dec esi
        .endif
        invoke lstrcpyn, offset tmpBuff, ebx, esi
        ;invoke StdOut, offset tmpBuff
        push ebx
        push esi
        invoke addNickNode, offset tmpBuff, pChanFHandlListOffset
        pop esi
        pop ebx
        add ebx, esi
        mov eax, rv(InString, 1, ebx, " ")
    .endw

    mov esi, ebx
    invoke lstrlen, addr inBuff
    mov edi, eax
    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff

    .if BYTE PTR [esi] == '@'
        inc esi
        dec edi
    .elseif BYTE PTR [esi] == '+'
        inc esi
        dec edi
    .endif

    invoke lstrcpyn, offset tmpBuff, esi, edi
    ;invoke StdOut, offset tmpBuff
    push ebx
    push esi
    invoke addNickNode, offset tmpBuff, pChanFHandlListOffset
    pop esi
    pop ebx
    add ebx, esi
    invoke HeapFree, heap, HEAP_NO_SERIALIZE, nickList

    ret
onNamesList ENDP


logQuit PROC,
    usrNick:DWORD

    LOCAL dummy:DWORD, pChanFHandlListOffset:DWORD

    mov ebx, chanFHandlCount

    mov esi, pChanFHandlList
    .while ebx != 0
        mov pChanFHandlListOffset, esi
        push esi
        push ebx

        invoke findNickNode, usrNick, pChanFHandlListOffset
        .if eax != 0
            invoke removeNickNode, eax, pChanFHandlListOffset
            invoke createLogFileIfNeeded, pChanFHandlListOffset
            invoke logTimestamp, pChanFHandlListOffset

            invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
            fn wsprintf, offset tmpBuff, addr logQuitFormat, usrNick
            mov edi, pChanFHandlListOffset
            lea ebx, dummy
            invoke WriteFile, (chanFHandl PTR[edi]).fHandl, offset tmpBuff, eax, ebx, 0

        .endif

        pop ebx
        pop esi
        add esi, type chanFHandl
        dec ebx
    .endw

    ret

logQuit ENDP


logNick PROC,
    usrNick:DWORD,
    spaces:spacing

    LOCAL newNick:DWORD, nickNodePTR:DWORD, pChanFHandlListOffset:DWORD, dummy:DWORD

    lea ebx, spaces
    mov esi, DWORD PTR [ebx+4]
    add esi, offset inBuff
    add esi, 2

    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, nickMaxLength
    mov newNick, eax
    mov ecx, offset inBuff
    add ecx, inBuffLength
    sub ecx, esi
    dec ecx
    invoke lstrcpyn, newNick, esi, ecx

    mov ebx, chanFHandlCount
    mov esi, pChanFHandlList
    .while ebx != 0
        mov pChanFHandlListOffset, esi
        push esi
        push ebx

        invoke findNickNode, usrNick, pChanFHandlListOffset
        .if eax != 0
            mov nickNodePTR, eax
            lea ecx, (nickNode PTR [eax]).nick
            push ecx
            invoke RtlZeroMemory, ecx, nickMaxLength
            invoke lstrlen, newNick
            inc eax
            pop ecx
            invoke lstrcpyn, ecx, newNick, eax

            invoke createLogFileIfNeeded, pChanFHandlListOffset
            invoke logTimestamp, pChanFHandlListOffset

            invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
            fn wsprintf, offset tmpBuff, addr logNickFormat, usrNick, newNick
            mov edi, pChanFHandlListOffset
            lea ebx, dummy
            invoke WriteFile, (chanFHandl PTR[edi]).fHandl, offset tmpBuff, eax, ebx, 0

        .endif
        invoke HeapFree, heap, HEAP_NO_SERIALIZE, newNick
        pop ebx
        pop esi
        add esi, type chanFHandl
        dec ebx
    .endw

    ret
logNick ENDP


process PROC
    LOCAL spaces:spacing, chan:DWORD, pChanFHandlListOffset:DWORD, usrNick:DWORD, NickNodePTR:nickNode
    lea ebx, spaces
    mov DWORD PTR [ebx+(0*4)], 0
    mov DWORD PTR [ebx+(1*4)], 0
    mov DWORD PTR [ebx+(2*4)], 0
    mov DWORD PTR [ebx+(3*4)], 0
    mov DWORD PTR [ebx+(4*4)], 0
    mov DWORD PTR [ebx+(5*4)], 0
    mov eax, 0  ;num of spaces
    mov esi, 0  ;offset for alloc
    mov ecx, 0  ;position in inBuff
    mov edx, offset inBuff
    .while ecx != inBuffLength
        .if (BYTE PTR [edx+ecx]) == ' '
            mov DWORD PTR [ebx+esi], ecx
            add esi, 4
            inc eax
        .endif
        inc ecx
        .if eax == 6
            .break
        .endif
    .endw

    .if eax == 1
        cld
        mov ecx, (lengthof ping)-1
        mov esi, offset inBuff
        mov edi, offset ping

        repe cmpsb
        jne notPing

        invoke onPing

        notPing:

        ret
    .elseif eax <= 0
        ret
    .endif

    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, chanNameMaxLength
    mov chan, eax

    invoke secondWordMatches, addr nameListBeg, spaces
    lea ebx, spaces
    .if eax == 1
        mov esi, DWORD PTR[ebx+(3*4)]
        add esi, offset inBuff
        inc esi
        mov edi, 0
        sub edi, DWORD PTR[ebx+(3*4)]
        add edi, DWORD PTR[ebx+(4*4)]
        invoke lstrcpyn, chan, esi, edi
        invoke getPChanFHandlListOffset, chan, edi
        mov pChanFHandlListOffset, eax
        invoke onNamesList, spaces, pChanFHandlListOffset
        jmp retFreeChan
    .endif


;get nick

    ;restore spaces
    lea ebx, spaces
    mov ecx, 0  ;position in inBuff
    mov edx, offset inBuff
    mov eax, 0
    .while ecx != DWORD PTR[ebx]
        .if (BYTE PTR [edx+ecx]) == '!'
            mov eax, 1
            .break
        .endif
        inc ecx
    .endw
;if no nick found - return
    .if eax == 0
        jmp retFreeChan
    .endif

    mov esi, ecx

    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, nickMaxLength
    mov usrNick, eax

    invoke lstrcpyn, usrNick, offset inBuff + 1, esi

;ignore if it's our nick

    cld
    mov ecx, pNickLength
    mov esi, usrNick
    mov edi, pNick

    repe cmpsb
    jne notOurNick

    ;don't process our nick
    jmp retFreeChanUsrNick

    notOurNick:



    ;check if quit
    invoke secondWordMatches, addr quit, spaces
    .if eax == 1
        ;print "quit!"
        invoke logQuit, usrNick
        jmp retFreeChanUsrNick
   .endif

    ;check if nick
    invoke secondWordMatches, addr nick, spaces
    .if eax == 1
        ;print "nick!"
        invoke logNick, usrNick, spaces
        jmp retFreeChanUsrNick
   .endif



; get channel name


    mov esi, DWORD PTR[ebx+4]
    add esi, offset inBuff
    inc esi
    mov edi, 0
    sub edi, DWORD PTR[ebx+4]
    .if DWORD PTR[ebx+(2*4)] == 0
        add edi, inBuffLength
        dec edi
        dec edi
    .else
        add edi, DWORD PTR[ebx+(2*4)]
    .endif


    invoke lstrcpyn, chan, esi, edi

;not a channel or no such channel

    invoke getPChanFHandlListOffset, chan, edi
    .if eax == 0
        jmp retFreeChanUsrNick
    .endif

    mov pChanFHandlListOffset, eax


;check if it's private message
;it's most commonly occured message from the server, so it makes sence to check it first

     invoke secondWordMatches, addr privmsg, spaces
    .if eax == 1
        ;check if this privmsg is action
        cld
        mov ecx, (lengthof action)-1
        lea ebx, spaces
        mov esi, DWORD PTR[ebx+(2*4)]
        add esi, offset inBuff
        inc esi
        mov edi, offset action
        repe cmpsb
        jne notAction

        ;print "action!"
        invoke logAction, usrNick, spaces, pChanFHandlListOffset
        jmp retFreeChanUsrNick

        notAction:

        ;print "privmsg!"
        invoke logPrivmsg, usrNick, spaces, pChanFHandlListOffset
        jmp retFreeChanUsrNick
    .endif

;check if join

    invoke secondWordMatches, addr join, spaces
    .if eax == 1
        ;print "join!"
        invoke logJoin, usrNick, chan, pChanFHandlListOffset
        invoke addNickNode, usrNick, pChanFHandlListOffset
        jmp retFreeChanUsrNick
   .endif

;check if quit
;

;check if part

    invoke secondWordMatches, addr part, spaces
    .if eax == 1
        ;print "part!"
        invoke logPart, usrNick, chan, pChanFHandlListOffset
        invoke findNickNode, usrNick, pChanFHandlListOffset
        .if eax == 0
            fn StdOut, "something is going wrong"
        .endif
        invoke removeNickNode, eax, pChanFHandlListOffset
        jmp retFreeChanUsrNick
   .endif

;check if mode

    invoke secondWordMatches, addr mode, spaces
    .if eax == 1
        ;print "mode!"
        invoke logMode, usrNick, spaces, pChanFHandlListOffset
        jmp retFreeChanUsrNick
    .endif

;check if topic

    invoke secondWordMatches, addr topic, spaces
    .if eax == 1
        ;print "topic!"
        invoke logTopic, usrNick, spaces, pChanFHandlListOffset

        ;mov ecx, pChanFHandlListOffset
        ;mov edi, (chanFHandl PTR [ecx]).nickHead

        ;.while edi != 0
        ;    lea ebx, (nickNode PTR [edi]).nick
        ;
        ;    invoke StdOut, ebx
        ;    print "|"
        ;    mov edi, (nickNode PTR [edi]).next
        ;
        ;.endw
        ret
    .endif

    invoke secondWordMatches, addr kick, spaces
    .if eax == 1
        ;print "kick!"
        invoke logKick, usrNick, spaces, pChanFHandlListOffset
        jmp retFreeChanUsrNick
    .endif

    retFreeChanUsrNick:
        invoke HeapFree, heap, HEAP_NO_SERIALIZE, usrNick
    retFreeChan:
        invoke HeapFree, heap, HEAP_NO_SERIALIZE, chan


    ret
process ENDP


checkIfDayChanged PROC
    invoke GetSystemTime, addr UTCtime
    mov ax, UTCtime.wDay
    .if ax != day
        mov day, ax

        mov edi, chanFHandlCount
        mov ebx, pChanFHandlList
        .while edi != 0
            .if BYTE PTR ((chanFHandl PTR [ebx]).fOpen) == 1
                invoke CloseHandle, (chanFHandl PTR [ebx]).fHandl
                mov (chanFHandl PTR [ebx]).fOpen, 0
            .endif
            add ebx, type chanFHandl
            dec edi
        .endw
    .endif

    ret
checkIfDayChanged ENDP

main PROC
    LOCAL fSettingsPath:DWORD

    invoke StdOut, offset info

    invoke WSAStartup, 101h, offset wsaData
    .if eax!=0
        print errWSAS
        ret
    .endif

    invoke socket, AF_INET, SOCK_STREAM, IPPROTO_TCP
    .if eax==INVALID_SOCKET
        print errSocket
        ret
    .endif

    mov sock, eax

    mov sin.sin_family, AF_INET

    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff ;zero out memory
    invoke GetProcessHeap
    mov heap, eax
    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, 1024 ;allocate memory for full path of settings.ini
    mov fSettingsPath, eax ;save pointer to memory in fSettingsPath
    invoke GetCurrentDirectory, 1024, fSettingsPath ;put current dir path into the allocated memory
    add eax, fSettingsPath ;calculate end of the string written in the allocated memory
    invoke lstrcat, eax, addr fSettings ;append filename there

    fn GetPrivateProfileString, "general", "port", "6667", offset tmpBuff, sizeof tmpBuff, fSettingsPath ;get string rep. of port


    invoke StrToInt, offset tmpBuff
    invoke htons, eax
    mov sin.sin_port, ax

    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
    fn GetPrivateProfileString, "general", "server", "chat.freenode.net", offset tmpBuff, sizeof tmpBuff, fSettingsPath

    invoke gethostbyname, offset tmpBuff

    mov eax, [eax+12]
    mov eax, [eax]
    mov eax, [eax]
    mov sin.sin_addr, eax

    invoke connect, sock, addr sin, sizeof sin

    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff

    fn GetPrivateProfileString, "general", "ident", "logBot", offset tmpBuff, sizeof tmpBuff, fSettingsPath

    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, 100
    mov esi, eax
    invoke wsprintf, esi, addr user, offset tmpBuff, offset tmpBuff

    invoke send, sock, esi, eax, 0
    invoke StdOut, esi

    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
    invoke RtlZeroMemory, esi, 100

    mov pNickLength, rv(GetPrivateProfileString, "general", "nick", "logBot", offset tmpBuff, sizeof tmpBuff, fSettingsPath)
    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, nickMaxLength
    mov pNick, eax
    invoke lstrcat, pNick, offset tmpBuff
    invoke wsprintf, esi, addr doNick, offset tmpBuff

    invoke send, sock, esi, eax, 0
    invoke StdOut, esi

    invoke HeapFree, heap, HEAP_NO_SERIALIZE, esi

    invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
    fn GetPrivateProfileString, "general", "chans", "1", offset tmpBuff, sizeof tmpBuff, fSettingsPath
    invoke StrToInt, offset tmpBuff
    mov chanFHandlCount, eax
    mov ecx, sizeof chanFHandl
    mul ecx
    jnc noErrChannels
        print errChannels
        ret
    noErrChannels:
    mov edi, eax
    invoke HeapAlloc, heap, HEAP_NO_SERIALIZE + HEAP_ZERO_MEMORY, edi
    mov pChanFHandlList, eax
    mov edi, chanFHandlCount
    mov ebx, pChanFHandlList
    .while edi != 0
        invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
        fn wsprintf, offset tmpBuff, "%d", edi
        fn GetPrivateProfileString, "channels", offset tmpBuff, "#freenode", addr (chanFHandl PTR [ebx]).chan, chanNameMaxLength, fSettingsPath
        invoke RtlZeroMemory, offset tmpBuff, sizeof tmpBuff
        fn wsprintf, offset tmpBuff, addr doJoin, addr (chanFHandl PTR [ebx]).chan
        invoke send, sock, offset tmpBuff, eax, 0
        invoke StdOut, offset tmpBuff

        add ebx, type chanFHandl
        dec edi
    .endw


    fn GetPrivateProfileString, "general", "logDirPath", ".\logs\", addr logDirPath, lengthof logDirPath, fSettingsPath
    invoke SHCreateDirectoryEx, 0, addr logDirPath, 0

    ;invoke StdOut, addr logDirPath

    invoke HeapFree, heap, HEAP_NO_SERIALIZE, fSettingsPath
    mov fSettingsPath, 0

    .while 1
        invoke receive
        invoke checkIfDayChanged
        invoke StdOut, addr inBuff
        invoke process
    .endw

    ret

main ENDP

end start
