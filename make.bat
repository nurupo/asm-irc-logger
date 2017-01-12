@echo off

    if exist "IRCLogger.obj" del "IRCLogger.obj"
    if exist "IRCLogger.exe" del "IRCLogger.exe"

    \masm32\bin\ml /c /coff "IRCLogger.asm"
    if errorlevel 1 goto errasm

    \masm32\bin\PoLink /SUBSYSTEM:CONSOLE "IRCLogger.obj"
    if errorlevel 1 goto errlink
    dir "IRCLogger.*"
    goto TheEnd

  :errlink
    echo _
    echo Link error
    goto TheEnd

  :errasm
    echo _
    echo Assembly Error
    goto TheEnd
    
  :TheEnd

pause

