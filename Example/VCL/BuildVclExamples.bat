@ECHO OFF
CD ..\..
CALL CleanInplace.bat
CD Example\VCL
SETLOCAL ENABLEDELAYEDEXPANSION
SET ROOTDIR=%~dp0
ECHO ROOTDIR=!ROOTDIR!
SET DCC32="C:\Program Files\Borland\Delphi7\Bin\dcc32.exe"
SET DCC7_32="C:\Program Files\Borland\Delphi7\Bin\dcc32.exe"
SET DCCR_32="C:\Program Files\Embarcadero\Studio\20.0\bin\dcc32.exe"

CD %ROOTDIR%
SET DPR="NOVALUE"
SET DIRNAME="NODIR"
FOR /D %%D IN (.\*) DO (
  ECHO =====================================
  ECHO Enter %%D
  CD %%D
  SET DIRNAME=%%D
  FOR %%F IN (.\*.dpr) DO (
    SET DPR=%%F
    IF "!DPR:~-3!" == "dpr" (
      ECHO Building !DPR! with !DPR:~0,-3!cfg in !DIRNAME!
      IF "!DIRNAME:~-3!" == "_D7" (
        %DCC7_32% "%%F"
        IF !ERRORLEVEL! NEQ 0 GOTO END
      ) ELSE (
        IF "!DIRNAME:~-4!" == "_FMX" (
          %DCCR_32% "%%F"
          IF !ERRORLEVEL! NEQ 0 GOTO END
        ) ELSE (
          IF "!DIRNAME!" == ".\JsonPersistent" (
             %DCCR_32% "%%F"
             IF !ERRORLEVEL! NEQ 0 GOTO END
          ) ELSE (
            IF "!DIRNAME!" == ".\XMLPersistent" (
              REM Can NOT Compile This Project.
            ) ELSE (
CALL :CLEANTMP
              %DCC32% "%%F" < "!DPR:~0,-3!cfg"
              IF !ERRORLEVEL! NEQ 0 GOTO END
            )
          )
        )
      )
    )
  )
  CD ..
)
ECHO Build VCL Examples Complete.
:END
CD %ROOTDIR%
PAUSE
:EXIT

REM 删除编译中的临时文件
:CLEANTMP
  DEL *.dcu 2> NUL
  DEL ..\..\..\Source\Crypto\*.dcu 2> NUL
  DEL ..\..\..\Source\Common\*.dcu 2> NUL
  DEL ..\..\..\Source\Graphics\*.dcu 2> NUL
  DEL ..\..\..\Source\DbReport\*.dcu 2> NUL
  DEL ..\..\..\Source\NetComm\*.dcu 2> NUL
  DEL ..\..\..\Source\NonVisual\*.dcu 2> NUL
GOTO :EOF