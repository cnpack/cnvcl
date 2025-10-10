@ECHO OFF
CD ..\..
CALL CleanInplace.bat
CD Examples\VCL
SETLOCAL ENABLEDELAYEDEXPANSION
SET ROOTDIR=%~dp0
ECHO ROOTDIR=!ROOTDIR!
SET FPC32="C:\lazarus\fpc\3.2.2\bin\i386-win32\fpc.exe" -MDelphi -Scghi -O1 -gw2 -godwarfsets -gl -WG -l -vewnhibq -Fi..\..\..\Source\Crypto -Fi..\..\..\Source\Common -Fi..\..\..\Source\NetComm -Fi..\..\..\Source\MultiLang -Fi..\..\..\Source\Graphic -Fu..\..\..\Source\Crypto -Fu..\..\..\Source\Common -Fu..\..\..\Source\NetComm -Fu..\..\..\Source\MultiLang -Fu..\..\..\Source\Graphic -FuC:\lazarus\lcl\units\i386-win32\win32 -FuC:\lazarus\lcl\units\i386-win32 -FuC:\lazarus\components\freetype\lib\i386-win32 -FuC:\lazarus\components\lazutils\lib\i386-win32 -FuC:\lazarus\packager\units\i386-win32 -Fu. -dLCL -dLCLwin32 -dPUREPASCAL 

CD %ROOTDIR%
SET DPR="NOVALUE"
SET DIRNAME="NODIR"
FOR /D %%D IN (.\*) DO (
  ECHO =====================================
  ECHO Enter %%D
  CD %%D
  SET DIRNAME=%%D
  FOR %%F IN (.\*.lpr) DO (
    SET LPR=%%F
    IF "!LPR:~-3!" == "lpr" (
      ECHO Building !LPR! with !LPR:~0,-3!cfg in !DIRNAME!
      %FPC32% "%%F"
      IF !ERRORLEVEL! NEQ 0 GOTO END
CALL :CLEANTMP
    )
  )
  CD ..
)
ECHO Build FPC Examples Complete.
:END
CD %ROOTDIR%
PAUSE
:EXIT

REM ɾ�������е���ʱ�ļ�
:CLEANTMP
  DEL *.dcu 2> NUL
  DEL ..\..\..\Source\Crypto\*.dcu 2> NUL
  DEL ..\..\..\Source\Common\*.dcu 2> NUL
  DEL ..\..\..\Source\Graphic\*.dcu 2> NUL
  DEL ..\..\..\Source\DbReport\*.dcu 2> NUL
  DEL ..\..\..\Source\NetComm\*.dcu 2> NUL
  DEL ..\..\..\Source\NonVisual\*.dcu 2> NUL
GOTO :EOF