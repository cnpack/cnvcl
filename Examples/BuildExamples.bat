@ECHO OFF
SET ROOTDIR="%CD%"
ECHO ROOTDIR=%DIR%
SET DCC32="C:\Program Files\Borland\Delphi5\Bin\dcc32.exe"
SET DCC7_32="C:\Program Files\Borland\Delphi7\Bin\dcc32.exe"

CD %ROOTDIR%
FOR /D %%D IN (.\*) DO (
  ECHO Enter %%D
  CD %%D
  FOR %%F IN (.\*.dpr) DO (
    SET DPR=%%F
    ECHO Building %DPR%
    SET CFG=%DPR:~0,-3%
    SET DIRNAME=%%D
    IF %DIRNAME:~0,-3% == "_D7" (
      %DCC7_32% %DPR% < %DPR:~0,-3%cfg
    ) ELSE (
      %DCC32% %DPR% < %DPR:~0,-3%cfg
    )
    ;IF %ERRORLEVEL%==0 GOTO END
  )
  CD ..
)
:END
PAUSE