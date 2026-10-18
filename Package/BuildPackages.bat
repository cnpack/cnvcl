@ECHO OFF
CD ..
CALL CleanInplace.bat
CD Package

REM ============================================================
REM Build CnVCL runtime packages for Delphi 5 to Latest.
REM Delphi High Versions also build the Win64 runtime packages with dcc64.
REM dcc32/dcc64 paths refer to cnwizards/Make/want.xml if not default.
REM BPL/DCP outputs are placed in each package directory.
REM ============================================================

SETLOCAL
SET ROOTDIR=%~dp0
ECHO ROOTDIR=%ROOTDIR%

REM Command Line Compilers of Each Delphi Version
SET DCC5="C:\Program Files\Borland\Delphi5\Bin\dcc32.exe"
SET DCC6="C:\Program Files\Borland\Delphi6\Bin\dcc32.exe"
SET DCC7="C:\Program Files\Borland\Delphi7\Bin\dcc32.exe"
SET DCC2005="C:\Program Files\Borland\BDS\3.0\Bin\dcc32.exe"
SET DCC2006="C:\Program Files\Borland\BDS\4.0\Bin\dcc32.exe"
SET DCC2007="C:\Program Files\CodeGear\RAD Studio\5.0\Bin\dcc32.exe"
SET DCC2009="C:\Program Files\CodeGear\RAD Studio\6.0\Bin\dcc32.exe"
SET DCC2010="C:\Program Files\Embarcadero\RAD Studio\7.0\Bin\dcc32.exe"
SET DCCXE="C:\Program Files\Embarcadero\RAD Studio\8.0\Bin\dcc32.exe"
SET DCCXE2="C:\Program Files\Embarcadero\RAD Studio\9.0\Bin\dcc32.exe"
SET DCCXE3="C:\Program Files\Embarcadero\RAD Studio\10.0\Bin\dcc32.exe"
SET DCCXE4="C:\Program Files\Embarcadero\RAD Studio\11.0\Bin\dcc32.exe"
SET DCCXE5="C:\Program Files\Embarcadero\RAD Studio\12.0\Bin\dcc32.exe"
SET DCCXE6="C:\Program Files\Embarcadero\Studio\14.0\Bin\dcc32.exe"
SET DCCXE7="C:\Program Files\Embarcadero\Studio\15.0\Bin\dcc32.exe"
SET DCCXE8="C:\Program Files\Embarcadero\Studio\16.0\Bin\dcc32.exe"
SET DCC10S="C:\Program Files\Embarcadero\Studio\17.0\Bin\dcc32.exe"
SET DCC101B="C:\Program Files\Embarcadero\Studio\18.0\Bin\dcc32.exe"
SET DCC102T="C:\Program Files\Embarcadero\Studio\19.0\Bin\dcc32.exe"
SET DCC103R="C:\Program Files\Embarcadero\Studio\20.0\Bin\dcc32.exe"
SET DCC104S="C:\Program Files\Embarcadero\Studio\21.0\Bin\dcc32.exe"
SET DCC110A="C:\Program Files\Embarcadero\Studio\22.0\Bin\dcc32.exe"
SET DCC120A="C:\Program Files\Embarcadero\Studio\23.0\Bin\dcc32.exe"
SET DCC120A_64="C:\Program Files\Embarcadero\Studio\23.0\Bin\dcc64.exe"
SET DCC130F="C:\Program Files\Embarcadero\Studio\37.0\Bin\dcc32.exe"
SET DCC130F_64="C:\Program Files\Embarcadero\Studio\37.0\Bin\dcc64.exe"

REM C++Builder 5/6 build tools (bpr2mak converts the XML .bpk to a makefile)
SET BCB5BIN=C:\Program Files\Borland\CBuilder5\Bin
SET BCB6BIN=C:\Program Files\Borland\CBuilder6\Bin

REM Common Compiler Options: unit/include/obj/res Search Paths
SET SOURCES=..\..\Source\Common;..\..\Source\Crypto;..\..\Source\Graphic;..\..\Source\NonVisual;..\..\Source\NetComm;..\..\Source\MultiLang;..\..\Source\DbReport;..\..\Source\ObjRep;..\..\Source\Skin;..\..\Source\ThirdParty
SET PATHOPTS=-U"%SOURCES%" -I"%SOURCES%" -O"%SOURCES%" -R"%SOURCES%"
SET OUTOPTS=-LE"." -LN"."
SET ALIASFLAG="-AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE;"
SET NSFLAG="-NSSystem;System.Win;Winapi;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;Data;Data.Win;Xml;Xml.Win;Bde;Datasnap;Datasnap.Win;Web;Web.Win;Soap;Soap.Win"

SET OFFICE7=-U"C:\Program Files\Borland\Delphi7\Imports;C:\Program Files\Borland\Delphi7\Ocx\Servers"
SET OFFICE2005=-U"C:\Program Files\Borland\BDS\3.0\Imports;C:\Program Files\Borland\BDS\3.0\Ocx\Servers"
SET OFFICE2006=-U"C:\Program Files\Borland\BDS\4.0\Imports;C:\Program Files\Borland\BDS\4.0\Ocx\Servers"
SET OFFICE2007=-U"C:\Program Files\CodeGear\RAD Studio\5.0\Imports;C:\Program Files\CodeGear\RAD Studio\5.0\Ocx\Servers"
SET OFFICE2009=-U"C:\Program Files\CodeGear\RAD Studio\6.0\Imports"
SET OFFICE2010=-U"C:\Program Files\Embarcadero\RAD Studio\7.0\Imports"
SET OFFICEXE=-U"C:\Program Files\Embarcadero\RAD Studio\8.0\Ocx\Servers"
SET OFFICEXE2=-U"C:\Program Files\Embarcadero\RAD Studio\9.0\Ocx\Servers"
SET OFFICEXE3=-U"C:\Program Files\Embarcadero\RAD Studio\10.0\Ocx\Servers"
SET OFFICEXE4=-U"C:\Program Files\Embarcadero\RAD Studio\11.0\Ocx\Servers"
SET OFFICEXE5=-U"C:\Program Files\Embarcadero\RAD Studio\12.0\Ocx\Servers"
SET OFFICEXE6=-U"C:\Program Files\Embarcadero\Studio\14.0\Ocx\Servers"
SET OFFICEXE7=-U"C:\Program Files\Embarcadero\Studio\15.0\Ocx\Servers"
SET OFFICEXE8=-U"C:\Program Files\Embarcadero\Studio\16.0\Ocx\Servers"
SET OFFICE10S=-U"C:\Program Files\Embarcadero\Studio\17.0\Ocx\Servers"
SET OFFICE101B=-U"C:\Program Files\Embarcadero\Studio\18.0\Ocx\Servers"
SET OFFICE102T=-U"C:\Program Files\Embarcadero\Studio\19.0\Ocx\Servers"
SET OFFICE103R=-U"C:\Program Files\Embarcadero\Studio\20.0\Ocx\Servers"
SET OFFICE104S=-U"C:\Program Files\Embarcadero\Studio\21.0\Ocx\Servers"
SET OFFICE110A=-U"C:\Program Files\Embarcadero\Studio\22.0\Ocx\Servers"
SET OFFICE120A=-U"C:\Program Files\Embarcadero\Studio\23.0\Ocx\Servers"
SET OFFICE130F=-U"C:\Program Files\Embarcadero\Studio\37.0\Ocx\Servers"

GOTO :PACKAGE_START

ECHO =====================================
ECHO === Building C++Builder 5 Packages
ECHO =====================================
CD BCB5
ECHO *** Building Runtime Package CnPack_CB5.bpk
CALL :CLEANTMP
"%BCB5BIN%\bpr2mak.exe" -t"%BCB5BIN%\DEFAULT.BMK" CnPack_CB5.bpk -q
IF ERRORLEVEL 1 GOTO END
"%BCB5BIN%\make.exe" -fCnPack_CB5.mak
IF ERRORLEVEL 1 GOTO END
DEL CnPack_CB5.mak 2> NUL
CD ..

ECHO =====================================
ECHO === Building C++Builder 6 Packages
ECHO =====================================
CD BCB6
ECHO *** Building Runtime Package CnPack_CB6.bpk
CALL :CLEANTMP
COPY /Y "..\..\Source\Common\CnPack.inc" . > NUL
"%BCB6BIN%\bpr2mak.exe" -t"%BCB6BIN%\DEFAULT.BMK" CnPack_CB6.bpk -q
IF ERRORLEVEL 1 GOTO END
"%BCB6BIN%\make.exe" -fCnPack_CB6.mak
IF ERRORLEVEL 1 GOTO END
DEL CnPack_CB6.mak 2> NUL
DEL CnPack.inc 2> NUL
CD ..

:PACKAGE_START

ECHO =====================================
ECHO === Building Delphi 5 Packages
ECHO =====================================
CD Delphi5
ECHO *** Building Runtime Package CnPack_D5.dpk
CALL :CLEANTMP
%DCC5% -B %ALIASFLAG% %PATHOPTS%  %OUTOPTS% "CnPack_D5.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 6 Packages
ECHO =====================================
CD Delphi6
ECHO *** Building Runtime Package CnPack_D6.dpk
CALL :CLEANTMP
%DCC6% -B %ALIASFLAG% %PATHOPTS%  %OUTOPTS% "CnPack_D6.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 7 Packages
ECHO =====================================
CD Delphi7
ECHO *** Building Runtime Package CnPack_D7.dpk
CALL :CLEANTMP
%DCC7% -B %ALIASFLAG% %PATHOPTS% %OFFICE7%  %OUTOPTS% "CnPack_D7.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 2005 Packages
ECHO =====================================
CD Delphi2005
ECHO *** Building Runtime Package CnPack_D2005.dpk
CALL :CLEANTMP
%DCC2005% -B %ALIASFLAG% %PATHOPTS% %OFFICE2005%  %OUTOPTS% "CnPack_D2005.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 2006 Packages
ECHO =====================================
CD Delphi2006
ECHO *** Building Runtime Package CnPack_D2006.dpk
CALL :CLEANTMP
%DCC2006% -B %ALIASFLAG% %PATHOPTS% %OFFICE2006%  %OUTOPTS% "CnPack_D2006.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 2007 Packages
ECHO =====================================
CD Delphi2007
ECHO *** Building Runtime Package CnPack_D2007.dpk
CALL :CLEANTMP
%DCC2007% -B %ALIASFLAG% %PATHOPTS% %OFFICE2007%  %OUTOPTS% "CnPack_D2007.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 2009 Packages
ECHO =====================================
CD Delphi2009
ECHO *** Building Runtime Package CnPack_D2009.dpk
CALL :CLEANTMP
%DCC2009% -B %ALIASFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICE2009% "CnPack_D2009.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 2010 Packages
ECHO =====================================
CD Delphi2010
ECHO *** Building Runtime Package CnPack_D2010.dpk
CALL :CLEANTMP
%DCC2010% -B %ALIASFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICE2010% "CnPack_D2010.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi XE Packages
ECHO =====================================
CD DelphiXE
ECHO *** Building Runtime Package CnPack_DXE.dpk
CALL :CLEANTMP
%DCCXE% -B %ALIASFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICEXE% "CnPack_DXE.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi XE2 Packages
ECHO =====================================
CD DelphiXE2
ECHO *** Building Runtime Package CnPack_DXE2.dpk
CALL :CLEANTMP
%DCCXE2% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICEXE2% "CnPack_DXE2.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi XE3 Packages
ECHO =====================================
CD DelphiXE3
ECHO *** Building Runtime Package CnPack_DXE3.dpk
CALL :CLEANTMP
%DCCXE3% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICEXE3% "CnPack_DXE3.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi XE4 Packages
ECHO =====================================
CD DelphiXE4
ECHO *** Building Runtime Package CnPack_DXE4.dpk
CALL :CLEANTMP
%DCCXE4% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICEXE4% "CnPack_DXE4.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi XE5 Packages
ECHO =====================================
CD DelphiXE5
ECHO *** Building Runtime Package CnPack_DXE5.dpk
CALL :CLEANTMP
%DCCXE5% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICEXE5% "CnPack_DXE5.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi XE6 Packages
ECHO =====================================
CD DelphiXE6
ECHO *** Building Runtime Package CnPack_DXE6.dpk
CALL :CLEANTMP
%DCCXE6% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICEXE6% "CnPack_DXE6.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi XE7 Packages
ECHO =====================================
CD DelphiXE7
ECHO *** Building Runtime Package CnPack_DXE7.dpk
CALL :CLEANTMP
%DCCXE7% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICEXE7% "CnPack_DXE7.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi XE8 Packages
ECHO =====================================
CD DelphiXE8
ECHO *** Building Runtime Package CnPack_DXE8.dpk
CALL :CLEANTMP
%DCCXE8% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICEXE8% "CnPack_DXE8.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 10 Seattle Packages
ECHO =====================================
CD Delphi10S
ECHO *** Building Runtime Package CnPack_D10S.dpk
CALL :CLEANTMP
%DCC10S% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICE10S% "CnPack_D10S.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 10.1 Berlin Packages
ECHO =====================================
CD Delphi101B
ECHO *** Building Runtime Package CnPack_D101B.dpk
CALL :CLEANTMP
%DCC101B% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICE101B% "CnPack_D101B.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 10.2 Tokyo Packages
ECHO =====================================
CD Delphi102T
ECHO *** Building Runtime Package CnPack_D102T.dpk
CALL :CLEANTMP
%DCC102T% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICE102T% "CnPack_D102T.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 10.3 Rio Packages
ECHO =====================================
CD Delphi103R
ECHO *** Building Runtime Package CnPack_D103R.dpk
CALL :CLEANTMP
%DCC103R% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICE103R% "CnPack_D103R.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 10.4 Sydney Packages
ECHO =====================================
CD Delphi104S
ECHO *** Building Runtime Package CnPack_D104S.dpk
CALL :CLEANTMP
%DCC104S% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICE104S% "CnPack_D104S.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 11 Alexandria Packages
ECHO =====================================
CD Delphi110A
ECHO *** Building Runtime Package CnPack_D110A.dpk
CALL :CLEANTMP
%DCC110A% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICE110A% "CnPack_D110A.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 12 Athens Packages
ECHO =====================================
CD Delphi120A
ECHO *** Building Runtime Package CnPack_D120A.dpk
CALL :CLEANTMP
%DCC120A% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICE120A% "CnPack_D120A.dpk"
IF ERRORLEVEL 1 GOTO END
ECHO *** Building 64bit Runtime Package CnPack_D120A64.dpk
CALL :CLEANTMP
%DCC120A_64% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICE120A% "CnPack_D120A64.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO =====================================
ECHO === Building Delphi 13 Florence Packages
ECHO =====================================
CD Delphi130F
ECHO *** Building Runtime Package CnPack_D130F.dpk
CALL :CLEANTMP
%DCC130F% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICE130F% "CnPack_D130F.dpk"
IF ERRORLEVEL 1 GOTO END
ECHO *** Building 64bit Runtime Package CnPack_D130F64.dpk
CALL :CLEANTMP
%DCC130F_64% -B %ALIASFLAG% %NSFLAG% %PATHOPTS%  %OUTOPTS%  %OFFICE130F% "CnPack_D130F64.dpk"
IF ERRORLEVEL 1 GOTO END
CD ..

ECHO Build CnVCL Packages Complete.
:END
CD %ROOTDIR%
PAUSE
:EXIT

REM Clean temp dcu files
:CLEANTMP
  DEL *.dcu 2> NUL
  DEL *.obj 2> NUL
  DEL ..\..\Source\Common\*.dcu 2> NUL
  DEL ..\..\Source\Crypto\*.dcu 2> NUL
  DEL ..\..\Source\Graphic\*.dcu 2> NUL
  DEL ..\..\Source\NonVisual\*.dcu 2> NUL
  DEL ..\..\Source\NetComm\*.dcu 2> NUL
  DEL ..\..\Source\MultiLang\*.dcu 2> NUL
  DEL ..\..\Source\DbReport\*.dcu 2> NUL
  DEL ..\..\Source\ObjRep\*.dcu 2> NUL
  DEL ..\..\Source\Skin\*.dcu 2> NUL
  DEL ..\..\Source\ThirdParty\*.dcu 2> NUL
  DEL ..\..\Source\Common\*.obj 2> NUL
  DEL ..\..\Source\Crypto\*.obj 2> NUL
  DEL ..\..\Source\Graphic\*.obj 2> NUL
  DEL ..\..\Source\NonVisual\*.obj 2> NUL
  DEL ..\..\Source\NetComm\*.obj 2> NUL
  DEL ..\..\Source\MultiLang\*.obj 2> NUL
  DEL ..\..\Source\DbReport\*.obj 2> NUL
  DEL ..\..\Source\ObjRep\*.obj 2> NUL
  DEL ..\..\Source\Skin\*.obj 2> NUL
  DEL ..\..\Source\ThirdParty\*.obj 2> NUL
GOTO :EOF
