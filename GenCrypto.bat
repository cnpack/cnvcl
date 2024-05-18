@ECHO OFF

CD ..
DEL /S /Q cncrypto
RMDIR /Q cncrypto /S
MKDIR cncrypto
CD cncrypto
COPY ..\cnvcl\License.chs.txt .\License.txt
COPY ..\cnvcl\Doc\Develop\CnCrypto_README.md .\README.md
MKDIR Source
CD Source
COPY ..\..\cnvcl\Source\Common\CnPack.inc .
COPY ..\..\cnvcl\Source\Common\CnBigDecimal.pas .
COPY ..\..\cnvcl\Source\Common\CnBigRational.pas .
COPY ..\..\cnvcl\Source\Common\CnClasses.pas .
COPY ..\..\cnvcl\Source\Common\CnConsts.pas .
COPY ..\..\cnvcl\Source\Common\CnContainers.pas .
COPY ..\..\cnvcl\Source\Common\CnFloat.pas .
COPY ..\..\cnvcl\Source\Common\CnHashMap.pas .
COPY ..\..\cnvcl\Source\Common\CnMath.pas .
COPY ..\..\cnvcl\Source\Common\CnMatrix.pas .
COPY ..\..\cnvcl\Source\Common\CnTree.pas .
COPY ..\..\cnvcl\Source\Crypto\*.pas .
CD ..
MKDIR Package
CD Package
CALL :PRINTHEAD CnCrypto.dpk
ECHO package CnCrypto;                                                         >> CnCrypto.dpk
ECHO.                                                                          >> CnCrypto.dpk
ECHO {$R *.res}                                                                >> CnCrypto.dpk
ECHO {$IFDEF IMPLICITBUILDING This IFDEF should not be used by users}          >> CnCrypto.dpk
ECHO {$ALIGN 8}                                                                >> CnCrypto.dpk
ECHO {$ASSERTIONS ON}                                                          >> CnCrypto.dpk
ECHO {$BOOLEVAL OFF}                                                           >> CnCrypto.dpk
ECHO {$DEBUGINFO OFF}                                                          >> CnCrypto.dpk
ECHO {$EXTENDEDSYNTAX ON}                                                      >> CnCrypto.dpk
ECHO {$IMPORTEDDATA ON}                                                        >> CnCrypto.dpk
ECHO {$IOCHECKS ON}                                                            >> CnCrypto.dpk
ECHO {$LOCALSYMBOLS ON}                                                        >> CnCrypto.dpk
ECHO {$LONGSTRINGS ON}                                                         >> CnCrypto.dpk
ECHO {$OPENSTRINGS ON}                                                         >> CnCrypto.dpk
ECHO {$OPTIMIZATION OFF}                                                       >> CnCrypto.dpk
ECHO {$OVERFLOWCHECKS OFF}                                                     >> CnCrypto.dpk
ECHO {$RANGECHECKS OFF}                                                        >> CnCrypto.dpk
ECHO {$REFERENCEINFO ON}                                                       >> CnCrypto.dpk
ECHO {$SAFEDIVIDE OFF}                                                         >> CnCrypto.dpk
ECHO {$STACKFRAMES ON}                                                         >> CnCrypto.dpk
ECHO {$TYPEDADDRESS OFF}                                                       >> CnCrypto.dpk
ECHO {$VARSTRINGCHECKS ON}                                                     >> CnCrypto.dpk
ECHO {$WRITEABLECONST OFF}                                                     >> CnCrypto.dpk
ECHO {$MINENUMSIZE 1}                                                          >> CnCrypto.dpk
ECHO {$IMAGEBASE $400000}                                                      >> CnCrypto.dpk
ECHO {$ENDIF IMPLICITBUILDING}                                                 >> CnCrypto.dpk
ECHO {$DESCRIPTION 'CnPack Cryptography Package'}                              >> CnCrypto.dpk
ECHO {$IMPLICITBUILD OFF}                                                      >> CnCrypto.dpk
ECHO.                                                                          >> CnCrypto.dpk
ECHO requires                                                                  >> CnCrypto.dpk
ECHO   vcl;                                                                    >> CnCrypto.dpk
ECHO.                                                                          >> CnCrypto.dpk
ECHO contains                                                                  >> CnCrypto.dpk
CALL :PRINTFILES CnCrypto.dpk ALLFILES
ECHO.                                                                          >> CnCrypto.dpk
ECHO end.                                                                      >> CnCrypto.dpk
ECHO.                                                                          >> CnCrypto.dpk
CD ..
MKDIR Test
CD Test
COPY ..\..\cnvcl\Example\Linux\Crypto\CryptoTest.pas .
COPY ..\..\cnvcl\Example\Linux\Crypto\Crypto.res .
CALL :PRINTHEAD Crypto.dpr
ECHO program Crypto;                                                           >> Crypto.dpr
ECHO.                                                                          >> Crypto.dpr
ECHO {$APPTYPE CONSOLE}                                                        >> Crypto.dpr
ECHO.                                                                          >> Crypto.dpr
ECHO {$R *.res}                                                                >> Crypto.dpr
ECHO.                                                                          >> Crypto.dpr
ECHO uses                                                                      >> Crypto.dpr
ECHO   SysUtils,                                                               >> Crypto.dpr
CALL :PRINTFILES Crypto.dpr
ECHO   CryptoTest in 'CryptoTest.pas';                                         >> Crypto.dpr
ECHO.                                                                          >> Crypto.dpr
ECHO begin                                                                     >> Crypto.dpr
ECHO   try                                                                     >> Crypto.dpr
ECHO     TestCrypto;                                                           >> Crypto.dpr
ECHO   except                                                                  >> Crypto.dpr
ECHO     on E: Exception do                                                    >> Crypto.dpr
ECHO       Writeln(E.ClassName, ': ', E.Message);                              >> Crypto.dpr
ECHO   end;                                                                    >> Crypto.dpr
ECHO end.                                                                      >> Crypto.dpr
ECHO.                                                                          >> Crypto.dpr
CALL :PRINTHEAD Crypto.lpr
ECHO program Crypto;                                                           >> Crypto.lpr
ECHO.                                                                          >> Crypto.lpr
ECHO.{$MODE Delphi}                                                            >> Crypto.lpr
ECHO.                                                                          >> Crypto.lpr
ECHO {$APPTYPE CONSOLE}                                                        >> Crypto.lpr
ECHO.                                                                          >> Crypto.lpr
ECHO {$R *.res}                                                                >> Crypto.lpr
ECHO.                                                                          >> Crypto.lpr
ECHO uses                                                                      >> Crypto.lpr
ECHO   SysUtils, Interfaces,                                                   >> Crypto.lpr
CALL :PRINTFILES Crypto.lpr
ECHO   CryptoTest in 'CryptoTest.pas';                                         >> Crypto.lpr
ECHO.                                                                          >> Crypto.lpr
ECHO begin                                                                     >> Crypto.lpr
ECHO   try                                                                     >> Crypto.lpr
ECHO     TestCrypto;                                                           >> Crypto.lpr
ECHO   except                                                                  >> Crypto.lpr
ECHO     on E: Exception do                                                    >> Crypto.lpr
ECHO       Writeln(E.ClassName, ': ', E.Message);                              >> Crypto.lpr
ECHO   end;                                                                    >> Crypto.lpr
ECHO end.                                                                      >> Crypto.lpr
ECHO.                                                                          >> Crypto.lpr
ECHO ^<^?xml version="1.0" encoding="UTF-8"^?^>                                 > Crypto.lpi
ECHO ^<CONFIG^>                                                                >> Crypto.lpi
ECHO   ^<ProjectOptions^>                                                      >> Crypto.lpi
ECHO     ^<Version Value="12"/^>                                               >> Crypto.lpi
ECHO     ^<PathDelim Value="\"/^>                                              >> Crypto.lpi
ECHO     ^<General^>                                                           >> Crypto.lpi
ECHO       ^<Flags^>                                                           >> Crypto.lpi
ECHO         ^<MainUnitHasCreateFormStatements Value="False"/^>                >> Crypto.lpi
ECHO         ^<MainUnitHasScaledStatement Value="False"/^>                     >> Crypto.lpi
ECHO       ^</Flags^>                                                          >> Crypto.lpi
ECHO       ^<SessionStorage Value="InProjectDir"/^>                            >> Crypto.lpi
ECHO       ^<Title Value="My Application"/^>                                   >> Crypto.lpi
ECHO       ^<UseAppBundle Value="False"/^>                                     >> Crypto.lpi
ECHO       ^<ResourceType Value="res"/^>                                       >> Crypto.lpi
ECHO     ^</General^>                                                          >> Crypto.lpi
ECHO     ^<BuildModes^>                                                        >> Crypto.lpi
ECHO       ^<Item Name="Default" Default="True"/^>                             >> Crypto.lpi
ECHO     ^</BuildModes^>                                                       >> Crypto.lpi
ECHO     ^<PublishOptions^>                                                    >> Crypto.lpi
ECHO       ^<Version Value="2"/^>                                              >> Crypto.lpi
ECHO       ^<UseFileFilters Value="True"/^>                                    >> Crypto.lpi
ECHO     ^</PublishOptions^>                                                   >> Crypto.lpi
ECHO     ^<RunParams^>                                                         >> Crypto.lpi
ECHO       ^<FormatVersion Value="2"/^>                                        >> Crypto.lpi
ECHO     ^</RunParams^>                                                        >> Crypto.lpi
ECHO     ^<RequiredPackages^>                                                  >> Crypto.lpi
ECHO       ^<Item^>                                                            >> Crypto.lpi
ECHO         ^<PackageName Value="LCL"/^>                                      >> Crypto.lpi
ECHO       ^</Item^>                                                           >> Crypto.lpi
ECHO     ^</RequiredPackages^>                                                 >> Crypto.lpi
ECHO     ^<Units^>                                                             >> Crypto.lpi
ECHO       ^<Unit^>                                                            >> Crypto.lpi
ECHO         ^<Filename Value="Crypto.lpr"/^>                                  >> Crypto.lpi
ECHO         ^<IsPartOfProject Value="True"/^>                                 >> Crypto.lpi
ECHO       ^</Unit^>                                                           >> Crypto.lpi
ECHO       ^<Unit^>                                                            >> Crypto.lpi
ECHO         ^<Filename Value="CryptoTest.pas"/^>                              >> Crypto.lpi
ECHO         ^<IsPartOfProject Value="True"/^>                                 >> Crypto.lpi
ECHO       ^</Unit^>                                                           >> Crypto.lpi
ECHO     ^</Units^>                                                            >> Crypto.lpi
ECHO   ^</ProjectOptions^>                                                     >> Crypto.lpi
ECHO   ^<CompilerOptions^>                                                     >> Crypto.lpi
ECHO     ^<Version Value="11"/^>                                               >> Crypto.lpi
ECHO     ^<PathDelim Value="\"/^>                                              >> Crypto.lpi
ECHO     ^<Target^>                                                            >> Crypto.lpi
ECHO       ^<Filename Value="Crypto"/^>                                        >> Crypto.lpi
ECHO     ^</Target^>                                                           >> Crypto.lpi
ECHO     ^<SearchPaths^>                                                       >> Crypto.lpi
ECHO       ^<IncludeFiles Value="$(ProjOutDir)"/^>                             >> Crypto.lpi
ECHO       ^<IncludeFiles Value="..\Source"/^>                                 >> Crypto.lpi
ECHO       ^<Libraries Value="..\Source"/^>                                    >> Crypto.lpi
ECHO       ^<OtherUnitFiles Value="..\Source"/^>                               >> Crypto.lpi
ECHO       ^<UnitOutputDirectory Value="lib\$(TargetCPU)-$(TargetOS)"/^>       >> Crypto.lpi
ECHO     ^</SearchPaths^>                                                      >> Crypto.lpi
ECHO   ^</CompilerOptions^>                                                    >> Crypto.lpi
ECHO   ^<Debugging^>                                                           >> Crypto.lpi
ECHO     ^<Exceptions^>                                                        >> Crypto.lpi
ECHO       ^<Item^>                                                            >> Crypto.lpi
ECHO         ^<Name Value="EAbort"/^>                                          >> Crypto.lpi
ECHO       ^</Item^>                                                           >> Crypto.lpi
ECHO       ^<Item^>                                                            >> Crypto.lpi
ECHO         ^<Name Value="ECodetoolError"/^>                                  >> Crypto.lpi
ECHO       ^</Item^>                                                           >> Crypto.lpi
ECHO       ^<Item^>                                                            >> Crypto.lpi
ECHO         ^<Name Value="EFOpenError"/^>                                     >> Crypto.lpi
ECHO       ^</Item^>                                                           >> Crypto.lpi
ECHO     ^</Exceptions^>                                                       >> Crypto.lpi
ECHO   ^</Debugging^>                                                          >> Crypto.lpi
ECHO ^</CONFIG^>                                                               >> Crypto.lpi
CD ..
MKDIR Doc
CD Doc
COPY ..\..\cnvcl\Doc\Develop\CnRSA*.txt .
CD ..
GOTO END

:PRINTHEAD
ECHO {******************************************************************************}  > %1
ECHO {                       CnPack For Delphi/C++Builder                           } >> %1
ECHO {                     中国人自己的开放源码第三方开发包                         } >> %1
ECHO {                   (C)Copyright 2001-2024 CnPack 开发组                       } >> %1
ECHO {                   ------------------------------------                       } >> %1
ECHO {                                                                              } >> %1
ECHO {            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        } >> %1
ECHO {        改和重新发布这一程序。                                                } >> %1
ECHO {                                                                              } >> %1
ECHO {            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        } >> %1
ECHO {        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        } >> %1
ECHO {                                                                              } >> %1
ECHO {            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        } >> %1
ECHO {        还没有，可访问我们的网站：                                            } >> %1
ECHO {                                                                              } >> %1
ECHO {            网站地址：https://www.cnpack.org                                  } >> %1
ECHO {            电子邮件：master@cnpack.org                                       } >> %1
ECHO {                                                                              } >> %1
ECHO {******************************************************************************} >> %1
ECHO.                                                                                 >> %1
GOTO :EOF

:PRINTFILES
ECHO   Cn25519 in '..\Source\Cn25519.pas',                                     >> %1
ECHO   CnAEAD in '..\Source\CnAEAD.pas',                                       >> %1
ECHO   CnAES in '..\Source\CnAES.pas',                                         >> %1
ECHO   CnBase64 in '..\Source\CnBase64.pas',                                   >> %1
ECHO   CnBerUtils in '..\Source\CnBerUtils.pas',                               >> %1
ECHO   CnBigDecimal in '..\Source\CnBigDecimal.pas',                           >> %1
ECHO   CnBigNumber in '..\Source\CnBigNumber.pas',                             >> %1
ECHO   CnBigRational in '..\Source\CnBigRational.pas',                         >> %1
ECHO   CnBits in '..\Source\CnBits.pas',                                       >> %1
ECHO   CnCertificateAuthority in '..\Source\CnCertificateAuthority.pas',       >> %1
ECHO   CnChaCha20 in '..\Source\CnChaCha20.pas',                               >> %1
ECHO   CnClasses in '..\Source\CnClasses.pas',                                 >> %1
ECHO   CnComplex in '..\Source\CnComplex.pas',                                 >> %1
ECHO   CnConsts in '..\Source\CnConsts.pas',                                   >> %1
ECHO   CnContainers in '..\Source\CnContainers.pas',                           >> %1
ECHO   CnCRC32 in '..\Source\CnCRC32.pas',                                     >> %1
ECHO   CnDES in '..\Source\CnDES.pas',                                         >> %1
ECHO   CnDFT in '..\Source\CnDFT.pas',                                         >> %1
ECHO   CnECC in '..\Source\CnECC.pas',                                         >> %1
ECHO   CnFEC in '..\Source\CnFEC.pas',                                         >> %1
ECHO   CnFloat in '..\Source\CnFloat.pas',                                     >> %1
ECHO   CnFNV in '..\Source\CnFNV.pas',                                         >> %1
ECHO   CnHashMap in '..\Source\CnHashMap.pas',                                 >> %1
ECHO   CnInt128 in '..\Source\CnInt128.pas',                                   >> %1
ECHO   CnKDF in '..\Source\CnKDF.pas',                                         >> %1
ECHO   CnLattice in '..\Source\CnLattice.pas',                                 >> %1
ECHO   CnMath in '..\Source\CnMath.pas',                                       >> %1
ECHO   CnMatrix in '..\Source\CnMatrix.pas',                                   >> %1
ECHO   CnMD5 in '..\Source\CnMD5.pas',                                         >> %1
ECHO   CnNative in '..\Source\CnNative.pas',                                   >> %1
ECHO   CnOTP in '..\Source\CnOTP.pas',                                         >> %1
ECHO   CnOTS in '..\Source\CnOTS.pas',                                         >> %1
ECHO   CnPaillier in '..\Source\CnPaillier.pas',                               >> %1
ECHO   CnPDFCrypt in '..\Source\CnPDFCrypt.pas',                               >> %1
ECHO   CnPemUtils in '..\Source\CnPemUtils.pas',                               >> %1
ECHO   CnPoly1305 in '..\Source\CnPoly1305.pas',                               >> %1
ECHO   CnPolynomial in '..\Source\CnPolynomial.pas',                           >> %1
ECHO   CnPrimeNumber in '..\Source\CnPrimeNumber.pas',                         >> %1
ECHO   CnRandom in '..\Source\CnRandom.pas',                                   >> %1
ECHO   CnRC4 in '..\Source\CnRC4.pas',                                         >> %1
ECHO   CnRSA in '..\Source\CnRSA.pas',                                         >> %1
ECHO   CnSecretSharing in '..\Source\CnSecretSharing.pas',                     >> %1
ECHO   CnSHA1 in '..\Source\CnSHA1.pas',                                       >> %1
ECHO   CnSHA2 in '..\Source\CnSHA2.pas',                                       >> %1
ECHO   CnSHA3 in '..\Source\CnSHA3.pas',                                       >> %1
ECHO   CnSM2 in '..\Source\CnSM2.pas',                                         >> %1
ECHO   CnSM3 in '..\Source\CnSM3.pas',                                         >> %1
ECHO   CnSM4 in '..\Source\CnSM4.pas',                                         >> %1
ECHO   CnSM9 in '..\Source\CnSM9.pas',                                         >> %1
ECHO   CnTEA in '..\Source\CnTEA.pas',                                         >> %1
ECHO   CnTree in '..\Source\CnTree.pas',                                       >> %1
ECHO   CnVector in '..\Source\CnVector.pas',                                   >> %1
IF "%2" == "ALLFILES" (
  ECHO   CnZUC in '..\Source\CnZUC.pas';                                       >> %1
) ELSE (
  ECHO   CnZUC in '..\Source\CnZUC.pas',                                       >> %1
)
GOTO :EOF

:END
ECHO CnCrypto is Ready!
PAUSE