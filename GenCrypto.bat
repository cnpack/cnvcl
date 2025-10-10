@ECHO OFF
REM ע�� Package �� Test �µĹ��̶�����Ҫ dof��cfg ����������·������Ϊ����Դ��ȫ�� Source Ŀ¼��
CD ..
DEL /S /Q cncrypto
RMDIR /Q cncrypto /S
MKDIR cncrypto
CD cncrypto
COPY ..\cnvcl\License.chs.txt .\License.txt
TYPE ..\cnvcl\License.enu.txt >> .\License.txt
COPY ..\cnvcl\Doc\Develop\CnCrypto_README.md .\README.md
COPY ..\cnvcl\CleanInplace.bat .\Clean.bat
COPY ..\cnvcl\.gitignore .
MKDIR Source
CD Source
COPY ..\..\cnvcl\Source\Common\CnPack.inc .
COPY ..\..\cnvcl\Source\Common\CnBigDecimal.pas .
COPY ..\..\cnvcl\Source\Common\CnBigRational.pas .
COPY ..\..\cnvcl\Source\Common\CnConsts.pas .
COPY ..\..\cnvcl\Source\Common\CnContainers.pas .
COPY ..\..\cnvcl\Source\Common\CnFileUtils.pas .
COPY ..\..\cnvcl\Source\Common\CnFloat.pas .
COPY ..\..\cnvcl\Source\Common\CnHashMap.pas .
COPY ..\..\cnvcl\Source\Common\CnMath.pas .
COPY ..\..\cnvcl\Source\Common\CnMatrix.pas .
COPY ..\..\cnvcl\Source\Common\CnTree.pas .
COPY ..\..\cnvcl\Source\Common\CnStrings.pas .
COPY ..\..\cnvcl\Source\Common\CnWideStrings.pas .
COPY ..\..\cnvcl\Source\Common\CnZip.pas .
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
ECHO   vcl;   // If in Delphi 5 or C++Builder 5, please change to vcl50        >> CnCrypto.dpk
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
COPY ..\..\cnvcl\Example\Linux\Crypto\Crypto.cpp .
ECHO USEUNIT("Crypto.cpp");                                                             > Crypto.bpf
ECHO USEUNIT("..\Source\Cn25519.pas");                                                 >> Crypto.bpf
ECHO USEUNIT("..\Source\CnAEAD.pas");                                                  >> Crypto.bpf
ECHO USEUNIT("..\Source\CnAES.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnBase64.pas");                                                >> Crypto.bpf
ECHO USEUNIT("..\Source\CnBerUtils.pas");                                              >> Crypto.bpf
ECHO USEUNIT("..\Source\CnBigNumber.pas");                                             >> Crypto.bpf
ECHO USEUNIT("..\Source\CnBits.pas");                                                  >> Crypto.bpf
ECHO USEUNIT("..\Source\CnBLAKE.pas");                                                 >> Crypto.bpf
ECHO USEUNIT("..\Source\CnBLAKE2.pas");                                                >> Crypto.bpf
ECHO USEUNIT("..\Source\CnCertificateAuthority.pas");                                  >> Crypto.bpf
ECHO USEUNIT("..\Source\CnChaCha20.pas");                                              >> Crypto.bpf
ECHO USEUNIT("..\Source\CnComplex.pas");                                               >> Crypto.bpf
ECHO USEUNIT("..\Source\CnCRC32.pas");                                                 >> Crypto.bpf
ECHO USEUNIT("..\Source\CnDES.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnDFT.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnDSA.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnECC.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnFEC.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnFNV.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnInt128.pas");                                                >> Crypto.bpf
ECHO USEUNIT("..\Source\CnKDF.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnLattice.pas");                                               >> Crypto.bpf
ECHO USEUNIT("..\Source\CnMD5.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnNative.pas");                                                >> Crypto.bpf
ECHO USEUNIT("..\Source\CnOTP.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnOTS.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnPaillier.pas");                                              >> Crypto.bpf
ECHO USEUNIT("..\Source\CnPDFCrypt.pas");                                              >> Crypto.bpf
ECHO USEUNIT("..\Source\CnPemUtils.pas");                                              >> Crypto.bpf
ECHO USEUNIT("..\Source\CnPoly1305.pas");                                              >> Crypto.bpf
ECHO USEUNIT("..\Source\CnPolynomial.pas");                                            >> Crypto.bpf
ECHO USEUNIT("..\Source\CnPrime.pas");                                                 >> Crypto.bpf
ECHO USEUNIT("..\Source\CnRandom.pas");                                                >> Crypto.bpf
ECHO USEUNIT("..\Source\CnRC4.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnRSA.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnSecretSharing.pas");                                         >> Crypto.bpf
ECHO USEUNIT("..\Source\CnSHA1.pas");                                                  >> Crypto.bpf
ECHO USEUNIT("..\Source\CnSHA2.pas");                                                  >> Crypto.bpf
ECHO USEUNIT("..\Source\CnSHA3.pas");                                                  >> Crypto.bpf
ECHO USEUNIT("..\Source\CnSM2.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnSM3.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnSM4.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnSM9.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnTEA.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnVector.pas");                                                >> Crypto.bpf
ECHO USEUNIT("..\Source\CnXXH.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnZUC.pas");                                                   >> Crypto.bpf
ECHO USEUNIT("..\Source\CnBigDecimal.pas");                                            >> Crypto.bpf
ECHO USEUNIT("..\Source\CnBigRational.pas");                                           >> Crypto.bpf
ECHO USEUNIT("..\Source\CnConsts.pas");                                                >> Crypto.bpf
ECHO USEUNIT("..\Source\CnContainers.pas");                                            >> Crypto.bpf
ECHO USEUNIT("..\Source\CnFloat.pas");                                                 >> Crypto.bpf
ECHO USEUNIT("..\Source\CnFileUtils.pas");                                             >> Crypto.bpf
ECHO USEUNIT("..\Source\CnHashMap.pas");                                               >> Crypto.bpf
ECHO USEUNIT("..\Source\CnMath.pas");                                                  >> Crypto.bpf
ECHO USEUNIT("..\Source\CnMatrix.pas");                                                >> Crypto.bpf
ECHO USEUNIT("..\Source\CnTree.pas");                                                  >> Crypto.bpf
ECHO USEUNIT("..\Source\CnStrings.pas");                                               >> Crypto.bpf
ECHO USEUNIT("..\Source\CnWideStrings.pas");                                           >> Crypto.bpf
ECHO USEUNIT("CryptoTest.pas");                                                               >> Crypto.bpf
ECHO //---------------------------------------------------------------------------            >> Crypto.bpf
ECHO This file is used by the project manager only and should be treated like project file    >> Crypto.bpf
ECHO.                                                                                         >> Crypto.bpf
ECHO.                                                                                         >> Crypto.bpf
ECHO main                                                                                     >> Crypto.bpf
ECHO ^<^?xml version='1.0' encoding='utf-8' ^?^>                                                    > Crypto.bpr
ECHO ^<!-- C++Builder XML Project --^>                                                             >> Crypto.bpr
ECHO ^<PROJECT^>                                                                                   >> Crypto.bpr
ECHO   ^<MACROS^>                                                                                  >> Crypto.bpr
ECHO     ^<VERSION value="BCB.05.03"/^>                                                            >> Crypto.bpr
ECHO     ^<PROJECT value="Crypto.exe"/^>                                                           >> Crypto.bpr
ECHO     ^<OBJFILES value=^"..\Source\Cn25519.obj ..\Source\CnAEAD.obj                >> Crypto.bpr
ECHO       ..\Source\CnAES.obj ..\Source\CnBase64.obj                                 >> Crypto.bpr
ECHO       ..\Source\CnBerUtils.obj ..\Source\CnBigNumber.obj                         >> Crypto.bpr
ECHO       ..\Source\CnBits.obj ..\Source\CnBLAKE.obj                                 >> Crypto.bpr
ECHO       ..\Source\CnBLAKE2.obj                                                     >> Crypto.bpr
ECHO       ..\Source\CnCertificateAuthority.obj                                       >> Crypto.bpr
ECHO       ..\Source\CnChaCha20.obj ..\Source\CnComplex.obj                           >> Crypto.bpr
ECHO       ..\Source\CnCRC32.obj ..\Source\CnDES.obj                                  >> Crypto.bpr
ECHO       ..\Source\CnDFT.obj ..\Source\CnDSA.obj                                    >> Crypto.bpr
ECHO       ..\Source\CnECC.obj ..\Source\CnFEC.obj                                    >> Crypto.bpr
ECHO       ..\Source\CnFNV.obj ..\Source\CnInt128.obj                                 >> Crypto.bpr
ECHO       ..\Source\CnKDF.obj ..\Source\CnLattice.obj                                >> Crypto.bpr
ECHO       ..\Source\CnMD5.obj ..\Source\CnNative.obj                                 >> Crypto.bpr
ECHO       ..\Source\CnOTP.obj ..\Source\CnOTS.obj                                    >> Crypto.bpr
ECHO       ..\Source\CnPaillier.obj ..\Source\CnPDFCrypt.obj                          >> Crypto.bpr
ECHO       ..\Source\CnPemUtils.obj ..\Source\CnPoly1305.obj                          >> Crypto.bpr
ECHO       ..\Source\CnPolynomial.obj ..\Source\CnPrime.obj                           >> Crypto.bpr
ECHO       ..\Source\CnRandom.obj ..\Source\CnRC4.obj                                 >> Crypto.bpr
ECHO       ..\Source\CnRSA.obj                                                        >> Crypto.bpr
ECHO       ..\Source\CnSecretSharing.obj ..\Source\CnSHA1.obj                         >> Crypto.bpr
ECHO       ..\Source\CnSHA2.obj ..\Source\CnSHA3.obj                                  >> Crypto.bpr
ECHO       ..\Source\CnSM2.obj ..\Source\CnSM3.obj                                    >> Crypto.bpr
ECHO       ..\Source\CnSM4.obj ..\Source\CnSM9.obj                                    >> Crypto.bpr
ECHO       ..\Source\CnTEA.obj ..\Source\CnVector.obj                                 >> Crypto.bpr
ECHO       ..\Source\CnXXH.obj                                                        >> Crypto.bpr
ECHO       ..\Source\CnZUC.obj ..\Source\CnBigDecimal.obj                             >> Crypto.bpr
ECHO       ..\Source\CnBigRational.obj                                                >> Crypto.bpr
ECHO       ..\Source\CnConsts.obj ..\Source\CnContainers.obj                          >> Crypto.bpr
ECHO       ..\Source\CnFileUtils.obj ..\Source\CnFloat.obj                            >> Crypto.bpr
ECHO       ..\Source\CnHashMap.obj ..\Source\CnMath.obj                               >> Crypto.bpr
ECHO       ..\Source\CnMatrix.obj ..\Source\CnTree.obj                                >> Crypto.bpr
ECHO       ..\Source\CnStrings.obj ..\Source\CnWideStrings.obj                        >> Crypto.bpr
ECHO       CryptoTest.obj Crypto.obj^"/^>                                                          >> Crypto.bpr
ECHO     ^<RESFILES value=""/^>                                                                    >> Crypto.bpr
ECHO     ^<IDLFILES value=""/^>                                                                    >> Crypto.bpr
ECHO     ^<IDLGENFILES value=""/^>                                                                 >> Crypto.bpr
ECHO     ^<DEFFILE value=""/^>                                                                     >> Crypto.bpr
ECHO     ^<RESDEPEN value="$(RESFILES)"/^>                                                         >> Crypto.bpr
ECHO     ^<LIBFILES value=""/^>                                                                    >> Crypto.bpr
ECHO     ^<LIBRARIES value="Vcl50.lib"/^>                                                          >> Crypto.bpr
ECHO     ^<SPARELIBS value="Vcl50.lib"/^>                                                          >> Crypto.bpr
ECHO     ^<PACKAGES value=^"Vcl50.bpi Vclx50.bpi bcbsmp50.bpi Vcldb50.bpi vclado50.bpi ibsmp50.bpi >> Crypto.bpr
ECHO       VCLBDE50.bpi vcldbx50.bpi Qrpt50.bpi TeeUI50.bpi TeeDB50.bpi Tee50.bpi                  >> Crypto.bpr
ECHO       Dss50.bpi TeeQR50.bpi VCLIB50.bpi Vclmid50.bpi vclie50.bpi Inetdb50.bpi                 >> Crypto.bpr
ECHO       Inet50.bpi NMFast50.bpi webmid50.bpi bcbie50.bpi dclocx50.bpi                           >> Crypto.bpr
ECHO       bcb2kaxserver50.bpi^"/^>                                                                >> Crypto.bpr
ECHO     ^<PATHCPP value=".;"/^>                                                                   >> Crypto.bpr
ECHO     ^<PATHPAS value=".;..\..\..\Source\Crypto;..\..\..\Source\Common"/^>                      >> Crypto.bpr
ECHO     ^<PATHRC value=".;"/^>                                                                    >> Crypto.bpr
ECHO     ^<PATHASM value=".;"/^>                                                                   >> Crypto.bpr
ECHO     ^<DEBUGLIBPATH value="$(BCB)\lib\debug"/^>                                                >> Crypto.bpr
ECHO     ^<RELEASELIBPATH value="$(BCB)\lib\release"/^>                                            >> Crypto.bpr
ECHO     ^<LINKER value="tlink32"/^>                                                               >> Crypto.bpr
ECHO     ^<USERDEFINES value="_DEBUG"/^>                                                           >> Crypto.bpr
ECHO     ^<SYSDEFINES value="NO_STRICT"/^>                                                         >> Crypto.bpr
ECHO     ^<MAINSOURCE value="Crypto.bpf"/^>                                                        >> Crypto.bpr
ECHO     ^<INCLUDEPATH value="..\..\..\Source\Common;..\..\..\Source\Crypto;$(BCB)\include;$(BCB)\include\vcl"/^>       >> Crypto.bpr
ECHO     ^<LIBPATH value="..\..\..\Source\Common;..\..\..\Source\Crypto;$(BCB)\lib\obj;$(BCB)\lib"/^>                   >> Crypto.bpr
ECHO     ^<WARNINGS value="-w-par"/^>                                                               >> Crypto.bpr
ECHO   ^</MACROS^>                                                                                  >> Crypto.bpr
ECHO   ^<OPTIONS^>                                                                                  >> Crypto.bpr
ECHO     ^<IDLCFLAGS value=^"-I..\..\..\Source\Common -I..\..\..\Source\Crypto -I$(BCB)\include     >> Crypto.bpr
ECHO       -I$(BCB)\include\vcl -src_suffix cpp -D_DEBUG -boa^"/^>                                  >> Crypto.bpr
ECHO     ^<CFLAG1 value=^"-Od -H=$(BCB)\lib\vcl50.csm -Hc -Vx -Ve -X- -r- -a8 -b- -k -y -v -vi- -tWC >> Crypto.bpr
ECHO       -tWM -c^"/^>                                                                              >> Crypto.bpr
ECHO     ^<PFLAGS value="-$YD -$W -$O- -v -JPHNE -M"/^>                                             >> Crypto.bpr
ECHO     ^<RFLAGS value=""/^>                                                                       >> Crypto.bpr
ECHO     ^<AFLAGS value="/mx /w2 /zd"/^>                                                            >> Crypto.bpr
ECHO     ^<LFLAGS value="-D&quot;&quot; -ap -Tpe -x -Gn -v"/^>                                      >> Crypto.bpr
ECHO   ^</OPTIONS^>                                                                                 >> Crypto.bpr
ECHO   ^<LINKER^>                                                                                   >> Crypto.bpr
ECHO     ^<ALLOBJ value="c0x32.obj sysinit.obj $(OBJFILES)"/^>                                      >> Crypto.bpr
ECHO     ^<ALLRES value="$(RESFILES)"/^>                                                            >> Crypto.bpr
ECHO     ^<ALLLIB value="$(LIBFILES) $(LIBRARIES) import32.lib cp32mt.lib"/^>                       >> Crypto.bpr
ECHO   ^</LINKER^>                                                                                  >> Crypto.bpr
ECHO   ^<IDEOPTIONS^>                                                                               >> Crypto.bpr
ECHO [Version Info]                                                                                 >> Crypto.bpr
ECHO IncludeVerInfo=0                                                                               >> Crypto.bpr
ECHO AutoIncBuild=0                                                                                 >> Crypto.bpr
ECHO MajorVer=1                                                                                     >> Crypto.bpr
ECHO MinorVer=0                                                                                     >> Crypto.bpr
ECHO Release=0                                                                                      >> Crypto.bpr
ECHO Build=0                                                                                        >> Crypto.bpr
ECHO Debug=0                                                                                        >> Crypto.bpr
ECHO PreRelease=0                                                                                   >> Crypto.bpr
ECHO Special=0                                                                                      >> Crypto.bpr
ECHO Private=0                                                                                      >> Crypto.bpr
ECHO DLL=0                                                                                          >> Crypto.bpr
ECHO Locale=2052                                                                                    >> Crypto.bpr
ECHO CodePage=936                                                                                   >> Crypto.bpr
ECHO.                                                                                               >> Crypto.bpr
ECHO [Version Info Keys]                                                                            >> Crypto.bpr
ECHO CompanyName=                                                                                   >> Crypto.bpr
ECHO FileDescription=                                                                               >> Crypto.bpr
ECHO FileVersion=1.0.0.0                                                                            >> Crypto.bpr
ECHO InternalName=                                                                                  >> Crypto.bpr
ECHO LegalCopyright=                                                                                >> Crypto.bpr
ECHO LegalTrademarks=                                                                               >> Crypto.bpr
ECHO OriginalFilename=                                                                              >> Crypto.bpr
ECHO ProductName=                                                                                   >> Crypto.bpr
ECHO ProductVersion=1.0.0.0                                                                         >> Crypto.bpr
ECHO Comments=                                                                                      >> Crypto.bpr
ECHO.                                                                                               >> Crypto.bpr
ECHO [HistoryLists\hlIncludePath]                                                                   >> Crypto.bpr
ECHO Count=1                                                                                        >> Crypto.bpr
ECHO Item0=$(BCB)\include;$(BCB)\include\vcl                                                        >> Crypto.bpr
ECHO.                                                                                               >> Crypto.bpr
ECHO [HistoryLists\hlLibraryPath]                                                                   >> Crypto.bpr
ECHO Count=1                                                                                        >> Crypto.bpr
ECHO Item0=$(BCB)\lib\obj;$(BCB)\lib                                                                >> Crypto.bpr
ECHO.                                                                                               >> Crypto.bpr
ECHO [HistoryLists\hlDebugSourcePath]                                                               >> Crypto.bpr
ECHO Count=1                                                                                        >> Crypto.bpr
ECHO Item0=$(BCB)\source\vcl                                                                        >> Crypto.bpr
ECHO.                                                                                               >> Crypto.bpr
ECHO [HistoryLists\hlConditionals]                                                                  >> Crypto.bpr
ECHO Count=1                                                                                        >> Crypto.bpr
ECHO Item0=_DEBUG                                                                                   >> Crypto.bpr
ECHO.                                                                                               >> Crypto.bpr
ECHO [Debugging]                                                                                    >> Crypto.bpr
ECHO DebugSourceDirs=$(BCB)\source\vcl                                                              >> Crypto.bpr
ECHO.                                                                                               >> Crypto.bpr
ECHO [Parameters]                                                                                   >> Crypto.bpr
ECHO RunParams=                                                                                     >> Crypto.bpr
ECHO HostApplication=                                                                               >> Crypto.bpr
ECHO RemoteHost=                                                                                    >> Crypto.bpr
ECHO RemotePath=                                                                                    >> Crypto.bpr
ECHO RemoteDebug=0                                                                                  >> Crypto.bpr
ECHO.                                                                                               >> Crypto.bpr
ECHO [Compiler]                                                                                     >> Crypto.bpr
ECHO ShowInfoMsgs=0                                                                                 >> Crypto.bpr
ECHO LinkDebugVcl=0                                                                                 >> Crypto.bpr
ECHO LinkCGLIB=0                                                                                    >> Crypto.bpr
ECHO.                                                                                               >> Crypto.bpr
ECHO [CORBA]                                                                                        >> Crypto.bpr
ECHO AddServerUnit=1                                                                                >> Crypto.bpr
ECHO AddClientUnit=1                                                                                >> Crypto.bpr
ECHO PrecompiledHeaders=1                                                                           >> Crypto.bpr
ECHO.                                                                                               >> Crypto.bpr
ECHO [Language]                                                                                     >> Crypto.bpr
ECHO ActiveLang=                                                                                    >> Crypto.bpr
ECHO ProjectLang=                                                                                   >> Crypto.bpr
ECHO RootDir=                                                                                       >> Crypto.bpr
ECHO   ^</IDEOPTIONS^>                                                                              >> Crypto.bpr
ECHO ^</PROJECT^>                                                                                   >> Crypto.bpr
CD ..
MKDIR Doc
CD Doc
COPY ..\..\cnvcl\Doc\Develop\CnRSA*.txt .
COPY ..\..\cnvcl\Doc\Develop\�����Delphi7���ֶ�֧��64λ�޷�����������.txt .
CD ..
SETLOCAL ENABLEDELAYEDEXPANSION
MKDIR Example
CD Example
MKDIR Delphi
MKDIR Lazarus
CALL :COPYEXAMPLE 25519
CALL :COPYEXAMPLE BerParse
CALL :COPYEXAMPLE BigDecimal
CALL :COPYEXAMPLE BigNumber
CALL :COPYEXAMPLE BigRational
CALL :COPYEXAMPLE CertificateAuthority
CALL :COPYEXAMPLE Complex
CALL :COPYEXAMPLE Crypto
CALL :COPYEXAMPLE DFT
CALL :COPYEXAMPLE DSA
CALL :COPYEXAMPLE ECC
CALL :COPYEXAMPLE Float
CALL :COPYEXAMPLE Int128
CALL :COPYEXAMPLE KDF
CALL :COPYEXAMPLE Native
CALL :COPYEXAMPLE OTP
CALL :COPYEXAMPLE OTS
CALL :COPYEXAMPLE Pailler
CALL :COPYEXAMPLE Polynomial
CALL :COPYEXAMPLE PrimeNumber
CALL :COPYEXAMPLE RSA
CALL :COPYEXAMPLE SecretSharing
CALL :COPYEXAMPLE SM2
CALL :COPYEXAMPLE SM9
CD Delphi
CALL :REPLACEPROJECT
CD ..
CD Lazarus
CALL :REPLACEPROJECT
CD ..
GOTO END

:PRINTHEAD
ECHO {******************************************************************************}  > %1
ECHO {                       CnPack For Delphi/C++Builder                           } >> %1
ECHO {                     �й����Լ��Ŀ���Դ�������������                         } >> %1
ECHO {                   (C)Copyright 2001-2025 CnPack ������                       } >> %1
ECHO {                   ------------------------------------                       } >> %1
ECHO {                                                                              } >> %1
ECHO {            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        } >> %1
ECHO {        �ĺ����·�����һ����                                                } >> %1
ECHO {                                                                              } >> %1
ECHO {            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        } >> %1
ECHO {        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        } >> %1
ECHO {                                                                              } >> %1
ECHO {            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        } >> %1
ECHO {        ��û�У��ɷ������ǵ���վ��                                            } >> %1
ECHO {                                                                              } >> %1
ECHO {            ��վ��ַ��https://www.cnpack.org                                  } >> %1
ECHO {            �����ʼ���master@cnpack.org                                       } >> %1
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
ECHO   CnBLAKE in '..\Source\CnBLAKE.pas',                                     >> %1
ECHO   CnBLAKE2 in '..\Source\CnBLAKE2.pas',                                   >> %1
ECHO   CnCertificateAuthority in '..\Source\CnCertificateAuthority.pas',       >> %1
ECHO   CnChaCha20 in '..\Source\CnChaCha20.pas',                               >> %1
ECHO   CnComplex in '..\Source\CnComplex.pas',                                 >> %1
ECHO   CnConsts in '..\Source\CnConsts.pas',                                   >> %1
ECHO   CnContainers in '..\Source\CnContainers.pas',                           >> %1
ECHO   CnCRC32 in '..\Source\CnCRC32.pas',                                     >> %1
ECHO   CnDES in '..\Source\CnDES.pas',                                         >> %1
ECHO   CnDFT in '..\Source\CnDFT.pas',                                         >> %1
ECHO   CnDSA in '..\Source\CnDSA.pas',                                         >> %1
ECHO   CnECC in '..\Source\CnECC.pas',                                         >> %1
ECHO   CnFEC in '..\Source\CnFEC.pas',                                         >> %1
ECHO   CnFileUtils in '..\Source\CnFileUtils.pas',                             >> %1
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
ECHO   CnPrime in '..\Source\CnPrime.pas',                                     >> %1
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
ECHO   CnStrings in '..\Source\CnStrings.pas',                                 >> %1
ECHO   CnTEA in '..\Source\CnTEA.pas',                                         >> %1
ECHO   CnTree in '..\Source\CnTree.pas',                                       >> %1
ECHO   CnVector in '..\Source\CnVector.pas',                                   >> %1
ECHO   CnWideStrings in '..\Source\CnWideStrings.pas',                         >> %1
ECHO   CnXXH in '..\Source\CnXXH.pas',                                         >> %1
ECHO   CnZip in '..\Source\CnZip.pas',                                         >> %1
IF "%2" == "ALLFILES" (
  ECHO   CnZUC in '..\Source\CnZUC.pas';                                       >> %1
) ELSE (
  ECHO   CnZUC in '..\Source\CnZUC.pas',                                       >> %1
)
GOTO :EOF

:COPYEXAMPLE
IF EXIST ..\..\cnvcl\Example\VCL\%1 (
  CD Delphi
  MKDIR %1
  CD %1
  COPY ..\..\..\..\cnvcl\Example\VCL\%1\* .
  CD ..\..
) ELSE (
  ECHO VCL\%1 NOT Exists
)
IF EXIST ..\..\cnvcl\Example\FPC\%1 (
  CD Lazarus
  MKDIR %1
  CD %1
  COPY ..\..\..\..\cnvcl\Example\FPC\%1\* .
  CD ..\..
) ELSE (
  ECHO FPC\%1 NOT Exists
)
GOTO :EOF

:REPLACEPROJECT
FOR /D %%D IN (.\*) DO (
  CD %%D
  FOR %%F IN (.\*.dpr) DO (
    CALL :REPONEPROJ %%F
  )
  FOR %%F IN (.\*.cfg) DO (
    CALL :REPONEPROJ %%F
  )
  FOR %%F IN (.\*.dof) DO (
    CALL :REPONEPROJ %%F
  )
  FOR %%F IN (.\*.lpr) DO (
    CALL :REPONEPROJ %%F
  )
  FOR %%F IN (.\*.lps) DO (
    CALL :REPONEPROJ %%F
  )
  CD ..
)
GOTO :EOF

:REPONEPROJ
  ECHO %1
  SET FIND0=..\..\..\Source\Common;..\..\..\Source\Crypto
  SET REP0=..\..\..\Source
  SET FIND1=..\..\..\Source\Crypto
  SET REP1=..\..\..\Source
  SET FIND2=..\..\..\Source\Common
  SET REP2=..\..\..\Source
  FOR /F "DELIMS=" %%A IN ('TYPE "%1"') DO (
    SET LINE=%%A
    SET LINE=!LINE:%FIND0%=%REP0%!
    SET LINE=!LINE:%FIND1%=%REP1%!
    SET LINE=!LINE:%FIND2%=%REP2%!
    ECHO !LINE!>>%1.txt
  )
  MOVE /Y %1.txt %1
GOTO :EOF

:END
ECHO CnCrypto is Ready!
PAUSE