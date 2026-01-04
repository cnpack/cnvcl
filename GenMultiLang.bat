@ECHO OFF
REM 注意 Package 和 Test 下的工程都不需要 dof、cfg 等设置搜索路径，因为所需源码全在 Source 目录里
CD ..
DEL /S /Q cnmultilang
RMDIR /Q cnmultilang /S
MKDIR cnmultilang
CD cnmultilang
COPY ..\cnvcl\License.chs.txt .\License.txt
COPY ..\cnvcl\Doc\Develop\CnMultiLang_README.md .\README.md
COPY ..\cnvcl\CleanInplace.bat .\Clean.bat
COPY ..\cnvcl\.gitignore .
MKDIR Source
CD Source
COPY ..\..\cnvcl\Source\Crypto\CnNative.pas .
COPY ..\..\cnvcl\Source\Common\*.inc .
COPY ..\..\cnvcl\Source\Common\CnClasses.pas .
COPY ..\..\cnvcl\Source\Common\CnCommon.pas .
COPY ..\..\cnvcl\Source\Common\CnCompAboutFrm.pas .
COPY ..\..\cnvcl\Source\Common\CnCompAboutFrm.dfm .
COPY ..\..\cnvcl\Source\Common\CnConsts.pas .
COPY ..\..\cnvcl\Source\Common\CnFloat.pas .
COPY ..\..\cnvcl\Source\Common\CnGB18030.pas .
COPY ..\..\cnvcl\Source\Common\CnHashMap.pas .
COPY ..\..\cnvcl\Source\Common\CnIni.pas .
COPY ..\..\cnvcl\Source\Common\CnIniStrUtils.pas .
COPY ..\..\cnvcl\Source\Common\CnOTAUtils.pas .
COPY ..\..\cnvcl\Source\Common\CnStream.pas .
COPY ..\..\cnvcl\Source\Common\CnStrings.pas .
COPY ..\..\cnvcl\Source\Common\CnWideStrings.pas .
COPY ..\..\cnvcl\Source\Common\CnPack.dcr .
COPY ..\..\cnvcl\Source\Graphic\CnHexEditor.pas .
COPY ..\..\cnvcl\Source\MultiLang\*.pas .
COPY ..\..\cnvcl\Source\MultiLang\*.dfm .
COPY ..\..\cnvcl\Source\MultiLang\*.inc .
CD ..
MKDIR Package
CD Package
CALL :PRINTHEAD CnMultiLang.dpk
ECHO package CnMultiLang;                                                      >> CnMultiLang.dpk
ECHO.                                                                          >> CnMultiLang.dpk
ECHO {$R *.res}                                                                >> CnMultiLang.dpk
ECHO {$IFDEF IMPLICITBUILDING This IFDEF should not be used by users}          >> CnMultiLang.dpk
ECHO {$ALIGN 8}                                                                >> CnMultiLang.dpk
ECHO {$ASSERTIONS ON}                                                          >> CnMultiLang.dpk
ECHO {$BOOLEVAL OFF}                                                           >> CnMultiLang.dpk
ECHO {$DEBUGINFO OFF}                                                          >> CnMultiLang.dpk
ECHO {$EXTENDEDSYNTAX ON}                                                      >> CnMultiLang.dpk
ECHO {$IMPORTEDDATA ON}                                                        >> CnMultiLang.dpk
ECHO {$IOCHECKS ON}                                                            >> CnMultiLang.dpk
ECHO {$LOCALSYMBOLS ON}                                                        >> CnMultiLang.dpk
ECHO {$LONGSTRINGS ON}                                                         >> CnMultiLang.dpk
ECHO {$OPENSTRINGS ON}                                                         >> CnMultiLang.dpk
ECHO {$OPTIMIZATION OFF}                                                       >> CnMultiLang.dpk
ECHO {$OVERFLOWCHECKS OFF}                                                     >> CnMultiLang.dpk
ECHO {$RANGECHECKS OFF}                                                        >> CnMultiLang.dpk
ECHO {$REFERENCEINFO ON}                                                       >> CnMultiLang.dpk
ECHO {$SAFEDIVIDE OFF}                                                         >> CnMultiLang.dpk
ECHO {$STACKFRAMES ON}                                                         >> CnMultiLang.dpk
ECHO {$TYPEDADDRESS OFF}                                                       >> CnMultiLang.dpk
ECHO {$VARSTRINGCHECKS ON}                                                     >> CnMultiLang.dpk
ECHO {$WRITEABLECONST OFF}                                                     >> CnMultiLang.dpk
ECHO {$MINENUMSIZE 1}                                                          >> CnMultiLang.dpk
ECHO {$IMAGEBASE $400000}                                                      >> CnMultiLang.dpk
ECHO {$ENDIF IMPLICITBUILDING}                                                 >> CnMultiLang.dpk
ECHO {$DESCRIPTION 'CnPack Multi-Language Package'}                            >> CnMultiLang.dpk
ECHO {$RUNONLY}                                                                >> CnMultiLang.dpk
ECHO {$IMPLICITBUILD OFF}                                                      >> CnMultiLang.dpk
ECHO.                                                                          >> CnMultiLang.dpk
ECHO requires                                                                  >> CnMultiLang.dpk
ECHO   vcl,    // If in Delphi 5 or C++Builder 5, please change to vcl50       >> CnMultiLang.dpk
ECHO   vclx;   // If in Delphi 5 or C++Builder 5, please change to vclx50      >> CnMultiLang.dpk
ECHO.                                                                          >> CnMultiLang.dpk
ECHO contains                                                                  >> CnMultiLang.dpk
CALL :PRINTFILES CnMultiLang.dpk
ECHO.                                                                          >> CnMultiLang.dpk
ECHO end.                                                                      >> CnMultiLang.dpk
ECHO.                                                                          >> CnMultiLang.dpk
CALL :PRINTHEAD dclCnMultiLang.dpk
ECHO package dclCnMultiLang;                                                   >> dclCnMultiLang.dpk
ECHO.                                                                          >> dclCnMultiLang.dpk
ECHO {$R *.res}                                                                >> dclCnMultiLang.dpk
ECHO {$R '..\Source\CnPack.dcr'}                                               >> dclCnMultiLang.dpk
ECHO {$ALIGN 8}                                                                >> dclCnMultiLang.dpk
ECHO {$ASSERTIONS ON}                                                          >> dclCnMultiLang.dpk
ECHO {$BOOLEVAL OFF}                                                           >> dclCnMultiLang.dpk
ECHO {$DEBUGINFO ON}                                                           >> dclCnMultiLang.dpk
ECHO {$EXTENDEDSYNTAX ON}                                                      >> dclCnMultiLang.dpk
ECHO {$IMPORTEDDATA ON}                                                        >> dclCnMultiLang.dpk
ECHO {$IOCHECKS ON}                                                            >> dclCnMultiLang.dpk
ECHO {$LOCALSYMBOLS ON}                                                        >> dclCnMultiLang.dpk
ECHO {$LONGSTRINGS ON}                                                         >> dclCnMultiLang.dpk
ECHO {$OPENSTRINGS ON}                                                         >> dclCnMultiLang.dpk
ECHO {$OPTIMIZATION ON}                                                        >> dclCnMultiLang.dpk
ECHO {$OVERFLOWCHECKS OFF}                                                     >> dclCnMultiLang.dpk
ECHO {$RANGECHECKS OFF}                                                        >> dclCnMultiLang.dpk
ECHO {$REFERENCEINFO ON}                                                       >> dclCnMultiLang.dpk
ECHO {$SAFEDIVIDE OFF}                                                         >> dclCnMultiLang.dpk
ECHO {$STACKFRAMES OFF}                                                        >> dclCnMultiLang.dpk
ECHO {$TYPEDADDRESS OFF}                                                       >> dclCnMultiLang.dpk
ECHO {$VARSTRINGCHECKS ON}                                                     >> dclCnMultiLang.dpk
ECHO {$WRITEABLECONST ON}                                                      >> dclCnMultiLang.dpk
ECHO {$MINENUMSIZE 1}                                                          >> dclCnMultiLang.dpk
ECHO {$IMAGEBASE $400000}                                                      >> dclCnMultiLang.dpk
ECHO {$DESCRIPTION 'CnPack Multi-Language Designtime Package'}                 >> dclCnMultiLang.dpk
ECHO {$DESIGNONLY}                                                             >> dclCnMultiLang.dpk
ECHO {$IMPLICITBUILD OFF}                                                      >> dclCnMultiLang.dpk
ECHO.                                                                          >> dclCnMultiLang.dpk
ECHO requires                                                                  >> dclCnMultiLang.dpk
ECHO   vcl,   // If in Delphi 5 or C++Builder 5, please change to vcl50        >> dclCnMultiLang.dpk
ECHO   designide,                                                              >> dclCnMultiLang.dpk
ECHO   CnMultiLang;                                                            >> dclCnMultiLang.dpk
ECHO.                                                                          >> dclCnMultiLang.dpk
ECHO contains                                                                  >> dclCnMultiLang.dpk
CALL :PRINTDCLFILES dclCnMultiLang.dpk
ECHO.                                                                          >> dclCnMultiLang.dpk
ECHO end.                                                                      >> dclCnMultiLang.dpk
ECHO.                                                                          >> dclCnMultiLang.dpk
ECHO ^<^?xml version="1.0" encoding="UTF-8"^?^>                                 > CnMultiLang.lpk
ECHO ^<CONFIG^>                                                                >> CnMultiLang.lpk
ECHO   ^<Package Version="5"^>                                                 >> CnMultiLang.lpk
ECHO     ^<PathDelim Value="\"/^>                                              >> CnMultiLang.lpk
ECHO     ^<Name Value="CnMultiLang"/^>                                         >> CnMultiLang.lpk
ECHO     ^<CompilerOptions^>                                                   >> CnMultiLang.lpk
ECHO       ^<Version Value="11"/^>                                             >> CnMultiLang.lpk
ECHO       ^<PathDelim Value="\"/^>                                            >> CnMultiLang.lpk
ECHO       ^<SearchPaths^>                                                     >> CnMultiLang.lpk
ECHO         ^<IncludeFiles Value="..\Source"/^>                               >> CnMultiLang.lpk
ECHO         ^<OtherUnitFiles Value="..\Source"/^>                             >> CnMultiLang.lpk
ECHO         ^<UnitOutputDirectory Value="lib\$(TargetCPU)-$(TargetOS)"/^>     >> CnMultiLang.lpk
ECHO       ^</SearchPaths^>                                                    >> CnMultiLang.lpk
ECHO     ^</CompilerOptions^>                                                  >> CnMultiLang.lpk
ECHO     ^<Version Major="1"/^>                                                >> CnMultiLang.lpk
ECHO     ^<Files^>                                                             >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnClasses.pas"/^>                     >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnClasses"/^>                                   >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnCommon.pas"/^>                      >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnCommon"/^>                                    >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnConsts.pas"/^>                      >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnConsts"/^>                                    >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnFloat.pas"/^>                       >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnFloat"/^>                                     >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnGB18030.pas"/^>                     >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnGB18030"/^>                                   >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnHashIniFile.pas"/^>                 >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnHashIniFile"/^>                               >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnHashLangStorage.pas"/^>             >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnHashLangStorage"/^>                           >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnHashMap.pas"/^>                     >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnHashMap"/^>                                   >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnHexEditor.pas"/^>                   >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnHexEditor"/^>                                 >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnIni.pas"/^>                         >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnIni"/^>                                       >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnIniLangFileStorage.pas"/^>          >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnIniLangFileStorage"/^>                        >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnIniStrUtils.pas"/^>                 >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnIniStrUtils"/^>                               >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnLangCollection.pas"/^>              >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnLangCollection"/^>                            >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnLangConsts.pas"/^>                  >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnLangConsts"/^>                                >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnLangMgr.pas"/^>                     >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnLangMgr"/^>                                   >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnLangStorage.pas"/^>                 >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnLangStorage"/^>                               >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnLangTranslator.pas"/^>              >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnLangTranslator"/^>                            >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnLangUtils.pas"/^>                   >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnLangUtils"/^>                                 >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnNative.pas"/^>                      >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnNative"/^>                                    >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnStream.pas"/^>                      >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnStream"/^>                                    >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnStrings.pas"/^>                     >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnStrings"/^>                                   >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnWideStrings.pas"/^>                 >> CnMultiLang.lpk
ECHO         ^<UnitName Value="CnWideStrings"/^>                               >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO     ^</Files^>                                                            >> CnMultiLang.lpk
ECHO     ^<RequiredPkgs^>                                                      >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<PackageName Value="LCLBase"/^>                                  >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO       ^<Item^>                                                            >> CnMultiLang.lpk
ECHO         ^<PackageName Value="FCL"/^>                                      >> CnMultiLang.lpk
ECHO       ^</Item^>                                                           >> CnMultiLang.lpk
ECHO     ^</RequiredPkgs^>                                                     >> CnMultiLang.lpk
ECHO     ^<UsageOptions^>                                                      >> CnMultiLang.lpk
ECHO       ^<UnitPath Value="$(PkgOutDir)"/^>                                  >> CnMultiLang.lpk
ECHO     ^</UsageOptions^>                                                     >> CnMultiLang.lpk
ECHO     ^<PublishOptions^>                                                    >> CnMultiLang.lpk
ECHO       ^<Version Value="2"/^>                                              >> CnMultiLang.lpk
ECHO       ^<UseFileFilters Value="True"/^>                                    >> CnMultiLang.lpk
ECHO     ^</PublishOptions^>                                                   >> CnMultiLang.lpk
ECHO   ^</Package^>                                                            >> CnMultiLang.lpk
ECHO ^</CONFIG^>                                                               >> CnMultiLang.lpk
CALL :PRINTHEAD CnMultiLang.pas
ECHO unit CnMultiLang;                                                               >> CnMultiLang.pas
ECHO .                                                                               >> CnMultiLang.pas
ECHO {$warn 5023 off : no warning about unused units}                                >> CnMultiLang.pas
ECHO interface                                                                       >> CnMultiLang.pas
ECHO .                                                                               >> CnMultiLang.pas
ECHO uses                                                                            >> CnMultiLang.pas
ECHO   CnClasses, CnCommon, CnConsts, CnFloat, CnGB18030, CnHashIniFile,             >> CnMultiLang.pas
ECHO   CnHashLangStorage, CnHashMap, CnHexEditor, CnIni, CnIniLangFileStorage,       >> CnMultiLang.pas
ECHO   CnIniStrUtils, CnLangCollection, CnLangConsts, CnLangMgr, CnLangStorage,      >> CnMultiLang.pas
ECHO   CnLangTranslator, CnLangUtils, CnNative, CnStream, CnStrings, CnWideStrings;  >> CnMultiLang.pas
ECHO .                                                                               >> CnMultiLang.pas
ECHO implementation                                                                  >> CnMultiLang.pas
ECHO .                                                                               >> CnMultiLang.pas
ECHO end.                                                                            >> CnMultiLang.pas
ECHO .                                                                               >> CnMultiLang.pas
ECHO ^<^?xml version="1.0" encoding="UTF-8"^?^>                                       > dclCnMultiLang.lpk
ECHO ^<CONFIG^>                                                                      >> dclCnMultiLang.lpk
ECHO   ^<Package Version="5"^>                                                       >> dclCnMultiLang.lpk
ECHO     ^<PathDelim Value="\"/^>                                                    >> dclCnMultiLang.lpk
ECHO     ^<Name Value="dclCnMultiLang"/^>                                            >> dclCnMultiLang.lpk
ECHO     ^<Type Value="RunAndDesignTime"/^>                                          >> dclCnMultiLang.lpk
ECHO     ^<CompilerOptions^>                                                         >> dclCnMultiLang.lpk
ECHO       ^<Version Value="11"/^>                                                   >> dclCnMultiLang.lpk
ECHO       ^<PathDelim Value="\"/^>                                                  >> dclCnMultiLang.lpk
ECHO       ^<SearchPaths^>                                                           >> dclCnMultiLang.lpk
ECHO         ^<IncludeFiles Value="..\Source"/^>                                     >> dclCnMultiLang.lpk
ECHO         ^<OtherUnitFiles Value="..\Source"/^>                                   >> dclCnMultiLang.lpk
ECHO         ^<UnitOutputDirectory Value="lib\$(TargetCPU)-$(TargetOS)\"/^>          >> dclCnMultiLang.lpk
ECHO       ^</SearchPaths^>                                                          >> dclCnMultiLang.lpk
ECHO       ^<Other^>                                                                 >> dclCnMultiLang.lpk
ECHO         ^<CustomOptions Value="-dCNMULTILANG"/^>                                >> dclCnMultiLang.lpk
ECHO         ^<OtherDefines Count="1"^>                                              >> dclCnMultiLang.lpk
ECHO           ^<Define0 Value="CNMULTILANG"/^>                                      >> dclCnMultiLang.lpk
ECHO         ^</OtherDefines^>                                                       >> dclCnMultiLang.lpk
ECHO       ^</Other^>                                                                >> dclCnMultiLang.lpk
ECHO     ^</CompilerOptions^>                                                        >> dclCnMultiLang.lpk
ECHO     ^<Version Major="1"/^>                                                      >> dclCnMultiLang.lpk
ECHO     ^<Files^>                                                                   >> dclCnMultiLang.lpk
ECHO       ^<Item^>                                                                  >> dclCnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnCompAboutFrm.pas"/^>                      >> dclCnMultiLang.lpk
ECHO         ^<UnitName Value="CnCompAboutFrm"/^>                                    >> dclCnMultiLang.lpk
ECHO       ^</Item^>                                                                 >> dclCnMultiLang.lpk
ECHO       ^<Item^>                                                                  >> dclCnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnLangEditors.pas"/^>                       >> dclCnMultiLang.lpk
ECHO         ^<UnitName Value="CnLangEditors"/^>                                     >> dclCnMultiLang.lpk
ECHO       ^</Item^>                                                                 >> dclCnMultiLang.lpk
ECHO       ^<Item^>                                                                  >> dclCnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnLangReg.pas"/^>                           >> dclCnMultiLang.lpk
ECHO         ^<HasRegisterProc Value="True"/^>                                       >> dclCnMultiLang.lpk
ECHO         ^<UnitName Value="CnLangReg"/^>                                         >> dclCnMultiLang.lpk
ECHO       ^</Item^>                                                                 >> dclCnMultiLang.lpk
ECHO       ^<Item^>                                                                  >> dclCnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnOTAUtils.pas"/^>                          >> dclCnMultiLang.lpk
ECHO         ^<UnitName Value="CnOTAUtils"/^>                                        >> dclCnMultiLang.lpk
ECHO       ^</Item^>                                                                 >> dclCnMultiLang.lpk
ECHO       ^<Item^>                                                                  >> dclCnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnTransEditor.pas"/^>                       >> dclCnMultiLang.lpk
ECHO         ^<UnitName Value="CnTransEditor"/^>                                     >> dclCnMultiLang.lpk
ECHO       ^</Item^>                                                                 >> dclCnMultiLang.lpk
ECHO       ^<Item^>                                                                  >> dclCnMultiLang.lpk
ECHO         ^<Filename Value="..\Source\CnTransFilter.pas"/^>                       >> dclCnMultiLang.lpk
ECHO         ^<UnitName Value="CnTransFilter"/^>                                     >> dclCnMultiLang.lpk
ECHO       ^</Item^>                                                                 >> dclCnMultiLang.lpk
ECHO     ^</Files^>                                                                  >> dclCnMultiLang.lpk
ECHO     ^<RequiredPkgs^>                                                            >> dclCnMultiLang.lpk
ECHO       ^<Item^>                                                                  >> dclCnMultiLang.lpk
ECHO         ^<PackageName Value="IDEIntf"/^>                                        >> dclCnMultiLang.lpk
ECHO       ^</Item^>                                                                 >> dclCnMultiLang.lpk
ECHO       ^<Item^>                                                                  >> dclCnMultiLang.lpk
ECHO         ^<PackageName Value="CnMultiLang"/^>                                    >> dclCnMultiLang.lpk
ECHO       ^</Item^>                                                                 >> dclCnMultiLang.lpk
ECHO       ^<Item^>                                                                  >> dclCnMultiLang.lpk
ECHO         ^<PackageName Value="FCL"/^>                                            >> dclCnMultiLang.lpk
ECHO       ^</Item^>                                                                 >> dclCnMultiLang.lpk
ECHO     ^</RequiredPkgs^>                                                           >> dclCnMultiLang.lpk
ECHO     ^<UsageOptions^>                                                            >> dclCnMultiLang.lpk
ECHO       ^<UnitPath Value="$(PkgOutDir)"/^>                                        >> dclCnMultiLang.lpk
ECHO     ^</UsageOptions^>                                                           >> dclCnMultiLang.lpk
ECHO     ^<PublishOptions^>                                                          >> dclCnMultiLang.lpk
ECHO       ^<Version Value="2"/^>                                                    >> dclCnMultiLang.lpk
ECHO       ^<UseFileFilters Value="True"/^>                                          >> dclCnMultiLang.lpk
ECHO     ^</PublishOptions^>                                                         >> dclCnMultiLang.lpk
ECHO   ^</Package^>                                                                  >> dclCnMultiLang.lpk
ECHO ^</CONFIG^>                                                                     >> dclCnMultiLang.lpk
CALL :PRINTHEAD dclCnMultiLang.pas
ECHO unit dclCnMultiLang;                                                            >> dclCnMultiLang.pas
ECHO .                                                                               >> dclCnMultiLang.pas
ECHO {$warn 5023 off : no warning about unused units}                                >> dclCnMultiLang.pas
ECHO interface                                                                       >> dclCnMultiLang.pas
ECHO .                                                                               >> dclCnMultiLang.pas
ECHO uses                                                                            >> dclCnMultiLang.pas
ECHO   CnCompAboutFrm, CnLangEditors, CnLangReg, CnOTAUtils, CnTransEditor,          >> dclCnMultiLang.pas
ECHO   CnTransFilter, LazarusPackageIntf;                                            >> dclCnMultiLang.pas
ECHO .                                                                               >> dclCnMultiLang.pas
ECHO implementation                                                                  >> dclCnMultiLang.pas
ECHO .                                                                               >> dclCnMultiLang.pas
ECHO procedure Register;                                                             >> dclCnMultiLang.pas
ECHO begin                                                                           >> dclCnMultiLang.pas
ECHO   RegisterUnit('CnLangReg', @CnLangReg.Register);                               >> dclCnMultiLang.pas
ECHO end;                                                                            >> dclCnMultiLang.pas
ECHO .                                                                               >> dclCnMultiLang.pas
ECHO initialization                                                                  >> dclCnMultiLang.pas
ECHO   RegisterPackage('dclCnMultiLang', @Register);                                 >> dclCnMultiLang.pas
ECHO .                                                                               >> dclCnMultiLang.pas
ECHO end.                                                                            >> dclCnMultiLang.pas
ECHO .                                                                               >> dclCnMultiLang.pas
CD ..
MKDIR Doc
CD Doc
COPY ..\..\cnvcl\Doc\Develop\CnPack多语组件帮助文档.doc .
COPY ..\..\cnvcl\Doc\Design\MultiLang\CnPack多语组件包概要设计说明书.doc .
CD ..
MKDIR Example
CD Example
MKDIR VCL\MultiLang\
CD VCL\MultiLang\
COPY ..\..\..\..\cnvcl\Example\VCL\Multilang\ .
CD ..\..\
MKDIR FMX\MultiLang\
CD FMX\MultiLang\
COPY ..\..\..\..\cnvcl\Example\FMX\Multilang\ .
CD ..\..\
MKDIR FPC\MultiLang\
CD FPC\MultiLang\
COPY ..\..\..\..\cnvcl\Example\FPC\Multilang\ .
CD ..\..\..
GOTO END

:PRINTHEAD
ECHO {******************************************************************************}  > %1
ECHO {                       CnPack For Delphi/C++Builder                           } >> %1
ECHO {                     中国人自己的开放源码第三方开发包                         } >> %1
ECHO {                   (C)Copyright 2001-2026 CnPack 开发组                       } >> %1
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
ECHO   CnFloat in '..\Source\CnFloat.pas',                                            >> %1
ECHO   CnNative in '..\Source\CnNative.pas',                                          >> %1
ECHO   CnConsts in '..\Source\CnConsts.pas',                                          >> %1
ECHO   CnClasses in '..\Source\CnClasses.pas',                                        >> %1
ECHO   CnHashMap in '..\Source\CnHashMap.pas',                                        >> %1
ECHO   CnCommon in '..\Source\CnCommon.pas',                                          >> %1
ECHO   CnIni in '..\Source\CnIni.pas',                                                >> %1
ECHO   CnIniStrUtils in '..\Source\CnIniStrUtils.pas',                                >> %1
ECHO   CnWideStrings in '..\Source\CnWideStrings.pas',                                >> %1
ECHO   CnStream in '..\Source\CnStream.pas',                                          >> %1
ECHO   CnStrings in '..\Source\CnStrings.pas',                                        >> %1
ECHO   CnGB18030 in '..\Source\CnGB18030.pas',                                        >> %1
ECHO   CnHexEditor in '..\Source\CnHexEditor.pas',                                    >> %1
ECHO   CnHashIniFile in '..\Source\CnHashIniFile.pas',                                >> %1
ECHO   CnHashLangStorage in '..\Source\CnHashLangStorage.pas',                        >> %1
ECHO   CnIniLangFileStorage in '..\Source\CnIniLangFileStorage.pas',                  >> %1
ECHO   CnLangCollection in '..\Source\CnLangCollection.pas',                          >> %1
ECHO   CnLangConsts in '..\Source\CnLangConsts.pas',                                  >> %1
ECHO   CnLangMgr in '..\Source\CnLangMgr.pas',                                        >> %1
ECHO   CnLangStorage in '..\Source\CnLangStorage.pas',                                >> %1
ECHO   CnLangTranslator in '..\Source\CnLangTranslator.pas',                          >> %1
ECHO   CnLangUtils in '..\Source\CnLangUtils.pas';                                    >> %1
GOTO :EOF

:PRINTDCLFILES
ECHO   CnOTAUtils in '..\Source\CnOTAUtils.pas',                                      >> %1
ECHO   CnCompAboutFrm in '..\Source\CnCompAboutFrm.pas' {CnCompAboutForm},            >> %1
ECHO   CnLangEditors in '..\Source\CnLangEditors.pas',                                >> %1
ECHO   CnLangReg in '..\Source\CnLangReg.pas',                                        >> %1
ECHO   CnTransEditor in '..\Source\CnTransEditor.pas' {FrmTransEditor},               >> %1
ECHO   CnTransFilter in '..\Source\CnTransFilter.pas' {FrmTransFilter};               >> %1
GOTO :EOF

:END
ECHO CnMultiLang is Ready!
PAUSE