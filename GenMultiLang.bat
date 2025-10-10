@ECHO OFF
REM ע�� Package �� Test �µĹ��̶�����Ҫ dof��cfg ����������·������Ϊ����Դ��ȫ�� Source Ŀ¼��
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
CD ..
MKDIR Doc
CD Doc
COPY ..\..\cnvcl\Doc\Develop\CnPack������������ĵ�.doc .
COPY ..\..\cnvcl\Doc\Design\MultiLang\CnPack�����������Ҫ���˵����.doc .
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
CD ..\..\..
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