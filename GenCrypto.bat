@ECHO OFF

CD ..
DEL /S /Q cncrypto
RMDIR cncrypto
MKDIR cncrypto
CD cncrypto
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
ECHO package CnCrypto;                                                          > CnCrypto.dpk
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
ECHO   Cn25519 in '..\Source\Cn25519.pas',                                     >> CnCrypto.dpk
ECHO   CnAEAD in '..\Source\CnAEAD.pas',                                       >> CnCrypto.dpk
ECHO   CnAES in '..\Source\CnAES.pas',                                         >> CnCrypto.dpk
ECHO   CnBase64 in '..\Source\CnBase64.pas',                                   >> CnCrypto.dpk
ECHO   CnBerUtils in '..\Source\CnBerUtils.pas',                               >> CnCrypto.dpk
ECHO   CnBigDecimal in '..\Source\CnBigDecimal.pas',                           >> CnCrypto.dpk
ECHO   CnBigNumber in '..\Source\CnBigNumber.pas',                             >> CnCrypto.dpk
ECHO   CnBigRational in '..\Source\CnBigRational.pas',                         >> CnCrypto.dpk
ECHO   CnBits in '..\Source\CnBits.pas',                                       >> CnCrypto.dpk
ECHO   CnCertificateAuthority in '..\Source\CnCertificateAuthority.pas',       >> CnCrypto.dpk
ECHO   CnChaCha20 in '..\Source\CnChaCha20.pas',                               >> CnCrypto.dpk
ECHO   CnClasses in '..\Source\CnClasses.pas',                                 >> CnCrypto.dpk
ECHO   CnComplex in '..\Source\CnComplex.pas',                                 >> CnCrypto.dpk
ECHO   CnConsts in '..\Source\CnConsts.pas',                                   >> CnCrypto.dpk
ECHO   CnContainers in '..\Source\CnContainers.pas',                           >> CnCrypto.dpk
ECHO   CnCRC32 in '..\Source\CnCRC32.pas',                                     >> CnCrypto.dpk
ECHO   CnDES in '..\Source\CnDES.pas',                                         >> CnCrypto.dpk
ECHO   CnDFT in '..\Source\CnDFT.pas',                                         >> CnCrypto.dpk
ECHO   CnECC in '..\Source\CnECC.pas',                                         >> CnCrypto.dpk
ECHO   CnFEC in '..\Source\CnFEC.pas',                                         >> CnCrypto.dpk
ECHO   CnFloat in '..\Source\CnFloat.pas',                                     >> CnCrypto.dpk
ECHO   CnFNV in '..\Source\CnFNV.pas',                                         >> CnCrypto.dpk
ECHO   CnHashMap in '..\Source\CnHashMap.pas',                                 >> CnCrypto.dpk
ECHO   CnInt128 in '..\Source\CnInt128.pas',                                   >> CnCrypto.dpk
ECHO   CnKDF in '..\Source\CnKDF.pas',                                         >> CnCrypto.dpk
ECHO   CnLattice in '..\Source\CnLattice.pas',                                 >> CnCrypto.dpk
ECHO   CnMath in '..\Source\CnMath.pas',                                       >> CnCrypto.dpk
ECHO   CnMatrix in '..\Source\CnMatrix.pas',                                   >> CnCrypto.dpk
ECHO   CnMD5 in '..\Source\CnMD5.pas',                                         >> CnCrypto.dpk
ECHO   CnNative in '..\Source\CnNative.pas',                                   >> CnCrypto.dpk
ECHO   CnOTP in '..\Source\CnOTP.pas',                                         >> CnCrypto.dpk
ECHO   CnOTS in '..\Source\CnOTS.pas',                                         >> CnCrypto.dpk
ECHO   CnPaillier in '..\Source\CnPaillier.pas',                               >> CnCrypto.dpk
ECHO   CnPDFCrypt in '..\Source\CnPDFCrypt.pas',                               >> CnCrypto.dpk
ECHO   CnPemUtils in '..\Source\CnPemUtils.pas',                               >> CnCrypto.dpk
ECHO   CnPoly1305 in '..\Source\CnPoly1305.pas',                               >> CnCrypto.dpk
ECHO   CnPolynomial in '..\Source\CnPolynomial.pas',                           >> CnCrypto.dpk
ECHO   CnPrimeNumber in '..\Source\CnPrimeNumber.pas',                         >> CnCrypto.dpk
ECHO   CnRandom in '..\Source\CnRandom.pas',                                   >> CnCrypto.dpk
ECHO   CnRC4 in '..\Source\CnRC4.pas',                                         >> CnCrypto.dpk
ECHO   CnRSA in '..\Source\CnRSA.pas',                                         >> CnCrypto.dpk
ECHO   CnSecretSharing in '..\Source\CnSecretSharing.pas',                     >> CnCrypto.dpk
ECHO   CnSHA1 in '..\Source\CnSHA1.pas',                                       >> CnCrypto.dpk
ECHO   CnSHA2 in '..\Source\CnSHA2.pas',                                       >> CnCrypto.dpk
ECHO   CnSHA3 in '..\Source\CnSHA3.pas',                                       >> CnCrypto.dpk
ECHO   CnSM2 in '..\Source\CnSM2.pas',                                         >> CnCrypto.dpk
ECHO   CnSM3 in '..\Source\CnSM3.pas',                                         >> CnCrypto.dpk
ECHO   CnSM4 in '..\Source\CnSM4.pas',                                         >> CnCrypto.dpk
ECHO   CnSM9 in '..\Source\CnSM9.pas',                                         >> CnCrypto.dpk
ECHO   CnTEA in '..\Source\CnTEA.pas',                                         >> CnCrypto.dpk
ECHO   CnTree in '..\Source\CnTree.pas',                                       >> CnCrypto.dpk
ECHO   CnVector in '..\Source\CnVector.pas',                                   >> CnCrypto.dpk
ECHO   CnZUC in '..\Source\CnZUC.pas';                                         >> CnCrypto.dpk
ECHO.                                                                          >> CnCrypto.dpk
ECHO end.                                                                      >> CnCrypto.dpk
ECHO.                                                                          >> CnCrypto.dpk