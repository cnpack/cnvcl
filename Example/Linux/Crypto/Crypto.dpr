{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

program Crypto;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  Cn25519 in '..\..\..\Source\Crypto\Cn25519.pas',
  CnAEAD in '..\..\..\Source\Crypto\CnAEAD.pas',
  CnAES in '..\..\..\Source\Crypto\CnAES.pas',
  CnBase64 in '..\..\..\Source\Crypto\CnBase64.pas',
  CnBerUtils in '..\..\..\Source\Crypto\CnBerUtils.pas',
  CnBigNumber in '..\..\..\Source\Crypto\CnBigNumber.pas',
  CnBits in '..\..\..\Source\Crypto\CnBits.pas',
  CnCertificateAuthority in '..\..\..\Source\Crypto\CnCertificateAuthority.pas',
  CnChaCha20 in '..\..\..\Source\Crypto\CnChaCha20.pas',
  CnComplex in '..\..\..\Source\Crypto\CnComplex.pas',
  CnCRC32 in '..\..\..\Source\Crypto\CnCRC32.pas',
  CnDES in '..\..\..\Source\Crypto\CnDES.pas',
  CnDFT in '..\..\..\Source\Crypto\CnDFT.pas',
  CnECC in '..\..\..\Source\Crypto\CnECC.pas',
  CnFEC in '..\..\..\Source\Crypto\CnFEC.pas',
  CnFNV in '..\..\..\Source\Crypto\CnFNV.pas',
  CnInt128 in '..\..\..\Source\Crypto\CnInt128.pas',
  CnKDF in '..\..\..\Source\Crypto\CnKDF.pas',
  CnLattice in '..\..\..\Source\Crypto\CnLattice.pas',
  CnMD5 in '..\..\..\Source\Crypto\CnMD5.pas',
  CnNative in '..\..\..\Source\Crypto\CnNative.pas',
  CnOTP in '..\..\..\Source\Crypto\CnOTP.pas',
  CnOTS in '..\..\..\Source\Crypto\CnOTS.pas',
  CnPaillier in '..\..\..\Source\Crypto\CnPaillier.pas',
  CnPDFCrypt in '..\..\..\Source\Crypto\CnPDFCrypt.pas',
  CnPemUtils in '..\..\..\Source\Crypto\CnPemUtils.pas',
  CnPoly1305 in '..\..\..\Source\Crypto\CnPoly1305.pas',
  CnPolynomial in '..\..\..\Source\Crypto\CnPolynomial.pas',
  CnPrimeNumber in '..\..\..\Source\Crypto\CnPrimeNumber.pas',
  CnRandom in '..\..\..\Source\Crypto\CnRandom.pas',
  CnRC4 in '..\..\..\Source\Crypto\CnRC4.pas',
  CnRSA in '..\..\..\Source\Crypto\CnRSA.pas',
  CnSecretSharing in '..\..\..\Source\Crypto\CnSecretSharing.pas',
  CnSHA1 in '..\..\..\Source\Crypto\CnSHA1.pas',
  CnSHA2 in '..\..\..\Source\Crypto\CnSHA2.pas',
  CnSHA3 in '..\..\..\Source\Crypto\CnSHA3.pas',
  CnSM2 in '..\..\..\Source\Crypto\CnSM2.pas',
  CnSM3 in '..\..\..\Source\Crypto\CnSM3.pas',
  CnSM4 in '..\..\..\Source\Crypto\CnSM4.pas',
  CnSM9 in '..\..\..\Source\Crypto\CnSM9.pas',
  CnTEA in '..\..\..\Source\Crypto\CnTEA.pas',
  CnVector in '..\..\..\Source\Crypto\CnVector.pas',
  CnZUC in '..\..\..\Source\Crypto\CnZUC.pas',
  CnBigDecimal in '..\..\..\Source\Common\CnBigDecimal.pas',
  CnBigRational in '..\..\..\Source\Common\CnBigRational.pas',
  CnConsts in '..\..\..\Source\Common\CnConsts.pas',
  CnContainers in '..\..\..\Source\Common\CnContainers.pas',
  CnFloat in '..\..\..\Source\Common\CnFloat.pas',
  CnHashMap in '..\..\..\Source\Common\CnHashMap.pas',
  CnMath in '..\..\..\Source\Common\CnMath.pas',
  CnMatrix in '..\..\..\Source\Common\CnMatrix.pas',
  CnTree in '..\..\..\Source\Common\CnTree.pas',
  CnStrings in '..\..\..\Source\Common\CnStrings.pas',
  CnWideStrings in '..\..\..\Source\Common\CnWideStrings.pas',
  CryptoTest in 'CryptoTest.pas';

begin
  try
    TestCrypto;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
