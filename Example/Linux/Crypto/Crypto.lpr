{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

program Crypto;

{$MODE Delphi}

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
  CnBLAKE in '..\..\..\Source\Crypto\CnBLAKE.pas',
  CnBLAKE2 in '..\..\..\Source\Crypto\CnBLAKE2.pas',
  CnCertificateAuthority in '..\..\..\Source\Crypto\CnCertificateAuthority.pas',
  CnChaCha20 in '..\..\..\Source\Crypto\CnChaCha20.pas',
  CnComplex in '..\..\..\Source\Crypto\CnComplex.pas',
  CnCRC32 in '..\..\..\Source\Crypto\CnCRC32.pas',
  CnDES in '..\..\..\Source\Crypto\CnDES.pas',
  CnDFT in '..\..\..\Source\Crypto\CnDFT.pas',
  CnDSA in '..\..\..\Source\Crypto\CnDSA.pas',
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
  CnPrime in '..\..\..\Source\Crypto\CnPrime.pas',
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
  CnXXH in '..\..\..\Source\Crypto\CnXXH.pas',
  CnZUC in '..\..\..\Source\Crypto\CnZUC.pas',
  CnBigDecimal in '..\..\..\Source\Common\CnBigDecimal.pas',
  CnBigRational in '..\..\..\Source\Common\CnBigRational.pas',
  CnConsts in '..\..\..\Source\Common\CnConsts.pas',
  CnContainers in '..\..\..\Source\Common\CnContainers.pas',
  CnFileUtils in '..\..\..\Source\Common\CnFileUtils.pas',
  CnFloat in '..\..\..\Source\Common\CnFloat.pas',
  CnHashMap in '..\..\..\Source\Common\CnHashMap.pas',
  CnMath in '..\..\..\Source\Common\CnMath.pas',
  CnMatrix in '..\..\..\Source\Common\CnMatrix.pas',
  CnTree in '..\..\..\Source\Common\CnTree.pas',
  CnStrings in '..\..\..\Source\Common\CnStrings.pas',
  CnWideStrings in '..\..\..\Source\Common\CnWideStrings.pas',
  CnZip in '..\..\..\Source\Common\CnZip.pas',
  CryptoTest in 'CryptoTest.pas';

begin
  try
    TestCrypto;
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
