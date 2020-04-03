{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnSM2;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：SM2 椭圆曲线算法单元
* 单元作者：刘啸
* 备    注：要实现基于 SM2 的数据加解密、签名验签、密钥交换
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2020.04.02 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnECC, CnBigNumber, CnSM3, CnKDF;

type
  TCnSM2 = class(TCnEcc)
  public
    constructor Create; override;
  end;

function CnSM2EncryptData(PlainData: Pointer; DataLen: Integer; OutStream:
  TStream; PublicKey: TCnEccPublicKey; Sm2: TCnSm2 = nil): Boolean;
{* 用公钥对数据块进行加密，参考 GM/T0003.4-2012《SM2椭圆曲线公钥密码算法
   第4部分:公钥加密算法》中的运算规则，不同于普通 ECC 与 RSA 的对齐规则}

function CnSM2DecryptData(EnData: Pointer; DataLen: Integer; OutStream: TStream;
  PrivateKey: TCnEccPrivateKey; Sm2: TCnSm2 = nil): Boolean;
{* 用公钥对数据块进行解密，参考 GM/T0003.4-2012《SM2椭圆曲线公钥密码算法
   第4部分:公钥加密算法》中的运算规则，不同于普通 ECC 与 RSA 的对齐规则}

implementation

{ TCnSM2 }

constructor TCnSM2.Create;
begin
  inherited;
  Load(ctSM2);
end;

{
  传入明文 M，长 MLen 字节，随机生成 k，计算

  Cl = k * G = (xl, yl)         // 非压缩存储，长度为两个数字位长加 1

  k * PublicKey = (x2, y2)
  t = KDF(x2‖y2, Mlen)
  C2 = M xor t                  // 长度 MLen

  C3 = SM3(x2‖M‖y2)           // 长度 32 字节

  密文为：C1‖C3‖C2
}
function CnSM2EncryptData(PlainData: Pointer; DataLen: Integer; OutStream:
  TStream; PublicKey: TCnEccPublicKey; Sm2: TCnSm2 = nil): Boolean;
var
  Py, P1, P2: TCnEccPoint;
  K: TCnBigNumber;
  B: Byte;
  M: PAnsiChar;
  I: Integer;
  Buf: array of Byte;
  KDFStr, T, C3H: AnsiString;
  Sm3Dig: TSM3Digest;
  Sm2IsNil: Boolean;
begin
  Result := False;
  if (PlainData = nil) or (DataLen <= 0) or (OutStream = nil) or (PublicKey = nil) then
    Exit;

  Py := nil;
  P1 := nil;
  P2 := nil;
  K := nil;
  Sm2IsNil := Sm2 = nil;

  try
    if Sm2IsNil then
      Sm2 := TCnSM2.Create;

    K := TCnBigNumber.Create;

    // 确保公钥 X Y 均存在
    if PublicKey.Y.IsZero then
    begin
      Py := TCnEccPoint.Create;
      if not Sm2.PlainToPoint(PublicKey.X, Py) then
        Exit;
      BigNumberCopy(PublicKey.Y, Py.Y);
    end;

    // 生成一个随机 K
    if not BigNumberRandRange(K, Sm2.Order) then
      Exit;
    // K.SetHex('384F30353073AEECE7A1654330A96204D37982A3E15B2CB5');

    P1 := TCnEccPoint.Create;
    P1.Assign(Sm2.Generator);
    Sm2.MultiplePoint(K, P1);  // 计算出 K * G 得到 X1 Y1

    B := 4;
    OutStream.Position := 0;

    OutStream.Write(B, 1);
    SetLength(Buf, P1.X.GetBytesCount);
    P1.X.ToBinary(@Buf[0]);
    OutStream.Write(Buf[0], P1.X.GetBytesCount);
    SetLength(Buf, P1.Y.GetBytesCount);
    P1.Y.ToBinary(@Buf[0]);
    OutStream.Write(Buf[0], P1.Y.GetBytesCount); // 拼成 C1

    P2 := TCnEccPoint.Create;
    P2.Assign(PublicKey);
    Sm2.MultiplePoint(K, P2); // 计算出 K * PublicKey 得到 X2 Y2

    SetLength(KDFStr, P2.X.GetBytesCount + P2.Y.GetBytesCount);
    P2.X.ToBinary(@KDFStr[1]);
    P2.Y.ToBinary(@KDFStr[P2.X.GetBytesCount + 1]);
    T := CnSM2KDF(KDFStr, DataLen);

    M := PAnsiChar(PlainData);
    for I := 1 to DataLen do
      T[I] := AnsiChar(Byte(T[I]) xor Byte(M[I - 1])); // T 里是 C2，但先不能写

    SetLength(C3H, P2.X.GetBytesCount + P2.Y.GetBytesCount + DataLen);
    P2.X.ToBinary(@C3H[1]);
    Move(M[0], C3H[P2.X.GetBytesCount + 1], DataLen);
    P2.Y.ToBinary(@C3H[P2.X.GetBytesCount + DataLen + 1]); // 拼成算 C3 的
    Sm3Dig := SM3(@C3H[1], Length(C3H));                   // 算出 C3

    OutStream.Write(Sm3Dig[0], SizeOf(TSM3Digest));        // 写入 C3
    OutStream.Write(T[1], DataLen);                        // 写入 C2
    Result := True;
  finally
    P2.Free;
    P1.Free;
    Py.Free;
    K.Free;
    if Sm2IsNil then
      Sm2.Free;
  end;
end;

{
  MLen = DataLen - SM3DigLength - 2 * Sm2 Byte Length - 1，劈开拿到 C1 C2 C3

  PrivateKey * C1 = (x2, y2)

  t = KDF(x2‖y2, Mlen)

  M' = C2 xor t

  还可对比 SM3(x2‖M‖y2) Hash 是否与 C3 相等
}
function CnSM2DecryptData(EnData: Pointer; DataLen: Integer; OutStream: TStream;
  PrivateKey: TCnEccPrivateKey; Sm2: TCnSM2): Boolean;
var
  MLen: Integer;
  M: PAnsiChar;
  MP: AnsiString;
  KDFStr, T, C3H: AnsiString;
  Sm2IsNil: Boolean;
  P2: TCnEccPoint;
  I: Integer;
  Sm3Dig: TSM3Digest;
begin
  Result := False;
  if (EnData = nil) or (DataLen <= 0) or (OutStream = nil) or (PrivateKey = nil) then
    Exit;

  P2 := nil;
  Sm2IsNil := Sm2 = nil;

  try
    if Sm2IsNil then
      Sm2 := TCnSM2.Create;

    MLen := DataLen - SizeOf(TSM3Digest) - (Sm2.BitsCount div 4) - 1;
    if MLen <= 0 then
      Exit;

    P2 := TCnEccPoint.Create;
    M := PAnsiChar(EnData);
    Inc(M);
    P2.X.SetBinary(M, Sm2.BitsCount div 8);
    Inc(M, Sm2.BitsCount div 8);
    P2.Y.SetBinary(M, Sm2.BitsCount div 8);
    Sm2.MultiplePoint(PrivateKey, P2);

    SetLength(KDFStr, P2.X.GetBytesCount + P2.Y.GetBytesCount);
    P2.X.ToBinary(@KDFStr[1]);
    P2.Y.ToBinary(@KDFStr[P2.X.GetBytesCount + 1]);
    T := CnSM2KDF(KDFStr, MLen);

    SetLength(MP, MLen);
    M := PAnsiChar(EnData);
    Inc(M, SizeOf(TSM3Digest) + (Sm2.BitsCount div 4) + 1);
    for I := 1 to MLen do
      MP[I] := AnsiChar(Byte(M[I - 1]) xor Byte(T[I])); // MP 得到明文

    SetLength(C3H, P2.X.GetBytesCount + P2.Y.GetBytesCount + MLen);
    P2.X.ToBinary(@C3H[1]);
    Move(MP[1], C3H[P2.X.GetBytesCount + 1], MLen);
    P2.Y.ToBinary(@C3H[P2.X.GetBytesCount + MLen + 1]);    // 拼成算 C3 的
    Sm3Dig := SM3(@C3H[1], Length(C3H));                   // 算出 C3

    M := PAnsiChar(EnData);
    Inc(M, (Sm2.BitsCount div 4) + 1);
    if CompareMem(@Sm3Dig[0], M, SizeOf(TSM3Digest)) then  // 比对 Hash 是否相等
    begin
      OutStream.Write(MP[1], Length(MP));
      Result := True;
    end;
  finally
    P2.Free;
    if Sm2IsNil then
      Sm2.Free;
  end;
end;

end.

