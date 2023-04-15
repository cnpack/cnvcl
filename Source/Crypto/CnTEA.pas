{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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

unit CnTEA;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：TEA 算法单元
* 单元作者：刘啸（liuxiao@cnpack.org)
* 备    注：包括 TEA/XTEA/XXTEA
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.04.26 V1.2
*               修改 LongWord 与 Integer 地址转换以支持 MacOS64
*           2019.04.15 V1.1
*               支持 Win32/Win64/MacOS32
*           2018.09.03 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, CnNative;

const
  CN_TEA_ROUND_COUNT = 32;

type
  ECnTeaException = class(Exception);

  TCnTeaKey = array[0..3] of Cardinal;
  {* TEA 算法的密钥格式，四个 32Bit 数}

  TCnTeaData = array[0..1] of Cardinal;
  {* TEA 算法的数据格式，二个 32Bit 数}

  TCnXXTeaData = array[0..16383] of Cardinal;
  {* XXTEA 算法支持更长的 32Bit 数组}

  PCnXXTeaData = ^TCnXXTeaData;
  {* XXTEA 算法的 32Bit 数组指针}

procedure CnTeaEncrypt(Key: TCnTeaKey; var Data: TCnTeaData;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
{* TEA 加密，128 Bits 密钥加密 64 Bits 明文为密文}

procedure CnTeaDecrypt(Key: TCnTeaKey; var Data: TCnTeaData;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
{* TEA 解密，128 Bits 密钥解密 64 Bits 密文为明文}

procedure CnXTeaEncrypt(Key: TCnTeaKey; var Data: TCnTeaData;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
{* XTEA 加密，128 Bits 密钥加密 64 Bits 明文为密文}

procedure CnXTeaDecrypt(Key: TCnTeaKey; var Data: TCnTeaData;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
{* XTEA 解密，128 Bits 密钥解密 64 Bits 密文为明文}

procedure CnXXTeaEncrypt(Key: TCnTeaKey; Data: PCnXXTeaData; DataLongWordLength: Integer);
{* XXTEA 加密，128 Bits 密钥加密 4 字节整数倍长度的明文内容为密文}

procedure CnXXTeaDecrypt(Key: TCnTeaKey; Data: PCnXXTeaData; DataLongWordLength: Integer);
{* XXTEA 解密，128 Bits 密钥解密 4 字节整数倍长度的密文内容为明文}

implementation

const
  CN_TEA_DELTA = $9E3779B9;

// 以 K[0]/K[1]/K[2]/K[3] 为密钥，用 TEA 算法将明文 L/R 加密成密文
procedure TeaEncrypt(K: TCnTeaKey; var L, R: Cardinal;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
var
  D, S: Cardinal;
  I: Integer;
begin
  if RoundCount <= 0 then
    raise ECnTeaException.Create('Error RoundCount.');

  D := CN_TEA_DELTA;
  S := 0;
  for I := 1 to RoundCount do
  begin
    S := S + D;
    L := L + (((R shl 4) + K[0]) xor (R + S) xor ((R shr 5) + K[1]));
    R := R + (((L shl 4) + K[2]) xor (L + S) xor ((L shr 5) + K[3]));
  end;
end;

// 以 K[0]/K[1]/K[2]/K[3] 为密钥，用 TEA 算法将密文 L/R 解密成明文
procedure TeaDecrypt(K: TCnTeaKey; var L, R: Cardinal;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
var
  D, S: Cardinal;
  I: Integer;
begin
  if RoundCount <= 0 then
    raise ECnTeaException.Create('Error RountCount.');

  D := CN_TEA_DELTA;
  if RoundCount >= CN_TEA_ROUND_COUNT then // 32 轮要移动 5 位，16 轮要 4 位
    S := D shl 5
  else
    S := D shl 4;

  for I := 1 to RoundCount do
  begin
    R := R - (((L shl 4) + K[2]) xor (L + S) xor ((L shr 5) + K[3]));
    L := L - (((R shl 4) + K[0]) xor (R + S) xor ((R shr 5) + K[1]));
    S := S - D;
  end;
end;

// 以 K[0]/K[1]/K[2]/K[3] 为密钥，用 XTEA 算法将明文 L/R 加密成密文
procedure XTeaEncrypt(K: TCnTeaKey; var L, R: Cardinal;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
var
  D, S: Cardinal;
  I: Integer;
begin
  if RoundCount <= 0 then
    raise ECnTeaException.Create('Error RountCount.');

  D := CN_TEA_DELTA;
  S := 0;
  for I := 1 to RoundCount do
  begin
    L := L + ((((R shl 4) xor (R shr 5)) + R) xor (S + K[S and 3]));
    S := S + D;
    R := R + ((((L shl 4) xor (L shr 5)) + L) xor (S + K[(S shr 11) and 3]));
  end;
end;

// 以 K[0]/K[1]/K[2]/K[3] 为密钥，用 XTEA 算法将密文 L/R 解密成明文
procedure XTeaDecrypt(K: TCnTeaKey; var L, R: Cardinal;
  RoundCount: Integer = CN_TEA_ROUND_COUNT);
var
  D, S: Cardinal;
  I: Integer;
begin
  if RoundCount <= 0 then
    raise ECnTeaException.Create('Error RountCount.');

  D := CN_TEA_DELTA;
  S := D * Cardinal(RoundCount);
  for I := 1 to RoundCount do
  begin
    R := R - ((((L shl 4) xor (L shr 5)) + L) xor (S + K[(S shr 11) and 3]));
    S := S - D;
    L := L - ((((R shl 4) xor (R shr 5)) + R) xor (S + K[S and 3]));
  end;
end;

function MX(Z, Y, S, P, E: Cardinal; var Key: TCnTeaKey): Cardinal;
begin
  Result := (((Z shr 5) xor (Y shl 2)) + ((Y shr 3) xor (Z shl 4))) xor
    ((S xor Y) + (Key[(P and 3) xor E] xor Z) );
end;

// TEA 加密，128 Bits 密钥加密 64 Bits 明文为密文
procedure CnTeaEncrypt(Key: TCnTeaKey; var Data: TCnTeaData; RoundCount: Integer = CN_TEA_ROUND_COUNT);
begin
  TeaEncrypt(Key, Data[0], Data[1], RoundCount);
end;

// TEA 解密，128 Bits 密钥解密 64 Bits 密文为明文
procedure CnTeaDecrypt(Key: TCnTeaKey; var Data: TCnTeaData; RoundCount: Integer = CN_TEA_ROUND_COUNT);
begin
  TeaDecrypt(Key, Data[0], Data[1], RoundCount);
end;

// XTEA 加密，128 Bits 密钥加密 64 Bits 明文为密文
procedure CnXTeaEncrypt(Key: TCnTeaKey; var Data: TCnTeaData; RoundCount: Integer = CN_TEA_ROUND_COUNT);
begin
  XTeaEncrypt(Key, Data[0], Data[1], RoundCount);
end;

// XTEA 解密，128 Bits 密钥解密 64 Bits 密文为明文
procedure CnXTeaDecrypt(Key: TCnTeaKey; var Data: TCnTeaData; RoundCount: Integer = CN_TEA_ROUND_COUNT);
begin
  XTeaDecrypt(Key, Data[0], Data[1], RoundCount);
end;

// XXTEA 加密，128 Bits 密钥加密 4 字节整数倍长度的明文内容为密文
procedure CnXXTeaEncrypt(Key: TCnTeaKey; Data: PCnXXTeaData; DataLongWordLength: Integer);
var
  Z, Y, X, Sum, E, P: Cardinal;
  Q: Integer;
begin
  if DataLongWordLength <= 0 then
    raise ECnTeaException.Create('Error Tea Data.');

  Q := 6 + 52 div DataLongWordLength;
  Z := Data^[DataLongWordLength - 1];
  Sum := 0;

  repeat
    Sum := Sum + CN_TEA_DELTA;
    E := (Sum shr 2) and 3;
    for P := 0 to DataLongWordLength - 2 do
    begin
      Y := Data^[P + 1];
      X := Data^[P];
      X := X + MX(Z, Y, Sum, P, E, Key);
      Data^[P] := X;
      Z := X;
    end;
    Y := Data^[0];
    X := Data^[DataLongWordLength - 1];
    X := X + MX(Z, Y, Sum, DataLongWordLength - 1, E, Key);
    Data^[DataLongWordLength - 1] := X;
    Z := X;
    Dec(Q);
  until Q = 0;
end;

// XXTEA 解密，128 Bits 密钥解密 4 字节整数倍长度的密文内容为明文
procedure CnXXTeaDecrypt(Key: TCnTeaKey; Data: PCnXXTeaData; DataLongWordLength: Integer);
var
  Z, Y, X, Sum, E, P: Cardinal;
  Q: Integer;
begin
  if DataLongWordLength <= 0 then
    raise ECnTeaException.Create('Error Tea Data.');

  Q := 6 + 52 div DataLongWordLength;
  Y := Data^[0];

  Sum := Cardinal(Q) * CN_TEA_DELTA;
  repeat
    E := (Sum shr 2) and 3;
    for P := DataLongWordLength - 1 downto 1 do
    begin
      Z := Data^[P - 1];
      X := Data^[P];
      X := X - MX(Z, Y, Sum, P, E, Key);
      Data^[P] := X;
      Y := X;
    end;
    Z := Data^[DataLongWordLength - 1];
    X := Data^[0];
    X := X - MX(Z, Y, Sum, 0, E, Key);
    Data^[0] := X;
    Y := X;
    Sum := Sum - CN_TEA_DELTA;
    Dec(Q);
  until Q = 0;
end;

end.
