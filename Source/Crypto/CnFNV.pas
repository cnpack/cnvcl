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

unit CnFNV;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：FNV 杂凑算法实现单元
* 单元作者：CnPack 开发组（master@cnpack.org)
* 备    注：本单元实现了一种简易的可变长度的杂凑算法 FNV，它有两个变种 FNV-1 和 FNV-1a，
*           均以特定素数与偏移量逐字节运算而来。算法创造者们为 Fowler-Noll-Vo
* 开发平台：Windows 7 + Delphi 5.0
* 兼容测试：
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2023.01.16 V1.0
*               移植并创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNative, CnBigNumber;

type
  TCnFNVType = (cft32, cft64, cft128, cft256, cft512, cft1024);
  {* 代表不同长度的 FNV 类型}

  TCnFNVHash32    = array[0..3] of Byte;
  {* 32 位也就是 4 字节长的 FNV 结果}

  TCnFNVHash64    = array[0..7] of Byte;
  {* 64 位也就是 8 字节长的 FNV 结果}

  TCnFNVHash128   = array[0..15] of Byte;
  {* 128 位也就是 16 字节长的 FNV 结果}

  TCnFNVHash256   = array[0..31] of Byte;
  {* 256 位也就是 32 字节长的 FNV 结果}

  TCnFNVHash512   = array[0..63] of Byte;
  {* 512 位也就是 64 字节长的 FNV 结果}

  TCnFNVHash1024  = array[0..127] of Byte;
  {* 1024 位也就是 128 字节长的 FNV 结果}

// 以上是 FNV 的结果类型，均以大端表示

function FNV1Hash32(Data: TBytes): TCnFNVHash32; overload;
{* 求字节数组的 FNV-1 杂凑结果，结果长度 32 位}

function FNV1Hash64(Data: TBytes): TCnFNVHash64; overload;
{* 求字节数组的 FNV-1 杂凑结果，结果长度 64 位}

function FNV1Hash128(Data: TBytes): TCnFNVHash128; overload;
{* 求字节数组的 FNV-1 杂凑结果，结果长度 128 位}

function FNV1Hash256(Data: TBytes): TCnFNVHash256; overload;
{* 求字节数组的 FNV-1 杂凑结果，结果长度 256 位}

function FNV1Hash512(Data: TBytes): TCnFNVHash512; overload;
{* 求字节数组的 FNV-1 杂凑结果，结果长度 512 位}

function FNV1Hash1024(Data: TBytes): TCnFNVHash1024; overload;
{* 求字节数组的 FNV-1 杂凑结果，结果长度 1024 位}

function FNV1aHash32(Data: TBytes): TCnFNVHash32; overload;
{* 求字节数组的 FNV-1a 杂凑结果，结果长度 32 位}

function FNV1aHash64(Data: TBytes): TCnFNVHash64; overload;
{* 求字节数组的 FNV-1a 杂凑结果，结果长度 64 位}

function FNV1aHash128(Data: TBytes): TCnFNVHash128; overload;
{* 求字节数组的 FNV-1a 杂凑结果，结果长度 128 位}

function FNV1aHash256(Data: TBytes): TCnFNVHash256; overload;
{* 求字节数组的 FNV-1a 杂凑结果，结果长度 256 位}

function FNV1aHash512(Data: TBytes): TCnFNVHash512; overload;
{* 求字节数组的 FNV-1a 杂凑结果，结果长度 512 位}

function FNV1aHash1024(Data: TBytes): TCnFNVHash1024; overload;
{* 求字节数组的 FNV-1a 杂凑结果，结果长度 1024 位}

function FNV1Hash32(Data: Pointer; DataByteLen: Integer): TCnFNVHash32; overload;
{* 求数据块的 FNV-1 杂凑结果，结果长度 32 位}

function FNV1Hash64(Data: Pointer; DataByteLen: Integer): TCnFNVHash64; overload;
{* 求数据块的 FNV-1 杂凑结果，结果长度 64 位}

function FNV1Hash128(Data: Pointer; DataByteLen: Integer): TCnFNVHash128; overload;
{* 求数据块的 FNV-1 杂凑结果，结果长度 128 位}

function FNV1Hash256(Data: Pointer; DataByteLen: Integer): TCnFNVHash256; overload;
{* 求数据块的 FNV-1 杂凑结果，结果长度 256 位}

function FNV1Hash512(Data: Pointer; DataByteLen: Integer): TCnFNVHash512; overload;
{* 求数据块的 FNV-1 杂凑结果，结果长度 512 位}

function FNV1Hash1024(Data: Pointer; DataByteLen: Integer): TCnFNVHash1024; overload;
{* 求数据块的 FNV-1 杂凑结果，结果长度 1024 位}

function FNV1aHash32(Data: Pointer; DataByteLen: Integer): TCnFNVHash32; overload;
{* 求数据块的 FNV-1a 杂凑结果，结果长度 32 位}

function FNV1aHash64(Data: Pointer; DataByteLen: Integer): TCnFNVHash64; overload;
{* 求数据块的 FNV-1a 杂凑结果，结果长度 64 位}

function FNV1aHash128(Data: Pointer; DataByteLen: Integer): TCnFNVHash128; overload;
{* 求数据块的 FNV-1a 杂凑结果，结果长度 128 位}

function FNV1aHash256(Data: Pointer; DataByteLen: Integer): TCnFNVHash256; overload;
{* 求数据块的 FNV-1a 杂凑结果，结果长度 256 位}

function FNV1aHash512(Data: Pointer; DataByteLen: Integer): TCnFNVHash512; overload;
{* 求数据块的 FNV-1a 杂凑结果，结果长度 512 位}

function FNV1aHash1024(Data: Pointer; DataByteLen: Integer): TCnFNVHash1024; overload;
{* 求数据块的 FNV-1a 杂凑结果，结果长度 1024 位}

implementation

const
  FNV_PRIME_32   = '01000193';
  FNV_PRIME_64   = '00000100000001B3';
  FNV_PRIME_128  = '0000000001000000000000000000013B';
  FNV_PRIME_256  = '0000000000000000000001000000000000000000000000000000000000000163';
  FNV_PRIME_512  = '00000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000157';
  FNV_PRIME_1024 = '00000000000000000000000000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000' +
    '0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000018D';

  FNV_OFFSET_BASIS_32   = '811C9DC5';
  FNV_OFFSET_BASIS_64   = 'CBF29CE484222325';
  FNV_OFFSET_BASIS_128  = '6C62272E07BB014262B821756295C58D';
  FNV_OFFSET_BASIS_256  = 'DD268DBCAAC550362D98C384C4E576CCC8B1536847B6BBB31023B4C8CAEE0535';
  FNV_OFFSET_BASIS_512  = 'B86DB0B1171F4416 DCA1E50F309990ACAC87D059C90000000000000000000D21E948F68A34C192F62EA79BC942DBE7CE182036415F56E34BAC982AAC4AFE9FD9';
  FNV_OFFSET_BASIS_1024 = '0000000000000000005F7A76758ECC4D32E56D5A591028B74B29FC4223FDADA16C3BF34EDA3674DA9A21D9000000000000000000000000000000000000000000' +
    '000000000000000000000000000000000000000000000000000000000004C6D7EB6E73802734510A555F256CC005AE556BDE8CC9C6A93B21AFF4B16C71EE90B3';

  FNV_PRIMES: array[Low(TCnFNVType)..High(TCnFNVType)] of string =
    (FNV_PRIME_32, FNV_PRIME_64, FNV_PRIME_128, FNV_PRIME_256, FNV_PRIME_512, FNV_PRIME_1024);

  FNV_OFFSET_BASISES: array[Low(TCnFNVType)..High(TCnFNVType)] of string =
    (FNV_OFFSET_BASIS_32, FNV_OFFSET_BASIS_64, FNV_OFFSET_BASIS_128,
    FNV_OFFSET_BASIS_256, FNV_OFFSET_BASIS_512, FNV_OFFSET_BASIS_1024);

  FNV_BIT_LENGTH: array[Low(TCnFNVType)..High(TCnFNVType)] of Integer =
    (32, 64, 128, 256, 512, 1024);

var
  FNV_PRIMES_BIGNUMBER: array[Low(TCnFNVType)..High(TCnFNVType)] of TCnBigNumber;

procedure SetPrime(FNVType: TCnFNVType; const Prime: TCnBigNumber);
begin
  Prime.SetHex(AnsiString(FNV_PRIMES[FNVType]));
end;

procedure SetOffsetBasis(FNVType: TCnFNVType; const Basis: TCnBigNumber);
begin
  Basis.SetHex(AnsiString(FNV_OFFSET_BASISES[FNVType]));
end;

//    hash := FNV_offset_basis
//
//    for each byte_of_data to be hashed do
//        hash := hash × FNV_prime
//        hash := hash XOR byte_of_data
//
//    return hash
procedure FNV1(FNVType: TCnFNVType; D: PByte; Len: Integer; const Res: TCnBigNumber);
begin
  if D = nil then Len := 0;
  SetOffsetBasis(FNVType, Res);

  while Len > 0 do
  begin
    BigNumberMul(Res, Res, FNV_PRIMES_BIGNUMBER[FNVType]);
    BigNumberKeepLowBits(Res, FNV_BIT_LENGTH[FNVType]);
    BigNumberXorWord(Res, D^);

    Inc(D);
    Dec(Len);
  end;
end;

//    hash := FNV_offset_basis
//
//    for each byte_of_data to be hashed do
//        hash := hash XOR byte_of_data
//        hash := hash × FNV_prime
//
//    return hash
procedure FNV1a(FNVType: TCnFNVType; D: PByte; Len: Integer; const Res: TCnBigNumber);
begin
  if D = nil then Len := 0;
  SetOffsetBasis(FNVType, Res);

  while Len > 0 do
  begin
    BigNumberXorWord(Res, D^);
    BigNumberMul(Res, Res, FNV_PRIMES_BIGNUMBER[FNVType]);
    BigNumberKeepLowBits(Res, FNV_BIT_LENGTH[FNVType]);

    Inc(D);
    Dec(Len);
  end;
end;

function FNV1Hash32(Data: TBytes): TCnFNVHash32;
begin
  if Length(Data) <= 0 then
    Result := FNV1Hash32(nil, 0)
  else
    Result := FNV1Hash32(@Data[0], Length(Data));
end;

function FNV1Hash64(Data: TBytes): TCnFNVHash64;
begin
  if Length(Data) <= 0 then
    Result := FNV1Hash64(nil, 0)
  else
    Result := FNV1Hash64(@Data[0], Length(Data));
end;

function FNV1Hash128(Data: TBytes): TCnFNVHash128;
begin
  if Length(Data) <= 0 then
    Result := FNV1Hash128(nil, 0)
  else
    Result := FNV1Hash128(@Data[0], Length(Data));
end;

function FNV1Hash256(Data: TBytes): TCnFNVHash256;
begin
  if Length(Data) <= 0 then
    Result := FNV1Hash256(nil, 0)
  else
    Result := FNV1Hash256(@Data[0], Length(Data));
end;

function FNV1Hash512(Data: TBytes): TCnFNVHash512;
begin
  if Length(Data) <= 0 then
    Result := FNV1Hash512(nil, 0)
  else
    Result := FNV1Hash512(@Data[0], Length(Data));
end;

function FNV1Hash1024(Data: TBytes): TCnFNVHash1024;
begin
  if Length(Data) <= 0 then
    Result := FNV1Hash1024(nil, 0)
  else
    Result := FNV1Hash1024(@Data[0], Length(Data));
end;

function FNV1aHash32(Data: TBytes): TCnFNVHash32;
begin
  if Length(Data) <= 0 then
    Result := FNV1aHash32(nil, 0)
  else
    Result := FNV1aHash32(@Data[0], Length(Data));
end;

function FNV1aHash64(Data: TBytes): TCnFNVHash64;
begin
  if Length(Data) <= 0 then
    Result := FNV1aHash64(nil, 0)
  else
    Result := FNV1aHash64(@Data[0], Length(Data));
end;

function FNV1aHash128(Data: TBytes): TCnFNVHash128;
begin
  if Length(Data) <= 0 then
    Result := FNV1aHash128(nil, 0)
  else
    Result := FNV1aHash128(@Data[0], Length(Data));
end;

function FNV1aHash256(Data: TBytes): TCnFNVHash256;
begin
  if Length(Data) <= 0 then
    Result := FNV1aHash256(nil, 0)
  else
    Result := FNV1aHash256(@Data[0], Length(Data));
end;

function FNV1aHash512(Data: TBytes): TCnFNVHash512;
begin
  if Length(Data) <= 0 then
    Result := FNV1aHash512(nil, 0)
  else
    Result := FNV1aHash512(@Data[0], Length(Data));
end;

function FNV1aHash1024(Data: TBytes): TCnFNVHash1024;
begin
  if Length(Data) <= 0 then
    Result := FNV1aHash1024(nil, 0)
  else
    Result := FNV1aHash1024(@Data[0], Length(Data));
end;

function FNV1Hash32(Data: Pointer; DataByteLen: Integer): TCnFNVHash32;
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  try
    FNV1(cft32, PByte(Data), DataByteLen, R);
    R.ToBinary(@Result[0], FNV_BIT_LENGTH[cft32] div 8);
  finally
    R.Free;
  end;
end;

function FNV1Hash64(Data: Pointer; DataByteLen: Integer): TCnFNVHash64;
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  try
    FNV1(cft64, PByte(Data), DataByteLen, R);
    R.ToBinary(@Result[0], FNV_BIT_LENGTH[cft64] div 8);
  finally
    R.Free;
  end;
end;

function FNV1Hash128(Data: Pointer; DataByteLen: Integer): TCnFNVHash128;
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  try
    FNV1(cft128, PByte(Data), DataByteLen, R);
    R.ToBinary(@Result[0], FNV_BIT_LENGTH[cft128] div 8);
  finally
    R.Free;
  end;
end;

function FNV1Hash256(Data: Pointer; DataByteLen: Integer): TCnFNVHash256;
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  try
    FNV1(cft256, PByte(Data), DataByteLen, R);
    R.ToBinary(@Result[0], FNV_BIT_LENGTH[cft256] div 8);
  finally
    R.Free;
  end;
end;

function FNV1Hash512(Data: Pointer; DataByteLen: Integer): TCnFNVHash512;
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  try
    FNV1(cft512, PByte(Data), DataByteLen, R);
    R.ToBinary(@Result[0], FNV_BIT_LENGTH[cft512] div 8);
  finally
    R.Free;
  end;
end;

function FNV1Hash1024(Data: Pointer; DataByteLen: Integer): TCnFNVHash1024;
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  try
    FNV1(cft1024, PByte(Data), DataByteLen, R);
    R.ToBinary(@Result[0], FNV_BIT_LENGTH[cft1024] div 8);
  finally
    R.Free;
  end;
end;

function FNV1aHash32(Data: Pointer; DataByteLen: Integer): TCnFNVHash32;
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  try
    FNV1a(cft32, PByte(Data), DataByteLen, R);
    R.ToBinary(@Result[0], FNV_BIT_LENGTH[cft32] div 8);
  finally
    R.Free;
  end;
end;

function FNV1aHash64(Data: Pointer; DataByteLen: Integer): TCnFNVHash64;
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  try
    FNV1a(cft64, PByte(Data), DataByteLen, R);
    R.ToBinary(@Result[0], FNV_BIT_LENGTH[cft64] div 8);
  finally
    R.Free;
  end;
end;

function FNV1aHash128(Data: Pointer; DataByteLen: Integer): TCnFNVHash128;
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  try
    FNV1a(cft128, PByte(Data), DataByteLen, R);
    R.ToBinary(@Result[0], FNV_BIT_LENGTH[cft128] div 8);
  finally
    R.Free;
  end;
end;

function FNV1aHash256(Data: Pointer; DataByteLen: Integer): TCnFNVHash256;
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  try
    FNV1a(cft256, PByte(Data), DataByteLen, R);
    R.ToBinary(@Result[0], FNV_BIT_LENGTH[cft256] div 8);
  finally
    R.Free;
  end;
end;

function FNV1aHash512(Data: Pointer; DataByteLen: Integer): TCnFNVHash512;
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  try
    FNV1a(cft512, PByte(Data), DataByteLen, R);
    R.ToBinary(@Result[0], FNV_BIT_LENGTH[cft512] div 8);
  finally
    R.Free;
  end;
end;

function FNV1aHash1024(Data: Pointer; DataByteLen: Integer): TCnFNVHash1024;
var
  R: TCnBigNumber;
begin
  R := TCnBigNumber.Create;
  try
    FNV1a(cft1024, PByte(Data), DataByteLen, R);
    R.ToBinary(@Result[0], FNV_BIT_LENGTH[cft1024] div 8);
  finally
    R.Free;
  end;
end;

procedure CreateFNVPrimes;
var
  I: TCnFNVType;
begin
  for I := Low(TCnFNVType) to High(TCnFNVType) do
  begin
    FNV_PRIMES_BIGNUMBER[I] := TCnBigNumber.Create;
    SetPrime(I, FNV_PRIMES_BIGNUMBER[I]);
  end;
end;

procedure FreeFNVPrimes;
var
  I: TCnFNVType;
begin
  for I := Low(TCnFNVType) to High(TCnFNVType) do
    FNV_PRIMES_BIGNUMBER[I].Free;
end;

initialization
  CreateFNVPrimes;

finalization
  FreeFNVPrimes;

end.
