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

unit CnOTS;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：一次性杂凑签名算法实现单元，目前实现了基于 SM3 与 SHA256 的简单实现
* 单元作者：刘啸
* 备    注：Hash Based One Time Signature，其他长度的杂凑算法暂未实现
* 开发平台：Win7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2023.11.25 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative, CnBits, CnRandom, CnSM3, CnSHA2;

type
  TCnOTSSM3PrivateKey = array[0..(SizeOf(TCnSM3Digest) * 8 * 2) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的一次性杂凑签名私钥，为 256 * 2 个随机值，为一致起见，长度取 SM3 的结果长度}

  TCnOTSSM3PublicKey = array[0..(SizeOf(TCnSM3Digest) * 8 * 2) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的一次性杂凑签名公钥，为 256 * 2 个随机值的 SM3 杂凑值}

  TCnOTSSM3Signature = array[0..(SizeOf(TCnSM3Digest) * 8) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的一次性杂凑签名值，实际上是 256 个 SM3 杂凑值}

  TCnOTSSM3VerificationKey = array[0..(SizeOf(TCnSM3Digest) * 8) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的一次性杂凑签名验证密钥，实际上是从私钥中抽取的 256 个随机值}

  TCnOTSSHA256PrivateKey = array[0..(SizeOf(TCnSHA256Digest) * 8 * 2) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的一次性杂凑签名私钥，为 256 * 2 个随机值，为一致起见，长度取 SHA256 的结果长度}

  TCnOTSSHA256PublicKey = array[0..(SizeOf(TCnSHA256Digest) * 8 * 2) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的一次性杂凑签名公钥，为 256 * 2 个随机值的 SHA256 杂凑值}

  TCnOTSSHA256Signature = array[0..(SizeOf(TCnSHA256Digest) * 8) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的一次性杂凑签名值，实际上是 256 个 SHA256 杂凑值}

  TCnOTSSHA256VerificationKey = array[0..(SizeOf(TCnSHA256Digest) * 8) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的一次性杂凑签名验证密钥，实际上是从私钥中抽取的 256 个随机值}

function CnOTSSM3GenerateKeys(var PrivateKey: TCnOTSSM3PrivateKey;
  var PublicKey: TCnOTSSM3PublicKey): Boolean;
{* 生成一对基于 SM3 杂凑算法的一次性杂凑签名公私钥，返回生成是否成功}

procedure CnOTSSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnOTSSM3PrivateKey; PublicKey: TCnOTSSM3PublicKey;
  var OutSignature: TCnOTSSM3Signature; var OutVerifyKey: TCnOTSSM3VerificationKey);
{* 根据公私钥生成指定内存块数据的一次性杂凑签名及验证这个签名的密钥。
  平时公布明文、签名值与公钥，有验证需求时，给验证方公布验证密钥。
  验证密钥实际上是私钥的一部分，因此验证密钥公布后等同于只能验证这一次，
  不能再用这批私钥给别的消息签名了，这正是一次性签名的含义}

function CnOTSSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnOTSSM3Signature; PublicKey: TCnOTSSM3PublicKey;
  VerifyKey: TCnOTSSM3VerificationKey): Boolean;
{* 根据明文、公布的验证密钥与公钥验证指定内存块数据的签名是否正确，返回验证是否成功}

procedure CnOTSSM3SignBytes(Data: TBytes; PrivateKey: TCnOTSSM3PrivateKey;
  PublicKey: TCnOTSSM3PublicKey; var OutSignature: TCnOTSSM3Signature;
  var OutVerifyKey: TCnOTSSM3VerificationKey);
{* 根据公私钥生成字节数组的一次性杂凑签名及验证这个签名的密钥。
  平时公布明文、签名值与公钥，有验证需求时，给验证方公布验证密钥。
  验证密钥实际上是私钥的一部分，因此验证密钥公布后等同于只能验证这一次，
  不能再用这批私钥给别的消息签名了，这正是一次性签名的含义}

function CnOTSSM3VerifyBytes(Data: TBytes; Signature: TCnOTSSM3Signature;
  PublicKey: TCnOTSSM3PublicKey; VerifyKey: TCnOTSSM3VerificationKey): Boolean;
{* 根据明文、公布的验证密钥与公钥验证字节数组的签名是否正确，返回验证是否成功}

function CnOTSSHA256GenerateKeys(var PrivateKey: TCnOTSSHA256PrivateKey;
  var PublicKey: TCnOTSSHA256PublicKey): Boolean;
{* 生成一对基于 SHA256 杂凑算法的一次性杂凑签名公私钥，返回生成是否成功}

procedure CnOTSSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnOTSSHA256PrivateKey; PublicKey: TCnOTSSHA256PublicKey;
  var OutSignature: TCnOTSSHA256Signature; var OutVerifyKey: TCnOTSSHA256VerificationKey);
{* 根据公私钥生成指定内存块数据的一次性杂凑签名及验证这个签名的密钥。
  平时公布明文、签名值与公钥，有验证需求时，给验证方公布验证密钥。
  验证密钥实际上是私钥的一部分，因此验证密钥公布后等同于只能验证这一次，
  不能再用这批私钥给别的消息签名了，这正是一次性签名的含义}

function CnOTSSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnOTSSHA256Signature; PublicKey: TCnOTSSHA256PublicKey;
  VerifyKey: TCnOTSSHA256VerificationKey): Boolean;
{* 根据明文、公布的验证密钥与公钥验证指定内存块数据的签名是否正确，返回验证是否成功}

procedure CnOTSSHA256SignBytes(Data: TBytes; PrivateKey: TCnOTSSHA256PrivateKey;
  PublicKey: TCnOTSSHA256PublicKey; var OutSignature: TCnOTSSHA256Signature;
  var OutVerifyKey: TCnOTSSHA256VerificationKey);
{* 根据公私钥生成字节数组的一次性杂凑签名及验证这个签名的密钥。
  平时公布明文、签名值与公钥，有验证需求时，给验证方公布验证密钥。
  验证密钥实际上是私钥的一部分，因此验证密钥公布后等同于只能验证这一次，
  不能再用这批私钥给别的消息签名了，这正是一次性签名的含义}

function CnOTSSHA256VerifyBytes(Data: TBytes; Signature: TCnOTSSHA256Signature;
  PublicKey: TCnOTSSHA256PublicKey; VerifyKey: TCnOTSSHA256VerificationKey): Boolean;
{* 根据明文、公布的验证密钥与公钥验证字节数组的签名是否正确，返回验证是否成功}

implementation

function CnOTSSM3GenerateKeys(var PrivateKey: TCnOTSSM3PrivateKey;
  var PublicKey: TCnOTSSM3PublicKey): Boolean;
var
  I: Integer;
begin
  Result := CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnOTSSM3PrivateKey));
  if Result then
    for I := Low(TCnOTSSM3PublicKey) to High(TCnOTSSM3PublicKey) do
      PublicKey[I] := SM3(@PrivateKey[I], SizeOf(TCnSM3Digest));
end;

procedure CnOTSSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnOTSSM3PrivateKey; PublicKey: TCnOTSSM3PublicKey;
  var OutSignature: TCnOTSSM3Signature; var OutVerifyKey: TCnOTSSM3VerificationKey);
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig: TCnSM3Digest;
begin
  Dig := SM3(PAnsiChar(Data), DataByteLen);
  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSM3Digest));

    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 是 1
      begin
        OutSignature[I] := PublicKey[I * 2 + 1];
        OutVerifyKey[I] := PrivateKey[I * 2 + 1];
      end
      else
      begin
        OutSignature[I] := PublicKey[I * 2];
        OutVerifyKey[I] := PrivateKey[I * 2];
      end;
    end;
  finally
    Bits.Free;
  end;
end;

function CnOTSSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnOTSSM3Signature; PublicKey: TCnOTSSM3PublicKey;
  VerifyKey: TCnOTSSM3VerificationKey): Boolean;
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig, Cmp: TCnSM3Digest;
begin
  Result := False;
  Dig := SM3(PAnsiChar(Data), DataByteLen);
  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSM3Digest));

    for I := 0 to Bits.BitLength - 1 do
    begin
      Cmp := SM3(@VerifyKey[I], SizeOf(TCnSM3Digest)); // 计算私钥的杂凑值
      if Bits.Bit[I] then 
        Result := SM3Match(Cmp, PublicKey[I * 2 + 1])  // 该位是 1，比较 1 对应的公钥
      else
        Result := SM3Match(Cmp, PublicKey[I * 2]);     // 该位是 0，比较 0 对应的公钥

      if not Result then
        Exit;
    end;
  finally
    Bits.Free;
  end;
end;

procedure CnOTSSM3SignBytes(Data: TBytes; PrivateKey: TCnOTSSM3PrivateKey;
  PublicKey: TCnOTSSM3PublicKey; var OutSignature: TCnOTSSM3Signature;
  var OutVerifyKey: TCnOTSSM3VerificationKey);
begin
  if Length(Data) = 0 then
    CnOTSSM3SignData(nil, 0, PrivateKey, PublicKey, OutSignature, OutVerifyKey)
  else
    CnOTSSM3SignData(@Data[0], Length(Data), PrivateKey, PublicKey, OutSignature, OutVerifyKey);
end;

function CnOTSSM3VerifyBytes(Data: TBytes; Signature: TCnOTSSM3Signature;
  PublicKey: TCnOTSSM3PublicKey; VerifyKey: TCnOTSSM3VerificationKey): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnOTSSM3VerifyData(nil, 0, Signature, PublicKey, VerifyKey)
  else
    Result := CnOTSSM3VerifyData(@Data[0], Length(Data), Signature, PublicKey, VerifyKey);
end;

function CnOTSSHA256GenerateKeys(var PrivateKey: TCnOTSSHA256PrivateKey;
  var PublicKey: TCnOTSSHA256PublicKey): Boolean;
var
  I: Integer;
  P: Pointer;
begin
  Result := CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnOTSSHA256PrivateKey));
  if Result then
  begin
    for I := Low(TCnOTSSHA256PublicKey) to High(TCnOTSSHA256PublicKey) do
    begin
      P := @PrivateKey[I];
      PublicKey[I] := SHA256Buffer(P, SizeOf(TCnSHA256Digest));
    end;
  end;
end;

procedure CnOTSSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnOTSSHA256PrivateKey; PublicKey: TCnOTSSHA256PublicKey;
  var OutSignature: TCnOTSSHA256Signature; var OutVerifyKey: TCnOTSSHA256VerificationKey);
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig: TCnSHA256Digest;
begin
  Dig := SHA256Buffer(PAnsiChar(Data), DataByteLen);
  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSHA256Digest));

    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 是 1
      begin
        OutSignature[I] := PublicKey[I * 2 + 1];
        OutVerifyKey[I] := PrivateKey[I * 2 + 1];
      end
      else
      begin
        OutSignature[I] := PublicKey[I * 2];
        OutVerifyKey[I] := PrivateKey[I * 2];
      end;
    end;
  finally
    Bits.Free;
  end;
end;

function CnOTSSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnOTSSHA256Signature; PublicKey: TCnOTSSHA256PublicKey;
  VerifyKey: TCnOTSSHA256VerificationKey): Boolean;
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig, Cmp: TCnSHA256Digest;
  P: Pointer;
begin
  Result := False;
  Dig := SHA256Buffer(PAnsiChar(Data), DataByteLen);
  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSHA256Digest));

    for I := 0 to Bits.BitLength - 1 do
    begin
      P := @VerifyKey[I];
      Cmp := SHA256Buffer(P, SizeOf(TCnSHA256Digest));    // 计算私钥的杂凑值
      if Bits.Bit[I] then 
        Result := SHA256Match(Cmp, PublicKey[I * 2 + 1])  // 该位是 1，比较 1 对应的公钥
      else
        Result := SHA256Match(Cmp, PublicKey[I * 2]);     // 该位是 0，比较 0 对应的公钥

      if not Result then
        Exit;
    end;
  finally
    Bits.Free;
  end;
end;

procedure CnOTSSHA256SignBytes(Data: TBytes; PrivateKey: TCnOTSSHA256PrivateKey;
  PublicKey: TCnOTSSHA256PublicKey; var OutSignature: TCnOTSSHA256Signature;
  var OutVerifyKey: TCnOTSSHA256VerificationKey);
begin
  if Length(Data) = 0 then
    CnOTSSHA256SignData(nil, 0, PrivateKey, PublicKey, OutSignature, OutVerifyKey)
  else
    CnOTSSHA256SignData(@Data[0], Length(Data), PrivateKey, PublicKey, OutSignature, OutVerifyKey);
end;

function CnOTSSHA256VerifyBytes(Data: TBytes; Signature: TCnOTSSHA256Signature;
  PublicKey: TCnOTSSHA256PublicKey; VerifyKey: TCnOTSSHA256VerificationKey): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnOTSSHA256VerifyData(nil, 0, Signature, PublicKey, VerifyKey)
  else
    Result := CnOTSSHA256VerifyData(@Data[0], Length(Data), Signature, PublicKey, VerifyKey);
end;

end.
