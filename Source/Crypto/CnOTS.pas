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

unit CnOTS;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：一次性杂凑签名算法实现单元，目前有基于 SM3 与 SHA256 的 OTS/M-OTS/W-OTS 简单实现
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

// ================ Lamport 发明的常规 OTS，结合 SM3 杂凑算法 ==================

  TCnOTSSM3PrivateKey = array[0..(SizeOf(TCnSM3Digest) * 8 * 2) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的一次性杂凑签名私钥，为 256 * 2 个随机值，为一致起见，单个随机值长度取 SM3 的结果长度}

  TCnOTSSM3PublicKey = array[0..(SizeOf(TCnSM3Digest) * 8 * 2) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的一次性杂凑签名公钥，为 256 * 2 个随机值的 SM3 杂凑值}

  TCnOTSSM3Signature = array[0..(SizeOf(TCnSM3Digest) * 8) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的一次性杂凑签名值，实际上是 256 个 SM3 杂凑值}

  TCnOTSSM3VerificationKey = array[0..(SizeOf(TCnSM3Digest) * 8) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的一次性杂凑签名验证密钥，实际上是从私钥中抽取的 256 个随机值}

// =============== Lamport 发明的常规 OTS，结合 SHA256 杂凑算法 ================

  TCnOTSSHA256PrivateKey = array[0..(SizeOf(TCnSHA256Digest) * 8 * 2) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的一次性杂凑签名私钥，为 256 * 2 个随机值，为一致起见，单个随机值长度取 SHA256 的结果长度}

  TCnOTSSHA256PublicKey = array[0..(SizeOf(TCnSHA256Digest) * 8 * 2) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的一次性杂凑签名公钥，为 256 * 2 个随机值的 SHA256 杂凑值}

  TCnOTSSHA256Signature = array[0..(SizeOf(TCnSHA256Digest) * 8) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的一次性杂凑签名值，实际上是 256 个 SHA256 杂凑值}

  TCnOTSSHA256VerificationKey = array[0..(SizeOf(TCnSHA256Digest) * 8) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的一次性杂凑签名验证密钥，实际上是从私钥中抽取的 256 个随机值}

// ========== Merkle 发明的 M-OTS，校验 0 的个数，结合 SM3 杂凑算法 ============

  TCnMOTSSM3PrivateKey = array[0..((SizeOf(TCnSM3Digest) + 1) * 8) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 M-OTS 私钥，为 256 个随机值加上一个单字节校验和，为一致起见，单个随机值长度取 SM3 的结果长度}

  TCnMOTSSM3PublicKey = array[0..((SizeOf(TCnSM3Digest)+ 1) * 8) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 M-OTS 公钥，为 256 个随机值加上一个单字节校验和的 SM3 杂凑值}

  TCnMOTSSM3Signature = array[0..((SizeOf(TCnSM3Digest) + 1) * 8) - 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 M-OTS 签名，实际上是 256 个 SM3 杂凑值加上一个单字节校验和，其中 0 对应全 0}

// ========== Merkle 发明的 M-OTS，校验 0 的个数，结合 SHA256 杂凑算法 ============

  TCnMOTSSHA256PrivateKey = array[0..((SizeOf(TCnSHA256Digest) + 1) * 8) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 M-OTS 私钥，为 256 个随机值加上一个单字节校验和，为一致起见，单个随机值长度取 SHA256 的结果长度}

  TCnMOTSSHA256PublicKey = array[0..((SizeOf(TCnSHA256Digest)+ 1) * 8) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 M-OTS 公钥，为 256 个随机值加上一个单字节校验和的 SHA256 杂凑值}

  TCnMOTSSHA256Signature = array[0..((SizeOf(TCnSHA256Digest) + 1) * 8) - 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 M-OTS 签名，实际上是 256 个 SHA256 杂凑值加上一个单字节校验和，其中 0 对应全 0}

// ===== Winternitz 改进的 W-OTS，取 n = 8 也即 1 字节，结合 SM3 杂凑算法 ======

  TCnWOTSSM3PrivateKey = array[0..SizeOf(TCnSM3Digest) + 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 W-OTS 私钥，为 32 个随机值加上一个双字节校验和，为一致起见，单个随机值长度取 SM3 的结果长度}

  TCnWOTSSM3PublicKey = array[0..SizeOf(TCnSM3Digest) + 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 W-OTS 公钥，为 32 个随机值加上一个双字节校验和各计算 256 次得到的 SM3 杂凑值}

  TCnWOTSSM3Signature = array[0..SizeOf(TCnSM3Digest) + 1] of TCnSM3Digest;
  {* 基于 SM3 杂凑算法的 W-OTS 签名，为 32 个 SM3 杂凑值加上一个双字节校验和，注意双字节按网络顺序存储}

// ==== Winternitz 改进的 W-OTS，取 n = 8 也即 1 字节，结合 SHA256 杂凑算法 ====

  TCnWOTSSHA256PrivateKey = array[0..SizeOf(TCnSHA256Digest) + 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 W-OTS 私钥，为 32 个随机值加上一个双字节校验和，为一致起见，单个随机值长度取 SHA256 的结果长度}

  TCnWOTSSHA256PublicKey = array[0..SizeOf(TCnSHA256Digest) + 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 W-OTS 公钥，为 32 个随机值加上一个双字节校验和各计算 256 次得到的 SHA256 杂凑值}

  TCnWOTSSHA256Signature = array[0..SizeOf(TCnSHA256Digest) + 1] of TCnSHA256Digest;
  {* 基于 SHA256 杂凑算法的 W-OTS 签名，为 32 个 SHA256 杂凑值加上一个双字节校验和，注意双字节按网络顺序存储}

// ================ Lamport 发明的常规 OTS，结合 SM3 杂凑算法 ==================

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

// =============== Lamport 发明的常规 OTS，结合 SHA256 杂凑算法 ================

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

// ========== Merkle 改进的 M-OTS，校验 0 的个数，结合 SM3 杂凑算法 ============

function CnMOTSSM3GenerateKeys(var PrivateKey: TCnMOTSSM3PrivateKey;
  var PublicKey: TCnMOTSSM3PublicKey): Boolean;
{* 生成一对基于 SM3 杂凑算法的 M-OTS 签名公私钥，返回生成是否成功}

procedure CnMOTSSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnMOTSSM3PrivateKey; var OutSignature: TCnMOTSSM3Signature);
{* 根据私钥生成指定内存块数据的一次性杂凑签名。
  平时公布明文、签名值与公钥，有验证需求时，给验证方公布私钥}

function CnMOTSSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnMOTSSM3Signature; PublicKey: TCnMOTSSM3PublicKey): Boolean;
{* 根据明文与公钥验证指定内存块数据的签名是否正确，返回验证是否成功}

procedure CnMOTSSM3SignBytes(Data: TBytes; PrivateKey: TCnMOTSSM3PrivateKey;
  var OutSignature: TCnMOTSSM3Signature);
{* 根据私钥生成字节数组的一次性杂凑签名。
  平时公布明文、签名值与公钥，有验证需求时，给验证方公布私钥}

function CnMOTSSM3VerifyBytes(Data: TBytes; Signature: TCnMOTSSM3Signature;
  PublicKey: TCnMOTSSM3PublicKey): Boolean;
{* 根据明文与公钥验证字节数组的签名是否正确，返回验证是否成功}

// ========== Merkle 改进的 M-OTS，校验 0 的个数，结合 SHA256 杂凑算法 ============

function CnMOTSSHA256GenerateKeys(var PrivateKey: TCnMOTSSHA256PrivateKey;
  var PublicKey: TCnMOTSSHA256PublicKey): Boolean;
{* 生成一对基于 SHA256 杂凑算法的 M-OTS 签名公私钥，返回生成是否成功}

procedure CnMOTSSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnMOTSSHA256PrivateKey; var OutSignature: TCnMOTSSHA256Signature);
{* 根据私钥生成指定内存块数据的一次性杂凑签名。
  平时公布明文、签名值与公钥，有验证需求时，给验证方公布私钥}

function CnMOTSSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnMOTSSHA256Signature; PublicKey: TCnMOTSSHA256PublicKey): Boolean;
{* 根据明文与公钥验证指定内存块数据的签名是否正确，返回验证是否成功}

procedure CnMOTSSHA256SignBytes(Data: TBytes; PrivateKey: TCnMOTSSHA256PrivateKey;
  var OutSignature: TCnMOTSSHA256Signature);
{* 根据私钥生成字节数组的一次性杂凑签名。
  平时公布明文、签名值与公钥，有验证需求时，给验证方公布私钥}

function CnMOTSSHA256VerifyBytes(Data: TBytes; Signature: TCnMOTSSHA256Signature;
  PublicKey: TCnMOTSSHA256PublicKey): Boolean;
{* 根据明文与公钥验证字节数组的签名是否正确，返回验证是否成功}

// ===== Winternitz 改进的 W-OTS，取 n = 8 也即 1 字节，结合 SM3 杂凑算法 ======

function CnWOTSSM3GenerateKeys(var PrivateKey: TCnWOTSSM3PrivateKey;
  var PublicKey: TCnWOTSSM3PublicKey): Boolean;
{* 生成一对基于 SM3 杂凑算法的 W-OTS 一次性杂凑签名公私钥，返回生成是否成功}

procedure CnWOTSSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnWOTSSM3PrivateKey; var OutSignature: TCnWOTSSM3Signature);
{* 根据私钥生成指定内存块数据的一次性杂凑签名及验证这个签名的密钥。
  平时公布明文、签名值，有验证需求时，给验证方公布公钥}

function CnWOTSSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnWOTSSM3Signature; PublicKey: TCnWOTSSM3PublicKey): Boolean;
{* 根据明文、公布的公钥验证指定内存块数据的签名是否正确，返回验证是否成功}

procedure CnWOTSSM3SignBytes(Data: TBytes; PrivateKey: TCnWOTSSM3PrivateKey;
  var OutSignature: TCnWOTSSM3Signature);
{* 根据私钥生成字节数组的一次性杂凑签名及验证这个签名的密钥。
  平时公布明文、签名值，有验证需求时，给验证方公布公钥}

function CnWOTSSM3VerifyBytes(Data: TBytes; Signature: TCnWOTSSM3Signature;
  PublicKey: TCnWOTSSM3PublicKey): Boolean;
{* 根据明文、公布的公钥验证指定内存块数据的签名是否正确，返回验证是否成功}

// ==== Winternitz 改进的 W-OTS，取 n = 8 也即 1 字节，结合 SHA256 杂凑算法 ====

function CnWOTSSHA256GenerateKeys(var PrivateKey: TCnWOTSSHA256PrivateKey;
  var PublicKey: TCnWOTSSHA256PublicKey): Boolean;
{* 生成一对基于 SHA256 杂凑算法的 W-OTS 一次性杂凑签名公私钥，返回生成是否成功}

procedure CnWOTSSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnWOTSSHA256PrivateKey; var OutSignature: TCnWOTSSHA256Signature);
{* 根据私钥生成指定内存块数据的一次性杂凑签名及验证这个签名的密钥。
  平时公布明文、签名值，有验证需求时，给验证方公布公钥}

function CnWOTSSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnWOTSSHA256Signature; PublicKey: TCnWOTSSHA256PublicKey): Boolean;
{* 根据明文、公布的公钥验证指定内存块数据的签名是否正确，返回验证是否成功}

procedure CnWOTSSHA256SignBytes(Data: TBytes; PrivateKey: TCnWOTSSHA256PrivateKey;
  var OutSignature: TCnWOTSSHA256Signature);
{* 根据私钥生成字节数组的一次性杂凑签名及验证这个签名的密钥。
  平时公布明文、签名值，有验证需求时，给验证方公布公钥}

function CnWOTSSHA256VerifyBytes(Data: TBytes; Signature: TCnWOTSSHA256Signature;
  PublicKey: TCnWOTSSHA256PublicKey): Boolean;
{* 根据明文、公布的公钥验证指定内存块数据的签名是否正确，返回验证是否成功}

implementation

const
  CN_WOTS_ROUND = 256;

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

function CnMOTSSM3GenerateKeys(var PrivateKey: TCnMOTSSM3PrivateKey;
  var PublicKey: TCnMOTSSM3PublicKey): Boolean;
var
  I: Integer;
begin
  Result := CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnMOTSSM3PrivateKey));
  if Result then
    for I := Low(TCnMOTSSM3PublicKey) to High(TCnMOTSSM3PublicKey) do
      PublicKey[I] := SM3(@PrivateKey[I], SizeOf(TCnSM3Digest));
end;

procedure CnMOTSSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnMOTSSM3PrivateKey; var OutSignature: TCnMOTSSM3Signature);
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig: TCnSM3Digest;
  Cnt: Byte;
begin
  FillChar(OutSignature[0], SizeOf(TCnMOTSSM3Signature), 0);
  Dig := SM3(PAnsiChar(Data), DataByteLen);

  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSM3Digest));

    Cnt := 0;
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 杂凑值的对应位是 1，签名搁私钥
        OutSignature[I] := PrivateKey[I]
      else
        Inc(Cnt);         // 杂凑不会全 0 所以不会溢出
    end;

    Bits.Clear;
    Bits.AppendByte(Cnt);
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 校验和的对应位是 1，签名继续搁私钥
        OutSignature[(SizeOf(TCnSM3Digest) * 8) + I] := PrivateKey[(SizeOf(TCnSM3Digest) * 8) + I];
    end;
  finally
    Bits.Free;
  end;
end;

function CnMOTSSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnMOTSSM3Signature; PublicKey: TCnMOTSSM3PublicKey): Boolean;
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig, Zero, Cmp: TCnSM3Digest;
  Cnt: Byte;
begin
  Result := False;
  FillChar(Zero[0], SizeOf(TCnSM3Digest), 0);
  Dig := SM3(PAnsiChar(Data), DataByteLen);

  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSM3Digest));

    Cnt := 0;
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 杂凑值的对应位是 1，签名是私钥，杂凑验证
      begin
        Cmp := SM3(@Signature[I], SizeOf(TCnSM3Digest)); // 计算私钥的杂凑值
        if not SM3Match(Cmp, PublicKey[I]) then
          Exit;
      end
      else
      begin
        if not SM3Match(Signature[I], Zero) then         // 对应位 0 得保证全 0
          Exit;
        Inc(Cnt);         // 杂凑不会全 0 所以不会溢出
      end;
    end;

    Bits.Clear;
    Bits.AppendByte(Cnt);
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 校验和的对应位是 1，签名继续是私钥，杂凑验证
      begin
        Cmp := SM3(@Signature[(SizeOf(TCnSM3Digest) * 8) + I], SizeOf(TCnSM3Digest));
        if not SM3Match(Cmp, PublicKey[(SizeOf(TCnSM3Digest) * 8) + I]) then
          Exit;
      end
      else
      begin
        if not SM3Match(Signature[(SizeOf(TCnSM3Digest) * 8) + I], Zero) then         // 对应位 0 得保证全 0
          Exit;
      end;
    end;
    Result := True;
  finally
    Bits.Free;
  end;
end;

procedure CnMOTSSM3SignBytes(Data: TBytes; PrivateKey: TCnMOTSSM3PrivateKey;
  var OutSignature: TCnMOTSSM3Signature);
begin
  if Length(Data) = 0 then
    CnMOTSSM3SignData(nil, 0, PrivateKey, OutSignature)
  else
    CnMOTSSM3SignData(@Data[0], Length(Data), PrivateKey, OutSignature);
end;

function CnMOTSSM3VerifyBytes(Data: TBytes; Signature: TCnMOTSSM3Signature;
  PublicKey: TCnMOTSSM3PublicKey): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnMOTSSM3VerifyData(nil, 0, Signature, PublicKey)
  else
    Result := CnMOTSSM3VerifyData(@Data[0], Length(Data), Signature, PublicKey);
end;

function CnMOTSSHA256GenerateKeys(var PrivateKey: TCnMOTSSHA256PrivateKey;
  var PublicKey: TCnMOTSSHA256PublicKey): Boolean;
var
  I: Integer;
  P: Pointer;
begin
  Result := CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnMOTSSHA256PrivateKey));
  if Result then
  begin
    for I := Low(TCnMOTSSHA256PublicKey) to High(TCnMOTSSHA256PublicKey) do
    begin
      P := @PrivateKey[I];
      PublicKey[I] := SHA256Buffer(P, SizeOf(TCnSHA256Digest));
    end;
  end;
end;

procedure CnMOTSSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnMOTSSHA256PrivateKey; var OutSignature: TCnMOTSSHA256Signature);
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig: TCnSHA256Digest;
  Cnt: Byte;
begin
  FillChar(OutSignature[0], SizeOf(TCnMOTSSHA256Signature), 0);
  Dig := SHA256Buffer(PAnsiChar(Data), DataByteLen);

  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSHA256Digest));

    Cnt := 0;
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 杂凑值的对应位是 1，签名搁私钥
        OutSignature[I] := PrivateKey[I]
      else
        Inc(Cnt);         // 杂凑不会全 0 所以不会溢出
    end;

    Bits.Clear;
    Bits.AppendByte(Cnt);
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 校验和的对应位是 1，签名继续搁私钥
        OutSignature[(SizeOf(TCnSHA256Digest) * 8) + I] := PrivateKey[(SizeOf(TCnSHA256Digest) * 8) + I];
    end;
  finally
    Bits.Free;
  end;
end;

function CnMOTSSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnMOTSSHA256Signature; PublicKey: TCnMOTSSHA256PublicKey): Boolean;
var
  I: Integer;
  Bits: TCnBitBuilder;
  Dig, Zero, Cmp: TCnSHA256Digest;
  Cnt: Byte;
  P: Pointer;
begin
  Result := False;
  FillChar(Zero[0], SizeOf(TCnSHA256Digest), 0);
  Dig := SHA256Buffer(PAnsiChar(Data), DataByteLen);

  Bits := TCnBitBuilder.Create;
  try
    Bits.AppendData(@Dig[0], SizeOf(TCnSHA256Digest));

    Cnt := 0;
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 杂凑值的对应位是 1，签名是私钥，杂凑验证
      begin
        P := @Signature[I];
        Cmp := SHA256Buffer(P, SizeOf(TCnSHA256Digest)); // 计算私钥的杂凑值
        if not SHA256Match(Cmp, PublicKey[I]) then
          Exit;
      end
      else
      begin
        if not SHA256Match(Signature[I], Zero) then         // 对应位 0 得保证全 0
          Exit;
        Inc(Cnt);         // 杂凑不会全 0 所以不会溢出
      end;
    end;

    Bits.Clear;
    Bits.AppendByte(Cnt);
    for I := 0 to Bits.BitLength - 1 do
    begin
      if Bits.Bit[I] then // 校验和的对应位是 1，签名继续是私钥，杂凑验证
      begin
        P := @Signature[(SizeOf(TCnSHA256Digest) * 8) + I];
        Cmp := SHA256Buffer(P, SizeOf(TCnSHA256Digest));
        if not SHA256Match(Cmp, PublicKey[(SizeOf(TCnSHA256Digest) * 8) + I]) then
          Exit;
      end
      else
      begin
        if not SHA256Match(Signature[(SizeOf(TCnSHA256Digest) * 8) + I], Zero) then         // 对应位 0 得保证全 0
          Exit;
      end;
    end;
    Result := True;
  finally
    Bits.Free;
  end;
end;

procedure CnMOTSSHA256SignBytes(Data: TBytes; PrivateKey: TCnMOTSSHA256PrivateKey;
  var OutSignature: TCnMOTSSHA256Signature);
begin
  if Length(Data) = 0 then
    CnMOTSSHA256SignData(nil, 0, PrivateKey, OutSignature)
  else
    CnMOTSSHA256SignData(@Data[0], Length(Data), PrivateKey, OutSignature);
end;

function CnMOTSSHA256VerifyBytes(Data: TBytes; Signature: TCnMOTSSHA256Signature;
  PublicKey: TCnMOTSSHA256PublicKey): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnMOTSSHA256VerifyData(nil, 0, Signature, PublicKey)
  else
    Result := CnMOTSSHA256VerifyData(@Data[0], Length(Data), Signature, PublicKey);
end;

function CnWOTSSM3GenerateKeys(var PrivateKey: TCnWOTSSM3PrivateKey;
  var PublicKey: TCnWOTSSM3PublicKey): Boolean;
var
  I, J: Integer;
  Dig: TCnSM3Digest;
begin
  Result := CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnWOTSSM3PrivateKey));
  if Result then
  begin
    for I := Low(TCnWOTSSM3PublicKey) to High(TCnWOTSSM3PublicKey) do
    begin
      Dig := PrivateKey[I];
      for J := 0 to CN_WOTS_ROUND - 1 do
        Dig := SM3(@Dig[0], SizeOf(TCnSM3Digest));

      PublicKey[I] := Dig;
    end;
  end;
end;

procedure CnWOTSSM3SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnWOTSSM3PrivateKey; var OutSignature: TCnWOTSSM3Signature);
var
  I, J: Integer;
  Dig, D: TCnSM3Digest;
  P: PByte;
  Sum, B: Word;
begin
  Dig := SM3(PAnsiChar(Data), DataByteLen);
  Sum := 0;

  for I := 0 to SizeOf(TCnSM3Digest) - 1 do
  begin
    D := PrivateKey[I];
    B := CN_WOTS_ROUND - Dig[I];             // 避免 Byte 溢出，要用 Word

    for J := 0 to B - 1 do
      D := SM3(@D[0], SizeOf(TCnSM3Digest)); // 根据字节数，用私钥计算 256 - 每个字节的杂凑次数

    OutSignature[I] := D;
    Sum := Sum + Dig[I];
  end;

  // 对两位校验和也同样计算
  Sum := UInt16HostToNetwork(Sum);
  P := PByte(@Sum);

  D := PrivateKey[High(TCnSM3Digest) + 1];
  B := CN_WOTS_ROUND - P^;
  for J := 0 to B - 1 do
    D := SM3(@D[0], SizeOf(TCnSM3Digest));
  OutSignature[High(TCnSM3Digest) + 1] := D;

  Inc(P);
  D := PrivateKey[High(TCnSM3Digest) + 2];
  B := CN_WOTS_ROUND - P^;
  for J := 0 to B - 1 do
    D := SM3(@D[0], SizeOf(TCnSM3Digest));
  OutSignature[High(TCnSM3Digest) + 2] := D;
end;

function CnWOTSSM3VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnWOTSSM3Signature; PublicKey: TCnWOTSSM3PublicKey): Boolean;
var
  I, J: Integer;
  Dig, D: TCnSM3Digest;
  P: PByte;
  Sum, B: Word;
begin
  Result := False;

  Dig := SM3(PAnsiChar(Data), DataByteLen);
  Sum := 0;

  for I := 0 to SizeOf(TCnSM3Digest) - 1 do
  begin
    D := Signature[I];
    B := Dig[I];                             // 避免 Byte 溢出，要用 Word

    for J := 0 to B - 1 do
      D := SM3(@D[0], SizeOf(TCnSM3Digest)); // 根据字节数，用私钥计算每个字节的杂凑次数

    if not SM3Match(D, PublicKey[I]) then
      Exit;

    Sum := Sum + Dig[I];
  end;

  // 对两位校验和也同样计算
  Sum := UInt16HostToNetwork(Sum);
  P := PByte(@Sum);

  D := Signature[High(TCnSM3Digest) + 1];
  B := P^;
  for J := 0 to B - 1 do
    D := SM3(@D[0], SizeOf(TCnSM3Digest));

  if not SM3Match(D, PublicKey[High(TCnSM3Digest) + 1]) then
    Exit;

  Inc(P);
  D := Signature[High(TCnSM3Digest) + 2];
  B := P^;
  for J := 0 to B - 1 do
    D := SM3(@D[0], SizeOf(TCnSM3Digest));

  if not SM3Match(D, PublicKey[High(TCnSM3Digest) + 2]) then
    Exit;

  Result := True;
end;

procedure CnWOTSSM3SignBytes(Data: TBytes; PrivateKey: TCnWOTSSM3PrivateKey;
  var OutSignature: TCnWOTSSM3Signature);
begin
  if Length(Data) = 0 then
    CnWOTSSM3SignData(nil, 0, PrivateKey, OutSignature)
  else
    CnWOTSSM3SignData(@Data[0], Length(Data), PrivateKey, OutSignature);
end;

function CnWOTSSM3VerifyBytes(Data: TBytes; Signature: TCnWOTSSM3Signature;
  PublicKey: TCnWOTSSM3PublicKey): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnWOTSSM3VerifyData(nil, 0, Signature, PublicKey)
  else
    Result := CnWOTSSM3VerifyData(@Data[0], Length(Data), Signature, PublicKey);
end;

function CnWOTSSHA256GenerateKeys(var PrivateKey: TCnWOTSSHA256PrivateKey;
  var PublicKey: TCnWOTSSHA256PublicKey): Boolean;
var
  I, J: Integer;
  Dig: TCnSHA256Digest;
  P: Pointer;
begin
  Result := CnRandomFillBytes(@PrivateKey[0], SizeOf(TCnWOTSSHA256PrivateKey));
  if Result then
  begin
    for I := Low(TCnWOTSSHA256PublicKey) to High(TCnWOTSSHA256PublicKey) do
    begin
      Dig := PrivateKey[I];
      for J := 0 to CN_WOTS_ROUND - 1 do
      begin
        P := @Dig[0];
        Dig := SHA256Buffer(P, SizeOf(TCnSHA256Digest));
      end;

      PublicKey[I] := Dig;
    end;
  end;
end;

procedure CnWOTSSHA256SignData(Data: Pointer; DataByteLen: Integer;
  PrivateKey: TCnWOTSSHA256PrivateKey; var OutSignature: TCnWOTSSHA256Signature);
var
  I, J: Integer;
  Dig, D: TCnSHA256Digest;
  P: PByte;
  Sum, B: Word;
  PB: Pointer;
begin
  Dig := SHA256Buffer(PAnsiChar(Data), DataByteLen);
  Sum := 0;

  for I := 0 to SizeOf(TCnSHA256Digest) - 1 do
  begin
    D := PrivateKey[I];
    B := CN_WOTS_ROUND - Dig[I];             // 避免 Byte 溢出，要用 Word

    for J := 0 to B - 1 do
    begin
      PB := @D[0];
      D := SHA256Buffer(PB, SizeOf(TCnSHA256Digest)); // 根据字节数，用私钥计算 256 - 每个字节的杂凑次数
    end;

    OutSignature[I] := D;
    Sum := Sum + Dig[I];
  end;

  // 对两位校验和也同样计算
  Sum := UInt16HostToNetwork(Sum);
  P := PByte(@Sum);

  D := PrivateKey[High(TCnSHA256Digest) + 1];
  B := CN_WOTS_ROUND - P^;
  for J := 0 to B - 1 do
  begin
    PB := @D[0];
    D := SHA256Buffer(PB, SizeOf(TCnSHA256Digest));
  end;
  OutSignature[High(TCnSHA256Digest) + 1] := D;

  Inc(P);
  D := PrivateKey[High(TCnSHA256Digest) + 2];
  B := CN_WOTS_ROUND - P^;
  for J := 0 to B - 1 do
  begin
    PB := @D[0];
    D := SHA256Buffer(PB, SizeOf(TCnSHA256Digest));
  end;
  OutSignature[High(TCnSHA256Digest) + 2] := D;
end;

function CnWOTSSHA256VerifyData(Data: Pointer; DataByteLen: Integer;
  Signature: TCnWOTSSHA256Signature; PublicKey: TCnWOTSSHA256PublicKey): Boolean;
var
  I, J: Integer;
  Dig, D: TCnSHA256Digest;
  P: PByte;
  Sum, B: Word;
  PB: Pointer;
begin
  Result := False;

  Dig := SHA256Buffer(PAnsiChar(Data), DataByteLen);
  Sum := 0;

  for I := 0 to SizeOf(TCnSHA256Digest) - 1 do
  begin
    D := Signature[I];
    B := Dig[I];                             // 避免 Byte 溢出，要用 Word

    for J := 0 to B - 1 do
    begin
      PB := @D[0];
      D := SHA256Buffer(PB, SizeOf(TCnSHA256Digest)); // 根据字节数，用私钥计算每个字节的杂凑次数
    end;

    if not SHA256Match(D, PublicKey[I]) then
      Exit;

    Sum := Sum + Dig[I];
  end;

  // 对两位校验和也同样计算
  Sum := UInt16HostToNetwork(Sum);
  P := PByte(@Sum);

  D := Signature[High(TCnSHA256Digest) + 1];
  B := P^;
  for J := 0 to B - 1 do
  begin
    PB := @D[0];
    D := SHA256Buffer(PB, SizeOf(TCnSHA256Digest));
  end;

  if not SHA256Match(D, PublicKey[High(TCnSHA256Digest) + 1]) then
    Exit;

  Inc(P);
  D := Signature[High(TCnSHA256Digest) + 2];
  B := P^;
  for J := 0 to B - 1 do
  begin
    PB := @D[0];
    D := SHA256Buffer(PB, SizeOf(TCnSHA256Digest));
  end;

  if not SHA256Match(D, PublicKey[High(TCnSHA256Digest) + 2]) then
    Exit;

  Result := True;
end;

procedure CnWOTSSHA256SignBytes(Data: TBytes; PrivateKey: TCnWOTSSHA256PrivateKey;
  var OutSignature: TCnWOTSSHA256Signature);
begin
  if Length(Data) = 0 then
    CnWOTSSHA256SignData(nil, 0, PrivateKey, OutSignature)
  else
    CnWOTSSHA256SignData(@Data[0], Length(Data), PrivateKey, OutSignature);
end;

function CnWOTSSHA256VerifyBytes(Data: TBytes; Signature: TCnWOTSSHA256Signature;
  PublicKey: TCnWOTSSHA256PublicKey): Boolean;
begin
  if Length(Data) = 0 then
    Result := CnWOTSSHA256VerifyData(nil, 0, Signature, PublicKey)
  else
    Result := CnWOTSSHA256VerifyData(@Data[0], Length(Data), Signature, PublicKey);
end;

end.
