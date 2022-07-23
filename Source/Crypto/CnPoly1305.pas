{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
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

unit CnPoly1305;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：Poly1305 消息认证算法实现单元
* 单元作者：刘啸（liuxiao@cnpack.org)
* 备    注：根据 RFC 7539 实现
*           输入为任意长度数据与 32 字节密钥，输出 16 字节杂凑值
* 开发平台：Windows 7 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.07.19 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, CnNative, CnBigNumber;

const
  POLY1305_KEYSIZE   = 32;       // 输入 32 字节也就是 256 位的 Key

  POLY1305_BLOCKSIZE = 16;       // 内部分块，每块 16 字节

  POLY1305_DIGSIZE   = 16;       // 输出 16 字节也就是 128 位的摘要

type
  TPoly1305Key = array[0..POLY1305_KEYSIZE - 1] of Byte;
  {* Poly1305 算法的 Key}

  TPoly1305Digest = array[0..POLY1305_DIGSIZE - 1] of Byte;
  {* Poly1305 算法的杂凑结果}

function Poly1305Bytes(Data: TBytes; Key: TBytes): TPoly1305Digest;
{* 计算字节数组的 Poly1305 杂凑值}

function Poly1305Data(Data: Pointer; DataByteLength: Integer;
  Key: TPoly1305Key): TPoly1305Digest;
{* 计算数据块的 Poly1305 杂凑值}

function Poly1305Print(const Digest: TPoly1305Digest): string;
{* 以十六进制格式输出 Poly1305 计算值}

implementation

var
  Prime: TCnBigNumber = nil; // Poly1305 使用的素数
  Clamp: TCnBigNumber = nil; // Poly1305 使用的 Clamp

function Poly1305Bytes(Data: TBytes; Key: TBytes): TPoly1305Digest;
var
  AKey: TPoly1305Key;
  L: Integer;
begin
  FillChar(AKey[0], SizeOf(TPoly1305Key), 0);
  L := Length(Key);
  if L > SizeOf(TPoly1305Key) then
    L := SizeOf(TPoly1305Key);

  Move(Key[0], AKey[0], L);
  Result := Poly1305Data(@Data[0], Length(Data), AKey);
end;

function Poly1305Data(Data: Pointer; DataByteLength: Integer;
  Key: TPoly1305Key): TPoly1305Digest;
var
  I, B, L: Integer;
  R, S, A, N: TCnBigNumber;
  Buf: array[0..POLY1305_BLOCKSIZE] of Byte;
  P: PByteArray;
  RKey: TPoly1305Key;
begin
  Move(Key[0], RKey[0], SizeOf(TPoly1305Key));
  ReverseMemory(@RKey[0], POLY1305_BLOCKSIZE);
  ReverseMemory(@RKey[POLY1305_BLOCKSIZE], POLY1305_BLOCKSIZE);

  R := nil;
  S := nil;
  A := nil;
  N := nil;

  try
    R := TCnBigNumber.FromBinary(@RKey[0], POLY1305_BLOCKSIZE);
    BigNumberAnd(R, R, Clamp);

    S := TCnBigNumber.FromBinary(@RKey[POLY1305_BLOCKSIZE], POLY1305_BLOCKSIZE);

    A := TCnBigNumber.Create;
    A.SetZero;

    N := TCnBigNumber.Create;

    B := (DataByteLength + POLY1305_BLOCKSIZE - 1) div POLY1305_BLOCKSIZE;
    P := PByteArray(Data);

    for I := 1 to B do
    begin
      if I <> B then // 普通块，16 字节满的
        L := POLY1305_BLOCKSIZE
      else           // 尾块，可能不够 16 字节
        L := DataByteLength mod POLY1305_BLOCKSIZE;

      Move(P^[(I - 1) * POLY1305_BLOCKSIZE], Buf[0], L);  // 内容塞上
      Buf[L] := 1;                                        // 高字节再置个 1

      ReverseMemory(@Buf[0], L + 1);
      N.SetBinary(@Buf[0], L + 1);

      BigNumberAdd(A, A, N);
      BigNumberDirectMulMod(A, R, A, Prime);
    end;

    BigNumberAdd(A, A, S);
    BigNumberKeepLowBits(A, 8 * POLY1305_DIGSIZE);

    A.ToBinary(@Result[0], POLY1305_DIGSIZE);
    ReverseMemory(@Result[0], SizeOf(TPoly1305Digest));
  finally
    N.Free;
    A.Free;
    S.Free;
    R.Free;
  end;
end;

function Poly1305Print(const Digest: TPoly1305Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TPoly1305Digest));
end;

initialization
  Prime := TCnBigNumber.Create;
  Prime.SetOne;
  Prime.ShiftLeft(130);
  Prime.SubWord(5);

  Clamp := TCnBigNumber.FromHex('0FFFFFFC0FFFFFFC0FFFFFFC0FFFFFFF');

finalization
  Clamp.Free;
  Prime.Free;

end.
