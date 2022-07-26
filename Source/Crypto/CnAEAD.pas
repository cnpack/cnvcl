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

unit CnAEAD;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：各类 AEAD 实现单元
* 单元作者：刘啸（Liu Xiao）
* 备    注：AEAD 是关联数据认证加密的简称。可以用密码、关联数据与初始化向量等对
*           数据进行加密与生成验证内容，解密时如果验证内容通不过则失败。
*           注：字节串转换为大整数时相当于大端表达法，且大下标指向高位地址
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWinXP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2022.07.23 V1.0
*               创建单元。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, CnNative;

const
  GHASH_BLOCK = 16;

type
  TGHash128Buffer = array[0..GHASH_BLOCK - 1] of Byte;

  TGHash128Key = array[0..GHASH_BLOCK - 1] of Byte;

  TGHash128Iv  = array[0..GHASH_BLOCK - 1] of Byte;

  TGHash128Tag    = array[0..GHASH_BLOCK - 1] of Byte;

procedure GMulBlock128(var X, Y: TGHash128Buffer; var R: TGHash128Buffer);
{* 实现 GHash 中的伽罗华域 (2^128) 上的块乘法操作。基本测试通过，也符合交换律
  注意 2 次幂有限域乘法里的加法是模 2 加也就是异或（也等同于模 2 减）
  同时还要模一个模多项式 GHASH_POLY，其中的单次减同样也即异或}

function GHash128(var HashKey: TGHash128Key; Data: Pointer; DataByteLength: Integer;
  AAD: Pointer; AADByteLength: Integer): TGHash128Tag;
{* 以指定 HashKey 与附加数据 AAD，对一块数据进行 GHash 计算得到 Tag 摘要，
  对应文档中的 GHash(H, A C)}

function GHash128Bytes(var HashKey: TGHash128Key; Data, AAD: TBytes): TGHash128Tag;
{* 字节数组方式进行 GHash 计算，内部调用 GHash128}


implementation

const
  GHASH_POLY: TGHash128Buffer = ($E1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

{
  GCM 使用的 Galois(2^128) 域内的乘法

  一般来说对于字节串，例如 $FEDC，其 BIT 顺序就应该是 FEDC，也就是 1111111011011100，
  GCM 中，它对应的整数也等同于这串二进制的常规表达。
  然后 LSB 指右边的低位的，MSB 指左边高位的，左右移位符合常规习惯。
  开发过程中碰到问题：下标 0 是指最左还是最右？
  GCM 用的模多项式是 128,7,2,1,0，而算法中的表达是 E1000000000……，说明下标是右边高位
  因此，127 下标是整数高位，右移是往整数的高位方向移。
  同时，127 也对应着地址高位（0 是地址低位），右移是往地址高位方向移
}
procedure GMulBlock128(var X, Y: TGHash128Buffer; var R: TGHash128Buffer);
var
  I: Integer;
  Z, V: TGHash128Buffer;
  B: Boolean;

  // 注意此处判断字节内 Bit 的顺序也是字节内的高位是 0，
  function GHashIsBitSet(AMem: Pointer; N: Integer): Boolean;
  var
    P: PCnByte;
    A1, B1: Integer;
    V: Byte;
  begin
    A1 := N div 8;
    B1 := 7 - (N mod 8);
    P := PCnByte(TCnNativeInt(AMem) + A1);

    V := Byte(1 shl B1);
    Result := (P^ and V) <> 0;
  end;

begin
  FillChar(Z[0], SizeOf(TGHash128Buffer), 0);
  Move(X[0], V[0], SizeOf(TGHash128Buffer));

  for I := 0 to 127 do
  begin
    if GHashIsBitSet(@Y[0], I) then
      MemoryXor(@Z[0], @V[0], SizeOf(TGHash128Buffer), @Z[0]);

    B := GHashIsBitSet(@V[0], 127); // 判断大整数的高位是否是 1
    MemoryShiftRight(@V[0], nil, SizeOf(TGHash128Buffer), 1);
    if B then
      MemoryXor(@V[0], @GHASH_POLY[0], SizeOf(TGHash128Buffer), @V[0]);
  end;
  Move(Z[0], R[0], SizeOf(TGHash128Buffer));
end;

function GHash128(var HashKey: TGHash128Key; Data: Pointer; DataByteLength: Integer;
  AAD: Pointer; AADByteLength: Integer): TGHash128Tag;
var
  AL, DL: Integer;
  AL64, DL64: Int64;
  X, Y, H: TGHash128Buffer;
begin
  // 对比 GHash(H, A, C)，Data 是 C，AAD 是 A，Key 是 H
  // 按 16 字节分，C 有 m 块，A 有 n 块（末块都可能不满），
  // 共有 m + n 轮针对数据的 GaloisMulBlock，再加一轮位长度

  FillChar(X[0], SizeOf(TGHash128Buffer), 0);  // 初始全 0
  Move(HashKey[0], H[0], SizeOf(TGHash128Buffer));

  AL := AADByteLength;
  DL := DataByteLength;
  if Data = nil then
    DL := 0;
  if AAD = nil then
    AL := 0;

  // 算整块 A
  while AL >= GHASH_BLOCK do
  begin
    Move(AAD^, Y[0], GHASH_BLOCK);

    MemoryXor(@Y[0], @X[0], SizeOf(TGHash128Buffer), @Y[0]);
    GMulBlock128(Y, H, X);  // 一轮计算结果再次放入 X

    AAD := Pointer(TCnNativeInt(AAD) + GHASH_BLOCK);
    Dec(AL, GHASH_BLOCK);
  end;

  // 算余块 A，如果有的话
  if AL > 0 then
  begin
    FillChar(Y[0], SizeOf(TGHash128Buffer), 0);
    Move(AAD^, Y[0], AL);

    MemoryXor(@Y[0], @X[0], SizeOf(TGHash128Buffer), @Y[0]);
    GMulBlock128(Y, H, X);
  end;

  // 算整块 C
  while DL >= GHASH_BLOCK do
  begin
    Move(Data^, Y[0], GHASH_BLOCK);

    MemoryXor(@Y[0], @X[0], SizeOf(TGHash128Buffer), @Y[0]);
    GMulBlock128(Y, H, X);  // 一轮计算结果再次放入 X

    Data := Pointer(TCnNativeInt(Data) + GHASH_BLOCK);
    Dec(DL, GHASH_BLOCK);
  end;

  // 算余块 C，如果有的话
  if DL > 0 then
  begin
    FillChar(Y[0], SizeOf(TGHash128Buffer), 0);
    Move(Data^, Y[0], DL);

    MemoryXor(@Y[0], @X[0], SizeOf(TGHash128Buffer), @Y[0]);
    GMulBlock128(Y, H, X);
  end;

  // 最后再算一轮长度，A 和 C 各四字节拼起来
  FillChar(Y[0], SizeOf(TGHash128Buffer), 0);
  AL64 := Int64ToBigEndian(AADByteLength * 8);
  DL64 := Int64ToBigEndian(DataByteLength * 8);

  Move(AL64, Y[0], SizeOf(Int64));
  Move(DL64, Y[SizeOf(Int64)], SizeOf(Int64));

  MemoryXor(@Y[0], @X[0], SizeOf(TGHash128Buffer), @Y[0]);
  GMulBlock128(Y, H, X); // 再乘一轮

  Move(X[0], Result[0], SizeOf(TGHash128Tag));
end;

function GHash128Bytes(var HashKey: TGHash128Key; Data, AAD: TBytes): TGHash128Tag;
var
  C, A: Pointer;
begin
  if Data = nil then
    C := nil
  else
    C := @Data[0];

  if AAD = nil then
    A := nil
  else
    A := @AAD[0];

  Result := GHash128(HashKey, C, Length(Data), A, Length(AAD));
end;

end.
