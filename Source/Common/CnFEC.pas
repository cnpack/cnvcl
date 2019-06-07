{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2019 CnPack 开发组                       }
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

unit CnFEC;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：前向纠错实现单元，目前包括 Hamming （汉明码）校验
* 单元作者：刘啸（liuxiao@cnpack.org）
* 备    注：该单元目前只处理二进制位
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2019.05.28 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes;

type
  ECnHammingException = class(Exception);

procedure CnCalcHammingCode(InBits, OutBits: TBits; BlockBitCount: Integer = 8);
{* 根据一批 Bits 计算其 Hamming 码，默认分组 8 bit 也就是 1 字节}

procedure CnVerifyHammingCode(InBits, OutBits: TBits; BlockBitCount: Integer = 8);
{* 根据 Hamming 编码过的 Bits 还原并校验其内容，默认分组 8 bit 也就是 1 字节}

function CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount: Integer): Integer;
{* 根据 Hamming 分组的 bit 长度计算校验 bit 的长度}

implementation

// BlockBitCount (n), VerificationBitCount (k) 满足 2^k - 1 >= n + k
function CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount: Integer): Integer;
begin
  if BlockBitCount = 1 then
    Result := 2
  else if BlockBitCount in [2..4] then
    Result := 3
  else if BlockBitCount in [5..11] then
    Result := 4
  else if BlockBitCount in [12..26] then
    Result := 5
  else if BlockBitCount in [27..57] then
    Result := 6
  else if BlockBitCount in [58..120] then
    Result := 7
  else
    raise ECnHammingException.CreateFmt('Error Hamming BlockBitCount: %d', [BlockBitCount]);
end;

procedure CnCalcHammingCode(InBits, OutBits: TBits; BlockBitCount: Integer = 8);
var
  OffsetIn, OffsetOut, VerificationBitCount: Integer;

  procedure CalcHammingBlock(InStartOffset, OutStartOffset: Integer);
  const
    VERIFICATION_BITS_COUNT: array[0..7] of Integer = (1, 2, 4, 8, 16, 32, 64, 128);
    VERIFICATION_BITS: set of Byte = [0, 1, 3, 7, 15, 31, 63, 127];
  var
    InIdx, OutIdx, BitIdx: Integer;
    Ver: Boolean;
  begin
    InIdx := 0;
    OutIdx := 0;

    // 拆开输入数据并将其填到输出里，留出 Hamming 码校验位空间
    while InIdx < BlockBitCount do
    begin
      while OutIdx in VERIFICATION_BITS do
      begin
        OutBits.Bits[OutStartOffset + OutIdx] := False;
        Inc(OutIdx);
      end;
      OutBits.Bits[OutStartOffset + OutIdx] := InBits.Bits[InStartOffset + InIdx];
      Inc(InIdx);
      Inc(OutIdx);
    end;

    BitIdx := 0;

    // 计算多位 Hamming 码校验位并填进去，BitIdx 以 0 开始，VerificationBitCount - 1 结束，
    // 为了便于理解，OutIdx 所代表的下标均改成 1 开始
    while BitIdx < VerificationBitCount do
    begin
      // 对于 BitIdx 号 Hamming 校验码，计算方法是数据中，下标数字第 BitIdx 位为 1 的要参与异或，
      // 异或结果放下标为 VERIFICATION_BITS_COUNT[BitIdx] 中
      Ver := False;
      for OutIdx := 1 to BlockBitCount + VerificationBitCount do
      begin
        if (OutIdx and (1 shl BitIdx)) <> 0 then
          Ver := Ver xor OutBits.Bits[OutStartOffset + OutIdx - 1];
      end;
      OutBits.Bits[OutStartOffset + VERIFICATION_BITS_COUNT[BitIdx] - 1] := Ver;

      Inc(BitIdx);
    end;
  end;

begin
  VerificationBitCount := CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount);

  if (InBits = nil) or (InBits.Size <= 0) then
    raise ECnHammingException.Create('Error InBits Calculate Hamming.');

  if InBits.Size mod BlockBitCount <> 0 then
    raise ECnHammingException.CreateFmt('Error Padding Size %d for Block Bit Count %d.', [InBits.Size, BlockBitCount]);

  OutBits.Size := (InBits.Size div BlockBitCount) * (BlockBitCount + VerificationBitCount);
  OffsetIn := 0;
  OffsetOut := 0;

  while OffsetIn < InBits.Size - 1 do
  begin
    CalcHammingBlock(OffsetIn, OffsetOut);
    Inc(OffsetIn, BlockBitCount);
    Inc(OffsetOut, BlockBitCount + VerificationBitCount);
  end;
end;

procedure CnVerifyHammingCode(InBits, OutBits: TBits; BlockBitCount: Integer = 8);
var
  OffsetIn, OffsetOut, VerificationBitCount: Integer;

  procedure VerifyHammingBlock(InStartOffset, OutStartOffset: Integer);
  const
    VERIFICATION_BITS_COUNT: array[0..7] of Integer = (1, 2, 4, 8, 16, 32, 64, 128);
    VERIFICATION_BITS: set of Byte = [0, 1, 3, 7, 15, 31, 63, 127];
  var
    InIdx, OutIdx, BitIdx, ErrIdx: Integer;
    Ver: Boolean;
  begin
    BitIdx := 0;
    ErrIdx := 0;

    // 计算多位 Hamming 码校验位并填进去，BitIdx 以 0 开始，VerificationBitCount - 1 结束，
    // 为了便于理解，OutIdx 所代表的下标均改成 1 开始
    while BitIdx < VerificationBitCount do
    begin
      // 对于 BitIdx 号 Hamming 校验码，计算方法是数据中，下标数字第 BitIdx 位为 1 的要参与异或，
      // 异或结果放下标为 VERIFICATION_BITS_COUNT[BitIdx] 中
      Ver := False;
      for OutIdx := 1 to BlockBitCount + VerificationBitCount do
      begin
        if (OutIdx and (1 shl BitIdx)) <> 0 then
          Ver := Ver xor InBits.Bits[InStartOffset + OutIdx - 1];
      end;

      if Ver then  // 有错误，拼纠错位置
        ErrIdx := ErrIdx or (1 shl BitIdx);

      Inc(BitIdx);
    end;

    // 纠错一位码
    if ErrIdx <> 0 then
    begin
      InBits.Bits[InStartOffset + ErrIdx - 1] := not
        InBits.Bits[InStartOffset + ErrIdx - 1];
    end;

    InIdx := 0;
    OutIdx := 0;
    // 纠错完毕后，拆开输入数据并将其填到输出里
    while InIdx < BlockBitCount + VerificationBitCount do
    begin
      while InIdx in VERIFICATION_BITS do
        Inc(InIdx);

      OutBits.Bits[OutStartOffset + OutIdx] := InBits.Bits[InStartOffset + InIdx];
      Inc(InIdx);
      Inc(OutIdx);
    end;
  end;

begin
  VerificationBitCount := CnCalcHammingVerificationBitCountFromBlockBitCount(BlockBitCount);

  if (InBits = nil) or (InBits.Size <= 0) then
    raise ECnHammingException.Create('Error InBits Verify Hamming.');

  if InBits.Size mod (BlockBitCount + VerificationBitCount) <> 0 then
    raise ECnHammingException.CreateFmt('Error Padding Size %d for Verify Bit Count %d.', [InBits.Size, VerificationBitCount]);

  OutBits.Size := (InBits.Size div (VerificationBitCount + BlockBitCount)) * BlockBitCount;
  OffsetIn := 0;
  OffsetOut := 0;

  while OffsetIn < InBits.Size - 1 do
  begin
    VerifyHammingBlock(OffsetIn, OffsetOut);
    Inc(OffsetIn, BlockBitCount + VerificationBitCount);
    Inc(OffsetOut, BlockBitCount);
  end;
end;

end.
