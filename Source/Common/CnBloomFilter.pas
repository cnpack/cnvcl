{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
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

unit CnBloomFilter;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：多哈希函数映射的快速查找 BloomFilter 实现单元
* 单元作者：刘啸
* 备    注：哈希函数数量 = ln2 * (Bit 长度 / 字符串容量) 时判断最优，但数据量大时
*               哈希函数数量不宜过大，以免计算耗时。所以分六个档次：
*               1、哈希函数数量 4，比率 32，用于小规模数据，计算量小，单位内存大
*               2、哈希函数数量 6，比率 30
*               3、哈希函数数量 8，比率 25
*               4、哈希函数数量 10，比率 20
*               5、哈希函数数量 12，比率 18
*               6、哈希函数数量 15，比率 15，用于大规模数据，计算量大，单位内存小
*               多哈希函数采用 CRC32 及不同的初始值来做
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2015.05.22 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, CnCRC32;

const
  CN_LN_2 = 0.69314718;

  CN_BLOOM_HASH_CRC32S: array[0..15] of DWORD =
    ($00000000, $11111111, $22222222, $33333333, $44444444,
     $55555555, $66666666, $77777777, $88888888, $99999999,
     $AAAAAAAA, $BBBBBBBB, $CCCCCCCC, $DDDDDDDD, $EEEEEEEE,
     $FFFFFFFF);

type
  ECnBloomFilterSizeException = class(Exception);

  TCnBloomFilterCapacity = (bfc10Power3, bfc10Power4, bfc10Power5, bfc10Power6,
    bfc10Power7, bfc10Power8);

  TCnHashStringFunc = function (const Str: string): Cardinal;

  TCnStringBloomFilter = class
  {* 字符串 Bloom Filter 多哈希快速查找单元}
  private
    FBits: TBits;
    FHashFuncCount: Integer;
    FHashResults: array of Cardinal;
    FBitSize: Cardinal;
    FCapacity: Integer;
    FCount: Integer;
    procedure CalcSize(ACapacity: TCnBloomFilterCapacity);
  public
    constructor Create(ACapacity: TCnBloomFilterCapacity = bfc10Power4);
    destructor Destroy; override;

    function StrExists(const Str: string): Boolean;
    {* 检测字符串是否存在}
    function AddString(const Str: string): Boolean;
    {* 添加一个字符串的哈希结果}
    property Count: Integer read FCount;
    {* 添加的字符串数量}
  end;

implementation

{ TCnStringBloomFilter }

function TCnStringBloomFilter.AddString(const Str: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Str = '' then
    Exit;

  for I := 0 to FHashFuncCount - 1 do
  begin
    FHashResults[I] := StrCRC32(CN_BLOOM_HASH_CRC32S[I], Str) mod FBitSize;
    FBits[FHashResults[I]] := True;
  end;
  Inc(FCount);
  Result := True;
end;

procedure TCnStringBloomFilter.CalcSize(ACapacity: TCnBloomFilterCapacity);
begin
  case ACapacity of
    bfc10Power3:
      begin
        FHashFuncCount := 4;
        FCapacity := 1000;
        FBitSize := FCapacity * 32;
      end;
    bfc10Power4:
      begin
        FHashFuncCount := 6;
        FCapacity := 10000;
        FBitSize := FCapacity * 30;
      end;
    bfc10Power5:
      begin
        FHashFuncCount := 8;
        FCapacity := 100000;
        FBitSize := FCapacity * 25
      end;
    bfc10Power6:
      begin
        FHashFuncCount := 10;
        FCapacity := 1000000;
        FBitSize := FCapacity * 20;
      end;
    bfc10Power7:
      begin
        FHashFuncCount := 12;
        FCapacity := 10000000;
        FBitSize := FCapacity * 18;
      end;
    bfc10Power8:
      begin
        FHashFuncCount := 15;
        FCapacity := 100000000;
        FBitSize := FCapacity * 15;
      end;
  else
    raise ECnBloomFilterSizeException.Create('NO Proper Size Specified.');
  end;

  SetLength(FHashResults, FHashFuncCount);
end;

constructor TCnStringBloomFilter.Create(ACapacity: TCnBloomFilterCapacity);
begin
  inherited Create;
  CalcSize(ACapacity);
  FBits := TBits.Create;
  FBits.Size := FBitSize;
end;

destructor TCnStringBloomFilter.Destroy;
begin
  FBits.Free;
  inherited;
end;

function TCnStringBloomFilter.StrExists(const Str: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FHashFuncCount - 1 do
  begin
    FHashResults[I] := StrCRC32(CN_BLOOM_HASH_CRC32S[I], Str) mod FBitSize;
    if not FBits[FHashResults[I]] then
      Exit;
  end;
  Result := True;
end;

end.
