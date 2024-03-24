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

unit CnPoly1305;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：Poly1305 消息认证算法实现单元
* 单元作者：刘啸（liuxiao@cnpack.org)
* 备    注：根据 RFC 7539 实现
*           输入为任意长度数据与 32 字节密钥，输出 16 字节杂凑值，发散性并不是很好
*           注意：由于 TCnBigNumber 使用的 Binary 均是网络字节顺序也就是大端
*           但 RFC 中又规定这里得小端因此代码中要手动调用 ReverseMemory
*           无论 CPU 是大端还是小端
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
  CN_POLY1305_KEYSIZE   = 32;
  {* Poly1305 算法的密码长度，输入 32 字节也就是 256 位的 Key}

  CN_POLY1305_BLOCKSIZE = 16;
  {* Poly1305 算法的内部分块长度，每块 16 字节}

  CN_POLY1305_DIGSIZE   = 16;
  {* Poly1305 算法的摘要输出长度，16 字节也就是 128 位}

type
  TCnPoly1305Key = array[0..CN_POLY1305_KEYSIZE - 1] of Byte;
  {* Poly1305 算法的 Key}

  TCnPoly1305Digest = array[0..CN_POLY1305_DIGSIZE - 1] of Byte;
  {* Poly1305 算法的杂凑结果}

  TCnPoly1305Context = class
  {* 分块计算 Poly1305 的上下文对象}
  private
    R: TCnBigNumber;
    S: TCnBigNumber;
    A: TCnBigNumber;
    N: TCnBigNumber;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function Poly1305Buffer(const Buffer; Count: Cardinal; Key: TCnPoly1305Key): TCnPoly1305Digest;
{* 对数据块进行 Poly1305 计算，Buffer 一般传个地址}

function Poly1305Bytes(Data: TBytes; Key: TBytes): TCnPoly1305Digest;
{* 计算字节数组的 Poly1305 杂凑值}

function Poly1305Data(Data: Pointer; DataByteLength: Cardinal;
  Key: TCnPoly1305Key): TCnPoly1305Digest;
{* 计算数据块的 Poly1305 杂凑值}

function Poly1305Print(const Digest: TCnPoly1305Digest): string;
{* 以十六进制格式输出 Poly1305 计算值}

function Poly1305Match(const D1, D2: TCnPoly1305Digest): Boolean;
{* 比较两个 Poly1305 计算值是否相等}

procedure Poly1305Init(out Context: TCnPoly1305Context; Key: TCnPoly1305Key);
{* 初始化一轮 Poly1305 计算上下文，内部创建 Context 准备计算 Poly1305 结果}

procedure Poly1305Update(Context: TCnPoly1305Context; Input: PAnsiChar;
  ByteLength: Cardinal; ZeroPadding: Boolean = False);
{* 以初始化后的上下文对一块数据进行 Poly1305 计算。
  可多次调用以连续计算不同的数据块，无需将不同的数据块拼凑在连续的内存中。
  但该 Update 的行为在碰见末尾非整块时会强行计算，不会像其他杂凑一样暂存等下一轮或 Final
  ZeroPadding 控制末尾块非 16 整时是否补 0}

procedure Poly1305Final(var Context: TCnPoly1305Context; var Digest: TCnPoly1305Digest);
{* 结束本轮计算，将 Poly130 结果返回至 Digest 中并释放 Context}

implementation

var
  Prime: TCnBigNumber = nil; // Poly1305 使用的素数
  Clamp: TCnBigNumber = nil; // Poly1305 使用的 Clamp

function Poly1305Bytes(Data: TBytes; Key: TBytes): TCnPoly1305Digest;
var
  AKey: TCnPoly1305Key;
  L: Integer;
begin
  FillChar(AKey[0], SizeOf(TCnPoly1305Key), 0);
  L := Length(Key);
  if L > SizeOf(TCnPoly1305Key) then
    L := SizeOf(TCnPoly1305Key);

  Move(Key[0], AKey[0], L);
  Result := Poly1305Data(@Data[0], Length(Data), AKey);
end;

function Poly1305Buffer(const Buffer; Count: Cardinal; Key: TCnPoly1305Key): TCnPoly1305Digest;
var
  C: TCnPoly1305Context;
begin
  Poly1305Init(C, Key);
  Poly1305Update(C, PAnsiChar(Buffer), Count);
  Poly1305Final(C, Result);
end;

function Poly1305Data(Data: Pointer; DataByteLength: Cardinal;
  Key: TCnPoly1305Key): TCnPoly1305Digest;
var
  I, B, L: Integer;
  R, S, A, N: TCnBigNumber;
  Buf: array[0..CN_POLY1305_BLOCKSIZE] of Byte;
  P: PByteArray;
  RKey: TCnPoly1305Key;
begin
  Move(Key[0], RKey[0], SizeOf(TCnPoly1305Key));

  // 由于 TCnBigNumber 使用的 Binary 均是网络字节顺序也就是大端
  // 但 RFC 中又规定这里得小端因此要手动调用 ReverseMemory 无论 CPU 是大端还是小端
  ReverseMemory(@RKey[0], CN_POLY1305_BLOCKSIZE);
  ReverseMemory(@RKey[CN_POLY1305_BLOCKSIZE], CN_POLY1305_BLOCKSIZE);

  R := nil;
  S := nil;
  A := nil;
  N := nil;

  try
    R := TCnBigNumber.FromBinary(@RKey[0], CN_POLY1305_BLOCKSIZE);
    BigNumberAnd(R, R, Clamp);

    S := TCnBigNumber.FromBinary(@RKey[CN_POLY1305_BLOCKSIZE], CN_POLY1305_BLOCKSIZE);

    A := TCnBigNumber.Create;
    A.SetZero;

    N := TCnBigNumber.Create;

    B := (DataByteLength + CN_POLY1305_BLOCKSIZE - 1) div CN_POLY1305_BLOCKSIZE;
    P := PByteArray(Data);

    for I := 1 to B do
    begin
      if I <> B then // 普通块，16 字节满的
        L := CN_POLY1305_BLOCKSIZE
      else           // 尾块，可能不够 16 字节
      begin
        L := DataByteLength mod CN_POLY1305_BLOCKSIZE;
        if L = 0 then
          L := CN_POLY1305_BLOCKSIZE;
      end;

      Move(P^[(I - 1) * CN_POLY1305_BLOCKSIZE], Buf[0], L);  // 内容塞上
      Buf[L] := 1;                                           // 紧邻的高字节再置个 1

      ReverseMemory(@Buf[0], L + 1);
      N.SetBinary(@Buf[0], L + 1);

      BigNumberAdd(A, A, N);
      BigNumberDirectMulMod(A, R, A, Prime);
    end;

    BigNumberAdd(A, A, S);
    BigNumberKeepLowBits(A, 8 * CN_POLY1305_DIGSIZE);

    A.ToBinary(@Result[0], CN_POLY1305_DIGSIZE);
    ReverseMemory(@Result[0], SizeOf(TCnPoly1305Digest));
  finally
    N.Free;
    A.Free;
    S.Free;
    R.Free;
  end;
end;

function Poly1305Print(const Digest: TCnPoly1305Digest): string;
begin
  Result := DataToHex(@Digest[0], SizeOf(TCnPoly1305Digest));
end;

function Poly1305Match(const D1, D2: TCnPoly1305Digest): Boolean;
begin
  Result := CompareMem(@D1[0], @D2[0], SizeOf(TCnPoly1305Digest));
end;

{ TCnPoly1305Context }

constructor TCnPoly1305Context.Create;
begin
  inherited;
  R := TCnBigNumber.Create;
  S := TCnBigNumber.Create;
  A := TCnBigNumber.Create;
  N := TCnBigNumber.Create;
end;

destructor TCnPoly1305Context.Destroy;
begin
  N.Free;
  A.Free;
  S.Free;
  R.Free;
  inherited;
end;

procedure Poly1305Init(out Context: TCnPoly1305Context; Key: TCnPoly1305Key);
var
  RKey: TCnPoly1305Key;
begin
  Move(Key[0], RKey[0], SizeOf(TCnPoly1305Key));
  ReverseMemory(@RKey[0], CN_POLY1305_BLOCKSIZE);
  ReverseMemory(@RKey[CN_POLY1305_BLOCKSIZE], CN_POLY1305_BLOCKSIZE);

  Context := TCnPoly1305Context.Create;

  Context.R.SetBinary(@RKey[0], CN_POLY1305_BLOCKSIZE);
  BigNumberAnd(Context.R, Context.R, Clamp);

  Context.S.SetBinary(@RKey[CN_POLY1305_BLOCKSIZE], CN_POLY1305_BLOCKSIZE);
  Context.A.SetZero;
  Context.N.SetZero;
end;

procedure Poly1305Update(Context: TCnPoly1305Context; Input: PAnsiChar;
  ByteLength: Cardinal; ZeroPadding: Boolean);
var
  I, B, L: Integer;
  Buf: array[0..CN_POLY1305_BLOCKSIZE] of Byte;
  P: PByteArray;
begin
  B := (ByteLength + CN_POLY1305_BLOCKSIZE - 1) div CN_POLY1305_BLOCKSIZE;
  P := PByteArray(Input);

  for I := 1 to B do
  begin
    if I <> B then // 普通块，16 字节满的
      L := CN_POLY1305_BLOCKSIZE
    else           // 尾块，可能不够 16 字节
    begin
      L := ByteLength mod CN_POLY1305_BLOCKSIZE;
      if L = 0 then
        L := CN_POLY1305_BLOCKSIZE
      else if ZeroPadding then
        FillChar(Buf[0], SizeOf(Buf), 0); // 末尾块不足 16 又要补 0 所以先要填充全 0
    end;

    Move(P^[(I - 1) * CN_POLY1305_BLOCKSIZE], Buf[0], L);  // 内容塞上
    if ZeroPadding then                                    // 末尾块如果要补 0 则上面补了，下面要补个 1
      L := CN_POLY1305_BLOCKSIZE;
    Buf[L] := 1;                                           // 紧邻的高字节（或补 0 时最高字节）再置个 1

    ReverseMemory(@Buf[0], L + 1);
    Context.N.SetBinary(@Buf[0], L + 1);

    BigNumberAdd(Context.A, Context.A, Context.N);
    BigNumberDirectMulMod(Context.A, Context.R, Context.A, Prime);
  end;
end;

procedure Poly1305Final(var Context: TCnPoly1305Context; var Digest: TCnPoly1305Digest);
begin
  BigNumberAdd(Context.A, Context.A, Context.S);
  BigNumberKeepLowBits(Context.A, 8 * CN_POLY1305_DIGSIZE);

  Context.A.ToBinary(@Digest[0], CN_POLY1305_DIGSIZE);
  ReverseMemory(@Digest[0], SizeOf(TCnPoly1305Digest));

  FreeAndNil(Context);
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
