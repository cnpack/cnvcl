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

unit CnRopes;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：字符串树 Ropes 实现单元
* 单元作者：刘啸
* 备    注：参考论文 Ropes: An Alternative to Strings 以及 Ropes for Java
* 开发平台：Win 7 + Delphi 5.0
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2015.06.10 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, Windows, Contnrs;

type
  ECnRopeIndexException = class(Exception);

  ICnRope = interface
  {* 所有 Ropes 字符串对象的公共基础接口，供外界使用}
    ['{D1E1DF7C-DE30-4D4A-A383-BF0ACACB5D9B}']
    function GetLength: Integer;
    function GetDepth: Integer;
    function GetCharAt(Index: Integer): Char;

    function Position(Pattern: Char; FromIndex: Integer = 1): Integer; overload;
    {* 查找字符，返回字符位置}
    function Position(const Pattern: string; FromIndex: Integer = 1): Integer; overload;
    {* 查找字符串，返回字符串位置}
    function Reverse: ICnRope;
    {* 返回本字符串的反向字符串}
    function ReBalance: ICnRope;
    {* 重新调整平衡，返回调整后的字符串}
    function ToString: string;
    {* 输出成字符串}

    function Append(const Str: string): ICnRope;
    {* 后面增加字符串，返回增加后的字符串}
    function AppendTo(const Str: string): ICnRope;
    {* 将自身增加在字符串后面，返回增加后的字符串}
    function AppendRope(const Rope: ICnRope): ICnRope;
    {* 后面增加字符串，返回增加后的字符串}
    function SubStr(StartIndex, EndIndex: Integer): ICnRope;
    {* 取从 StartIndex 到 EndIndex 的子串，返回子串}
    function Delete(StartIndex, EndIndex: Integer): ICnRope;
    {* 删除从 StartIndex 到 EndIndex 的子串，返回删除后的字符串}
    function Insert(const Str: string; StartIndex: Integer): ICnRope;
    {* 在第 StartIndex 个字符前插入字符串}
    function InsertRope(const Rope: ICnRope; StartIndex: Integer): ICnRope;
    {* 在第 StartIndex 个字符前插入字符串}
    function Duplicate: ICnRope;
    {* 复制一份字符串}

    function Equals(ARope: ICnRope): Boolean;
    {* 判断字符串内容是否相等，区分大小写}
    function EqualsStr(const AStr: string): Boolean;
    {* 判断字符串内容是否相等，区分大小写}

    function Trim: ICnRope;
    {* 修剪两端的空格与不可见字符，返回修剪后的字符串}
    function TrimStart: ICnRope;
    {* 修剪首端的空格与不可见字符，返回修剪后的字符串}
    function TrimEnd: ICnRope;
    {* 修剪尾端的空格与不可见字符，返回修剪后的字符串}

    property Depth: Integer read GetDepth;
    {* 树深度}
    property Length: Integer read GetLength;
    {* 字符串长度}
    property CharAt[Index: Integer]: Char read GetCharAt;
    {* 下标字符，从 1 开始}
  end;

  ICnFlatRope = interface(ICnRope)
    ['{994B424D-A521-4A33-A788-C5C823870CE6}']
    function SubString(StartIndex, Len: Integer): string;
    {* 返回 StartIndex 起长度为 Len 的子串}
  end;

  ICnReverseRope = interface(ICnRope)
    ['{588AB329-33D4-4F18-B874-C4175C1A9A92}']
  end;

  ICnSubStrRope = interface(ICnRope)
    ['{D2522C67-8F9B-426F-91B8-EAE59B4FB36F}']
  end;

  ICnConcatRope = interface(ICnRope)
    ['{2A687E3E-B2E3-476E-91FF-DDC2907B920E}']
    function GetLeft: ICnRope;
    function GetRight: ICnRope;

    property Left: ICnRope read GetLeft;
    {* 左子树}
    property Right: ICnRope read GetRight;
    {* 右子树}
  end;

function CreateRope(const Str: string): ICnRope;
{* 根据字符串创建 Rope 对象}

function ReBalanceRope(const Rope: ICnRope): ICnRope;
{* 使用朝鲜树的模式推倒重建 Rope 二叉树以保持左右平衡}

function ConcatRopes(ALeft, ARight: ICnRope): ICnRope;
{* 连接两个 Rope 字符串，并做必要的平衡操作}

implementation

const
  CN_COMBINE_LENGTH = 16;

type
  TCnRope = class(TInterfacedObject, ICnRope)
  private

  protected
    function GetDepth: Integer; virtual;
    function GetLength: Integer; virtual;
    function GetCharAt(Index: Integer): Char; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function Position(Pattern: Char; FromIndex: Integer = 1): Integer; overload; virtual;
    function Position(const Pattern: string; FromIndex: Integer = 1): Integer; overload; virtual;
    function Reverse: ICnRope; virtual;
    function ReBalance: ICnRope; virtual;
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ELSE} virtual; {$ENDIF}

    function Append(const Str: string): ICnRope; virtual;
    function AppendTo(const Str: string): ICnRope; virtual;
    function AppendRope(const Rope: ICnRope): ICnRope; virtual;
    function SubStr(StartIndex, EndIndex: Integer): ICnRope; virtual;
    function Delete(StartIndex, EndIndex: Integer): ICnRope; virtual;
    function Insert(const Str: string; StartIndex: Integer): ICnRope; virtual;
    function InsertRope(const Rope: ICnRope; StartIndex: Integer): ICnRope; virtual;
    function Duplicate: ICnRope;

    function Equals(ARope: ICnRope): Boolean; {$IFDEF OBJECT_HAS_TOSTRING} reintroduce; {$ENDIF}
    function EqualsStr(const AStr: string): Boolean;

    function Trim: ICnRope;
    function TrimStart: ICnRope;
    function TrimEnd: ICnRope;

    property Depth: Integer read GetDepth;
    property Length: Integer read GetLength;
    property CharAt[Index: Integer]: Char read GetCharAt;
  end;

  TCnFlatRope = class(TCnRope, ICnFlatRope)
  private
    FChars: PChar;
    FLength: Integer;
  protected
    function GetDepth: Integer; override;
    function GetLength: Integer; override;
    function GetCharAt(Index: Integer): Char; override;
  public
    constructor Create(const Str: string); overload; virtual;
    constructor Create(const Str: PChar; Len: Integer); overload; virtual;
    destructor Destroy; override;

    function ToString: string; override;
    function SubString(StartIndex, Len: Integer): string;
    function SubStr(StartIndex, EndIndex: Integer): ICnRope; override;
  end;

  TCnReverseRope = class(TCnRope, ICnReverseRope)
  private
    FRope: ICnRope;
  protected
    function GetDepth: Integer; override;
    function GetLength: Integer; override;
    function GetCharAt(Index: Integer): Char; override;
  public
    constructor Create(ARope: ICnRope); virtual;
    destructor Destroy; override;

    function Reverse: ICnRope; override;
    function ToString: string; override;
  end;

  TCnSubStrRope = class(TCnRope, ICnSubStrRope)
  private
    FRope: ICnFlatRope;
    FOffset: Integer;
    FLength: Integer;
  protected
    function GetDepth: Integer; override;
    function GetLength: Integer; override;
    function GetCharAt(Index: Integer): Char; override;
  public
    constructor Create(ARope: ICnFlatRope; AnOffset: Integer; ALength: Integer); virtual;
    destructor Destroy; override;

    function SubStr(StartIndex, EndIndex: Integer): ICnRope; override;
    function ToString: string; override;
  end;

  TCnConcatRope = class(TCnRope, ICnConcatRope)
  private
    FLeft: ICnRope;
    FRight: ICnRope;
    function GetLeft: ICnRope;
    function GetRight: ICnRope;
  protected
    function GetDepth: Integer; override;
    function GetLength: Integer; override;
    function GetCharAt(Index: Integer): Char; override;
  public
    constructor Create(ALeft: ICnRope; ARight: ICnRope); virtual;
    destructor Destroy; override;

    function Reverse: ICnRope; override;
    function ReBalance: ICnRope; override;

    function SubStr(StartIndex, EndIndex: Integer): ICnRope; override;
    function ToString: string; override;

    property Left: ICnRope read GetLeft;
    property Right: ICnRope read GetRight;
  end;

var
  FRopeCount: Integer = 0;

function CreateRope(const Str: string): ICnRope;
begin
  Result := TCnFlatRope.Create(Str);
end;

function MergeRope(List: TInterfaceList; StartIdx, EndIdx: Integer): ICnRope;
var
  I, M: Integer;
begin
  I := EndIdx - StartIdx;
  if I = 1 then
    Result := TCnRope(List[StartIdx])
  else if I = 2 then
    Result := TCnConcatRope.Create(List[StartIdx] as ICnRope, List[StartIdx + 1] as ICnRope)
  else
  begin
    M := StartIdx + (I div 2);
    Result := TCnConcatRope.Create(MergeRope(List, StartIdx, M), MergeRope(List, M, EndIdx));
  end;
end;

function ReBalanceRope(const Rope: ICnRope): ICnRope;
var
  List: TInterfaceList;
  Queue: TQueue;
  R: ICnRope;
  CR: ICnConcatRope;
begin
  Result := nil;
  if Rope = nil then
    Exit;

  List := nil;
  Queue := nil;

  try
    List := TInterfaceList.Create;
    Queue := TQueue.Create;

    Queue.Push(Pointer(Rope));
    while Queue.Count > 0 do
    begin
      R := ICnRope(Queue.Pop);
      if Supports(R, ICnConcatRope, CR) then
      begin
        Queue.Push(Pointer(CR.Left));
        Queue.Push(Pointer(CR.Right));
      end
      else
        List.Add(R);
    end;

    Result := MergeRope(List, 0, List.Count);
  finally
    List.Free;
    Queue.Free;
  end;
end;

function ConcatRopes(ALeft, ARight: ICnRope): ICnRope;
var
  CL, CR: ICnConcatRope;
begin
  if (ALeft = nil) or (ALeft.Length = 0) then
  begin
    Result := ARight;
    Exit;
  end;
  if (ARight = nil) or (ARight.Length = 0) then
  begin
    Result := ALeft;
    Exit;
  end;

  if Int64(ALeft.Length) + Int64(ARight.Length) > MaxInt then
    raise EOutOfResources.Create('String Length Too Long.');

  if ALeft.Length + ARight.Length <= CN_COMBINE_LENGTH then
  begin
    Result := TCnFlatRope.Create(ALeft.ToString + ARight.ToString);
    Exit;
  end;

  if not Supports(ALeft, ICnConcatRope, CL) then
  begin
    if Supports(ARight, ICnConcatRope, CR) then
    begin
      if ALeft.Length + CR.Left.Length <= CN_COMBINE_LENGTH then
      begin
        Result := TCnConcatRope.Create(TCnFlatRope.Create(ALeft.ToString + CR.Left.ToString),
          CR.Right);
        Result := ReBalanceRope(Result);
        Exit;
      end;
    end;
  end;

  if not Supports(ARight, ICnConcatRope, CR) then
  begin
    if Supports(ALeft, ICnConcatRope, CL) then
    begin
      if ARight.Length + CL.Right.Length <= CN_COMBINE_LENGTH then
      begin
        Result := TCnConcatRope.Create(CL.Left,
          TCnFlatRope.Create(ARight.ToString + CL.Right.ToString));
        Result := ReBalanceRope(Result);
        Exit;
      end;
    end;
  end;

  Result := ReBalanceRope(TCnConcatRope.Create(ALeft, ARight));
end;

{ TCnFlatRope }

constructor TCnFlatRope.Create(const Str: string);
begin
  Create(PChar(Str), System.Length(Str));
end;

constructor TCnFlatRope.Create(const Str: PChar; Len: Integer);
begin
  inherited Create;
  if Str <> nil then
  begin
    if Len = 0 then
      Len := StrLen(Str);

    if Len > 0 then
    begin
      FChars := GetMemory((Len + 1) * SizeOf(Char));
      CopyMemory(FChars, Str, Len * SizeOf(Char));
      FChars[Len] := #0;
      FLength := Len;
    end;
  end;
end;

destructor TCnFlatRope.Destroy;
begin
  if FChars <> nil then
  begin
    FreeMemory(FChars);
    FChars := nil;
  end;
  inherited;
end;

function TCnFlatRope.GetCharAt(Index: Integer): Char;
begin
  if (Index <= 0) or (Index > Length) or (FChars = nil) then
    raise ECnRopeIndexException.Create('Invalid Char Index.');

  Result := FChars[Index - 1];
end;

function TCnFlatRope.GetDepth: Integer;
begin
  Result := 0;
end;

function TCnFlatRope.GetLength: Integer;
begin
  Result := FLength;
end;

function TCnFlatRope.SubStr(StartIndex, EndIndex: Integer): ICnRope;
begin
  if (StartIndex <= 0) or (StartIndex > EndIndex) then
    raise ECnRopeIndexException.Create('Invalid Start and End Index.');

  if (StartIndex = 1) and (EndIndex > FLength) then
    Result := Self
  else if EndIndex - StartIndex < CN_COMBINE_LENGTH then
    Result := TCnFlatRope.Create(SubString(StartIndex, EndIndex - StartIndex + 1))
  else
    Result := TCnSubStrRope.Create(Self, StartIndex, EndIndex - StartIndex + 1);
end;

function TCnFlatRope.SubString(StartIndex, Len: Integer): string;
begin
  if (StartIndex > Length) or (FChars = nil) then
    Result := ''
  else
    Result := Copy(StrNew(FChars), StartIndex, Len);
end;

function TCnFlatRope.ToString: string;
begin
  if (FChars <> nil) and (Length > 0) then
  begin
    SetLength(Result, Length);
    CopyMemory(@Result[1], FChars, Length * SizeOf(Char));
  end
  else
    Result := '';
end;

{ TCnRope }

function TCnRope.Append(const Str: string): ICnRope;
begin
  Result := ConcatRopes(Self, CreateRope(Str));
end;

function TCnRope.AppendRope(const Rope: ICnRope): ICnRope;
begin
  Result := ConcatRopes(Self, Rope);
end;

function TCnRope.AppendTo(const Str: string): ICnRope;
begin
  Result := ConcatRopes(TCnFlatRope.Create(Str), Self);
end;

constructor TCnRope.Create;
begin
  inherited;
  InterlockedIncrement(FRopeCount);
end;

function TCnRope.Delete(StartIndex, EndIndex: Integer): ICnRope;
begin
  if StartIndex > EndIndex then
    Result := Self
  else
    Result := SubStr(1, StartIndex - 1).AppendRope(SubStr(EndIndex + 1, Length));
end;

destructor TCnRope.Destroy;
begin
  InterlockedDecrement(FRopeCount);
  inherited;
end;

function TCnRope.Equals(ARope: ICnRope): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ARope = nil then
    Exit;
  if Length <> ARope.Length then
    Exit;

  for I := 1 to Length do
    if CharAt[I] <> ARope.CharAt[I] then
      Exit;

  Result := True;
end;

function TCnRope.EqualsStr(const AStr: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if Length <> System.Length(AStr) then
    Exit;

  for I := 1 to Length do
    if CharAt[I] <> AStr[I] then
      Exit;

  Result := True;
end;

function TCnRope.GetCharAt(Index: Integer): Char;
begin
  Result := #0;
end;

function TCnRope.GetDepth: Integer;
begin
  Result := 0;
end;

function TCnRope.GetLength: Integer;
begin
  Result := 0;
end;

function TCnRope.Position(Pattern: Char; FromIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if FromIndex < 1 then
    FromIndex := 1;

  for I := FromIndex to Length do
  begin
    if CharAt[I] = Pattern then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TCnRope.Insert(const Str: string; StartIndex: Integer): ICnRope;
begin
  if StartIndex < 0 then
    raise ECnRopeIndexException.Create('Invalid Insert Index.');

  if Str = '' then
    Result := Self
  else if StartIndex = 1 then
    Result := TCnFlatRope.Create(Str).AppendRope(Self)
  else
  begin
    Result := SubStr(1, StartIndex - 1).Append(Str).AppendRope(SubStr(StartIndex, Length));
  end;
end;

function TCnRope.Position(const Pattern: string; FromIndex: Integer): Integer;
var
  C: Char;
  I, L, X, Y, PLen: Integer;
  BCS: array[0..255] of Integer;
begin
  Result := 0;
  PLen := System.Length(Pattern);
  if (PLen = 0) or (Length = 0) then
    Exit;

  if PLen = 1 then
  begin
    Result := Position(Pattern[1]);
    Exit;
  end;

  if FromIndex < 1 then
    FromIndex := 1;

  // 填充快速跃进表
  for I := Low(BCS) to High(BCS) do
    BCS[I] := PLen;

  for I := 0 to PLen - 2 do
  begin
    C := Pattern[I + 1];
    L := Ord(C) and $FF;
    if PLen - I - 1 < BCS[L] then
      BCS[L] := PLen - I - 1;
  end;

  // 再进行搜索
  I := FromIndex + PLen - 1;
  while I < Length do
  begin
    X := I;
    Y := PLen - 1;
    while True do
    begin
      if Pattern[Y + 1] <> CharAt[X + 1] then
      begin
        Inc(I, BCS[Ord(CharAt[X + 1]) and $FF]);
        Break;
      end;

      if Y = 0 then
      begin
        Result := X + 1;
        Exit;
      end;

      Dec(X);
      Dec(Y);
    end;
  end;
end;

function TCnRope.ReBalance: ICnRope;
begin
  Result := Self;
end;

function TCnRope.Reverse: ICnRope;
begin
  Result := TCnReverseRope.Create(Self);
end;

function TCnRope.SubStr(StartIndex, EndIndex: Integer): ICnRope;
begin
  Result := nil;
end;

function TCnRope.ToString: string;
begin
  Result := '';
end;

function TCnRope.Trim: ICnRope;
begin
  Result := TrimStart.TrimEnd;
end;

function TCnRope.TrimEnd: ICnRope;
var
  I, L: Integer;
begin
  L := Length;
  for I := Length downto 1 do
  begin
    if CharAt[I] > ' ' then
      Break;
    Dec(L);
  end;

  if L >= Length then
    Result := Self
  else
    Result := SubStr(1, L);
end;

function TCnRope.TrimStart: ICnRope;
var
  I, L: Integer;
begin
  L := 0;
  for I := 1 to Length do
  begin
    Inc(L);
    if CharAt[I] > ' ' then
      Break;
  end;

  if L <= 1 then
    Result := Self
  else
    Result := SubStr(L, Length);
end;

function TCnRope.InsertRope(const Rope: ICnRope; StartIndex: Integer): ICnRope;
begin
  if StartIndex < 0 then
    raise ECnRopeIndexException.Create('Invalid Insert Index.');

  if (Rope = nil) or (Rope.Length = 0) then
    Result := Self
  else if StartIndex = 1 then
    Result := Rope.AppendRope(Self)
  else
  begin
    Result := SubStr(1, StartIndex - 1).AppendRope(Rope).AppendRope(SubStr(StartIndex, Length));
  end;
end;

function TCnRope.Duplicate: ICnRope;
begin
  Result := TCnFlatRope.Create(ToString);
end;

{ TCnReverseRope }

constructor TCnReverseRope.Create(ARope: ICnRope);
begin
  inherited Create;
  FRope := ARope;
end;

destructor TCnReverseRope.Destroy;
begin

  inherited;
end;

function TCnReverseRope.GetCharAt(Index: Integer): Char;
begin
  Result := FRope.CharAt[FRope.Length - Index + 1];
end;

function TCnReverseRope.GetDepth: Integer;
begin
  Result := FRope.Depth;
end;

function TCnReverseRope.GetLength: Integer;
begin
  Result := FRope.Length;
end;

function TCnReverseRope.Reverse: ICnRope;
begin
  Result := FRope;
end;

function TCnReverseRope.ToString: string;
var
  I: Integer;
begin
  // FRope 倒序
  Result := '';
  if (FRope <> nil) and (FRope.Length >  0) then
    SetLength(Result, FRope.Length);

  for I := FRope.Length downto 1 do
    Result[FRope.Length - I + 1] := FRope.CharAt[I];
end;

{ TCnConcatRope }

constructor TCnConcatRope.Create(ALeft, ARight: ICnRope);
begin
  inherited Create;
  FLeft := ALeft;
  FRight := ARight;
end;

destructor TCnConcatRope.Destroy;
begin

  inherited;
end;

function TCnConcatRope.GetCharAt(Index: Integer): Char;
begin
  if (Index <= 0) or (Index > Length) then
    raise ECnRopeIndexException.Create('Invalid Char Index.');

  if Index <= FLeft.Length then
    Result := FLeft.CharAt[Index]
  else
    Result := FRight.CharAt[Index - FLeft.Length];
end;

function TCnConcatRope.GetDepth: Integer;
begin
  Result := FLeft.Depth;
  if FRight.Depth > Result then
    Result := FRight.Depth;
end;

function TCnConcatRope.GetLeft: ICnRope;
begin
  Result := FLeft;
end;

function TCnConcatRope.GetLength: Integer;
begin
  Result := FLeft.Length + FRight.Length;
end;

function TCnConcatRope.GetRight: ICnRope;
begin
  Result := FRight;
end;

function TCnConcatRope.ReBalance: ICnRope;
begin
  Result := ReBalanceRope(Self);
end;

function TCnConcatRope.Reverse: ICnRope;
begin
  Result := TCnConcatRope.Create(FRight.Reverse, FLeft.Reverse);
end;

function TCnConcatRope.SubStr(StartIndex, EndIndex: Integer): ICnRope;
var
  L: Integer;
begin
  if (StartIndex = 1) and (EndIndex = Length) then
    Result := Self
  else
  begin
    L := FLeft.Length;
    if EndIndex <= L then
      Result := FLeft.SubStr(StartIndex, EndIndex)
    else if StartIndex > L then
      Result := FRight.SubStr(StartIndex - L, EndIndex - L)
    else
      Result := ConcatRopes(FLeft.SubStr(StartIndex, L), FRight.SubStr(1, EndIndex - L));
  end;
end;

function TCnConcatRope.ToString: string;
begin
  Result := FLeft.ToString + FRight.ToString;
end;

{ TCnSubStrRope }

constructor TCnSubStrRope.Create(ARope: ICnFlatRope; AnOffset,
  ALength: Integer);
begin
  inherited Create;
  FRope := ARope;
  FOffset := AnOffset;
  FLength := ALength;
end;

destructor TCnSubStrRope.Destroy;
begin

  inherited;
end;

function TCnSubStrRope.GetCharAt(Index: Integer): Char;
begin
  if (Index <= 0) or (Index > Length) then
    raise ECnRopeIndexException.Create('Invalid Char Index.');

  Result := FRope.CharAt[FOffset + Index - 1]; // PChar 数组 0 开始所以要减一
end;

function TCnSubStrRope.GetDepth: Integer;
begin
  Result := FRope.Depth;
end;

function TCnSubStrRope.GetLength: Integer;
begin
  Result := FLength;
end;

function TCnSubStrRope.SubStr(StartIndex, EndIndex: Integer): ICnRope;
begin
  if (StartIndex = 1) and (EndIndex = Length) then
    Result := Self
  else
    Result := TCnSubStrRope.Create(FRope, FOffset + StartIndex - 1, EndIndex - StartIndex + 1);
end;

function TCnSubStrRope.ToString: string;
begin
  Result := FRope.SubString(FOffset, FLength);
end;

initialization

finalization
  Assert(FRopeCount = 0);

end.
