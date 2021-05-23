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

unit CnMemo;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：有行号显示功能的 Memo
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：该单元当前仅为内部参考测试用
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：PWinXP/7 + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2015.07.26 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Classes, Messages, Controls, Graphics, StdCtrls, ExtCtrls,
  Dialogs, SysConst, Forms, Clipbrd, CnTextControl, CnCommon;

type
{$IFDEF UNICODE}
  TCnEditorString = string;
{$ELSE}
  TCnEditorString = WideString;
{$ENDIF}

  PCnEditorStringMark = ^TCnEditorStringMark;
  TCnEditorStringMark = packed record
    LineNumNoWrap: Integer;       // 未自动换行时当前行的物理行号，1 开始，供显示在侧边栏上。如自动换行，值和上一行相同
    LineNumAfterElide: Integer;   // 折叠后的行号，绘制用，1 开始
    ElidedStartIndex: Word;       // 本行有折叠头时，折叠头距行头的偏移量，0 开始
    ElidedStartLength: Word;      // 本行有折叠头时，折叠头的字符长度，0 表示无
    ElidedEndIndex: Word;         // 本行有折叠尾时，折叠尾距行头的偏移量，0 开始
    ElidedEndLength: Word;        // 本行有折叠尾时，折叠尾的字符长度，0 表示无
    Elided: Boolean;              // 本行是否折叠显示
  end;

  PCnEditorStringItem = ^TCnEditorStringItem;
  TCnEditorStringItem = packed record
  {* 编辑器中代表一行的内容}
    FString: string;              // 当前行内容
    FMark: TCnEditorStringMark;   // 当前行附加标记
  end;

  PCnEditorStringItemList = ^TCnEditorStringItemList;
  TCnEditorStringItemList = array[1..MaxListSize div 4] of TCnEditorStringItem;

  TCnEditorStringList = class(TPersistent)
  {* 编辑器中的字符串列表对象，下标以 1 开始}
  private
    FUpdateCount: Integer;
    FList: PCnEditorStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FAutoWrap: Boolean;
    FWrapWidth: Integer;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure SetAutoWrap(const Value: Boolean);
    procedure SetWrapWidth(const Value: Integer);
    function GetMark(Index: Integer): PCnEditorStringMark;
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    procedure Error(const Msg: string; Data: Integer);
    function Get(Index: Integer): string; virtual;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual;
    function GetTextStr: string; virtual;
    procedure Put(Index: Integer; const S: string); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: string); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    procedure InsertItem(Index: Integer; const S: string); virtual;
    property UpdateCount: Integer read FUpdateCount;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Add(const S: string): Integer; virtual;
    procedure BeginUpdate;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure EndUpdate;

    function GetText: PChar; virtual;
    procedure Insert(Index: Integer; const S: string); virtual;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(Text: PChar); virtual;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount;

    property Strings[Index: Integer]: string read Get write Put; default;
    property Mark[Index: Integer]: PCnEditorStringMark read GetMark;
    property Text: string read GetTextStr write SetTextStr;

    property AutoWrap: Boolean read FAutoWrap write SetAutoWrap;
    {* 是否自动换行，改变时触发内部排版}
    property WrapWidth: Integer read FWrapWidth write SetWrapWidth;
    {* 自动换行时的字符宽度}

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

type
  TCnStringsControl = class(TCnVirtualTextControl)
  {* 有具体字符串列表显示功能的文本组件}
  private
    FStrings: TStringList;
    procedure StringsChange(Sender: TObject);
    function GetLines: TStringList;
    function GetSelectText: string;
  protected
    procedure DoPaintLine(LineCanvas: TCanvas; LineNumber, HoriCharOffset: Integer;
      LineRect: TRect); override;

    function CalcColumnFromPixelOffsetInLine(ARow, VirtualX: Integer;
      out Col: Integer; out LeftHalf, DoubleWidth: Boolean): Boolean; override;

    function CalcPixelOffsetFromColumnInLine(ARow, ACol: Integer; out Rect: TRect;
      out DoubleWidth: Boolean): Boolean; override;

    function GetLastColumnFromLine(LineNumber: Integer): Integer; override;

    function GetPrevColumn(AColumn, ARow: Integer): Integer; override;

    function GetNextColumn(AColumn, ARow: Integer; ACaretAfterLineEnd: Boolean): Integer; override;

    function GetNearestColumn(AColumn, ARow: Integer): Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(const AFile: string);
    procedure SaveToFile(const AFile: string);

    procedure CopySelectionToClipboard;
    {* 将选择内容复制到剪贴板}

    property Lines: TStringList read GetLines;
    property SelectText: string read GetSelectText;
  published

  end;

  TCnMemo = class(TCnStringsControl)
  {* 有字符串编辑功能的文本组件}
  private
    FReadOnly: Boolean;
    FPrevChar: Char;
    procedure DisableStringsChange;
    procedure EnableStringsChange;
    function DeleteText(StartRow, StartCol, EndRow, EndCol: Integer): Boolean;
    {* 删除起始行列到结束行列间的内容}
    function InsertTextAt(const Text: string; ARow, ACol: Integer;
      out DeltaRow, DeltaCol: Integer): Boolean;
    {* 在指定光标位置处插入文本，并返回计算的光标移动偏移量供调用者移动}
  protected
    procedure WMKeyChar(var Message: TMessage); message WM_CHAR;
    procedure KeyDown(var Key: WORD; Shift: TShiftState); override;

  public
    procedure DeleteSelection;
    {* 删除选择区，但不重画}
    procedure CutSelectionToClipboard;
    {* 将选择区内容剪切至剪贴板}
    procedure PasteFromClipboard;
    {* 从剪贴板粘贴}

    procedure InsertText(const Text: string);

    property ReadOnly: Boolean read FReadOnly write FReadOnly;
  end;

function MapColumnToWideCharIndexes(const S: TCnEditorString; AColumn: Integer;
  out LeftCharIndex, RightCharIndex: Integer): Boolean;
{* 返回宽字符串中指定 Column 左、右的字符索引，俩均 1 开始。非法的或超大的 Column 返回 False
   注意 Column 是字符串尾时，RightIndex 会比 Length 大 1
   注意 Column 是字符串头也就是 1 时，LeftIndex 是 0
   更要注意的是字符串是空且 Column 是 1 时，LeftIndex 是 0，RightIndex 是 1
}

function MapWideCharIndexToColumns(const S: TCnEditorString; ACharIndex: Integer;
  out LeftColumn, RightColumn: Integer; AfterEnd: Boolean = False): Boolean;
{* 返回宽字符串中指定字符索引左右两边的 Column，俩均 1 开始，非法的 CharIndex 返回 False
   CharIndex 指向最后一个字符时 RightColumn 返回末列
   CharIndex 超过末字符时，根据 AfterEnd 的值判断。AfterEnd 表示 Column 是否允许超行尾
     False 时 LeftColumn 返回末列，RightColumn 未定义
       特别的：空字符串只有当 CharIndex >= 1 时 LeftColumn 返回 1，RightColumn 未定义
     True 时 CharInde 是末字符 + 1 对应 末列与末列 + 1，以空格堆叠过去以此类推
   特别的，AfterEnd 为 False 且 CharIndex 超大如 MaxInt 时，LeftColumn 返回末列，
     LeftColumn - 1 为该字符串所占的列宽
   }
function GetColumnWidthFromWideString(const S: TCnEditorString): Integer;
{* 返回宽字符串所占的列宽}

implementation

resourcestring
  SCnListIndexError = 'Index %d out of Range.';

const
{$IFDEF MSWINDOWS}
  CRLF = #13#10;
{$ELSE}
  CRLF = #10;
{$ENDIF}

  CRLF_LEN = Length(CRLF);

  csDefaultLineNumberBkColor = clBtnface;
  csDefaultLineNumberHighlightColor = clRed;
  csDefaultLineNumberColor = clBtnText;

{ TCnEditorStringList }

function TCnEditorStringList.Add(const S: string): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

procedure TCnEditorStringList.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TStringList then
  begin
    (Dest as TStringList).Clear;
    for I := 1 to Count do
      (Dest as TStringList).AddObject(Strings[I], TObject(Mark[I]^.LineNumNoWrap));
  end
  else if Dest is TCnEditorStringList then
  begin
    (Dest as TCnEditorStringList).Clear;
    for I := 1 to Count do
      (Dest as TCnEditorStringList).Add(Strings[I]);
  end
  else
    inherited;
end;

procedure TCnEditorStringList.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TCnEditorStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCnEditorStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TCnEditorStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[1], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

constructor TCnEditorStringList.Create;
begin

end;

procedure TCnEditorStringList.Delete(Index: Integer);
begin
  if (Index <= 0) or (Index > FCount) then
    Error(SCnListIndexError, Index);

  Changing;
  Finalize(FList^[Index]);
  Dec(FCount);

  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TCnEditorStringItem));
  Changed;
end;

destructor TCnEditorStringList.Destroy;
begin

  inherited;
end;

procedure TCnEditorStringList.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

procedure TCnEditorStringList.Error(const Msg: string; Data: Integer);
begin
  raise EStringListError.CreateFmt(Msg, [Data]);
end;

procedure TCnEditorStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PCnEditorStringItem;
  TempMark: TCnEditorStringMark;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FString);

  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;

  TempMark := Item1^.FMark;
  Item1^.FMark := Item2^.FMark;
  Item2^.FMark := TempMark;
end;

function TCnEditorStringList.Get(Index: Integer): string;
begin
  if (Index <= 0) or (Index > FCount) then
    Error(SCnListIndexError, Index);
  Result := FList^[Index].FString;
end;

function TCnEditorStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TCnEditorStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TCnEditorStringList.GetMark(Index: Integer): PCnEditorStringMark;
begin
  if (Index <= 0) or (Index > FCount) then
    Error(SCnListIndexError, Index);
  Result := @(FList^[Index].FMark);
end;

function TCnEditorStringList.GetText: PChar;
begin
  Result := StrNew(PChar(GetTextStr));
end;

function TCnEditorStringList.GetTextStr: string;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S: string;
begin
  Count := GetCount;
  Size := 0;
  for I := 1 to Count do
    Inc(Size, Length(Get(I)) + 2);
  SetString(Result, nil, Size);

  P := Pointer(Result);
  for I := 1 to Count do
  begin
    S := Get(I);
    L := Length(S) * SizeOf(Char);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L);
      Inc(P, L);
    end;

    P^ := #13;
    Inc(P);
    P^ := #10;
    Inc(P);
  end;
end;

procedure TCnEditorStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 2
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;

  SetCapacity(FCapacity + Delta);
end;

procedure TCnEditorStringList.Insert(Index: Integer; const S: string);
begin

end;

procedure TCnEditorStringList.InsertItem(Index: Integer; const S: string);
begin
  Changing;
  if FCount = FCapacity then
    Grow;

  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TCnEditorStringItem));

  Pointer(FList^[Index].FString) := nil; // 必须直接赋值为 nil
  FList^[Index].FString := S;

  Inc(FCount);
  Changed;
end;

procedure TCnEditorStringList.LoadFromFile(const FileName: string);
begin

end;

procedure TCnEditorStringList.LoadFromStream(Stream: TStream);
begin

end;

procedure TCnEditorStringList.Move(CurIndex, NewIndex: Integer);
begin

end;

procedure TCnEditorStringList.Put(Index: Integer; const S: string);
begin
  if (Index <= 0) or (Index > FCount) then
    Error(SCnListIndexError, Index);

  Changing;
  FList^[Index].FString := S;
  Changed;
end;

procedure TCnEditorStringList.SaveToFile(const FileName: string);
begin

end;

procedure TCnEditorStringList.SaveToStream(Stream: TStream);
begin

end;

procedure TCnEditorStringList.SetAutoWrap(const Value: Boolean);
begin
  FAutoWrap := Value;
end;

procedure TCnEditorStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TCnEditorStringItem));
  FCapacity := NewCapacity;
end;

procedure TCnEditorStringList.SetText(Text: PChar);
begin
  SetTextStr(Text);
end;

procedure TCnEditorStringList.SetTextStr(const Value: string);
var
  P, Start: PChar;
  S: string;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not (P^ in [#0, #10, #13]) do
          Inc(P);
        SetString(S, Start, P - Start);
        Add(S);

        if P^ = #13 then
          Inc(P);
        if P^ = #10 then
          Inc(P);
      end;
  finally
    EndUpdate;
  end;
end;

procedure TCnEditorStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

procedure TCnEditorStringList.SetWrapWidth(const Value: Integer);
begin
  FWrapWidth := Value;
end;

function MapColumnToWideCharIndexes(const S: TCnEditorString; AColumn: Integer;
  out LeftCharIndex, RightCharIndex: Integer): Boolean;
var
  L, I, Col: Integer;
  C: WideChar;
begin
  Result := False;
  if AColumn <= 0 then
    Exit;

  if AColumn = 1 then
  begin
    LeftCharIndex := 0;
    RightCharIndex := 1;
    Result := True;
    Exit;
  end;

  L := Length(S);
  if L = 0 then
    Exit;  // 空字符只能处理 Column 为 1 的情况

  I := 1;
  Col := 1;

  while I <= L do
  begin
    C := S[I];

    if WideCharIsWideLength(C) then
      Inc(Col, 2)
    else
      Inc(Col);

    if Col = AColumn then
    begin
      LeftCharIndex := I;
      RightCharIndex := I + 1;
      Result := True;
      Exit;
    end
    else if Col > AColumn then
      Exit;

    Inc(I);
  end;
end;

function MapWideCharIndexToColumns(const S: TCnEditorString; ACharIndex: Integer;
  out LeftColumn, RightColumn: Integer; AfterEnd: Boolean): Boolean;
var
  L, I: Integer;
  C: WideChar;
begin
  Result := False;
  if ACharIndex <= 0 then
    Exit;

  L := Length(S);

  if L = 0 then
  begin
    if AfterEnd then
    begin
      LeftColumn := ACharIndex;
      RightColumn := ACharIndex + 1;
    end
    else if ACharIndex >= 1 then // 不允许超过行尾
    begin
      LeftColumn := 1;
      RightColumn := -1;
    end;

    Result := True;
    Exit;
  end;

  I := 1;
  LeftColumn := 1;
  RightColumn := 1;

  while I <= L do
  begin
    C := S[I];

    LeftColumn := RightColumn;
    if WideCharIsWideLength(C) then
      Inc(RightColumn, 2)
    else
      Inc(RightColumn);

    if I >= ACharIndex then
      Break;

    Inc(I);
  end;

  if I > L then // 此时 I 指向末字符的后一个字符
  begin
    if AfterEnd then
    begin
      LeftColumn := RightColumn + ACharIndex - L - 1;
      RightColumn := LeftColumn + 1;
    end
    else
    begin
      LeftColumn := RightColumn;
      RightColumn := -1;
    end;
  end;
  Result := True;
end;

function GetColumnWidthFromWideString(const S: TCnEditorString): Integer;
var
  R: Integer;
begin
  if MapWideCharIndexToColumns(S, MaxInt, Result, R) then
    Dec(Result)
  else
    raise ECnTextControlException.Create(SCnTextControlErrorColumn);
end;

{ TCnStringsControl }

function TCnStringsControl.CalcColumnFromPixelOffsetInLine(ARow, VirtualX: Integer;
  out Col: Integer; out LeftHalf, DoubleWidth: Boolean): Boolean;
var
  I, L, X, OldX, W2, T: Integer;
  W, DirectCalc: Boolean;
  S: string;
  SW: WideString;
  C: WideChar;
  Size: TSize;
begin
  Dec(ARow); // TODO: 更新下标

  if (ARow >= 0) and (ARow < FStrings.Count) then
  begin
    // 有字符串，按字符串实际来
    S := FStrings[ARow];

{$IFDEF UNICODE}
    L := Length(S);
{$ELSE}
    SW := WideString(S);
    L := Length(SW);
{$ENDIF}

    I := 1;
    X := 0;
    Col := 1;

    DirectCalc := FFontIsFixedWidth or not HandleAllocated;
    // 从 S[1] 到 S[length] 累加计算 Column，并直接计算横坐标右侧（等宽字体）
    // 或 TextWidth （非等宽），看哪个刚好超出 X

    W2 := FAveCharWidth * 2;
    while I <= L do
    begin
{$IFDEF UNICODE}
      C := S[I];
{$ELSE}
      C := SW[I];
{$ENDIF}

      W := WideCharIsWideLength(C);
      OldX := X;

      if not DirectCalc then
      begin
        // 用 Canvas.TextWidth 计算 S[1] 到 S[I] 的长度 X，而不是下面的字符宽度累加
{$IFDEF UNICODE}
        GetTextExtentPoint32(Canvas.Handle, PChar(S), I, Size);
{$ELSE}
        // I 指向宽字符串的下标索引，非 Unicode 环境下要转成 Ansi 模式再量宽度
        T := CalcAnsiLengthFromWideStringOffset(PWideChar(SW), I);
        GetTextExtentPoint32(Canvas.Handle, PChar(S), T, Size);
{$ENDIF}
        X := Size.cx;
      end;

      if W then
      begin
        if DirectCalc then
          Inc(X, W2);
        Inc(Col, 2);
      end
      else
      begin
        if DirectCalc then
          Inc(X, FAveCharWidth);  // 增加后，X 是第 I 个字符的右侧坐标，Col 是第 I 个字符右侧
        Inc(Col);
      end;

      if X > VirtualX then
      begin
        // 刚好超过，说明就是这个字符，X 和 Col 要把刚加的给减回去
        X := OldX;
        if W then
          Dec(Col, 2)
        else
          Dec(Col);

        DoubleWidth := W;
        if DoubleWidth then
          LeftHalf := (VirtualX - X) <= FAveCharWidthHalf
        else
          LeftHalf := (VirtualX - X) <= FAveCharWidth;

        Result := True;
        Exit;
      end;
      Inc(I);
    end;

    // 如果到这都还没超过，说明光标过尾巴了，此时 X 是尾字符右侧，I 是尾字符，Col 是尾字符右侧
    DoubleWidth := False;
    I := (VirtualX - X) div FAveCharWidth;
    Inc(Col, I);
    LeftHalf := (VirtualX - X - FAveCharWidth * I) < FAveCharWidthHalf;
    Result := True;
  end
  else
  begin
    // 没有字符串，按等宽直接计算
    Result := inherited CalcColumnFromPixelOffsetInLine(ARow + 1, VirtualX, Col,
      LeftHalf, DoubleWidth);
  end;
end;

function TCnStringsControl.CalcPixelOffsetFromColumnInLine(ARow, ACol: Integer;
  out Rect: TRect; out DoubleWidth: Boolean): Boolean;
var
  W, DirectCalc: Boolean;
  I, W2, X, OldX, Col, OldCol, T, L: Integer;
  S: string;
  SW: WideString;
  C: WideChar;
  Size: TSize;
begin
  Dec(ARow); // TODO: 更新下标

  if (ARow >= 0) and (ARow < FStrings.Count) then
  begin
    // 有字符串，按字符串实际来
    S := FStrings[ARow];
{$IFDEF UNICODE}
    L := Length(S);
{$ELSE}
    SW := WideString(S);
    L := Length(SW);
{$ENDIF}

    I := 1;
    X := 0;
    Col := 1;

    DirectCalc := FFontIsFixedWidth or not HandleAllocated;
    W2 := FAveCharWidth * 2;

    while I <= L do
    begin
{$IFDEF UNICODE}
      C := S[I];
{$ELSE}
      C := SW[I];
{$ENDIF}

      W := WideCharIsWideLength(C);
      OldX := X;
      OldCol := Col;

      if not DirectCalc then
      begin
        // 用 Canvas.TextWidth 计算 S[1] 到 S[I] 的长度 X，而不是下面的字符宽度累加
{$IFDEF UNICODE}
        GetTextExtentPoint32(Canvas.Handle, PChar(S), I, Size);
{$ELSE}
        // I 指向宽字符串的下标索引，非 Unicode 环境下要转成 Ansi 模式再量宽度
        T := CalcAnsiLengthFromWideStringOffset(PWideChar(SW), I);
        GetTextExtentPoint32(Canvas.Handle, PChar(S), T, Size);
{$ENDIF}
        X := Size.cx;
      end;

      if W then
      begin
        if DirectCalc then
          Inc(X, W2);
        Inc(Col, 2);
      end
      else
      begin
        if DirectCalc then
          Inc(X, FAveCharWidth);
        Inc(Col);
      end;
      // 增加后，X 是第 I 个字符的右侧坐标，OldX 是左侧，
      // OldCol 是第 I 个字符左侧，Col 是第 I 个字符右侧

      if OldCol = ACol then
      begin
        // 刚好相等，说明就是这个字符
        Rect.Left := OldX;
        Rect.Right := X;
        Rect.Top := 0;
        Rect.Bottom := FLineHeight;

        DoubleWidth := W;
        Result := True;
        Exit;
      end
      else if OldCol > ACol then // 本列无效，退出
      begin
        Result := False;
        Exit;
      end;

      Inc(I);
    end;

    // 如果到这都还没超过，说明光标过尾巴了，此时 X 是尾字符右侧，OldX 是左侧，
    // I 是尾字符，Col 是尾字符右侧，OldCol 是左侧
    DoubleWidth := False;
    Rect.Top := 0;
    Rect.Bottom := FLineHeight;
    Rect.Left := X + (ACol - Col) * FAveCharWidth;
    Rect.Right := Rect.Left + FAveCharWidth;

    Result := True;
  end
  else
  begin
    // 没有字符串，按等宽直接计算
    Result := inherited CalcPixelOffsetFromColumnInLine(ARow, ACol, Rect, DoubleWidth)
  end;
end;

procedure TCnStringsControl.CopySelectionToClipboard;
var
  S: string;
begin
  S := SelectText;
  if S <> '' then
    Clipboard.AsText := S;
end;

constructor TCnStringsControl.Create(AOwner: TComponent);
begin
  inherited;
  FStrings := TStringList.Create;
  FStrings.OnChange := StringsChange;
end;

destructor TCnStringsControl.Destroy;
begin
  FStrings.OnChange := nil;
  FStrings.Clear;
  inherited;
end;

procedure TCnStringsControl.DoPaintLine(LineCanvas: TCanvas; LineNumber,
  HoriCharOffset: Integer; LineRect: TRect);
var
  S, S1: string;
{$IFNDEF UNICODE}
  WS: WideString;
{$ENDIF}
  SSR, SSC, SER, SEC, T, NewValue: Integer;
begin
  // Dec(LineNumber); // TODO: 更新下标

  if (LineNumber - 1 >= 0) and (LineNumber - 1 < FStrings.Count) then
  begin
    S := FStrings[LineNumber - 1];
    if UseSelection and HasSelection then
    begin
      // 判断本行是否在选择区，分五种情况
      // 不在、整行全是、整行左半是、整行右半是、整行中间是
      SSR := SelectStartRow;
      SSC := SelectStartCol;
      SER := SelectEndRow;
      SEC := SelectEndCol;

      MakeOrderSelection(SSR, SSC, SER, SEC);
      // 确保 StartRow/Col 在 EndRow/Col 前面

      // 注意 SSC 与 SRC 是视觉列号也就是 Column，Ansi 下大部分类似于 Ansi 的字符下标
      // 但在 Unicode 环境下不能直接拿来做字符串下标，得转成字符串下标

      if ((LineNumber < SSR) and (LineNumber < SER)) or
        ((LineNumber > SSR) and (LineNumber > SER)) then
      begin
        // 在选择区外，正常画
        LineCanvas.Font.Color := Font.Color;
        LineCanvas.Brush.Style := bsClear;
        LineCanvas.TextOut(LineRect.Left, LineRect.Top, S);
      end
      else if (LineNumber = SSR) and (LineNumber <> SER) then
      begin
        // 等于起始行但不等于结尾行，从 1 到 SSC - 1 画正常区，SSC 后画选择区
        // SSC 转成 CharIndex，要 Column 右边的 CharIndex
        if MapColumnToWideCharIndexes(S, SSC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SSC := NewValue;
{$ELSE}
          WS := WideString(S);
          SSC := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        LineCanvas.Font.Color := Font.Color;
        LineCanvas.Brush.Style := bsClear;
        S1 := Copy(S, 1, SSC - 1);
        if S1 <> '' then
        begin
          LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
          T := LineCanvas.TextWidth(S1);
          Inc(LineRect.Left, T);
        end;

        LineCanvas.Brush.Style := bsSolid;
        LineCanvas.Brush.Color := clHighlight;
        LineCanvas.FillRect(LineRect);
        S1 := Copy(S, SSC, MaxInt);
        if S1 <> '' then
        begin
          LineCanvas.Font.Color := clHighlightText;
          LineCanvas.Brush.Style := bsClear;
          LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
        end;
      end
      else if (LineNumber = SER) and (LineNumber <> SSR) then
      begin
        // 等于结尾行但不等于起始行，从 1 到 SEC - 1 画选择区，SEC 后画正常区
        // SEC 转成 CharIndex，要 Column 右边的 CharIndex
        if MapColumnToWideCharIndexes(S, SEC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SEC := NewValue;
{$ELSE}
          WS := WideString(S);
          SEC := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        S1 := Copy(S, 1, SEC - 1);
        if S1 <> '' then
        begin
          T := LineCanvas.TextWidth(S1);

          LineCanvas.Brush.Style := bsSolid;
          LineCanvas.Brush.Color := clHighlight;

          LineRect.Right := T;
          LineCanvas.FillRect(LineRect);

          LineCanvas.Font.Color := clHighlightText;
          LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
          Inc(LineRect.Left, T);
        end;
        S1 := Copy(S, SEC, MaxInt);
        if S1 <> '' then
        begin
          LineCanvas.Brush.Style := bsClear;
          LineCanvas.Font.Color := Font.Color;
          LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
        end;
      end
      else if (LineNumber > SSR) and (LineNumber < SER) then
      begin
        // 在选择区内，全画选择色
        LineCanvas.Brush.Style := bsSolid;
        LineCanvas.Brush.Color := clHighlight;
        LineCanvas.FillRect(LineRect);

        LineCanvas.Font.Color := clHighlightText;
        LineCanvas.Brush.Style := bsClear;
        LineCanvas.TextOut(LineRect.Left, LineRect.Top, S);
      end
      else
      begin
        // 在选择行内，从 1 到 SSC - 1 画正常，SSC 到 SEC 中间画选择区，SEC + 1 后画正常
        // SSC，SEC 都转成 CharIndex，要 Column 右边的 CharIndex
{$IFNDEF UNICODE}
        WS := WideString(S);
{$ENDIF}
        if MapColumnToWideCharIndexes(S, SSC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SSC := NewValue;
{$ELSE}
          SSC := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        if MapColumnToWideCharIndexes(S, SEC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SEC := NewValue;
{$ELSE}
          SEC := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        S1 := Copy(S, 1, SSC - 1);
        if S1 <> '' then   // 画正常区
        begin
          T := LineCanvas.TextWidth(S1);
          LineCanvas.Font.Color := Font.Color;
          LineCanvas.Brush.Style := bsClear;
          LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
          Inc(LineRect.Left, T);
        end;

        S1 := Copy(S, SSC, SEC - SSC);
        if S1 <> '' then   // 画选择区
        begin
          T := LineCanvas.TextWidth(S1);
          LineCanvas.Brush.Style := bsSolid;
          LineCanvas.Brush.Color := clHighlight;
          LineRect.Right := LineRect.Left + T;
          LineCanvas.FillRect(LineRect);

          LineCanvas.Font.Color := clHighlightText;
          LineCanvas.Brush.Style := bsClear;
          LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);

          Inc(LineRect.Left, T);
        end;

        S1 := Copy(S, SEC, MaxInt);
        if S1 <> '' then   // 画正常区
        begin
          LineCanvas.Font.Color := Font.Color;
          LineCanvas.Brush.Style := bsClear;
          LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
        end;
      end;
    end
    else
    begin
      LineCanvas.Font.Color := Font.Color;
      LineCanvas.Brush.Style := bsClear;
      LineCanvas.TextOut(LineRect.Left, LineRect.Top, S);
    end;
  end;
end;

function TCnStringsControl.GetLastColumnFromLine(LineNumber: Integer): Integer;
var
  R: Integer;
begin
  Result := 1;
  Dec(LineNumber); // TODO: 更新下标

  if (LineNumber >= 1) and (LineNumber <= FStrings.Count) then
    MapWideCharIndexToColumns(FStrings[LineNumber], MaxInt, Result, R);
end;

function TCnStringsControl.GetLines: TStringList;
begin
  Result := FStrings;
end;

function TCnStringsControl.GetNearestColumn(AColumn, ARow: Integer): Integer;
var
  L, R: Integer;
begin
  Result := AColumn;
  Dec(ARow); // TODO: 更新下标

  if (ARow >= 0) and (ARow < FStrings.Count) then
  begin
    if not MapColumnToWideCharIndexes(FStrings[ARow], AColumn, L, R) then
      Result := AColumn - 1;
  end;
end;

function TCnStringsControl.GetNextColumn(AColumn, ARow: Integer;
  ACaretAfterLineEnd: Boolean): Integer;
var
  L, R: Integer;
begin
  Result := AColumn + 1;
  Dec(ARow); // TODO: 更新下标

  if (ARow >= 0) and (ARow < FStrings.Count) then
  begin
    if not MapColumnToWideCharIndexes(FStrings[ARow], AColumn, L, R) then
      Exit;
    if not MapWideCharIndexToColumns(FStrings[ARow], R, L, Result, ACaretAfterLineEnd) then
      Exit;

    if Result = -1 then // 超过末列时返回末列
      Result := L;
  end;
end;

function TCnStringsControl.GetPrevColumn(AColumn, ARow: Integer): Integer;
var
  L, R: Integer;
begin
  Result := AColumn - 1;
  Dec(ARow); // TODO: 更新下标

  if (ARow >= 0) and (ARow < FStrings.Count) then
  begin
    if not MapColumnToWideCharIndexes(FStrings[ARow], AColumn, L, R) then
      Exit;

    if L = 0 then // 超过首列时返回首列
      Result := 1
    else if not MapWideCharIndexToColumns(FStrings[ARow], L, Result, R) then
      Exit;
  end;
end;

function TCnStringsControl.GetSelectText: string;
var
  SSR, SSC, SER, SEC, I, Line, T, NewValue: Integer;
  S: string;
{$IFNDEF UNICODE}
  WS: WideString;
{$ENDIF}
begin
  Result := '';
  if HasSelection then
  begin
    SSR := SelectStartRow;
    SSC := SelectStartCol;
    SER := SelectEndRow;
    SEC := SelectEndCol;

    MakeOrderSelection(SSR, SSC, SER, SEC);
    // 确保 StartRow/Col 在 EndRow/Col 前面

    if SSR = SER then
    begin
      Line := SSR - 1;
      if (Line >= 0) and (Line < FStrings.Count) then
      begin
        S := FStrings[Line];
{$IFNDEF UNICODE}
        WS := WideString(S);
{$ENDIF}

        if MapColumnToWideCharIndexes(S, SSC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SSC := NewValue;
{$ELSE}
          SSC := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        if MapColumnToWideCharIndexes(S, SEC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SEC := NewValue;
{$ELSE}
          SEC := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        // 截取 S 的中间
        Result := Copy(S, SSC, SEC - SSC); 
      end;
    end
    else
    begin
      for I := SSR to SER do
      begin
        Line := I - 1;
        if (Line < 0) or (Line >= FStrings.Count) then
        begin
          Result := Result + CRLF;
          Continue;
        end;

        // S 是当前行内容
        S := FStrings[Line];
        if I = SSR then
        begin
          // 拿首行，这到尾
          if MapColumnToWideCharIndexes(S, SSC, T, NewValue) then
          begin
{$IFDEF UNICODE}
            SSC := NewValue;
{$ELSE}
            WS := WideString(S);
            SSC := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
            Result := Copy(S, SSC, MaxInt);
          end;
        end
        else if I = SER then
        begin
          // 拿末行，头到这
          if MapColumnToWideCharIndexes(S, SEC, T, NewValue) then
          begin
{$IFDEF UNICODE}
            SEC := NewValue;
{$ELSE}
            WS := WideString(S);
            SEC := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
            Result := Result + CRLF + Copy(S, 1, SEC - 1);
          end
          else
            Result := Result + CRLF + S; // Column 转换失败说明超过字符串尾巴了，全加上
        end
        else // 拿整行
        begin
          Result := Result + CRLF + S;
        end;
      end;
    end;
  end;
end;

procedure TCnStringsControl.LoadFromFile(const AFile: string);
begin
  FStrings.LoadFromFile(AFile);
end;

procedure TCnStringsControl.SaveToFile(const AFile: string);
begin
  FStrings.SaveToFile(AFile);
end;

procedure TCnStringsControl.StringsChange(Sender: TObject);
begin
  if FStrings.Count <> MaxLineCount then
  begin
    MaxLineCount := FStrings.Count;

    if HandleAllocated then
      Invalidate;
  end;
end;

{ TCnMemo }

procedure TCnMemo.CutSelectionToClipboard;
var
  S: string;
begin
  if FReadOnly then
    Exit;

  S := SelectText;
  if S <> '' then
  begin
    Clipboard.AsText := S;
    DeleteSelection;
    Invalidate;
  end;
end;

procedure TCnMemo.DeleteSelection;
begin
  if not FReadOnly and HasSelection then
  begin
    MakeOrderSelection(FSelectStartRow, FSelectStartCol, FSelectEndRow, FSelectEndCol);
    DeleteText(FSelectStartRow, FSelectStartCol, FSelectEndRow, FSelectEndCol);
    SetCaretRowCol(FSelectStartRow, FSelectStartCol);
    SyncSelectionStartEnd(True);
    ScrollToVisibleCaret;
    DoSelectChange;
  end;
end;

function TCnMemo.DeleteText(StartRow, StartCol, EndRow,
  EndCol: Integer): Boolean;
var
  SSR, SSC, SER, SEC, I, Line, T, NewValue: Integer;
  S, LastS: string;
{$IFNDEF UNICODE}
  WS: WideString;
{$ENDIF}
begin
  Result := False;
  if (StartRow = EndRow) and (StartCol = EndCol) then
    Exit;

  SSR := StartRow;
  SSC := StartCol;
  SER := EndRow;
  SEC := EndCol;

  MakeOrderSelection(SSR, SSC, SER, SEC);
  // 确保 StartRow/Col 在 EndRow/Col 前面

  try
    FStrings.BeginUpdate;
    if SSR = SER then
    begin
      Line := SSR - 1;
      if (Line >= 0) and (Line < FStrings.Count) then
      begin
        S := FStrings[Line];
{$IFNDEF UNICODE}
        WS := WideString(S);
{$ENDIF}

        if MapColumnToWideCharIndexes(S, SSC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SSC := NewValue;
{$ELSE}
          SSC := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        if MapColumnToWideCharIndexes(S, SEC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SEC := NewValue;
{$ELSE}
          SEC := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        // 删除 S 的中间，从 SSC 开始，长 SEC - SSC 的字符
        Delete(S, SSC, SEC - SSC);
        FStrings[Line] := S;
        Result := True;
      end;
    end
    else
    begin
      LastS := '';
      for I := SER downto SSR do
      begin
        Line := I - 1;
        if (Line < 0) or (Line >= FStrings.Count) then
          Continue;

        // S 是当前行内容
        S := FStrings[Line];
        if I = SSR then
        begin
          // 删首行中，这到尾的内容
          if MapColumnToWideCharIndexes(S, SSC, T, NewValue) then
          begin
{$IFDEF UNICODE}
            SSC := NewValue;
{$ELSE}
            WS := WideString(S);
            SSC := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
            Delete(S, SSC, MaxInt);
          end
          else
          begin
            // 如果光标超末尾，则 S 要加上相应的空格到指定 SSC
            T := GetLastColumnFromLine(SSR);
            if SSC > T then
              S := S + StringOfChar(' ', SSC - T);
          end;

          if LastS <> '' then // 再拼上末行剩下的内容
            S := S + LastS;
          FStrings[Line] := S;;
        end
        else if I = SER then
        begin
          // 删末行中，头到这的内容
          if MapColumnToWideCharIndexes(S, SEC, T, NewValue) then
          begin
{$IFDEF UNICODE}
            SEC := NewValue;
{$ELSE}
            WS := WideString(S);
            SEC := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
            Delete(S, 1, SEC - 1);

            // 此时末行剩下这部分内容，最后还要拼到首行头上去，末行本身不用改动
            LastS := S;
          end;
          FStrings.Delete(Line); // Column 转换失败表示超过行尾，要全删，删后就不用往上拼了
        end
        else // 拿整行
        begin
          FStrings.Delete(Line);
        end;
      end;
      Result := True;
    end;
  finally
    FStrings.EndUpdate;
  end;
end;

procedure TCnMemo.DisableStringsChange;
begin
  FStrings.OnChange := nil;
end;

procedure TCnMemo.EnableStringsChange;
begin
  FStrings.OnChange := StringsChange;
end;

procedure TCnMemo.InsertText(const Text: string);
var
  DR, DC: Integer;
begin
  if not UseCaret or FReadOnly then
    Exit;

  if UseSelection and HasSelection then
    DeleteSelection;

  InsertTextAt(Text, CaretRow, CaretCol, DR, DC);
  SetCaretRowCol(CaretRow + DR, CaretCol + DC);

  ScrollToVisibleCaret;
  Invalidate;
end;

function TCnMemo.InsertTextAt(const Text: string; ARow, ACol: Integer;
  out DeltaRow, DeltaCol: Integer): Boolean;
var
  SL: TStringList;
  I, P, T, NewValue, OldCol: Integer;
  S, LastS: string;
{$IFNDEF UNICODE}
  WS: WideString;
{$ENDIF}
begin
  Result := False;
  if (ARow <= 0) or (ACol <= 0) then
    Exit;

  if ARow > MaxLineCount then
    Exit;

  SetCaretRowCol(ARow, ACol);

  // 解析 Text 为多个回车分开的内容
  S := Text;
  SL := TStringList.Create;
  FStrings.BeginUpdate;

  try
    P := Pos(CRLF, S);
    if P > 0 then
    begin
      // 有回车
      while P > 0 do
      begin
        SL.Add(Copy(S, 1, P - 1));
        Delete(S, 1, P - 1 + Length(CRLF));
        P := Pos(CRLF, S);
      end;
      SL.Add(S); // 最后一个回车后面的部分
    end
    else
      SL.Add(S); // 没回车，直接加

    if SL.Count = 1 then
    begin
      // 单行，拿 ARow 行内容，找插入的 CharIndex
      S := FStrings[ARow - 1];
      DeltaRow := 0;
      DeltaCol := 0;

      // 删首行中，这到尾的内容
      if MapColumnToWideCharIndexes(S, ACol, T, NewValue) then
      begin
{$IFDEF UNICODE}
        ACol := NewValue;
{$ELSE}
        WS := WideString(S);
        ACol := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        FStrings[ARow - 1] := Copy(S, 1, ACol - 1) + SL[0] + Copy(S, ACol, MaxInt);
      end
      else
      begin
        // 如果光标超末尾，则 S 要加上相应的空格到指定 SSC
        T := GetLastColumnFromLine(ARow);
        if ACol > T then
        begin
          S := S + StringOfChar(' ', ACol - T);
          Inc(DeltaCol, ACol - T);
        end;
        FStrings[ARow - 1] := S + SL[0];
      end;
      Inc(DeltaCol, GetColumnWidthFromWideString(SL[0]));
      Result := True;
    end
    else
    begin
      DeltaRow := SL.Count - 1;
      LastS := '';
      OldCol := ACol;

      P := 0; // 复用 P 作为插行的增量
      for I := 0 to SL.Count - 1 do
      begin
        // 挨个插入 SL[I]
        if I = 0 then
        begin
          // 第 CaretRow 行的 CaretCol 对应的字符前面的内容 + S[0]
          // 取出首行中，光标前后的内容
          S := FStrings[ARow - 1];
          if MapColumnToWideCharIndexes(S, ACol, T, NewValue) then
          begin
{$IFDEF UNICODE}
            ACol := NewValue;
{$ELSE}
            WS := WideString(S);
            ACol := CalcAnsiLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
            FStrings[ARow - 1] := Copy(S, 1, ACol - 1) + SL[0];
            LastS := Copy(S, ACol, MaxInt);
          end
          else
          begin
            // 如果光标超末尾，则 S 要加上相应的空格到指定 SSC
            T := GetLastColumnFromLine(ARow);
            if ACol > T then
              S := S + StringOfChar(' ', ACol - T);
            FStrings[ARow - 1] := S + SL[0];
          end;
        end
        else if I = SL.Count - 1 then
        begin
          // SL[LAST] 拼在原有的 LastS 之前
          FStrings.Insert(ARow - 1 + I, '');
          FStrings[ARow - 1 + I] := SL[I] + LastS;
          DeltaCol := 1 + GetColumnWidthFromWideString(SL[I]) - OldCol;
          // SL[Last] 在行头，会把光标朝右推 GetColumnWidthFromWideString，所以末尾要加一
        end
        else
        begin
          FStrings.Insert(ARow + P, SL[I]); // 都在原处插的话，后来的会插到先来的后面，所以需要一个 P 累加
          Inc(P);
        end;
      end;
    end;
  finally
    FStrings.EndUpdate;
    SL.Free;
  end;
end;

procedure TCnMemo.KeyDown(var Key: WORD; Shift: TShiftState);
var
  SR, SC, ER, EC, NC: Integer;
begin
  inherited;

  if Key = VK_DELETE then
  begin
    if FReadOnly then
      Exit;

    if UseSelection and HasSelection then // 有选择区就删选择区，光标要停留在顺序的 StartRow/Col
      DeleteSelection
    else
    begin
      // 删光标后的字符，如果后面没字符，则把下一行拼上来并删下一行，如果没下一行就啥都不做
      SR := CaretRow;
      SC := CaretCol;
      ER := CaretRow;
      EC := CaretCol;

      // 如果 EC 是行尾或超出行尾，则跑下一行
      if EC >= GetLastColumnFromLine(SR) then
      begin
        Inc(ER);
        EC := 1;
      end
      else
        EC := GetNextColumn(SC, SR, False);

      DeleteText(SR, SC, ER, EC);
      SetCaretRowCol(SR, SC);
      SyncSelectionStartEnd(True);
      ScrollToVisibleCaret;
      Invalidate;
    end;
  end
  else if Key = VK_BACK then
  begin
    if FReadOnly then
      Exit;

    if UseSelection and HasSelection then // 有选择区就删选择区，光标要停留在顺序的 StartRow/Col
      DeleteSelection
    else
    begin
      // 删光标前的字符，如果前面没字符，则把当前行往上拼并删当前行，如果没上一行就啥都不做
      SR := CaretRow;
      SC := CaretCol;
      ER := CaretRow;
      EC := CaretCol;
      NC := GetPrevColumn(SC, SR);
      if NC = SC then // 前面没字符了，跑上一行
      begin
        if SR = 1 then
          Exit;

        Dec(SR);
        SC := GetLastColumnFromLine(SR);
      end
      else
        SC := NC;

      DeleteText(SR, SC, ER, EC);
      SetCaretRowCol(SR, SC);
      SyncSelectionStartEnd(True);
      ScrollToVisibleCaret;
      Invalidate;
    end;
  end;
end;

procedure TCnMemo.PasteFromClipboard;
var
  S: string;
begin
  if FReadOnly then
    Exit;

  S := ClipBoard.AsText;
  if S <> '' then
    InsertText(S);
end;

procedure TCnMemo.WMKeyChar(var Message: TMessage);
var
  Key: Word;
  Ch: Char;
begin
  Key := Message.wParam;
  Ch := Char(Key);

  if FReadOnly then
    Exit;

  if Ch in [#13, #10] then
    InsertText(CRLF)
  else if Ch = #8 then
  begin
    // TODO: 处理 Tab
  end
  else if Ord(Ch) >= Ord(20) then
  begin
{$IFDEF UNICODE}
    InsertText(string(Ch)); // Unicode 时输入法敲出来的双字节字符直接通过一个 WM_CHAR 发来
{$ELSE}
    if Ord(Ch) > 127 then // 非 Unicode 时输入法敲出来的双字节字符会连来两个 WM_CHAR
    begin
      if FPrevChar <> #0 then
      begin
        InsertText(string(FPrevChar) + string(Ch));
        FPrevChar := #0;
      end
      else
        FPrevChar := Ch;
    end
    else
    begin
      InsertText(string(Ch));
      FPrevChar := #0;
    end;
{$ENDIF}
  end;
end;

end.
