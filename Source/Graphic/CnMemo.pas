{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnMemo;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ����к���ʾ���ܵ� Memo
* ��Ԫ���ߣ�CnPack ������ (master@cnpack.org)
* ��    ע���õ�Ԫ��ǰ��Ϊ�ڲ��ο�������
* ����ƽ̨��PWin7 + Delphi 5.0
* ���ݲ��ԣ�PWinXP/7 + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2015.07.26 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Classes, Messages, Controls, Graphics, StdCtrls, ExtCtrls,
  Dialogs, SysConst, Forms, Clipbrd, CnNative, CnTextControl, CnCommon, CnWideStrings;

type
{$IFDEF UNICODE}
  TCnEditorString = string;
{$ELSE}
  TCnEditorString = WideString;
{$ENDIF}

  PCnEditorStringMark = ^TCnEditorStringMark;
  TCnEditorStringMark = packed record
    LineNumNoWrap: Integer;       // δ�Զ�����ʱ��ǰ�е������кţ�1 ��ʼ������ʾ�ڲ�����ϡ����Զ����У�ֵ����һ����ͬ
    LineNumAfterElide: Integer;   // �۵�����кţ������ã�1 ��ʼ
    ElidedStartIndex: Word;       // �������۵�ͷʱ���۵�ͷ����ͷ��ƫ������0 ��ʼ
    ElidedStartLength: Word;      // �������۵�ͷʱ���۵�ͷ���ַ����ȣ�0 ��ʾ��
    ElidedEndIndex: Word;         // �������۵�βʱ���۵�β����ͷ��ƫ������0 ��ʼ
    ElidedEndLength: Word;        // �������۵�βʱ���۵�β���ַ����ȣ�0 ��ʾ��
    Elided: Boolean;              // �����Ƿ��۵���ʾ
  end;

  PCnEditorStringItem = ^TCnEditorStringItem;
  TCnEditorStringItem = packed record
  {* �༭���д���һ�е�����}
    FString: string;              // ��ǰ������
    FMark: TCnEditorStringMark;   // ��ǰ�и��ӱ��
  end;

  PCnEditorStringItemList = ^TCnEditorStringItemList;
  TCnEditorStringItemList = array[1..MaxListSize div 4] of TCnEditorStringItem;

  TCnEditorStringList = class(TPersistent)
  {* �༭���е��ַ����б�����±��� 1 ��ʼ}
  private
    FUpdateCount: Integer;
    FList: PCnEditorStringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FAutoWrap: Boolean;
    FWrapWidth: Integer;
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
    procedure ExchangeItems(Index1, Index2: Integer); virtual;
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
    {* �Ƿ��Զ����У��ı�ʱ�����ڲ��Ű�}
    property WrapWidth: Integer read FWrapWidth write SetWrapWidth;
    {* �Զ�����ʱ���ַ����}

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;

  TCnStringsControl = class(TCnVirtualTextControl)
  {* �о����ַ����б���ʾ���ܵ��ı����}
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
    {* ��ѡ�����ݸ��Ƶ�������}

    property Lines: TStringList read GetLines;
    property SelectText: string read GetSelectText;
  published

  end;

  TCnMemo = class(TCnStringsControl)
  {* ���ַ����༭���ܵ��ı����}
  private
    FReadOnly: Boolean;
{$IFNDEF UNICODE}
    FPrevChar: Char;
{$ENDIF}
    function DeleteText(StartRow, StartCol, EndRow, EndCol: Integer): Boolean;
    {* ɾ����ʼ���е��������м������}
    function InsertTextAt(const Text: string; ARow, ACol: Integer;
      out DeltaRow, DeltaCol: Integer): Boolean;
    {* ��ָ�����λ�ô������ı��������ؼ���Ĺ���ƶ�ƫ�������������ƶ�}
  protected
    procedure WMKeyChar(var Message: TMessage); message WM_CHAR;
    procedure KeyDown(var Key: WORD; Shift: TShiftState); override;

    procedure DisableStringsChange;
    procedure EnableStringsChange;
  public
    procedure DeleteSelection;
    {* ɾ��ѡ�����������ػ�}
    procedure CutSelectionToClipboard;
    {* ��ѡ�������ݼ�����������}
    procedure PasteFromClipboard;
    {* �Ӽ�����ճ��}

    procedure InsertText(const Text: string);

    property ReadOnly: Boolean read FReadOnly write FReadOnly;
  end;

function MapColumnToWideCharIndexes(const S: TCnEditorString; AColumn: Integer;
  out LeftCharIndex, RightCharIndex: Integer): Boolean;
{* ���ؿ��ַ�����ָ�� Column ���ҵ��ַ����������� 1 ��ʼ���Ƿ��Ļ򳬴�� Column ���� False
   ע�� Column ���ַ���βʱ��RightIndex ��� Length �� 1
   ע�� Column ���ַ���ͷҲ���� 1 ʱ��LeftIndex �� 0
   ��Ҫע������ַ����ǿ��� Column �� 1 ʱ��LeftIndex �� 0��RightIndex �� 1
}

function MapWideCharIndexToColumns(const S: TCnEditorString; ACharIndex: Integer;
  out LeftColumn, RightColumn: Integer; AfterEnd: Boolean = False): Boolean;
{* ���ؿ��ַ�����ָ���ַ������������ߵ� Column������ 1 ��ʼ���Ƿ��� CharIndex ���� False
   CharIndex ָ�����һ���ַ�ʱ RightColumn ����ĩ��
   CharIndex ����ĩ�ַ�ʱ������ AfterEnd ��ֵ�жϡ�AfterEnd ��ʾ Column �Ƿ�������β
     False ʱ LeftColumn ����ĩ�У�RightColumn δ����
       �ر�ģ����ַ���ֻ�е� CharIndex >= 1 ʱ LeftColumn ���� 1��RightColumn δ����
     True ʱ CharInde ��ĩ�ַ� + 1 ��Ӧ ĩ����ĩ�� + 1���Կո�ѵ���ȥ�Դ�����
   �ر�ģ�AfterEnd Ϊ False �� CharIndex ������ MaxInt ʱ��LeftColumn ����ĩ�У�
     LeftColumn - 1 Ϊ���ַ�����ռ���п�
   }
function GetColumnWidthFromWideString(const S: TCnEditorString): Integer;
{* ���ؿ��ַ�����ռ���п�}

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
  Temp: TCnNativeInt;
  Item1, Item2: PCnEditorStringItem;
  TempMark: TCnEditorStringMark;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := TCnNativeInt(Item1^.FString);

  TCnNativeInt(Item1^.FString) := TCnNativeInt(Item2^.FString);
  TCnNativeInt(Item2^.FString) := Temp;

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

  Pointer(FList^[Index].FString) := nil; // ����ֱ�Ӹ�ֵΪ nil
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

{$WARNINGS OFF}

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

{$WARNINGS ON}

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
    Exit;  // ���ַ�ֻ�ܴ��� Column Ϊ 1 �����

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
  L, I, T: Integer;
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
    else if ACharIndex >= 1 then // ����������β
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

    T := RightColumn; // 2 out can NOT direct assign
    LeftColumn := T;

    if WideCharIsWideLength(C) then
      Inc(RightColumn, 2)
    else
      Inc(RightColumn);

    if I >= ACharIndex then
      Break;

    Inc(I);
  end;

  if I > L then // ��ʱ I ָ��ĩ�ַ��ĺ�һ���ַ�
  begin
    if AfterEnd then
    begin
      LeftColumn := RightColumn + ACharIndex - L - 1;
      RightColumn := LeftColumn + 1;
    end
    else
    begin
      T := RightColumn;  // 2 out can NOT direct assign
      LeftColumn := T;
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
  I, L, X, OldX, W2: Integer;
  W, DirectCalc: Boolean;
  S: string;
{$IFNDEF UNICODE}
  T: Integer;
  SW: WideString;
{$ENDIF}
  C: WideChar;
  Size: TSize;
begin
  Dec(ARow); // TODO: �����±�

  if (ARow >= 0) and (ARow < FStrings.Count) then
  begin
    // ���ַ��������ַ���ʵ����
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
    // �� S[1] �� S[length] �ۼӼ��� Column����ֱ�Ӽ���������Ҳࣨ�ȿ����壩
    // �� TextWidth ���ǵȿ������ĸ��պó��� X

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
        // �� Canvas.TextWidth ���� S[1] �� S[I] �ĳ��� X��������������ַ�����ۼ�
{$IFDEF UNICODE}
        GetTextExtentPoint32(Canvas.Handle, PChar(S), I, Size);
{$ELSE}
        // I ָ����ַ������±��������� Unicode ������Ҫת�� Ansi ģʽ�������
        T := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(SW), I);
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
          Inc(X, FAveCharWidth);  // ���Ӻ�X �ǵ� I ���ַ����Ҳ����꣬Col �ǵ� I ���ַ��Ҳ�
        Inc(Col);
      end;

      if X > VirtualX then
      begin
        // �պó�����˵����������ַ���X �� Col Ҫ�Ѹռӵĸ�����ȥ
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

    // ������ⶼ��û������˵������β���ˣ���ʱ X ��β�ַ��Ҳ࣬I ��β�ַ���Col ��β�ַ��Ҳ�
    DoubleWidth := False;
    I := (VirtualX - X) div FAveCharWidth;
    Inc(Col, I);
    LeftHalf := (VirtualX - X - FAveCharWidth * I) < FAveCharWidthHalf;
    Result := True;
  end
  else
  begin
    // û���ַ��������ȿ�ֱ�Ӽ���
    Result := inherited CalcColumnFromPixelOffsetInLine(ARow + 1, VirtualX, Col,
      LeftHalf, DoubleWidth);
  end;
end;

function TCnStringsControl.CalcPixelOffsetFromColumnInLine(ARow, ACol: Integer;
  out Rect: TRect; out DoubleWidth: Boolean): Boolean;
var
  W, DirectCalc: Boolean;
  I, W2, X, OldX, Col, OldCol, L: Integer;
  S: string;
{$IFNDEF UNICODE}
  T: Integer;
  SW: WideString;
{$ENDIF}
  C: WideChar;
  Size: TSize;
begin
  Dec(ARow); // TODO: �����±�

  if (ARow >= 0) and (ARow < FStrings.Count) then
  begin
    // ���ַ��������ַ���ʵ����
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
        // �� Canvas.TextWidth ���� S[1] �� S[I] �ĳ��� X��������������ַ�����ۼ�
{$IFDEF UNICODE}
        GetTextExtentPoint32(Canvas.Handle, PChar(S), I, Size);
{$ELSE}
        // I ָ����ַ������±��������� Unicode ������Ҫת�� Ansi ģʽ�������
        T := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(SW), I);
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
      // ���Ӻ�X �ǵ� I ���ַ����Ҳ����꣬OldX ����࣬
      // OldCol �ǵ� I ���ַ���࣬Col �ǵ� I ���ַ��Ҳ�

      if OldCol = ACol then
      begin
        // �պ���ȣ�˵����������ַ�
        Rect.Left := OldX;
        Rect.Right := X;
        Rect.Top := 0;
        Rect.Bottom := FLineHeight;

        DoubleWidth := W;
        Result := True;
        Exit;
      end
      else if OldCol > ACol then // ������Ч���˳�
      begin
        Result := False;
        Exit;
      end;

      Inc(I);
    end;

    // ������ⶼ��û������˵������β���ˣ���ʱ X ��β�ַ��Ҳ࣬OldX ����࣬
    // I ��β�ַ���Col ��β�ַ��Ҳ࣬OldCol �����
    DoubleWidth := False;
    Rect.Top := 0;
    Rect.Bottom := FLineHeight;
    Rect.Left := X + (ACol - Col) * FAveCharWidth;
    Rect.Right := Rect.Left + FAveCharWidth;

    Result := True;
  end
  else
  begin
    // û���ַ��������ȿ�ֱ�Ӽ���
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
  // Dec(LineNumber); // TODO: �����±�

  if (LineNumber - 1 >= 0) and (LineNumber - 1 < FStrings.Count) then
  begin
    S := FStrings[LineNumber - 1];
    if UseSelection and HasSelection then
    begin
      // �жϱ����Ƿ���ѡ���������������
      // ���ڡ�����ȫ�ǡ���������ǡ������Ұ��ǡ������м���
      SSR := SelectStartRow;
      SSC := SelectStartCol;
      SER := SelectEndRow;
      SEC := SelectEndCol;

      MakeOrderSelection(SSR, SSC, SER, SEC);
      // ȷ�� StartRow/Col �� EndRow/Col ǰ��

      // ע�� SSC �� SRC ���Ӿ��к�Ҳ���� Column��Ansi �´󲿷������� Ansi ���ַ��±�
      // ���� Unicode �����²���ֱ���������ַ����±꣬��ת���ַ����±�

      if ((LineNumber < SSR) and (LineNumber < SER)) or
        ((LineNumber > SSR) and (LineNumber > SER)) then
      begin
        // ��ѡ�����⣬������
        LineCanvas.Font.Color := Font.Color;
        LineCanvas.Brush.Style := bsClear;
        LineCanvas.TextOut(LineRect.Left, LineRect.Top, S);
      end
      else if (LineNumber = SSR) and (LineNumber <> SER) then
      begin
        // ������ʼ�е������ڽ�β�У��� 1 �� SSC - 1 ����������SSC ��ѡ����
        // SSC ת�� CharIndex��Ҫ Column �ұߵ� CharIndex
        if MapColumnToWideCharIndexes(S, SSC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SSC := NewValue;
{$ELSE}
          WS := WideString(S);
          SSC := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
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
        // ���ڽ�β�е���������ʼ�У��� 1 �� SEC - 1 ��ѡ������SEC ��������
        // SEC ת�� CharIndex��Ҫ Column �ұߵ� CharIndex
        if MapColumnToWideCharIndexes(S, SEC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SEC := NewValue;
{$ELSE}
          WS := WideString(S);
          SEC := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
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
        // ��ѡ�����ڣ�ȫ��ѡ��ɫ
        LineCanvas.Brush.Style := bsSolid;
        LineCanvas.Brush.Color := clHighlight;
        LineCanvas.FillRect(LineRect);

        LineCanvas.Font.Color := clHighlightText;
        LineCanvas.Brush.Style := bsClear;
        LineCanvas.TextOut(LineRect.Left, LineRect.Top, S);
      end
      else
      begin
        // ��ѡ�����ڣ��� 1 �� SSC - 1 ��������SSC �� SEC �м仭ѡ������SEC + 1 ������
        // SSC��SEC ��ת�� CharIndex��Ҫ Column �ұߵ� CharIndex
{$IFNDEF UNICODE}
        WS := WideString(S);
{$ENDIF}
        if MapColumnToWideCharIndexes(S, SSC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SSC := NewValue;
{$ELSE}
          SSC := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        if MapColumnToWideCharIndexes(S, SEC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SEC := NewValue;
{$ELSE}
          SEC := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        S1 := Copy(S, 1, SSC - 1);
        if S1 <> '' then   // ��������
        begin
          T := LineCanvas.TextWidth(S1);
          LineCanvas.Font.Color := Font.Color;
          LineCanvas.Brush.Style := bsClear;
          LineCanvas.TextOut(LineRect.Left, LineRect.Top, S1);
          Inc(LineRect.Left, T);
        end;

        S1 := Copy(S, SSC, SEC - SSC);
        if S1 <> '' then   // ��ѡ����
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
        if S1 <> '' then   // ��������
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
  Dec(LineNumber); // TODO: �����±�

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
  Dec(ARow); // TODO: �����±�

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
  Dec(ARow); // TODO: �����±�

  if (ARow >= 0) and (ARow < FStrings.Count) then
  begin
    if not MapColumnToWideCharIndexes(FStrings[ARow], AColumn, L, R) then
      Exit;
    if not MapWideCharIndexToColumns(FStrings[ARow], R, L, Result, ACaretAfterLineEnd) then
      Exit;

    if Result = -1 then // ����ĩ��ʱ����ĩ��
      Result := L;
  end;
end;

function TCnStringsControl.GetPrevColumn(AColumn, ARow: Integer): Integer;
var
  L, R: Integer;
begin
  Result := AColumn - 1;
  Dec(ARow); // TODO: �����±�

  if (ARow >= 0) and (ARow < FStrings.Count) then
  begin
    if not MapColumnToWideCharIndexes(FStrings[ARow], AColumn, L, R) then
      Exit;

    if L = 0 then // ��������ʱ��������
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
    // ȷ�� StartRow/Col �� EndRow/Col ǰ��

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
          SSC := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        if MapColumnToWideCharIndexes(S, SEC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SEC := NewValue;
{$ELSE}
          SEC := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        // ��ȡ S ���м�
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

        // S �ǵ�ǰ������
        S := FStrings[Line];
        if I = SSR then
        begin
          // �����У��⵽β
          if MapColumnToWideCharIndexes(S, SSC, T, NewValue) then
          begin
{$IFDEF UNICODE}
            SSC := NewValue;
{$ELSE}
            WS := WideString(S);
            SSC := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
            Result := Copy(S, SSC, MaxInt);
          end;
        end
        else if I = SER then
        begin
          // ��ĩ�У�ͷ����
          if MapColumnToWideCharIndexes(S, SEC, T, NewValue) then
          begin
{$IFDEF UNICODE}
            SEC := NewValue;
{$ELSE}
            WS := WideString(S);
            SEC := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
            Result := Result + CRLF + Copy(S, 1, SEC - 1);
          end
          else
            Result := Result + CRLF + S; // Column ת��ʧ��˵�������ַ���β���ˣ�ȫ����
        end
        else // ������
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
  // ȷ�� StartRow/Col �� EndRow/Col ǰ��

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
          SSC := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        if MapColumnToWideCharIndexes(S, SEC, T, NewValue) then
        begin
{$IFDEF UNICODE}
          SEC := NewValue;
{$ELSE}
          SEC := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        end;

        // ɾ�� S ���м䣬�� SSC ��ʼ���� SEC - SSC ���ַ�
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

        // S �ǵ�ǰ������
        S := FStrings[Line];
        if I = SSR then
        begin
          // ɾ�����У��⵽β������
          if MapColumnToWideCharIndexes(S, SSC, T, NewValue) then
          begin
{$IFDEF UNICODE}
            SSC := NewValue;
{$ELSE}
            WS := WideString(S);
            SSC := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
            Delete(S, SSC, MaxInt);
          end
          else
          begin
            // �����곬ĩβ���� S Ҫ������Ӧ�Ŀո�ָ�� SSC
            T := GetLastColumnFromLine(SSR);
            if SSC > T then
              S := S + StringOfChar(' ', SSC - T);
          end;

          if LastS <> '' then // ��ƴ��ĩ��ʣ�µ�����
            S := S + LastS;
          FStrings[Line] := S;;
        end
        else if I = SER then
        begin
          // ɾĩ���У�ͷ���������
          if MapColumnToWideCharIndexes(S, SEC, T, NewValue) then
          begin
{$IFDEF UNICODE}
            SEC := NewValue;
{$ELSE}
            WS := WideString(S);
            SEC := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
            Delete(S, 1, SEC - 1);

            // ��ʱĩ��ʣ���ⲿ�����ݣ����Ҫƴ������ͷ��ȥ��ĩ�б����øĶ�
            LastS := S;
          end;
          FStrings.Delete(Line); // Column ת��ʧ�ܱ�ʾ������β��Ҫȫɾ��ɾ��Ͳ�������ƴ��
        end
        else // ������
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

  // ���� Text Ϊ����س��ֿ�������
  S := Text;
  SL := TStringList.Create;
  FStrings.BeginUpdate;

  try
    P := Pos(CRLF, S);
    if P > 0 then
    begin
      // �лس�
      while P > 0 do
      begin
        SL.Add(Copy(S, 1, P - 1));
        Delete(S, 1, P - 1 + Length(CRLF));
        P := Pos(CRLF, S);
      end;
      SL.Add(S); // ���һ���س�����Ĳ���
    end
    else
      SL.Add(S); // û�س���ֱ�Ӽ�

    if SL.Count = 1 then
    begin
      // ���У��� ARow �����ݣ��Ҳ���� CharIndex
      S := FStrings[ARow - 1];
      DeltaRow := 0;
      DeltaCol := 0;

      // ɾ�����У��⵽β������
      if MapColumnToWideCharIndexes(S, ACol, T, NewValue) then
      begin
{$IFDEF UNICODE}
        ACol := NewValue;
{$ELSE}
        WS := WideString(S);
        ACol := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
        FStrings[ARow - 1] := Copy(S, 1, ACol - 1) + SL[0] + Copy(S, ACol, MaxInt);
      end
      else
      begin
        // �����곬ĩβ���� S Ҫ������Ӧ�Ŀո�ָ�� SSC
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

      P := 0; // ���� P ��Ϊ���е�����
      for I := 0 to SL.Count - 1 do
      begin
        // �������� SL[I]
        if I = 0 then
        begin
          // �� CaretRow �е� CaretCol ��Ӧ���ַ�ǰ������� + S[0]
          // ȡ�������У����ǰ�������
          S := FStrings[ARow - 1];
          if MapColumnToWideCharIndexes(S, ACol, T, NewValue) then
          begin
{$IFDEF UNICODE}
            ACol := NewValue;
{$ELSE}
            WS := WideString(S);
            ACol := CalcAnsiDisplayLengthFromWideStringOffset(PWideChar(WS), NewValue - 1) + 1;
{$ENDIF}
            FStrings[ARow - 1] := Copy(S, 1, ACol - 1) + SL[0];
            LastS := Copy(S, ACol, MaxInt);
          end
          else
          begin
            // �����곬ĩβ���� S Ҫ������Ӧ�Ŀո�ָ�� SSC
            T := GetLastColumnFromLine(ARow);
            if ACol > T then
              S := S + StringOfChar(' ', ACol - T);
            FStrings[ARow - 1] := S + SL[0];
          end;
        end
        else if I = SL.Count - 1 then
        begin
          // SL[LAST] ƴ��ԭ�е� LastS ֮ǰ
          FStrings.Insert(ARow - 1 + I, '');
          FStrings[ARow - 1 + I] := SL[I] + LastS;
          DeltaCol := 1 + GetColumnWidthFromWideString(SL[I]) - OldCol;
          // SL[Last] ����ͷ����ѹ�곯���� GetColumnWidthFromWideString������ĩβҪ��һ
        end
        else
        begin
          FStrings.Insert(ARow + P, SL[I]); // ����ԭ����Ļ��������Ļ�嵽�����ĺ��棬������Ҫһ�� P �ۼ�
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

    if UseSelection and HasSelection then // ��ѡ������ɾѡ���������Ҫͣ����˳��� StartRow/Col
      DeleteSelection
    else
    begin
      // ɾ������ַ����������û�ַ��������һ��ƴ������ɾ��һ�У����û��һ�о�ɶ������
      SR := CaretRow;
      SC := CaretCol;
      ER := CaretRow;
      EC := CaretCol;

      // ��� EC ����β�򳬳���β��������һ��
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

    if UseSelection and HasSelection then // ��ѡ������ɾѡ���������Ҫͣ����˳��� StartRow/Col
      DeleteSelection
    else
    begin
      // ɾ���ǰ���ַ������ǰ��û�ַ�����ѵ�ǰ������ƴ��ɾ��ǰ�У����û��һ�о�ɶ������
      SR := CaretRow;
      SC := CaretCol;
      ER := CaretRow;
      EC := CaretCol;
      NC := GetPrevColumn(SC, SR);
      if NC = SC then // ǰ��û�ַ��ˣ�����һ��
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

{$WARNINGS OFF}

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
    // TODO: ���� Tab
  end
  else if Ord(Ch) >= 20 then
  begin
{$IFDEF UNICODE}
    InsertText(string(Ch)); // Unicode ʱ���뷨�ó�����˫�ֽ��ַ�ֱ��ͨ��һ�� WM_CHAR ����
{$ELSE}
    if Ord(Ch) > 127 then // �� Unicode ʱ���뷨�ó�����˫�ֽ��ַ����������� WM_CHAR
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

{$WARNINGS ON}

end.
