{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2006 CnPack 开发组                       }
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

unit CnHexEditor;
{* |<PRE>
================================================================================
* 软件名称：CnPack 可视化组件包
* 单元名称：CnHexEditor 文件十六进制查看修改实现单元，只支持 Windows
* 单元作者：Zswang(原创) 2006-12-28 wjhu111@21cn.com
*           Guye (移植)
* 备    注：该单元为 CnPack 组件包的一部分，实现了文件十六进制查看功能, 为可视
            化组件, 但本次版本功能上欠佳, 预计复制等功能在下个版本中升级修改。
* 开发平台：PWinXP + Delphi 7
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2024.05.18 V1.4
*               按需暴露 StyleElements 属性供高版本 Delphi 使用
*           2024.01.12 V1.3
*               增加 SelBytes 属性以处理 Unicode 下 SelText 错乱的冲突
*               增加 Ctrl+C 复制选中十六进制字符串的机制
*           2012.09.26 V1.2
*               增加一 DataChange 方法供修改 MemoryStream 后更新界面用，感谢 veket
*           2012.03.03 V1.1
*               暂时屏蔽 CMFONTCHANGED 的第一次消息以免画错，原因不详
*           2008.01.15 V1.0 by Guye
*               优化代码，修改移植入 CnPack
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, Clipbrd,
  CnNative;

type
  TCnWMImeChar = packed record
    Msg: Cardinal;
    case Integer of
      0:
        (CharCode: Word;
        KeyData: Longint;
        Result: Longint);
      1:
        (CharCode1: Byte;
        CharCode2: Byte);
  end;

type
  TCnMouseObject = (moNone, moAddress, moHex, moChar);

type
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnHexEditor = class(TCustomControl)
  private
    FFirstCmFontChanged: Boolean;
    FMemoryStream: TMemoryStream;
    FBaseAddress: Integer;
    FLineCount: Integer;
    FVisibleChars: Integer;
    FTopLine: Integer;
    FLeftLine: Integer;
    FRowIndex: Integer;
    FVisibleLines: Integer;
    FItemHeight: Integer;
    FItemWidth: Integer;
    FColIndex: Integer;
    FColType: TCnMouseObject;
    FReadOnly: Boolean;
    FSelLength: Integer;
    FSelStart: Integer;
    FAnchorStart: Integer;
    FAnchorOffset: Integer;
    FHexChar: Char;
    FOnSelectionChange: TNotifyEvent;
    FChangeDataSize: Boolean;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure AdjustScrollBars;

    procedure SetRowIndex(Value: Integer);
    procedure SetColIndex(Value: Integer);
    procedure SetLeftLine(Value: Integer);
    procedure SetTopLine(Value: Integer);

    procedure SetBaseAddress(const Value: Integer);
    function LineViewText(mLineIndex: Integer): string;
    function SelectionViewText(mColType: TCnMouseObject; mLineIndex: Integer;
      mStart, mEnd: Integer): string;
    property TopLine: Integer read FTopLine write SetTopLine;
    property LeftLine: Integer read FLeftLine write SetLeftLine;
    function MouseObject(mPoint: TPoint; var nCoordinate: TPoint): TCnMouseObject;
    function CoordinateToPoint(mMouseObject: TCnMouseObject; mCoordinate: TPoint): TPoint;
    function PositionToCoordinate(mPosition: Integer): TPoint;
    function CoordinatePosition(mCoordinate: TPoint): Integer;
    function ColToChar(mColType: TCnMouseObject; mCol: Integer): Integer;
    procedure SetColType(const Value: TCnMouseObject);
    function RowMaxIndex(mRowIndex: Integer): Integer;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetSelLength(const Value: Integer);
    procedure SetSelStart(Value: Integer);
    procedure SetAnchorOffset(Value: Integer);
    procedure WMIMECHAR(var Msg: TCnWMImeChar); message WM_IME_CHAR;
    procedure WMCHAR(var Msg: TWMChar); message WM_CHAR;
    function GetSelBytes: TBytes;
  protected
    function GetSelText: string; virtual;
    procedure SetSelText(const Value: string); virtual;
    procedure DoChange; virtual;
    procedure SelectionChange; virtual;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure DoExit; override;
    procedure DoEnter; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromBuffer(const Buffer; Size: Integer);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(FileName: TFileName);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(FileName: TFileName);
    procedure SaveToBuffer(var Buffer; Size: Integer);
    procedure Clear;

    property SelBytes: TBytes read GetSelBytes;
    property MemoryStream: TMemoryStream read FMemoryStream;
    property BaseAddress: Integer read FBaseAddress write SetBaseAddress; // 基地址
    property RowIndex: Integer read FRowIndex write SetRowIndex;          // 当前行数
    property ColIndex: Integer read FColIndex write SetColIndex;          // 当前列数
    property ColType: TCnMouseObject read FColType write SetColType;      // 当前列是否十六进制
    property SelStart: Integer read FSelStart write SetSelStart;          // 选择文本的开始位置
    property SelLength: Integer read FSelLength write SetSelLength;       // 选择文本的长度
    property SelText: string read GetSelText write SetSelText;            // 选中的文本，注意 Unicode 编译器下有问题
    property AnchorOffset: Integer read FAnchorOffset write SetAnchorOffset;
    function ScrollIntoView: Boolean;
    procedure UpdateCaret;
    procedure DataChange;
  published
    property Align;
    property Anchors;
    property Enabled;
    property Font;
    property Color;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property ChangeDataSize: Boolean read FChangeDataSize write FChangeDataSize
      default True;
    property ParentFont;
    property ParentColor;
    property PopupMenu;
{$IFDEF TCONTROL_HAS_STYLEELEMENTS}
    property StyleElements;
{$ENDIF}
    property TabOrder;
    property TabStop;
    property Visible;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write
      FOnSelectionChange;
  end;

implementation

uses
  Math;

//------------------------------------------------------------------------------
// 往流中插入数据
//------------------------------------------------------------------------------

function InsertToStream(Stream: TStream; Offset: Integer; const Buffer; Length:
  Integer): Boolean;
var
  vBuffer: array[0..$1000 - 1] of Char;
  I, L: Integer;
begin
  Result := False;
  if not Assigned(Stream) then
    Exit;
  if Length <= 0 then
    Exit;
  if Offset >= Stream.Size then
    Exit;
  if Offset < 0 then
    Exit;
  I := Stream.Size;
  Stream.Size := Stream.Size + Length;
  repeat
    if Offset + Length <= I - SizeOf(vBuffer) then
      L := SizeOf(vBuffer)
    else
      L := I - Offset;
    Stream.Position := I - L;
    Stream.Read(vBuffer, L);
    Stream.Position := I - L + Length;
    Stream.Write(vBuffer, L);
    I := I - L + Length;
  until L < SizeOf(vBuffer);
  Stream.Position := Offset;
  Stream.Write(Buffer, Length);
end;

//------------------------------------------------------------------------------
// 删除流中的数据
//------------------------------------------------------------------------------

function DeleteFromStream(Stream: TStream; Offset: Integer; Length: Integer): Boolean;
var
  Buffer: array[0..$1000 - 1] of Char;
  I, L: Integer;
begin
  Result := False;
  if not Assigned(Stream) then
    Exit;
  if Length <= 0 then
    Exit;
  if Offset >= Stream.Size then
    Exit;
  if Offset < 0 then
    Exit;
  if Offset + Length >= Stream.Size then
    Stream.Size := Offset
  else
  begin
    I := Offset;
    repeat
      Stream.Position := I + Length;
      L := Stream.Read(Buffer, SizeOf(Buffer));
      Stream.Position := I;
      Stream.Write(Buffer, L);
      Inc(I, L);
    until L < SizeOf(Buffer);
    Stream.Size := Stream.Size - Length;
  end;
  Result := True;
end;

procedure TCnHexEditor.AdjustScrollBars;
var
  ScrlInfo: TScrollInfo;
begin
  SetScrollRange(Handle, SB_VERT, 0, FLineCount, True);
  SetScrollRange(Handle, SB_HORZ, 0, 76, True);
  ScrlInfo.fMask := SIF_PAGE;
  ScrlInfo.nPage := FVisibleLines;
  SetScrollInfo(Handle, SB_VERT, ScrlInfo, True);

  ScrlInfo.fMask := SIF_PAGE;
  ScrlInfo.nPage := FVisibleChars;
  SetScrollInfo(Handle, SB_HORZ, ScrlInfo, True);
end;

procedure TCnHexEditor.Clear;
begin
  FMemoryStream.Clear;
  FSelLength := 0;
  FSelStart := 0;
  FColIndex := 0;
  FRowIndex := 0;
  DoChange;
end;

procedure TCnHexEditor.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Canvas.Font := Self.Font;
  // First Font Changed Message Will cause Draw Invert to out of Parent. Dont know why.
  if FFirstCmFontChanged then
  begin
    FFirstCmFontChanged := False;
    Exit;
  end;
  DoChange;
end;

function TCnHexEditor.CoordinateToPoint(mMouseObject: TCnMouseObject;
  mCoordinate: TPoint): TPoint;
begin
  case mMouseObject of
    moChar, moHex:
      begin
        Result.Y := mCoordinate.Y * FItemHeight;
        Result.X := ColToChar(mMouseObject, mCoordinate.X) * FItemWidth;
      end;
    moAddress:
      begin
        Result.Y := mCoordinate.Y * FItemHeight;
        Result.X := 0;
      end;
  else
    Result := Point(-1, -1);
  end;

  Result.X := Result.X - FLeftLine * FItemWidth;
  Result.Y := Result.Y - FTopLine * FItemHeight;
end;

constructor TCnHexEditor.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := [csFramed, csCaptureMouse];
  Width := 300;
  Height := 200;
  ParentColor := False;
  Color := clWindow;
  FMemoryStream := TMemoryStream.Create;
  DoubleBuffered := True;
  FChangeDataSize := True;
  FColType := moHex;
  FFirstCmFontChanged := True;

  try
    Font.Name := 'Fixedsys'; // 用等宽字体
  except
    ;
  end;
end;

procedure TCnHexEditor.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style or WS_VSCROLL or WS_HSCROLL;
end;

destructor TCnHexEditor.Destroy;
begin
  FMemoryStream.Free;
  inherited;
end;

procedure TCnHexEditor.DoChange;
begin
  FItemHeight := Canvas.TextHeight('A');
  FItemWidth := Canvas.TextWidth('D');
  FLineCount := (FMemoryStream.Size div 16) + 1;
  FVisibleChars := (ClientWidth div Canvas.TextWidth('D')) + 1;
  FVisibleLines := (ClientHeight div FItemHeight) + 1;
  LeftLine := Min(LeftLine, 76 - FVisibleChars + 1);
  TopLine := Min(TopLine, FLineCount - FVisibleLines + 1);

  AdjustScrollBars;
  UpdateCaret;
  Invalidate;
  ScrollIntoView;
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

function TCnHexEditor.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  Perform(WM_VSCROLL, MakeWParam(SB_PAGEDOWN, 0), 0);
end;

function TCnHexEditor.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  Perform(WM_VSCROLL, MakeWParam(SB_PAGEUP, 0), 0);
end;

procedure TCnHexEditor.KeyDown(var Key: Word; Shift: TShiftState);
var
  CaretPoint: TPoint;
  B: TBytes;
begin
  inherited;
  case Key of
    VK_BACK:
      begin
        if not FChangeDataSize then
          Exit;
        if FSelLength <= 0 then
        begin
          if FSelStart <= 0 then
            Exit;
          Dec(FSelStart);
          if DeleteFromStream(FMemoryStream, FSelStart, 1) then
          begin
            CaretPoint := PositionToCoordinate(FSelStart);
            FColIndex := CaretPoint.X;
            FRowIndex := CaretPoint.Y;
            DoChange;
          end;
        end
        else
        begin
          if DeleteFromStream(FMemoryStream, FSelStart, FSelLength) then
          begin
            FSelLength := 0;
            CaretPoint := PositionToCoordinate(FSelStart + FSelLength);
            FColIndex := CaretPoint.X;
            FRowIndex := CaretPoint.Y;
            DoChange;
          end;
        end;
      end;
    VK_DELETE:
      begin
        if not FChangeDataSize then
          Exit;

        if FSelLength <= 0 then
        begin
          if DeleteFromStream(FMemoryStream, FSelStart, 1) then
            DoChange;
        end
        else
        begin
          if DeleteFromStream(FMemoryStream, FSelStart, FSelLength) then
          begin
            FSelLength := 0;
            CaretPoint := PositionToCoordinate(FSelStart + FSelLength);
            FColIndex := CaretPoint.X;
            FRowIndex := CaretPoint.Y;
            DoChange;
          end;
        end;
      end;
    VK_SHIFT:
      begin
        if FSelLength <= 0 then
        begin
          FAnchorStart := FSelStart;
          FAnchorOffset := 0;
          FHexChar := #0;
        end;
      end;
    VK_DOWN:
      begin
        if ssShift in Shift then
          AnchorOffset := AnchorOffset + 16
        else
        begin
          RowIndex := RowIndex + 1;
          SelectionChange;
        end;
      end;
    VK_UP:
      begin
        if ssShift in Shift then
          AnchorOffset := AnchorOffset - 16
        else
        begin
          RowIndex := RowIndex - 1;
          SelectionChange;
        end;
      end;
    VK_NEXT:
      begin
        RowIndex := RowIndex + FVisibleLines;
        if not (ssShift in Shift) then
          SelectionChange;
      end;
    VK_PRIOR:
      begin
        RowIndex := RowIndex - FVisibleLines;
        if not (ssShift in Shift) then
          SelectionChange;
      end;
    VK_HOME:
      begin
        ColIndex := 0;
        if ssCtrl in Shift then
          RowIndex := 0;
        if not (ssShift in Shift) then
          SelectionChange;
      end;
    VK_END:
      begin
        ColIndex := 15;
        if ssCtrl in Shift then
          RowIndex := FLineCount - 1;
        if not (ssShift in Shift) then
          SelectionChange;
      end;
    VK_LEFT:
      begin
        if ssShift in Shift then
          AnchorOffset := AnchorOffset - 1
        else
        begin
          if ColIndex > 0 then
            ColIndex := ColIndex - 1
          else if RowIndex > 0 then
          begin
            RowIndex := RowIndex - 1;
            ColIndex := RowMaxIndex(RowIndex);
          end;
          SelectionChange;
        end;
      end;
    VK_RIGHT:
      begin
        if ssShift in Shift then
          AnchorOffset := AnchorOffset + 1
        else
        begin
          if ColIndex < 15 then
            ColIndex := ColIndex + 1
          else if RowIndex < FLineCount - 1 then
          begin
            ColIndex := 0;
            RowIndex := RowIndex + 1;
          end;
          SelectionChange;
        end;
      end;
    VK_TAB:
      if ColType = moHex then
        ColType := moChar
      else
        ColType := moHex;
    Ord('C'):
      begin
        if ssCtrl in Shift then
        begin
          B := SelBytes;
          if Length(B) > 0 then
            Clipboard.AsText := BytesToHex(B);
        end;
      end;
  end;
end;

function TCnHexEditor.LineViewText(mLineIndex: Integer): string;
const
  SHexDigits: array[0..15] of Char = '0123456789ABCDEF';
var
  I, L: Integer;
  vBytes: array[0..15] of Byte;
  S: string;
begin
  Result := '';
  if mLineIndex < 0 then
    Exit;
  FMemoryStream.Position := mLineIndex * 16;
  L := FMemoryStream.Read(vBytes, 16);
  Result := Format('%.8x  ', [FBaseAddress + mLineIndex * 16]);
  S := '';
  for I := 0 to 15 do
  begin
    if I = 8 then
      Result := Result + ' ';
    if I < L then
    begin
      if vBytes[I] in [32..126] then
        S := S + Chr(vBytes[I])
      else
        S := S + '.';
      Result := Result + SHexDigits[vBytes[I] shr $04] +
        SHexDigits[vBytes[I] and $0F] + ' '
    end
    else
    begin
      Result := Result + '   ';
      S := S + ' ';
    end;
  end;
  Result := Result + ' ' + S;
end;

procedure TCnHexEditor.LoadFromFile(FileName: TFileName);
begin
  if FileExists(FileName) then
    FMemoryStream.LoadFromFile(FileName)
  else
    FMemoryStream.Clear;
  FSelLength := 0;
  FSelStart := 0;
  FColIndex := 0;
  FRowIndex := 0;
  DoChange;
end;

procedure TCnHexEditor.LoadFromStream(Stream: TStream);
begin
  FMemoryStream.Clear;
  FMemoryStream.LoadFromStream(Stream);
  FSelLength := 0;
  FSelStart := 0;
  FColIndex := 0;
  FRowIndex := 0;
  DoChange;
end;

procedure TCnHexEditor.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vCoordinate: TPoint;
begin
  inherited;
  if not Focused then
    SetFocus;
  if Button = mbRight then
    Exit;
  case MouseObject(Point(X, Y), vCoordinate) of
    moAddress:
      ;
    moHex:
      begin
        FColIndex := vCoordinate.X;
        FColType := moHex;
        FRowIndex := vCoordinate.Y;
        FSelStart := Max(Min(CoordinatePosition(vCoordinate), FMemoryStream.Size), 0);
        vCoordinate := PositionToCoordinate(FSelStart);
        FColIndex := vCoordinate.X;
        FRowIndex := vCoordinate.Y;
        FAnchorStart := FSelStart;
        FAnchorOffset := 0;
        FHexChar := #0;
        SelLength := 0;
        UpdateCaret;
        SelectionChange;
      end;
    moChar:
      begin
        FColIndex := vCoordinate.X;
        FColType := moChar;
        RowIndex := vCoordinate.Y;
        FSelStart := Max(Min(CoordinatePosition(vCoordinate), FMemoryStream.Size), 0);
        vCoordinate := PositionToCoordinate(FSelStart);
        FColIndex := vCoordinate.X;
        FRowIndex := vCoordinate.Y;
        FAnchorStart := FSelStart;
        FAnchorOffset := 0;
        FHexChar := #0;
        SelLength := 0;
        UpdateCaret;
        SelectionChange;
      end;
    moNone:
      ;
  end;
end;

procedure TCnHexEditor.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  vCoordinate: TPoint;
  vAnchorType: TCnMouseObject;
begin
  inherited;
  if not Focused then
    Exit;
  { TODO -c2006.11.17 -oZswangY37 : 考虑拖拽移动内容 }
  if ssLeft in Shift then
  begin
    vCoordinate := CoordinateToPoint(FColType, Point(15, 0));
    if X >= vCoordinate.X + FItemWidth then
    begin
      vCoordinate := CoordinateToPoint(FColType, Point(0, 0));
      X := vCoordinate.X;
      Y := Y + FItemHeight;
    end;

    vCoordinate := CoordinateToPoint(FColType, Point(0, 0));
    X := Max(vCoordinate.X, X);
    vCoordinate := CoordinateToPoint(FColType, Point(15, 0));
    X := Min(vCoordinate.X, X);

    vAnchorType := MouseObject(Point(X, Y), vCoordinate);
    if vAnchorType <> FColType then
      Exit;
    AnchorOffset := CoordinatePosition(vCoordinate) - FAnchorStart;
  end;

  case MouseObject(Point(X, Y), vCoordinate) of
    moAddress:
      Cursor := crDefault;
    moHex:
      Cursor := crIBeam;
    moChar:
      Cursor := crIBeam;
    moNone:
      Cursor := crDefault;
  end;
end;

function TCnHexEditor.MouseObject(mPoint: TPoint; var nCoordinate: TPoint):
  TCnMouseObject;
var
  vRow, vCol: Integer;
begin
  vRow := (mPoint.Y + FItemHeight * FTopLine) div FItemHeight;
  vCol := (mPoint.X + FItemWidth * FLeftLine + FItemWidth div 2) div FItemWidth;
  case vCol of
    0..9:
      begin
        Result := moAddress;
        nCoordinate.X := vRow;
        nCoordinate.Y := vRow;
      end;
    10..58:
      begin
        Result := moHex;
        case vCol of
          10..33:
            nCoordinate.X := (vCol - 10) div 3;
          34..35:
            nCoordinate.X := 8;
          36..58:
            begin
              nCoordinate.X := (vCol - 11) div 3;
            end;
        else
          nCoordinate.X := vCol;
        end;
        nCoordinate.Y := vRow;
      end;
    60..76:
      begin
        Result := moChar;
        nCoordinate.X := Min(vCol - 60, 15);
        nCoordinate.Y := vRow;
      end;
  else
    Result := moNone;
  end;
end;

procedure TCnHexEditor.Paint;
var
  I: Integer;
  vSelStart, vSelEnd: TPoint;
  vCurrLine: Integer;
  vPoint: TPoint;
  Rect: TRect;
  vUnColType: TCnMouseObject;
begin
  inherited;
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(Font);

  if FSelLength > 0 then
  begin
    vSelStart := PositionToCoordinate(FSelStart);
    vSelEnd := PositionToCoordinate(FSelStart + FSelLength - 1);
  end;
  for I := 0 to FVisibleLines - 1 do
  begin
    vCurrLine := I + FTopLine;
    if vCurrLine >= FLineCount then
      Break;
    Canvas.TextOut(
      -FItemWidth * FLeftLine, I * FItemHeight, LineViewText(vCurrLine));
    ///////Begin 绘制选中区域
    if (FSelLength > 0) and
      (vCurrLine >= vSelStart.Y) and (vCurrLine <= vSelEnd.Y) then
    begin
      Canvas.Brush.Color := clHighlight;
      Canvas.Font.Color := clHighlightText;
      if (vCurrLine = vSelStart.Y) and (vCurrLine = vSelEnd.Y) then
      begin
        vPoint := CoordinateToPoint(FColType, Point(vSelStart.X, vCurrLine));
        Canvas.TextOut(
          vPoint.X, vPoint.Y, SelectionViewText(FColType, vCurrLine, vSelStart.X, vSelEnd.X));
      end
      else if vCurrLine = vSelStart.Y then
      begin
        vPoint := CoordinateToPoint(FColType, Point(vSelStart.X, vCurrLine));
        Canvas.TextOut(
          vPoint.X, vPoint.Y, SelectionViewText(FColType, vCurrLine, vSelStart.X, 15));
      end
      else if vCurrLine = vSelEnd.Y then
      begin
        vPoint := CoordinateToPoint(FColType, Point(0, vCurrLine));
        Canvas.TextOut(
          vPoint.X, vPoint.Y, SelectionViewText(FColType, vCurrLine, 0, vSelEnd.X))
      end
      else if (vCurrLine > vSelStart.Y) and (vCurrLine < vSelEnd.Y) then
      begin
        vPoint := CoordinateToPoint(FColType, Point(0, vCurrLine));
        Canvas.TextOut(
          vPoint.X, vPoint.Y, SelectionViewText(FColType, vCurrLine, 0, 15))
      end;

      Canvas.Brush.Style := bsClear;
      if FColType = moChar then
        vUnColType := moHex
      else
        vUnColType := moChar;
      if (vCurrLine = vSelStart.Y) and (vCurrLine = vSelEnd.Y) then
      begin
        Rect.TopLeft := CoordinateToPoint(vUnColType, Point(vSelStart.X, vCurrLine));
        Rect.BottomRight := CoordinateToPoint(vUnColType, Point(vSelEnd.X, vCurrLine));
        Rect.BottomRight.X := Rect.BottomRight.X + FItemWidth * (1 + Ord(vUnColType
          = moHex));
        Rect.BottomRight.Y := Rect.BottomRight.Y + FItemHeight;
        Canvas.Rectangle(Rect);
      end
      else if vCurrLine = vSelStart.Y then
      begin
        Rect.TopLeft := CoordinateToPoint(vUnColType, Point(vSelStart.X, vCurrLine));
        Rect.BottomRight := CoordinateToPoint(vUnColType, Point(15, vCurrLine));
        Rect.BottomRight.X := Rect.BottomRight.X + FItemWidth * (1 + Ord(vUnColType
          = moHex));
        Rect.BottomRight.Y := Rect.BottomRight.Y + FItemHeight;

        Canvas.MoveTo(Rect.TopLeft.X, Rect.TopLeft.Y);
        Canvas.LineTo(Rect.TopLeft.X, Rect.BottomRight.Y);
        Canvas.MoveTo(Rect.BottomRight.X, Rect.TopLeft.Y);
        Canvas.LineTo(Rect.BottomRight.X, Rect.BottomRight.Y);

        Canvas.MoveTo(Rect.TopLeft.X, Rect.TopLeft.Y);
        Canvas.LineTo(Rect.BottomRight.X, Rect.TopLeft.Y);

        Rect.BottomRight := CoordinateToPoint(vUnColType, Point(0, vCurrLine));
        Rect.BottomRight.Y := Rect.BottomRight.Y + FItemHeight;
        Canvas.MoveTo(Rect.TopLeft.X, Rect.BottomRight.Y);
        Canvas.LineTo(Rect.BottomRight.X, Rect.BottomRight.Y);
      end
      else if vCurrLine = vSelEnd.Y then
      begin
        Rect.TopLeft := CoordinateToPoint(vUnColType, Point(0, vCurrLine));
        Rect.BottomRight := CoordinateToPoint(vUnColType, Point(vSelEnd.X, vCurrLine));
        Rect.BottomRight.X := Rect.BottomRight.X + FItemWidth * (1 + Ord(vUnColType
          = moHex));
        Rect.BottomRight.Y := Rect.BottomRight.Y + FItemHeight;
        Canvas.MoveTo(Rect.TopLeft.X, Rect.TopLeft.Y);
        Canvas.LineTo(Rect.TopLeft.X, Rect.BottomRight.Y);
        Canvas.MoveTo(Rect.BottomRight.X, Rect.TopLeft.Y);
        Canvas.LineTo(Rect.BottomRight.X, Rect.BottomRight.Y);

        Canvas.MoveTo(Rect.TopLeft.X, Rect.BottomRight.Y);
        Canvas.LineTo(Rect.BottomRight.X, Rect.BottomRight.Y);

        Rect.TopLeft := CoordinateToPoint(vUnColType, Point(vSelEnd.X, vCurrLine));
        Rect.TopLeft.X := Rect.TopLeft.X + FItemWidth * (1 + Ord(vUnColType = moHex));
        Rect.BottomRight := CoordinateToPoint(vUnColType, Point(15, vCurrLine));
        Rect.BottomRight.X := Rect.BottomRight.X + FItemWidth * (1 + Ord(vUnColType
          = moHex));
        Canvas.MoveTo(Rect.TopLeft.X, Rect.TopLeft.Y);
        Canvas.LineTo(Rect.BottomRight.X, Rect.TopLeft.Y);
      end
      else if (vCurrLine > vSelStart.Y) and (vCurrLine < vSelEnd.Y) then
      begin
        Rect.TopLeft := CoordinateToPoint(vUnColType, Point(0, vCurrLine));
        Rect.BottomRight := CoordinateToPoint(vUnColType, Point(15, vCurrLine));
        Rect.BottomRight.X := Rect.BottomRight.X + FItemWidth * (1 + Ord(vUnColType
          = moHex));
        Rect.BottomRight.Y := Rect.BottomRight.Y + FItemHeight;
        Canvas.MoveTo(Rect.TopLeft.X, Rect.TopLeft.Y);
        Canvas.LineTo(Rect.TopLeft.X, Rect.BottomRight.Y);
        Canvas.MoveTo(Rect.BottomRight.X, Rect.TopLeft.Y);
        Canvas.LineTo(Rect.BottomRight.X, Rect.BottomRight.Y);
      end;
      Canvas.Font.Assign(Font);
    end;
    ///////End 绘制选中区域
  end;
end;

procedure TCnHexEditor.SaveToFile(FileName: TFileName);
begin
  FMemoryStream.SaveToFile(FileName);
end;

procedure TCnHexEditor.SaveToStream(Stream: TStream);
begin
  FMemoryStream.SaveToStream(Stream);
end;

function TCnHexEditor.ScrollIntoView: Boolean;
var
  vCharIndex: Integer;
begin
  Result := False;
  if FRowIndex < FTopLine then
  begin
    Result := True;
    TopLine := FRowIndex;
  end
  else if FRowIndex >= (FTopLine + FVisibleLines) - 1 then
  begin
    TopLine := FRowIndex - (FVisibleLines - 2);
    Result := True;
  end;

  vCharIndex := ColToChar(FColType, FColIndex);
  if vCharIndex < FLeftLine then
  begin
    Result := True;
    LeftLine := vCharIndex;
  end
  else if vCharIndex >= (FLeftLine + FVisibleChars) - 1 then
  begin
    Result := True;
    LeftLine := vCharIndex - (FVisibleChars - 2);
  end;
  AdjustScrollBars;
end;

procedure TCnHexEditor.SetBaseAddress(const Value: Integer);
begin
  FBaseAddress := Value;
  Invalidate;
end;

procedure TCnHexEditor.SetRowIndex(Value: Integer);
var
  R: TRect;
begin
  if Value <> FRowIndex then
  begin
    if Value < 0 then
      Value := 0;
    if Value >= FLineCount then
      Value := FLineCount - 1;

    if (FRowIndex >= FTopLine) and (FRowIndex < FTopLine + FVisibleLines - 1) then
    begin
      R := Bounds(0, 0, 1, FItemHeight);
      OffsetRect(R, 0, (FRowIndex - FTopLine) * FItemHeight);
      Windows.InvalidateRect(Handle, @R, True);
    end;
    FRowIndex := Value;

    R := Bounds(0, 0, 1, FItemHeight);
    OffsetRect(R, 0, (FRowIndex - FTopLine) * FItemHeight);
    Windows.InvalidateRect(Handle, @R, True);

    if FRowIndex = FLineCount - 1 then
    begin
      ColIndex := Min(ColIndex, RowMaxIndex(FRowIndex));
      ScrollIntoView;
      UpdateCaret;
      Exit;
    end;
    ScrollIntoView;
    UpdateCaret;
  end;
end;

procedure TCnHexEditor.SetLeftLine(Value: Integer);
var
  LinesMoved: Integer;
  Rect: TRect;
begin
  if Value <> FLeftLine then
  begin
    if Value < 0 then
      Value := 0;
    if Value >= 76 then
      Value := 76 - 1;
    LinesMoved := FLeftLine - Value;
    FLeftLine := Value;
    SetScrollPos(Handle, SB_HORZ, FLeftLine, True);
    if Abs(LinesMoved) = 1 then
    begin
      Rect := Bounds(1, 0, ClientWidth - FItemWidth, ClientHeight);
      if LinesMoved = 1 then
        OffsetRect(Rect, FItemWidth, 0);
      ScrollWindow(Handle, FItemWidth * LinesMoved, 0, @Rect, nil);
      if LinesMoved = -1 then
      begin
        Rect.Left := ClientWidth - FItemWidth;
        Rect.Right := ClientWidth;
      end
      else
      begin
        Rect.Left := 0;
        Rect.Right := FItemWidth;
      end;
      InvalidateRect(Handle, @Rect, False);
    end
    else
      Invalidate;
    UpdateCaret;
  end;
end;

procedure TCnHexEditor.SetTopLine(Value: Integer);
var
  LinesMoved: Integer;
  Rect: TRect;
begin
  if Value <> FTopLine then
  begin
    if Value < 0 then
      Value := 0;
    if Value >= FLineCount then
      Value := FLineCount - 1;
    LinesMoved := FTopLine - Value;
    FTopLine := Value;
    SetScrollPos(Handle, SB_VERT, FTopLine, True);
    if Abs(LinesMoved) = 1 then
    begin
      Rect := Bounds(1, 0, ClientWidth, ClientHeight - FItemHeight);
      if LinesMoved = 1 then
        OffsetRect(Rect, 0, FItemHeight);
      ScrollWindow(Handle, 0, FItemHeight * LinesMoved, @Rect, nil);
      if LinesMoved = -1 then
      begin
        Rect.Top := ClientHeight - FItemHeight;
        Rect.Bottom := ClientHeight;
      end
      else
      begin
        Rect.Top := 0;
        Rect.Bottom := FItemHeight;
      end;
      InvalidateRect(Handle, @Rect, False);
    end
    else
      Invalidate;
    UpdateCaret;
  end;
end;

procedure TCnHexEditor.UpdateCaret;
var
  vPos: TPoint;
begin
  DestroyCaret;
  if not Focused then
    Exit;
  if FSelLength > 0 then
    Exit;
  CreateCaret(Handle, 0, 2, Canvas.TextHeight('|'));
  ShowCaret(Handle);

  vPos := CoordinateToPoint(FColType, Point(FColIndex, FRowIndex));
  if (FColType = moHex) and (FHexChar <> #0) then
    vPos.X := vPos.X + FItemWidth * 2;
  SetCaretPos(vPos.X, vPos.Y);
end;

procedure TCnHexEditor.DataChange;
begin
  DoChange;
end;

procedure TCnHexEditor.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTTAB;
end;

procedure TCnHexEditor.WMHScroll(var Message: TWMHScroll);
var
  NewLeftLine: Integer;
  LinesMoved: Integer;
  Rect: TRect;
begin
  inherited;
  if not Focused then
    SetFocus;
  NewLeftLine := FLeftLine;
  case Message.ScrollCode of
    SB_LINEDOWN:
      Inc(NewLeftLine);
    SB_LINEUP:
      Dec(NewLeftLine);
    SB_PAGEDOWN:
      Inc(NewLeftLine, FVisibleLines - 1);
    SB_PAGEUP:
      Dec(NewLeftLine, FVisibleLines - 1);
    SB_THUMBPOSITION, SB_THUMBTRACK:
      NewLeftLine := Message.Pos;
  end;

  if NewLeftLine >= 76 - FVisibleChars + 1 then
    NewLeftLine := 76 - FVisibleChars + 1;
  if NewLeftLine < 0 then
    NewLeftLine := 0;

  if NewLeftLine <> FLeftLine then
  begin
    LinesMoved := FLeftLine - NewLeftLine;
    FLeftLine := NewLeftLine;
    SetScrollPos(Handle, SB_HORZ, FLeftLine, True);
    if Abs(LinesMoved) = 1 then
    begin
      Rect := Bounds(0, 0, ClientWidth - FItemWidth, ClientHeight);
      if LinesMoved = 1 then
        OffsetRect(Rect, FItemWidth, 0);
      ScrollWindow(Handle, FItemWidth * LinesMoved, 0, @Rect, nil);
      if LinesMoved = -1 then
      begin
        Rect.Left := ClientWidth;
        Rect.Right := ClientWidth - FItemWidth;
      end
      else
      begin
        Rect.Left := 0;
        Rect.Right := FItemWidth;
      end;
      Windows.InvalidateRect(Handle, @Rect, False);
    end
    else
      Invalidate;
    UpdateCaret;
  end;
end;

procedure TCnHexEditor.WMSize(var Message: TWMSize);
begin
  inherited;
  DoChange;
end;

procedure TCnHexEditor.WMVScroll(var Message: TWMVScroll);
{$J+}
const
  vPos: Integer = 0;
  vTracking: Boolean = False;
  vMouseY: Integer = 0;
{$J-}
var
  NewTopLine: Integer;
  LinesMoved: Integer;
  I: Integer;
  vRect: TRect;
begin
  inherited;
  if not Focused then
    SetFocus;
  NewTopLine := FTopLine;
  case Message.ScrollCode of
    SB_LINEDOWN:
      Inc(NewTopLine);
    SB_LINEUP:
      Dec(NewTopLine);
    SB_PAGEDOWN:
      Inc(NewTopLine, FVisibleLines div 2);
    SB_PAGEUP:
      Dec(NewTopLine, FVisibleLines div 2);
    SB_THUMBPOSITION:
      vTracking := False;
    SB_THUMBTRACK:
      begin
        if not vTracking then
        begin
          vPos := Message.Pos;
          vMouseY := Mouse.CursorPos.Y;
        end;
        vTracking := True;
        I := Message.Pos - vPos;
        if (I > 0) and (vMouseY > Mouse.CursorPos.Y) then
          I := (Message.Pos) - (High(Smallint) * 2 + vPos);
        if (I < 0) and (vMouseY < Mouse.CursorPos.Y) then
          I := (High(Smallint) * 2 + Message.Pos) - vPos;
        NewTopLine := GetScrollPos(Handle, SB_VERT) + I;
        vPos := Message.Pos;
        vMouseY := Mouse.CursorPos.Y;
      end;
  end;
  if NewTopLine >= FLineCount - FVisibleLines + 1 then
    NewTopLine := FLineCount - FVisibleLines + 1;
  if NewTopLine < 0 then
    NewTopLine := 0;

  if NewTopLine <> FTopLine then
  begin
    LinesMoved := FTopLine - NewTopLine;
    FTopLine := NewTopLine;
    SetScrollPos(Handle, SB_VERT, FTopLine, True);
    if Abs(LinesMoved) = 1 then
    begin
      vRect := Bounds(0, 0, ClientWidth, ClientHeight - FItemHeight);
      if LinesMoved = 1 then
        OffsetRect(vRect, 0, FItemHeight);
      ScrollWindow(Handle, 0, FItemHeight * LinesMoved, @vRect, nil);
      if LinesMoved = -1 then
      begin
        vRect.Top := ClientHeight - FItemHeight;
        vRect.Bottom := ClientHeight;
      end
      else
      begin
        vRect.Top := 0;
        vRect.Bottom := FItemHeight;
      end;
      Windows.InvalidateRect(Handle, @vRect, False);
    end
    else
      Invalidate;
    UpdateCaret;
  end;
end;

procedure TCnHexEditor.SetColIndex(Value: Integer);
var
  R: TRect;
  vCharIndex: Integer;
begin
  if Value <> FColIndex then
  begin
    if Value < 0 then
      Value := 0;
    if Value > RowMaxIndex(FRowIndex) then
      Value := RowMaxIndex(FRowIndex);
    FColIndex := Value;
    vCharIndex := ColToChar(FColType, FColIndex);

    if (vCharIndex >= FLeftLine) and (vCharIndex < FLeftLine + 76 - 1) then
    begin
      R := Bounds(0, 0, 1, FItemHeight);
      OffsetRect(R, (vCharIndex - FLeftLine) * FItemWidth, 0);
      Windows.InvalidateRect(Handle, @R, True);
    end;
    FColIndex := Value;

    vCharIndex := ColToChar(FColType, FColIndex);
    R := Bounds(0, 0, 1, FItemHeight);
    OffsetRect(R, (vCharIndex - FLeftLine) * FItemWidth, 0);
    Windows.InvalidateRect(Handle, @R, True);

    ScrollIntoView;
    UpdateCaret;
  end;
end;

procedure TCnHexEditor.SetColType(const Value: TCnMouseObject);
begin
  if FColType = Value then
    Exit;
  FColType := Value;
  ScrollIntoView;
  UpdateCaret;
  Invalidate;
end;

function TCnHexEditor.RowMaxIndex(mRowIndex: Integer): Integer;
begin
  if mRowIndex < 0 then
    Result := 0
  else if mRowIndex >= FLineCount then
    Result := 0
  else if mRowIndex = FLineCount - 1 then
    Result := FMemoryStream.Size mod 16
  else
    Result := 15;
end;

function TCnHexEditor.ColToChar(mColType: TCnMouseObject; mCol: Integer): Integer;
begin
  Result := 0;
  case mColType of
    moChar:
      Result := 60 + mCol;
    moHex:
      begin
        case mCol of
          0..7:
            Result := 10 + mCol * 3;
          8..15:
            Result := 11 + mCol * 3;
        end;
      end;
  end;
end;

procedure TCnHexEditor.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly = Value then
    Exit;
  FReadOnly := Value;
  if FReadOnly then
    Cursor := crDefault;
end;

procedure TCnHexEditor.SetSelLength(const Value: Integer);
var
  vCaretPoint: TPoint;
begin
  if FSelLength = Value then
    Exit;
  FSelLength := Max(Min(Value, FMemoryStream.Size - FSelStart), 0);
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
  vCaretPoint := PositionToCoordinate(FSelStart + FSelLength);
  FColIndex := vCaretPoint.X;
  FRowIndex := vCaretPoint.Y;
  Invalidate;
end;

procedure TCnHexEditor.SetSelStart(Value: Integer);
var
  vCaretPoint: TPoint;
begin
  if FSelStart = Value then
    Exit;
  FSelStart := Max(Min(Value, FMemoryStream.Size), 0);
  FSelLength := Max(Min(FSelLength, FMemoryStream.Size - FSelStart), 0);
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
  vCaretPoint := PositionToCoordinate(FSelStart + FSelLength);
  FColIndex := vCaretPoint.X;
  FRowIndex := vCaretPoint.Y;
  Invalidate;
end;

procedure TCnHexEditor.SelectionChange;
var
  vSelLength: Integer;
begin
  vSelLength := FSelLength;

  FSelStart := Max(Min(FRowIndex * 16 + FColIndex, FMemoryStream.Size), 0);
  FSelLength := 0;
  FHexChar := #0;
  if vSelLength > 0 then
    Invalidate;
  UpdateCaret;
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

function TCnHexEditor.PositionToCoordinate(mPosition: Integer): TPoint;
begin
  Result := Point(-1, -1);
  if mPosition < 0 then
    Exit;
  if mPosition > FMemoryStream.Size then
    Exit;
  Result.X := mPosition mod 16;
  Result.Y := mPosition div 16;
end;

function TCnHexEditor.SelectionViewText(mColType: TCnMouseObject; mLineIndex:
  Integer; mStart, mEnd: Integer): string;
const
  cHexDigits: array[0..15] of Char = '0123456789ABCDEF';
var
  I, L: Integer;
  vBytes: array[0..15] of Byte;
  S: string;
begin
  Result := '';
  if mLineIndex < 0 then
    Exit;
  FMemoryStream.Position := mLineIndex * 16;
  L := FMemoryStream.Read(vBytes, 16);
  S := '';
  for I := Max(0, mStart) to Min(15, mEnd) do
  begin
    case mColType of
      moHex:
        if I = 8 then
          Result := Result + ' ';
      moChar:
        ;
    end;
    if I < L then
    begin
      case mColType of
        moHex:
          Result := Result + cHexDigits[vBytes[I] shr $04] +
            cHexDigits[vBytes[I] and $0F] + ' ';
        moChar:
          if vBytes[I] in [32..126] then
            Result := Result + Chr(vBytes[I])
          else
            Result := Result + '.';
      end;
    end;
  end;
  if mColType = moHex then
    Result := Trim(Result);
end;

procedure TCnHexEditor.SetAnchorOffset(Value: Integer);
var
  vCaretPoint: TPoint;
begin
  if FAnchorStart = Value then
    Exit;
  if FAnchorStart + Value < 0 then
    Exit;
  if FAnchorStart + Value > FMemoryStream.Size then
    Exit;
  FAnchorOffset := Value;
  FSelLength := Abs(FAnchorOffset);

  if FAnchorOffset < 0 then
  begin
    FSelStart := FAnchorStart + FAnchorOffset;
    vCaretPoint := PositionToCoordinate(FSelStart);
  end
  else
  begin
    FSelStart := FAnchorStart;
    vCaretPoint := PositionToCoordinate(FSelStart + FSelLength);
  end;
  FColIndex := vCaretPoint.X;
  FRowIndex := vCaretPoint.Y;
  ScrollIntoView;
  UpdateCaret;
  Invalidate;
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

procedure TCnHexEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Coordinate: TPoint;
begin
  inherited;
  case MouseObject(Point(X, Y), Coordinate) of
    moAddress:
      Cursor := crDefault;
    moHex:
      Cursor := crIBeam;
    moChar:
      Cursor := crIBeam;
    moNone:
      Cursor := crDefault;
  end;
end;

function TCnHexEditor.CoordinatePosition(mCoordinate: TPoint): Integer;
begin
  Result :=
    Max(Min(mCoordinate.Y * 16 + mCoordinate.X, FMemoryStream.Size), 0);
end;

procedure TCnHexEditor.WMCHAR(var Msg: TWMChar);
var
  vChar: Char;
  vCoordinate: TPoint;
  vRect: TRect;
  vSelStart: Integer;
begin
  inherited;
  if FReadOnly then
    Exit;
  if not FChangeDataSize and (FSelStart >= FMemoryStream.Size) then
    Exit;
  case Msg.CharCode of
    0..27, 128..255:
      Exit;
  end;

  FMemoryStream.Position := FSelStart;
  vSelStart := FSelStart;
  if FColType = moHex then
  begin
    case Msg.CharCode of
      Ord('0')..Ord('9'):
        ;
      Ord('A')..Ord('F'):
        ;
      Ord('a')..Ord('f'):
        ;
    else
      Exit;
    end;
    if FHexChar = #0 then
    begin
      FHexChar := Char(Msg.CharCode);
      vChar := Char(StrToIntDef('$' + FHexChar, 0));
    end
    else
    begin
      vChar := Char(StrToIntDef('$' + FHexChar + Char(Msg.CharCode), 0));
      FSelStart := FSelStart + 1;
      FHexChar := #0;
    end;
  end
  else if FColType = moChar then
  begin
    vChar := Char(Msg.CharCode);
    FSelStart := FSelStart + 1;
  end;
  FMemoryStream.Position := vSelStart;
  FMemoryStream.Write(vChar, SizeOf(vChar));
  vCoordinate := PositionToCoordinate(FSelStart);
  FRowIndex := vCoordinate.Y;
  FColIndex := vCoordinate.X;
  if FSelStart = FMemoryStream.Size then
    DoChange;
  if FSelLength > 0 then
  begin
    FSelLength := 0;
    Invalidate;
  end
  else
  begin
    vCoordinate := PositionToCoordinate(vSelStart);
    vRect.TopLeft := CoordinateToPoint(moChar, vCoordinate);
    vRect.BottomRight.X := vRect.TopLeft.X + FItemWidth;
    vRect.BottomRight.Y := vRect.TopLeft.Y + FItemHeight;
    Windows.InvalidateRect(Handle, @vRect, True);

    vRect.TopLeft := CoordinateToPoint(moHex, vCoordinate);
    vRect.BottomRight.X := vRect.TopLeft.X + FItemWidth * 3;
    vRect.BottomRight.Y := vRect.TopLeft.Y + FItemHeight;
    Windows.InvalidateRect(Handle, @vRect, True);
  end;
  UpdateCaret;
end;

procedure TCnHexEditor.WMIMECHAR(var Msg: TCnWMImeChar);
var
  vCoordinate: TPoint;
  vRect: TRect;
begin
  inherited;
  if FReadOnly then
    Exit;
  FMemoryStream.Position := FSelStart;
  if FColType = moChar then
  begin
    { TODO -c2006.11.17 -oZswangY37 : 考虑采用插入模式输入 }
    FMemoryStream.Write(Msg.CharCode, 2);
    FSelStart := FSelStart + 2;
    vCoordinate := PositionToCoordinate(FSelStart);
    FRowIndex := vCoordinate.Y;
    FColIndex := vCoordinate.X;
    if FSelStart = FMemoryStream.Size then
      DoChange;
    if FSelLength > 0 then
    begin
      FSelLength := 0;
      Invalidate;
    end
    else
    begin
      vCoordinate := PositionToCoordinate(FSelStart - 2);
      vRect.TopLeft := CoordinateToPoint(moChar, vCoordinate);
      vRect.BottomRight.X := vRect.TopLeft.X + FItemWidth * 2;
      vRect.BottomRight.Y := vRect.TopLeft.Y + FItemHeight;
      Windows.InvalidateRect(Handle, @vRect, True);

      vRect.TopLeft := CoordinateToPoint(moHex, vCoordinate);
      vRect.BottomRight.X := vRect.TopLeft.X + FItemWidth * 4 * 2;
      vRect.BottomRight.Y := vRect.TopLeft.Y + FItemHeight;
      Windows.InvalidateRect(Handle, @vRect, True);
    end;
    UpdateCaret;
  end;
end;

function TCnHexEditor.GetSelBytes: TBytes;
begin
  Result := nil;
  if FSelLength <= 0 then
    Exit;

  SetLength(Result, FSelLength);
  FMemoryStream.Position := FSelStart;
  FMemoryStream.Read(Result[0], FSelLength);
end;

function TCnHexEditor.GetSelText: string;
begin
  Result := '';
  if FSelLength <= 0 then
    Exit;
  SetLength(Result, FSelLength);
  FMemoryStream.Position := FSelStart;
  FMemoryStream.Read(Result[1], FSelLength);
end;

procedure TCnHexEditor.SetSelText(const Value: string);
var
  vCaretPoint: TPoint;
  L: Integer;
begin
  L := Length(Value);
  if (L <= 0) and (FSelLength <= 0) then
    Exit;
  if FSelLength > 0 then
    DeleteFromStream(FMemoryStream, FSelStart, FSelLength);
  if L > 0 then
    InsertToStream(FMemoryStream, FSelStart, Value[1], L);
  FSelLength := 0;
  FSelStart := FSelStart + L;
  vCaretPoint := PositionToCoordinate(FSelStart + FSelLength);
  FColIndex := vCaretPoint.X;
  FRowIndex := vCaretPoint.Y;
  DoChange;
end;

procedure TCnHexEditor.DoEnter;
begin
  inherited;
  UpdateCaret;
end;

procedure TCnHexEditor.DoExit;
begin
  inherited;
  UpdateCaret;
end;

procedure TCnHexEditor.LoadFromBuffer(const Buffer; Size: Integer);
begin
  FMemoryStream.Clear;
  FMemoryStream.Write(Buffer, Size);
  FSelLength := 0;
  FSelStart := 0;
  FColIndex := 0;
  FRowIndex := 0;
  DoChange;
end;

procedure TCnHexEditor.SaveToBuffer(var Buffer; Size: Integer);
begin
  FMemoryStream.Position := 0;
  FMemoryStream.Read(Buffer, Size);
end;

end.

