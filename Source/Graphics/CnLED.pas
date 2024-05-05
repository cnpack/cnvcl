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

unit CnLED;
{* |<PRE>
================================================================================
* 软件名称：界面组件包
* 单元名称：一个可以显示汉字导出字库的 LED 控件实现单元
* 单元作者：康红志 (khzide@163.com)
* 备    注：控件起因：因同事需要一个特定字库，上网搜索没有找到中意的，
*           于是就想到直接从系统字体里生成，这样就有了更多字体字号可以选择
*           为了测试生成效果做了这么个东西。
*           本控件基本稳定，您可随意添删改，使用。
*           本控件本意是用于导出汉字库时在电脑上先测试效果，
*           尤其是其动画显示功能.可以让硬件工程师更直观的了解
*           字库导出的格式.硬件工程师们也都这么懒?^^)
*           本控件总共没用多少时间，所以现在只是功能实现，并未经过优化，
*           速度和内存占用都有可优化的地方。
*           有其它问题可联系作者：qq:382689788 Email:khzide@163.com
* 开发平台：PWinXP + Delphi 2007
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.11.13 V1.1
*                创建单元，增加注释
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Graphics, Controls, ExtCtrls, Forms, Messages;

type
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnLEDText = class(TGraphicControl)
  private
    FCellBorderColor: TColor;
    FWordBorderWidth: Integer;
    FWordBorderColor: TColor;
    FCellColor: TColor;
    FCellHotColor: TColor;
    FCellBorderWidth: Integer;
    FPointSize: Integer;
    FFirstLowBit: Integer;
    FModeRight: Integer;
    FModeStructOut: Integer;
    FModeBottom: Integer;
    FModeColumn: Integer;
    FAnimate: Boolean;
    FCellAnimateColor: TColor;
    FFontInit: Boolean;
    procedure SetCellBorderColor(const Value: TColor);
    procedure SetCellBorderWidth(const Value: Integer);
    procedure SetCellColor(const Value: TColor);
    procedure SetCellHotColor(const Value: TColor);
    procedure SetWordBorderColor(const Value: TColor);
    procedure SetWordBorderWidth(const Value: Integer);
    procedure SetPointSize(const Value: Integer);
    procedure DrawDisplayText;
    procedure DrawWordCell;
    procedure EraseBackGround;
    function PerpareBitBmp: TPoint;
    procedure SetAnimate(const Value: Boolean);
    procedure PerparePointStream;
    procedure OnTimer(Sender: TObject);
    procedure SetCellAnimateColor(const Value: TColor);
    procedure SetFirstLowBit(const Value: Integer);
    procedure SetModeBottom(const Value: Integer);
    procedure SetModeColumn(const Value: Integer);
    procedure SetModeRight(const Value: Integer);
    function GetPixels(inx: Integer): Boolean;
    procedure SetPixels(inx: Integer; const Value: Boolean);
    function GetLength: Integer;
    procedure ReDraw;
    procedure DrawCell; overload;
    procedure DrawCell(X: Integer; Y: Integer); overload;
    procedure DrawHotCell; overload;
    procedure DrawHotCell(X: Integer; Y: Integer); overload;
    procedure SetTextDraw(const Value: string);
  protected
    FTextDraw: string;
    FBitBmp: TBitmap;
    FBackBmp: TBitmap;
    FInfoBmp: TBitmap;
    FPointStream: TMemoryStream;
    FTimer: TTimer;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetBmpSize;
    procedure UpdateAnimate;
    procedure ExportPointInfo(const Text: string; Pt: TPoint; Stream: TStream);
    function GetPointIndexInfo(inx: Integer): TRect;
    function GetPointInfo(Rt: TRect): Byte;
    procedure Loaded; override;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ShowText(const Text: string);
    {* 显示文本}
    procedure ExportWordInfo(const Text: string; Stream: TStream);
    {* 导出点阵信息}
    property Pixels[Inx: Integer]: Boolean read GetPixels write SetPixels;
    {* 获取或设置某一点的值}
    property PointCount: Integer read GetLength;
    {* 总共可以表示的点数}
  published
    property Align;

    property Font;

    property PointSize: Integer read FPointSize write SetPointSize;
    {*  每一个 LED 点的高度}
    property CellBorderWidth: Integer read FCellBorderWidth write SetCellBorderWidth;
    {*  点边框宽度}
    property CellColor: TColor read FCellColor write SetCellColor;
    {*  点颜色}
    property CellBorderColor: TColor read FCellBorderColor write SetCellBorderColor;
    {*  点边框颜色}
    property CellHotColor: TColor read FCellHotColor write SetCellHotColor;
    {*  点高亮颜色}
    property WordBorderColor: TColor read FWordBorderColor write SetWordBorderColor;
    {*  字边框颜色}
    property CellAnimateColor: TColor read FCellAnimateColor write SetCellAnimateColor;
    {*  动画演示时的点颜色}
    property WordBorderWidth: Integer read FWordBorderWidth write SetWordBorderWidth;
    {*  字边框线宽度}
    property ModeRight: Integer read FModeRight write SetModeRight;
    {*  0 是从左向右}
    property ModeBottom: Integer read FModeBottom write SetModeBottom;
    {* 0 是从上向下}
    property ModeColumn: Integer read FModeColumn write SetModeColumn;
    {*  0 是先行后列}
    property FirstLowBit: Integer read FFirstLowBit write SetFirstLowBit;
    {* 0 先高位后低位}
    property ModeStructOut: Integer read FModeStructOut write FModeStructOut;
    {* 0 是结构化输出}
    property Animate: Boolean read FAnimate write SetAnimate;
    {* 是否显示动画}
    property Text: string read FTextDraw write SetTextDraw;
    {* 待显示文字}
  end;

implementation

{ TCnLEDText }

constructor TCnLEDText.Create(AOwner: TComponent);
begin
  inherited;
  FBitBmp := TBitmap.Create;
  FInfoBmp := TBitmap.Create;
  FInfoBmp.PixelFormat := pf1bit;
  FBitBmp.PixelFormat := pf1bit;
  FBackBmp := TBitmap.Create;
  FAnimate := False;
  FTextDraw := '数码显示LED';
  FCellBorderColor := clBlack;
  FWordBorderWidth := 1;
  FWordBorderColor := clRed;
  FCellAnimateColor := clYellow;
  FCellColor := $00003300;
  FCellHotColor := clLime;

  FFontInit := True;
  try
    Font.Name := '宋体';
  except
    ;
  end;

  try
    Font.Height := 16;
  except
    ;
  end;
  FFontInit := False;
  
  FCellBorderWidth := 1;
  FPointSize := 6;
  Width := 289;
  Height := 193;
  SetBmpSize;
end;

destructor TCnLEDText.Destroy;
begin
  FreeAndNil(FBitBmp);
  FreeAndNil(FBackBmp);
  FreeAndNil(FInfoBmp);
  FreeAndNil(FPointStream);
  FreeAndNil(FTimer);
  inherited;
end;

procedure TCnLEDText.Paint;
begin
  inherited;
  Canvas.Draw(0, 0, FBackBmp);
end;

procedure TCnLEDText.ReDraw;
begin
  if csLoading in ComponentState then
    Exit;

  EraseBackGround;
  DrawCell;
  DrawDisplayText;
  DrawWordCell;
  DrawHotCell;
  Invalidate;
end;

procedure TCnLEDText.Resize;
begin
  inherited;
  SetBmpSize;
  ReDraw;
end;

procedure TCnLEDText.SetCellAnimateColor(const Value: TColor);
begin
  FCellAnimateColor := Value;
end;

procedure TCnLEDText.SetCellBorderColor(const Value: TColor);
begin
  if FCellBorderColor = Value then
    Exit;

  FCellBorderColor := Value;
  ReDraw;
end;

procedure TCnLEDText.SetCellBorderWidth(const Value: Integer);
begin
  if FCellBorderWidth = Value then
    Exit;

  FCellBorderWidth := Value;
  ReDraw;
end;

procedure TCnLEDText.SetCellColor(const Value: TColor);
begin
  if FCellColor = Value then
    Exit;

  FCellColor := Value;
  ReDraw;
end;

procedure TCnLEDText.SetCellHotColor(const Value: TColor);
begin
  if FCellHotColor = Value then
    Exit;

  FCellHotColor := Value;
  ReDraw;
end;

procedure TCnLEDText.SetFirstLowBit(const Value: Integer);
begin
  if FFirstLowBit = Value then
    Exit;

  FFirstLowBit := Value;
  UpdateAnimate;
end;

procedure TCnLEDText.SetModeBottom(const Value: Integer);
begin
  if FModeBottom = Value then
    Exit;

  FModeBottom := Value;
  UpdateAnimate;
end;

procedure TCnLEDText.SetModeColumn(const Value: Integer);
begin
  if FModeColumn = Value then
    Exit;

  FModeColumn := Value;
  UpdateAnimate;
end;

procedure TCnLEDText.SetModeRight(const Value: Integer);
begin
  if FModeRight = Value then
    Exit;

  FModeRight := Value;
  UpdateAnimate;
end;

procedure TCnLEDText.SetPixels(inx: Integer; const Value: Boolean);
var
  I, J: Integer;
begin
  J := inx div FBitBmp.Width;
  if J >= FBitBmp.Height then
    Exit;
  I := inx - J * FBitBmp.Width;
  if Value then
    FBitBmp.Canvas.Pixels[I, J] := clBlack
  else FBitBmp.Canvas.Pixels[I, J] := clWhite;
end;

procedure TCnLEDText.SetPointSize(const Value: Integer);
begin
  if FPointSize = Value then
    Exit;

  FPointSize := Value;
  SetBmpSize;
  ReDraw;
end;

procedure TCnLEDText.SetWordBorderColor(const Value: TColor);
begin
  if FWordBorderColor = Value then
    Exit;

  FWordBorderColor := Value;
  ReDraw;
end;

procedure TCnLEDText.SetWordBorderWidth(const Value: Integer);
begin
  if FWordBorderWidth = Value then
    Exit;

  FWordBorderWidth := Value;
  ReDraw;
end;

procedure TCnLEDText.ShowText(const Text: string);
begin
  FTextDraw := Text;
  ReDraw;
end;

procedure TCnLEDText.EraseBackGround;
begin
  // 擦除背景
  FBackBmp.Canvas.Brush.Color := clBlack;
  FBackBmp.Canvas.FillRect(FBackBmp.Canvas.ClipRect);
end;

procedure TCnLEDText.ExportPointInfo(const Text: string; Pt: TPoint; Stream: TStream);
var
  Rt: TRect;
  I, PtCnt: Integer;
  B: Byte;
  S: string;
begin
  FInfoBmp.Canvas.FillRect(FInfoBmp.Canvas.ClipRect);
  FInfoBmp.Canvas.TextOut(Pt.X, Pt.Y, Text);

  // 默认为是从左向右,先行后列,高位在前,从上到下
  PtCnt := FInfoBmp.Width div 8 * FInfoBmp.Width;
  if FModeStructOut = 0 then
  begin
    Stream.Write(#13#10'  "', 5);
    Stream.Write(Text[1], Length(Text));
    Stream.Write('", ', 3);
  end;
  for I := 0 to PtCnt - 1 do
  begin
    Rt := GetPointIndexInfo(I);
    B := GetPointInfo(Rt);
    if FModeStructOut <> 0 then
      Stream.Write(B, 1)
    else begin
      S := '0x' + IntToHex(B, 2) + ',';
      Stream.Write(S[1], 5);
      if (I mod 8) = 7 then
        Stream.Write(#13#10'        ', 10);
    end;
  end;

end;

procedure TCnLEDText.ExportWordInfo(const Text: string; Stream: TStream);
const
  sHead = '// ------------------  汉字字模的数据结构定义 ------------------------ //'#13#10 +
    'typedef struct typFNT_GB%0:d                 // 汉字字模数据结构'#13#10 +
    '{'#13#10 +
    '       signed char Index[2];               // 汉字内码索引'#13#10 +
    '       char Msk[%1:d];                       // 点阵码数据'#13#10 +
    '};'#13#10#13#10 +
    '/////////////////////////////////////////////////////////////////////////'#13#10 +
    '// 汉字字模表                                                          //'#13#10 +
    '/////////////////////////////////////////////////////////////////////////'#13#10 +
    'struct typFNT_GB%0:d code GB%0:d[] =          // 数据表'#13#10 +
    '{';

var
  I: Integer;
  Str: string;
  Pt: TPoint;
begin
  Pt := PerpareBitBmp;
  if FModeStructOut = 0 then
  begin
    Str := Format(sHead, [FInfoBmp.Width, FInfoBmp.Width div 8 * FInfoBmp.Width]);
    Stream.Write(Str[1], Length(Str));
  end;
  I := 1;
  while I <= Length(Text) do
  begin
    // Application.ProcessMessages;
{$IFNDEF UNICODE}
    if Ord(Text[I]) > $80 then
    begin
      ExportPointInfo(Copy(Text, I, SizeOf(WideChar)), Pt, Stream);
      Inc(I);
    end
    else
{$ENDIF}
      ExportPointInfo(Text[I], Pt, Stream);
    Inc(I);
  end;
  if FModeStructOut = 0 then
  begin
    Str := #13#10'};';
    Stream.Position := Stream.Position - 11;
    Stream.Write(Str[1], Length(Str));
  end;
end;

procedure TCnLEDText.DrawHotCell(X: Integer; Y: Integer);
var
  PointRect: TRect;
begin
  with PointRect do
  begin
    Left := X * FPointSize + FCellBorderWidth;
    Top := Y * FPointSize + FCellBorderWidth;
    Right := Left + FPointSize - FCellBorderWidth;
    Bottom := Top + FPointSize - FCellBorderWidth;
  end;
  if FBitBmp.Canvas.Pixels[X, Y] <> clWhite then
    FBackBmp.Canvas.FillRect(PointRect);
end;

function TCnLEDText.GetLength: Integer;
begin
  Result := FBitBmp.Width * FBitBmp.Height;
end;

function TCnLEDText.GetPixels(inx: Integer): Boolean;
var
  I, J: Integer;
begin
  J := inx div FBitBmp.Width;
  if J >= FBitBmp.Height then
  begin
    Result := False;
    Exit;
  end;
  I := inx - J * FBitBmp.Width;
  Result := FBitBmp.Canvas.Pixels[I, J] = clBlack;
end;

function TCnLEDText.GetPointIndexInfo(inx: Integer): TRect;
var
  LineByteCount: Integer;
begin
  LineByteCount := FInfoBmp.Width div 8; // 一行几个字节
  // 默认为是从左向右，先行后列，高位在前，从上到下
  if LineByteCount = 0 then
    LineByteCount := 1;
  Result.Left := inx mod LineByteCount * 8;
  Result.Top := inx div LineByteCount;
  Result.Right := 1;
  Result.Bottom := 0;
  if FModeColumn <> 0 then // 先列后行
  begin
    Result.Left := inx mod FInfoBmp.Width;
    Result.Top := inx div FInfoBmp.Width * 8;
    Result.Right := 0;
    Result.Bottom := 1;
  end;
  if FModeRight <> 0 then // 从右向左
  begin
    Result.Left := FInfoBmp.Width - Result.Left;
    Result.Right := -Result.Right;
    Result.Left := Result.Left + Result.Right;
  end;
  if FModeBottom <> 0 then // 从下向上
  begin
    Result.Top := FInfoBmp.Height - Result.Top;
    Result.Bottom := -Result.Bottom;
    Result.top := Result.top + result.Bottom;
  end;
  if FFirstLowBit <> 0 then // 先低位后高位
  begin
    if Result.Right <> 0 then
    begin
      Result.Left := Result.Left + Result.Right * 8;
      Result.Right := -Result.Right;
      Result.Left := Result.Left + Result.Right;
    end;
    if Result.Bottom <> 0 then
    begin
      Result.Top := Result.Top + Result.Bottom * 8;
      Result.Bottom := -Result.Bottom;
      Result.Top := Result.Top + result.Bottom; // 上面多加了一个点
    end;
  end;
end;

function TCnLEDText.GetPointInfo(Rt: TRect): Byte;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to 7 do
  begin
    Result := Result shl 1;
    if FInfoBmp.Canvas.Pixels[Rt.Left, Rt.Top] <> clWhite then
      Inc(Result);
    Rt.Left := Rt.Left + Rt.Right;
    Rt.Top := Rt.Top + Rt.Bottom;
  end;
end;

procedure TCnLEDText.OnTimer(Sender: TObject);
var
  B: Byte;
  Rt: TRect;
  I: Integer;
begin
  if not Assigned(FPointStream) then
    Exit;

  if FPointStream.Position >= FPointStream.Size then
  begin
    FPointStream.Position := 0;
    FBitBmp.Canvas.FillRect(FBitBmp.Canvas.ClipRect);
    DrawCell;
    DrawDisplayText;
    DrawHotCell;
  end;

  Rt := GetPointIndexInfo(FPointStream.Position);
  FPointStream.Read(B, 1);
  FBackBmp.Canvas.Brush.Color := FCellAnimateColor;
  for I := 0 to 7 do
  begin
    if (B and $80) <> 0 then
      FBitBmp.Canvas.Pixels[Rt.Left, Rt.Top] := clBlack;
    DrawCell(Rt.Left,Rt.Top);
    Rt.Left := Rt.Left + Rt.Right;
    Rt.Top := Rt.Top + Rt.Bottom;
    B := (B and $7F) shl 1;
  end;
  Invalidate;
end;

function TCnLEDText.PerpareBitBmp: TPoint;
var
  ID, IM: Integer;
begin
  ID := (Font.Height + 7) div 8;
  FInfoBmp.Width := ID * 8;
  FInfoBmp.Height := FInfoBmp.Width;
  IM := FInfoBmp.Width - Font.Height;
  FInfoBmp.Canvas.Font.Assign(Font);

  FInfoBmp.Canvas.FillRect(FInfoBmp.Canvas.ClipRect);
  Result.X := 0;
  Result.Y := 0;
  if FModeRight <> 0 then
    Result.X := IM;
  if FModeBottom <> 0 then
    Result.Y := IM;
end;

procedure TCnLEDText.PerparePointStream;
begin
  if not Assigned(FPointStream) then
    FPointStream := TMemoryStream.Create;
  FPointStream.Position := 0;
  FPointStream.Size := Font.Height * ((Font.Height + 7) div 8);
  ModeStructOut := 1;
  ExportWordInfo('汉', FPointStream);
end;

procedure TCnLEDText.DrawHotCell;
var
  OldColor: TColor;
  X, Y, RC, CC: Integer;
begin
  // 画栅格
  RC := Height div FPointSize; // 纵向多少个点
  CC := Width div FPointSize; // 模向多少个点
  OldColor := FBackBmp.Canvas.Brush.Color;

  try
    FBackBmp.Canvas.Brush.Color := FCellHotColor;
    for Y := 0 to RC - 1 do
      for X := 0 to CC - 1 do
        DrawHotCell(X,Y);
  finally
    FBackBmp.Canvas.Brush.Color := OldColor;
  end;
end;

procedure TCnLEDText.DrawCell;
var
  OldPenColor, OldBrushColor: TColor;
  OldPenWidth: Integer;
  I, RC, CC: Integer;
begin
  OldPenColor := FBackBmp.Canvas.Pen.Color;
  OldPenWidth := FBackBmp.Canvas.Pen.Width;
  OldBrushColor := FBackBmp.Canvas.Brush.Color;
  try
    // 擦除背景
    FBackBmp.Canvas.Brush.Color := FCellColor;
    FBackBmp.Canvas.FillRect(FBackBmp.Canvas.ClipRect);

    // 画栅格边框
    if FCellBorderWidth > 0 then
    begin
      RC := Height div FPointSize; // 纵向多少个点
      CC := Width div FPointSize; // 模向多少个点
      FBackBmp.Canvas.Pen.Color := FCellBorderColor;
      FBackBmp.Canvas.Pen.Width := FCellBorderWidth;
      for I := 0 to RC do
      begin
        FBackBmp.Canvas.MoveTo(0, I * FPointSize + FCellBorderWidth div 2);
        FBackBmp.Canvas.LineTo(FBackBmp.Canvas.ClipRect.Right, FBackBmp.Canvas.PenPos.Y);
      end;
      for I := 0 to CC do
      begin
        FBackBmp.Canvas.MoveTo(I * FPointSize + FCellBorderWidth div 2, 0);
        FBackBmp.Canvas.LineTo(FBackBmp.Canvas.PenPos.X, FBackBmp.Canvas.ClipRect.Bottom);
      end;
    end;
  finally
    FBackBmp.Canvas.Pen.Color := OldPenColor;
    FBackBmp.Canvas.Pen.Width := OldPenWidth;
    FBackBmp.Canvas.Brush.Color := OldBrushColor;
  end;
end;

procedure TCnLEDText.DrawCell(X, Y: Integer);
var
  PointRect: TRect;
begin
  with PointRect do
  begin
    Left := X * FPointSize + FCellBorderWidth;
    Top := Y * FPointSize + FCellBorderWidth;
    Right := Left + FPointSize - FCellBorderWidth;
    Bottom := Top + FPointSize - FCellBorderWidth;
  end;
  if FBitBmp.Canvas.Pixels[X, Y] = clWhite then
    FBackBmp.Canvas.FillRect(PointRect);
end;

procedure TCnLEDText.DrawDisplayText;
var
  Rt: TRect;
begin
  if FTextDraw = '' then
    Exit;

  FBitBmp.Canvas.FillRect(FBitBmp.Canvas.ClipRect);
  FBitBmp.Canvas.Font.Assign(Font);
  Rt := fBitBmp.Canvas.ClipRect;
  DrawText(FBitBmp.Canvas.Handle, PChar(FTextDraw), Length(FTextDraw),
    Rt, DT_WORDBREAK);
end;

procedure TCnLEDText.DrawWordCell;
var
  OldPenColor: TColor;
  OldPenWidth: Integer;
  OldBrushStyle: TBrushStyle;
  K, I, RC, CC: Integer;
begin
  OldPenWidth := FBackBmp.Canvas.Pen.Width;
  OldPenColor := FBackBmp.Canvas.Pen.Color;
  OldBrushStyle := FBackBmp.Canvas.Brush.Style;

  try
    //画栅格边框
    if FWordBorderWidth > 0 then
    begin
      K := FPointSize * Font.Height;
      RC := Height div k; //纵向多少个点
      CC := Width div k; //模向多少个点

      FBackBmp.Canvas.Pen.Color := FWordBorderColor;
      FBackBmp.Canvas.Pen.Width := FWordBorderWidth;
      FBackBmp.Canvas.Brush.Style := bsClear;
      for I := 0 to RC do
      begin
        FBackBmp.Canvas.Rectangle(0, I * k + FWordBorderWidth div 2,
        FBackBmp.Canvas.ClipRect.Right, FBackBmp.Canvas.PenPos.Y);
      end;
      for I := 0 to CC do
      begin
        FBackBmp.Canvas.Rectangle(I * k + FWordBorderWidth div 2, 0,
        FBackBmp.Canvas.PenPos.X, FBackBmp.Canvas.ClipRect.Bottom);
      end;
    end;
  finally
    FBackBmp.Canvas.Pen.Width := OldPenWidth;
    FBackBmp.Canvas.Pen.Color := OldPenColor;
    FBackBmp.Canvas.Brush.Style := OldBrushStyle;
  end;
end;

procedure TCnLEDText.SetAnimate(const Value: Boolean);
begin
  if FAnimate <> Value then
  begin
    FAnimate := Value;
    UpdateAnimate;
  end;
end;

procedure TCnLEDText.SetBmpSize;
begin
  FBitBmp.Width := Width div FPointSize;
  FBitBmp.Height := Height div FPointSize;
  FBackBmp.Width := Width;
  FBackBmp.Height := Height;
end;

procedure TCnLEDText.SetTextDraw(const Value: string);
begin
  FTextDraw := Value;
  ReDraw;
end;

procedure TCnLEDText.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not FFontInit then
    ReDraw;
end;

procedure TCnLEDText.UpdateAnimate;
begin
  if FAnimate then
  begin
    PerparePointStream;
    if not Assigned(FTimer) then
    begin
      FTimer := TTimer.Create(self);
      FTimer.Enabled := False;
      FTimer.Interval := 200;
      FTimer.OnTimer := OnTimer;
    end;
    FTimer.Enabled := True;
  end
  else
  begin
    FreeAndNil(FPointStream);
    FreeAndNil(FTimer);
  end;
end;

procedure TCnLEDText.Loaded;
begin
  inherited;
  SetBmpSize;
  ReDraw;
end;

end.
