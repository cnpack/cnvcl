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

unit CnLED;
{* |<PRE>
================================================================================
* ������ƣ����������
* ��Ԫ���ƣ�һ��������ʾ���ֵ����ֿ�� LED �ؼ�ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�����־ (khzide@163.com)
* ��    ע���ؼ�������ͬ����Ҫһ���ض��ֿ⣬��������û���ҵ�����ģ�
*           ���Ǿ��뵽ֱ�Ӵ�ϵͳ���������ɣ����������˸��������ֺſ���ѡ��
*           Ϊ�˲�������Ч��������ô��������
*           ���ؼ������ȶ�������������ɾ�ģ�ʹ�á�
*           ���ؼ����������ڵ������ֿ�ʱ�ڵ������Ȳ���Ч����
*           �������䶯����ʾ����.������Ӳ������ʦ��ֱ�۵��˽�
*           �ֿ⵼���ĸ�ʽ.Ӳ������ʦ��Ҳ����ô��?^^)
*           ���ؼ��ܹ�û�ö���ʱ�䣬��������ֻ�ǹ���ʵ�֣���δ�����Ż���
*           �ٶȺ��ڴ�ռ�ö��п��Ż��ĵط���
*           �������������ϵ���ߣ�qq:382689788 Email:khzide@163.com
* ����ƽ̨��PWinXP + Delphi 2007
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.11.13 V1.1
*                ������Ԫ������ע��
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
    {* ��ʾ�ı�}
    procedure ExportWordInfo(const Text: string; Stream: TStream);
    {* ����������Ϣ}
    property Pixels[Inx: Integer]: Boolean read GetPixels write SetPixels;
    {* ��ȡ������ĳһ���ֵ}
    property PointCount: Integer read GetLength;
    {* �ܹ����Ա�ʾ�ĵ���}
  published
    property Align;

    property Font;

    property PointSize: Integer read FPointSize write SetPointSize;
    {*  ÿһ�� LED ��ĸ߶�}
    property CellBorderWidth: Integer read FCellBorderWidth write SetCellBorderWidth;
    {*  ��߿���}
    property CellColor: TColor read FCellColor write SetCellColor;
    {*  ����ɫ}
    property CellBorderColor: TColor read FCellBorderColor write SetCellBorderColor;
    {*  ��߿���ɫ}
    property CellHotColor: TColor read FCellHotColor write SetCellHotColor;
    {*  �������ɫ}
    property WordBorderColor: TColor read FWordBorderColor write SetWordBorderColor;
    {*  �ֱ߿���ɫ}
    property CellAnimateColor: TColor read FCellAnimateColor write SetCellAnimateColor;
    {*  ������ʾʱ�ĵ���ɫ}
    property WordBorderWidth: Integer read FWordBorderWidth write SetWordBorderWidth;
    {*  �ֱ߿��߿��}
    property ModeRight: Integer read FModeRight write SetModeRight;
    {*  0 �Ǵ�������}
    property ModeBottom: Integer read FModeBottom write SetModeBottom;
    {* 0 �Ǵ�������}
    property ModeColumn: Integer read FModeColumn write SetModeColumn;
    {*  0 �����к���}
    property FirstLowBit: Integer read FFirstLowBit write SetFirstLowBit;
    {* 0 �ȸ�λ���λ}
    property ModeStructOut: Integer read FModeStructOut write FModeStructOut;
    {* 0 �ǽṹ�����}
    property Animate: Boolean read FAnimate write SetAnimate;
    {* �Ƿ���ʾ����}
    property Text: string read FTextDraw write SetTextDraw;
    {* ����ʾ����}
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
  FTextDraw := '������ʾLED';
  FCellBorderColor := clBlack;
  FWordBorderWidth := 1;
  FWordBorderColor := clRed;
  FCellAnimateColor := clYellow;
  FCellColor := $00003300;
  FCellHotColor := clLime;

  FFontInit := True;
  try
    Font.Name := '����';
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
  // ��������
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

  // Ĭ��Ϊ�Ǵ�������,���к���,��λ��ǰ,���ϵ���
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
  sHead = '// ------------------  ������ģ�����ݽṹ���� ------------------------ //'#13#10 +
    'typedef struct typFNT_GB%0:d                 // ������ģ���ݽṹ'#13#10 +
    '{'#13#10 +
    '       signed char Index[2];               // ������������'#13#10 +
    '       char Msk[%1:d];                       // ����������'#13#10 +
    '};'#13#10#13#10 +
    '/////////////////////////////////////////////////////////////////////////'#13#10 +
    '// ������ģ��                                                          //'#13#10 +
    '/////////////////////////////////////////////////////////////////////////'#13#10 +
    'struct typFNT_GB%0:d code GB%0:d[] =          // ���ݱ�'#13#10 +
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
  LineByteCount := FInfoBmp.Width div 8; // һ�м����ֽ�
  // Ĭ��Ϊ�Ǵ������ң����к��У���λ��ǰ�����ϵ���
  if LineByteCount = 0 then
    LineByteCount := 1;
  Result.Left := inx mod LineByteCount * 8;
  Result.Top := inx div LineByteCount;
  Result.Right := 1;
  Result.Bottom := 0;
  if FModeColumn <> 0 then // ���к���
  begin
    Result.Left := inx mod FInfoBmp.Width;
    Result.Top := inx div FInfoBmp.Width * 8;
    Result.Right := 0;
    Result.Bottom := 1;
  end;
  if FModeRight <> 0 then // ��������
  begin
    Result.Left := FInfoBmp.Width - Result.Left;
    Result.Right := -Result.Right;
    Result.Left := Result.Left + Result.Right;
  end;
  if FModeBottom <> 0 then // ��������
  begin
    Result.Top := FInfoBmp.Height - Result.Top;
    Result.Bottom := -Result.Bottom;
    Result.top := Result.top + result.Bottom;
  end;
  if FFirstLowBit <> 0 then // �ȵ�λ���λ
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
      Result.Top := Result.Top + result.Bottom; // ��������һ����
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
  ExportWordInfo('��', FPointStream);
end;

procedure TCnLEDText.DrawHotCell;
var
  OldColor: TColor;
  X, Y, RC, CC: Integer;
begin
  // ��դ��
  RC := Height div FPointSize; // ������ٸ���
  CC := Width div FPointSize; // ģ����ٸ���
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
    // ��������
    FBackBmp.Canvas.Brush.Color := FCellColor;
    FBackBmp.Canvas.FillRect(FBackBmp.Canvas.ClipRect);

    // ��դ��߿�
    if FCellBorderWidth > 0 then
    begin
      RC := Height div FPointSize; // ������ٸ���
      CC := Width div FPointSize; // ģ����ٸ���
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
    //��դ��߿�
    if FWordBorderWidth > 0 then
    begin
      K := FPointSize * Font.Height;
      RC := Height div k; //������ٸ���
      CC := Width div k; //ģ����ٸ���

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
