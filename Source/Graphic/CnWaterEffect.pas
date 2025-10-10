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

{******************************************************************************}
{ Unit Note:                                                                   }
{    The formulas used in this class I found on a website                      }
{    http://freespace.virgin.net/hugo.elias/graphics/x_water.htm               }
{ Reference:                                                                   }
{    TortoiseSVN Source                                                        }
{    http://tortoisesvn.tigris.org/                                            }
{******************************************************************************}

unit CnWaterEffect;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ�ˮ��Ч������Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע��
* ����ƽ̨��PWinXP SP2 + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2005.06.28
              ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Graphics, Math, CnNative;

const
  csDefDamping = 20;

type
  PIntArray = ^TIntArray;
  TIntArray = array[0..65535] of Integer;
  
  PPIntArray = ^TPIntArray;
  TPIntArray = array[0..65535] of PIntArray;

  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..65535] of TRGBTriple;

  PPRGBArray = ^TPRGBArray;
  TPRGBArray = array[0..65535] of PRGBArray;

  TWaterDamping = 1..99;

  TCnWaterEffect = class(TObject)
  private
    FLightModifier: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FBuff1: Pointer;
    FBuff2: Pointer;
    FScanLine1: PPIntArray;
    FScanLine2: PPIntArray;
    FScanLineSrc: PPRGBArray;
    FXLeft: PIntArray;
    FXRight: PIntArray;
    FYUp: PIntArray;
    FYDown: PIntArray;
    FDamping: TWaterDamping;
    procedure SetDamping(Value: TWaterDamping);
  protected
    procedure CalcWater;
    procedure DrawWater(ALightModifier: Integer; Src, Dst: TBitmap);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearWater;
    procedure SetSize(AWidth, AHeight: Integer);
    procedure Render(Src, Dst: TBitmap);
    procedure Blob(x, y: Integer; ARadius, AHeight: Integer);
    property Damping: TWaterDamping read FDamping write SetDamping;
  end;

implementation

{ TCnWaterEffect }

const
  RAND_MAX = $7FFF;

procedure TCnWaterEffect.Blob(x, y: Integer; ARadius, AHeight: Integer);
var
  Rquad: Integer;
  cx, cy, cyq: Integer;
  Left, Top, Right, Bottom: Integer;
begin
  if (x < 0) or (x > FWidth - 1) then
    x := 1 + ARadius + Random(RAND_MAX) mod (FWidth - 2 * ARadius - 1);
  if (y < 0) or (y > FHeight - 1) then
    y := 1 + ARadius + Random(RAND_MAX) mod (FHeight - 2 * ARadius - 1);

  Left := -Min(x, ARadius);
  Right := Min(FWidth - 1 - x, ARadius);
  Top := -Min(y, ARadius);
  Bottom := Min(FHeight - 1 - y, ARadius);
  Rquad := ARadius * ARadius;
  for cy := Top to Bottom do
  begin
    cyq := cy * cy;
    for cx := Left to Right do
    begin
      if (cx * cx + cyq <= Rquad) then
      begin
        Inc(FScanLine1[cy + y][cx + x], AHeight);
      end;
    end;
  end;
end;

procedure TCnWaterEffect.CalcWater;
var
  x, y, xl, xr: Integer;
  NewH: Integer;
  P, P1, P2, P3: PIntArray;
  PT: Pointer;
  Rate: Integer;
begin
  Rate := (100 - FDamping) * 256 div 100;
  for y := 0 to FHeight - 1 do
  begin
    P := FScanLine2[y];
    P1 := FScanLine1[FYUp[y]];
    P2 := FScanLine1[y];
    P3 := FScanLine1[FYDown[y]];
    for x := 0 to FWidth - 1 do
    begin
      xl := FXLeft[x];
      xr := FXRight[x];
      NewH := (P1[xl] + P1[x] + P1[xr] + P2[xl] + P2[xr] + P3[xl] + P3[x] +
        P3[xr]) div 4 - P[x];
      P[x] := NewH * Rate div 256;
    end;
  end;
  
  PT := FBuff1;
  FBuff1 := FBuff2;
  FBuff2 := PT;
  PT := FScanLine1;
  FScanLine1 := FScanLine2;
  FScanLine2 := PT;
end;

procedure TCnWaterEffect.ClearWater;
begin
 if FBuff1 <> nil then
    ZeroMemory(FBuff1, (FWidth * FHeight) * SizeOf(Integer));
 if FBuff2 <> nil then
    ZeroMemory(FBuff2, (FWidth * FHeight) * SizeOf(Integer));
end;

constructor TCnWaterEffect.Create;
begin
  inherited;
  FLightModifier := 10;
  FDamping := csDefDamping;
end;

destructor TCnWaterEffect.Destroy;
begin
  if FBuff1 <> nil then
    FreeMem(FBuff1);
  if FBuff2 <> nil then
    FreeMem(FBuff2);
  if FScanLine1 <> nil then
    FreeMem(FScanLine1);
  if FScanLine2 <> nil then
    FreeMem(FScanLine2);
  if FScanLineSrc <> nil then
    FreeMem(FScanLineSrc);
  if FXLeft <> nil then
    FreeMem(FXLeft);
  if FXRight <> nil then
    FreeMem(FXRight);
  if FYUp <> nil then
    FreeMem(FYUp);
  if FYDown <> nil then
    FreeMem(FYDown);
  inherited;
end;

procedure TCnWaterEffect.DrawWater(ALightModifier: Integer; Src, Dst:
  TBitmap);
var
  dx, dy: Integer;
  I, c, x, y: Integer;
  P1, P2, P3: PIntArray;
  PDst: PRGBArray;
  PSrcDot, PDstDot: PRGBTriple;
  BytesPerLine1, BytesPerLine2: Integer;
begin
  // �Ƚ�Դͼ���Ƶ�Ŀ��ͼ������б仯�����ں��水�㴦��
  Src.PixelFormat := pf24bit;
  Dst.PixelFormat := pf24bit;
  BitBlt(Dst.Canvas.Handle, 0, 0, Src.Width, Src.Height, Src.Canvas.Handle, 0, 0, SRCCOPY);

  FScanLineSrc[0] := Src.ScanLine[0];
  BytesPerLine1 := TCnNativePointer(Src.ScanLine[1]) - TCnNativePointer(FScanLineSrc[0]);
  for I := 1 to FHeight - 1 do
    FScanLineSrc[I] := PRGBArray(TCnNativePointer(FScanLineSrc[I - 1]) + BytesPerLine1);

  PDst := Dst.ScanLine[0];
  BytesPerLine2 := TCnNativePointer(Dst.ScanLine[1]) - TCnNativePointer(PDst);

  for y := 0 to FHeight - 1 do
  begin
    P1 := FScanLine1[FYUp[y]];
    P2 := FScanLine1[y];
    P3 := FScanLine1[FYDown[y]];
    for x := 0 to FWidth - 1 do
    begin
      dx := P2[FXLeft[x]] - P2[FXRight[x]];
      dy := P1[x] - P3[x];

      if (dx = 0) and (dy = 0) then
      begin
        Continue;
      end;

      if (x + dx >= 0) and (x + dx < FWidth) and (y + dy >= 0) and
        (y + dy < FHeight) then
      begin
        PSrcDot := @FScanLineSrc[y + dy][x + dx];
        PDstDot := @PDst[x];

        c := PSrcDot.rgbtBlue - dx;
        if c < 0 then
          PDstDot.rgbtBlue := 0
        else if c > 255 then
          PDstDot.rgbtBlue := 255
        else
          PDstDot.rgbtBlue := c;

        c := PSrcDot.rgbtGreen - dx;
        if c < 0 then
          PDstDot.rgbtGreen := 0
        else if c > 255 then
          PDstDot.rgbtGreen := 255
        else
          PDstDot.rgbtGreen := c;

        c := PSrcDot.rgbtRed - dx;
        if c < 0 then
          PDstDot.rgbtRed := 0
        else if c > 255 then
          PDstDot.rgbtRed := 255
        else
          PDstDot.rgbtRed := c;
      end;
    end;
    PDst := PRGBArray(TCnNativePointer(PDst) + BytesPerLine2);
  end;
end;

procedure TCnWaterEffect.Render(Src, Dst: TBitmap);
begin
  if (FWidth > 0) and (FHeight > 0) then
  begin
    CalcWater;
    DrawWater(FLightModifier, Src, Dst);
  end;
end;

procedure TCnWaterEffect.SetDamping(Value: TWaterDamping);
begin
  if (Value >= Low(TWaterDamping)) and (Value <= High(TWaterDamping)) then
    FDamping := Value;
end;

procedure TCnWaterEffect.SetSize(AWidth, AHeight: Integer);
var
  I: Integer;
begin
  if (AWidth <= 0) or (AHeight <= 0) then
  begin
    AWidth := 0;
    AHeight := 0;
  end;

  FWidth := AWidth;
  FHeight := AHeight;
  ReallocMem(FBuff1, FWidth * FHeight * SizeOf(Integer));
  ReallocMem(FBuff2, FWidth * FHeight * SizeOf(Integer));
  ReallocMem(FScanLine1, FHeight * SizeOf(PIntArray));
  ReallocMem(FScanLine2, FHeight * SizeOf(PIntArray));
  ReallocMem(FScanLineSrc, FHeight * SizeOf(PRGBArray));
  ReallocMem(FXLeft, FWidth * SizeOf(Integer));
  ReallocMem(FXRight, FWidth * SizeOf(Integer));
  ReallocMem(FYUp, FHeight * SizeOf(Integer));
  ReallocMem(FYDown, FHeight * SizeOf(Integer));
  ClearWater;

  if FHeight > 0 then
  begin
    FScanLine1[0] := FBuff1;
    FScanLine2[0] := FBuff2;
    for I := 1 to FHeight - 1 do
    begin
      FScanLine1[I] := @FScanLine1[I - 1][FWidth];
      FScanLine2[I] := @FScanLine2[I - 1][FWidth];
    end;
    for I := 0 to FHeight - 1 do
    begin
      FYUp[I] := Max(I - 1, 0);
      FYDown[I] := Min(I + 1, FHeight - 1);
    end;
  end;

  if FWidth > 0 then
  begin
    for I := 0 to FWidth - 1 do
    begin
      FXLeft[I] := Max(I - 1, 0);
      FXRight[I] := Min(I + 1, FWidth - 1);
    end;
  end;     
end;

end.

