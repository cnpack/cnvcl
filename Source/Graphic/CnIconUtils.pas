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

unit CnIconUtils;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ�Ico ͼ�괦������Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע��
* ����ƽ̨��Win7 + Delphi 2007
* ���ݲ��ԣ�
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2014.07.02
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Classes, Graphics;

function CnCreateAlphaCursorFromGraphic(Graphic: TGraphic; Width, Height,
  xHotspot, yHotspot: Integer): HCURSOR;
{* �� Graphic ͼ�񴴽���СΪ Width, Height �ȵ����� xHotspot, yHotspot �Ĵ� Alpha ͨ������� }

procedure CnCreateAlphaCursorFileFromGraphic(Graphic: TGraphic; Width, Height,
  xHotspot, yHotspot: Integer; const FileName: string);
{* �� Graphic ͼ�񴴽���СΪ Width, Height �ȵ����� xHotspot, yHotspot �Ĵ� Alpha ͨ������ļ� }

function CnCreateAlphaIconFromGraphic(Graphic: TGraphic; Width, Height: Integer): HICON;
{* �� Graphic ͼ�񴴽���СΪ Width, Height �Ĵ� Alpha ͨ��ͼ���� }

procedure CnCreateAlphaIconFileFromGraphic(Graphic: TGraphic; Width, Height: Integer;
  const FileName: string);
{* �� Graphic ͼ�񴴽���СΪ Width, Height �Ĵ� Alpha ͨ��ͼ���ļ� }

implementation

procedure FastSmoothDrawBitmap32(Src, Dst: TBitmap);
type
  PColor32 = ^TColor32;
  TColor32 = array[0..3] of Byte;
  PColor32Array = ^TColor32Array;
  TColor32Array = array[0..4095] of TColor32;
var
  x, y, xP, yP: Integer;
  yP2, xP2: Integer;
  Read, Read2: PColor32Array;
  t, z, z2, iz2: Integer;
  pc: PColor32;
  w1, w2, w3, w4: Integer;
  Col1, Col2, Col3, Col4: PColor32;
begin
  Src.PixelFormat := pf32bit;
  Dst.PixelFormat := pf32bit;
  xP2 := ((Src.Width - 1) shl 15) div Dst.Width; // ���ű���
  yP2 := ((Src.Height - 1) shl 15) div Dst.Height;
  yP := 0;
  for y := 0 to Dst.Height - 1 do
  begin
    pc := Dst.ScanLine[y]; // Ŀ��ɨ����
    Read := Src.ScanLine[yP shr 15]; // Դ��ɨ����
    Read2 := Src.ScanLine[yP shr 15 + 1]; // Դ��ɨ����
    z2 := yP and $7FFF;       // Դ����������ɨ����֮�� "y"
    iz2 := $8000 - z2;        // Դ����������ɨ����֮�� "1-y"
    xP := 0;
    for x := 0 to Dst.Width - 1 do
    begin
      t := xP shr 15;
      z := xP and $7FFF;      // Դ����������������֮�� "x"
      Col1 := @Read[t];       // �������� "f(0,0)"
      Col2 := @Read[t + 1];   // �������� "f(1,0)"
      Col3 := @Read2[t];      // �������� "f(0,1)"
      Col4 := @Read2[t + 1];  // �������� "f(1,1)"
      w2 := (z * iz2) shr 15;
      w1 := iz2 - w2;
      w4 := (z * z2) shr 15;
      w3 := z2 - w4;
      pc[0] := (Col1[0] * w1 + Col2[0] * w2 + Col3[0] * w3 + Col4[0] * w4) shr 15;
      pc[1] := (Col1[1] * w1 + Col2[1] * w2 + Col3[1] * w3 + Col4[1] * w4) shr 15;
      pc[2] := (Col1[2] * w1 + Col2[2] * w2 + Col3[2] * w3 + Col4[2] * w4) shr 15;
      pc[3] := (Col1[3] * w1 + Col2[3] * w2 + Col3[3] * w3 + Col4[3] * w4) shr 15;
      Inc(pc);
      Inc(xP, xP2);
    end;
    Inc(yP, yP2);
  end;
end;

function DoCreateAlphaIconFromGraphic(Graphic: TGraphic; Width, Height,
  xHotspot, yHotspot: Integer; IsIcon: Boolean): HICON;
var
  IconInfo: TIconInfo;
  Bmp, AlphaBmp: TBitmap;
begin
  if (Graphic = nil) or (Graphic.Empty) then
  begin
    Result := 0;
    Exit;
  end;

  if Graphic is TIcon then
  begin
    Result := TIcon(Graphic).Handle;
    Exit;
  end;
  
  IconInfo.fIcon := IsIcon;
  IconInfo.xHotspot := xHotspot;
  IconInfo.yHotspot := yHotspot;
  if Width = 0 then
    Width := Graphic.Width;
  if Height = 0 then
    Height := Graphic.Height;
  Bmp := TBitmap.Create;
  AlphaBmp := TBitmap.Create;
  try
    Bmp.Assign(Graphic);
    Bmp.PixelFormat := pf32bit;
    AlphaBmp.PixelFormat := pf32bit;
{$IFDEF DELPHI2007_UP}
    AlphaBmp.SetSize(Width, Height);
{$ELSE}
    AlphaBmp.Width := Width;
    AlphaBmp.Height := Height;
{$ENDIF}
    FastSmoothDrawBitmap32(Bmp, AlphaBmp);
    IconInfo.hbmColor := AlphaBmp.Handle;
    IconInfo.hbmMask := CreateBitmap(Width, Height, 1, 1, nil);
    Result := CreateIconIndirect(IconInfo);
  finally
    Bmp.Free;
    AlphaBmp.Free;
  end;
end;

function CnCreateAlphaCursorFromGraphic(Graphic: TGraphic; Width, Height,
  xHotspot, yHotspot: Integer): HCURSOR;
begin
  Result := DoCreateAlphaIconFromGraphic(Graphic, Width, Height, xHotspot, yHotspot, False);
end;

procedure CnCreateAlphaCursorFileFromGraphic(Graphic: TGraphic; Width, Height,
  xHotspot, yHotspot: Integer; const FileName: string);
var
  Icon: TIcon;
begin
  Icon := TIcon.Create;
  try
    Icon.Handle := CnCreateAlphaCursorFromGraphic(Graphic, Width, Height,
      xHotspot, yHotspot);
    Icon.SaveToFile(FileName);
  finally
    Icon.Free;
  end;
end;

function CnCreateAlphaIconFromGraphic(Graphic: TGraphic; Width, Height: Integer): HICON;
begin
  Result := DoCreateAlphaIconFromGraphic(Graphic, Width, Height, 0, 0, True);
end;

procedure CnCreateAlphaIconFileFromGraphic(Graphic: TGraphic; Width, Height: Integer;
  const FileName: string);
var
  Icon: TIcon;
begin
  Icon := TIcon.Create;
  try
    Icon.Handle := CnCreateAlphaIconFromGraphic(Graphic, Width, Height);
    Icon.SaveToFile(FileName);
  finally
    Icon.Free;
  end;
end;

end.
