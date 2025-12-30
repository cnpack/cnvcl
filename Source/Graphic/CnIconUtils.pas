{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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

unit CnIconUtils;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：Ico 图标处理辅助单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：
* 开发平台：Win7 + Delphi 2007
* 兼容测试：
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2014.07.02
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Classes, Graphics;

function CnCreateAlphaCursorFromGraphic(Graphic: TGraphic; Width, Height,
  xHotspot, yHotspot: Integer): HCURSOR;
{* 从 Graphic 图像创建大小为 Width, Height 热点坐标 xHotspot, yHotspot 的带 Alpha 通道光标句柄 }

procedure CnCreateAlphaCursorFileFromGraphic(Graphic: TGraphic; Width, Height,
  xHotspot, yHotspot: Integer; const FileName: string);
{* 从 Graphic 图像创建大小为 Width, Height 热点坐标 xHotspot, yHotspot 的带 Alpha 通道光标文件 }

function CnCreateAlphaIconFromGraphic(Graphic: TGraphic; Width, Height: Integer): HICON;
{* 从 Graphic 图像创建大小为 Width, Height 的带 Alpha 通道图标句柄 }

procedure CnCreateAlphaIconFileFromGraphic(Graphic: TGraphic; Width, Height: Integer;
  const FileName: string);
{* 从 Graphic 图像创建大小为 Width, Height 的带 Alpha 通道图标文件 }

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
  xP2 := ((Src.Width - 1) shl 15) div Dst.Width; // 缩放比例
  yP2 := ((Src.Height - 1) shl 15) div Dst.Height;
  yP := 0;
  for y := 0 to Dst.Height - 1 do
  begin
    pc := Dst.ScanLine[y]; // 目标扫描线
    Read := Src.ScanLine[yP shr 15]; // 源上扫描线
    Read2 := Src.ScanLine[yP shr 15 + 1]; // 源下扫描线
    z2 := yP and $7FFF;       // 源计算行与上扫描线之差 "y"
    iz2 := $8000 - z2;        // 源计算行与下扫描线之差 "1-y"
    xP := 0;
    for x := 0 to Dst.Width - 1 do
    begin
      t := xP shr 15;
      z := xP and $7FFF;      // 源计算像素与左像素之差 "x"
      Col1 := @Read[t];       // 左上像素 "f(0,0)"
      Col2 := @Read[t + 1];   // 右上像素 "f(1,0)"
      Col3 := @Read2[t];      // 左下像素 "f(0,1)"
      Col4 := @Read2[t + 1];  // 右下像素 "f(1,1)"
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
