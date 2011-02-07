{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2011 CnPack 开发组                       }
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

{------------------------------------------------------------------------------}
{ 单元名称: CnSkinMagic_Sample.pas                                             }
{                                                                              }
{ 单元作者: savetime (savetime2k@hotmail.com, http://savetime.delphibbs.com)   }
{ 创建日期: 2004-12-03                                                         }
{                                                                              }
{ 功能介绍:                                                                    }
{     SkinMagic 框架使用举例                                                   }
{                                                                              }
{ 使用说明:                                                                    }
{                                                                              }
{ 更新历史:                                                                    }
{                                                                              }
{ 尚存问题:                                                                    }
{                                                                              }
{------------------------------------------------------------------------------}
unit CnSkinMagic_Sample;

interface

uses
  Windows, Messages, Controls, Graphics, StdCtrls, ExtCtrls, Buttons, Mask,
  DBCtrls, CnSkinMagic;

implementation

type

  TControlEx = class(TControl)
  public
    property Color;
  end;

procedure DrawLineLeftTop(DC: HDC; Pt: TPoint; Color: COLORREF);
var
  Pen: HPEN;
begin
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 1, ColorToRGB(Color)));
  MoveToEx(DC, 0, 0, nil); LineTo(DC, Pt.X - 1, 0);
  MoveToEx(DC, 0, 1, nil); LineTo(DC, 0, Pt.Y - 1);
  DeleteObject(SelectObject(DC, Pen));
end;

procedure DrawLineLeftTop2(DC: HDC; Pt: TPoint; Color: COLORREF);
var
  Pen: HPEN;
begin
  Pen := SelectObject(DC, CreatePen(PS_SOLID, 1, ColorToRGB(Color)));
  MoveToEx(DC, 1, 1, nil); LineTo(DC, Pt.X - 2, 1);
  MoveToEx(DC, 1, 2, nil); LineTo(DC, 1, Pt.Y - 2);
  DeleteObject(SelectObject(DC, Pen));
end;

procedure DrawFrame(DC: HDC; Pt: TPoint; Color: COLORREF);
var
  Pen: HPEN;
  Brush: HBRUSH;
begin
  Pen := SelectObject(DC, CreatePen(PS_INSIDEFRAME, 1, ColorToRGB(Color)));
  Brush := SelectObject(DC, GetStockObject(NULL_BRUSH));
  Rectangle(DC, 0, 0, Pt.X, Pt.Y);
  DeleteObject(SelectObject(DC, Pen));
  SelectObject(DC, Brush);
end;

procedure Cn_WindowProc_FrameWindow(Self: TControlSubClass; var Message: TMessage);
var
  Wnd: HWND;
  DC: HDC;
  Pt: TPoint;
  Control: TControl;          // 使用临时变量减少对象引用的开销
  WinControl: TWinControl;
begin
  Self.OldWindowProc(Message);

  if (Message.Msg = WM_PAINT) or
     (Message.Msg = WM_LBUTTONDOWN) or (Message.Msg = WM_LBUTTONUP) or
     (Message.Msg = CM_MOUSEENTER)  or (Message.Msg = CM_MOUSELEAVE) or
     (Message.Msg = WM_KILLFOCUS)   or (Message.Msg = WM_SETFOCUS) then
  begin
    Control := Self.Control;
    Wnd := 0;
    WinControl := nil;
    
    if Self.IsWinControl then
    begin
      WinControl := TWinControl(Control);
      Wnd := WinControl.Handle;
      DC := GetWindowDC(Wnd);
    end
    else
    begin
      DC := HDC(Message.WParam);
    end;

    Pt.X := Control.Width;
    Pt.Y := Control.Height;

    if Self.IsWinControl then
    begin
      if(WinControl is TEdit) or (WinControl is TListBox) or
        (WinControl is TMemo) or (WinControl is TComboBox) or
        (WinControl is TMaskEdit) or (WinControl is TDBEdit) or
        (WinControl is TDBMemo) then
      begin
        DrawLineLeftTop(DC, Pt, ColorToRGB(clBtnShadow));
        if Self.MouseInControl or WinControl.Focused then
          DrawLineLeftTop2(DC, Pt, ColorToRGB(cl3DDkShadow))
        else
          DrawLineLeftTop2(DC, Pt, ColorToRGB(clBtnFace));
      end
      else if (WinControl is TButton) or (WinControl is TBitBtn) then
      begin
        if Self.MouseInControl or WinControl.Focused then
          DrawLineLeftTop(DC, Pt, ColorToRGB(cl3DDkShadow))
        else
          DrawLineLeftTop(DC, Pt, ColorToRGB(clBtnShadow));

        DrawLineLeftTop2(DC, Pt, ColorToRGB(clBtnHighlight));
      end
      else
      begin
        DrawFrame(DC, Pt, ColorToRGB(clRed));   // 其他类型画个红色框
      end;
    end
    else      // 非 WinControl 类, 画个兰色框
    begin
      DrawFrame(DC, Pt, ColorToRGB(clBlue));
    end;

    if Self.IsWinControl then
      ReleaseDC(Wnd, DC);
  end;
end;


initialization
  // TWinControl
  TCnSkinMagic.RegisterClass(TEdit, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TButton, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TListBox, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TMemo, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TCheckBox, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TRadioButton, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TRadioGroup, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TPanel, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TComboBox, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TBitBtn, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TMaskEdit, @Cn_WindowProc_FrameWindow);

  // TControl
  TCnSkinMagic.RegisterClass(TSpeedButton, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TBevel, @Cn_WindowProc_FrameWindow);

  // DB Controls
  TCnSkinMagic.RegisterClass(TDBEdit, @Cn_WindowProc_FrameWindow);
  TCnSkinMagic.RegisterClass(TDBMemo, @Cn_WindowProc_FrameWindow);
end.
