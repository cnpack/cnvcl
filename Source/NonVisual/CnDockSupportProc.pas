{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2010 CnPack 开发组                       }
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

{*******************************************************}
{                                                       }
{       一些通用的函数                                  }
{       CnDockSupportProc 单元                          }
{                                                       }
{       版权 (C) 2002,2003 鲁小班                       }
{                                                       }
{*******************************************************}

unit CnDockSupportProc;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包停靠单元
* 单元名称：一些通用的函数单元 
* 单元作者：CnPack开发组 周益波（鲁小班）
* 备    注：本单元由原作者授权CnPack开发组移植，已保留原作者版权信息
* 开发平台：
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.11.18 V1.1
*                wqyfavor 修正 D2009 下的不兼容问题
*           2007.07.13 V1.0
*                移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses Classes, Windows, SysUtils, Graphics, Forms, Controls, Messages;

type
  TListScanKind = (lskForward, lskBackward);

{ ---------------------------------------------------------------------------- }
function Cn_StreamDataToString(Stream: TStream): string;
procedure Cn_StringToStreamData(Stream: TStream; Data: string);
{ ---------------------------------------------------------------------------- }
function Cn_FindDockFormWithName(FormName: string;
              FromDockManager: Boolean = False;
              FromList: Boolean = True;
              ScanKind: TListScanKind = lskForward): TCustomForm;
function Cn_FindDockServerFormWithName(FormName: string;
              FromDockManager: Boolean = False;
              FromList: Boolean = True;
              ScanKind: TListScanKind = lskForward): TCustomForm;
function Cn_FindDockClientFormWithName(FormName: string;
              FromDockManager: Boolean = False;
              FromList: Boolean = True;
              ScanKind: TListScanKind = lskForward): TCustomForm;
function Cn_FindDockServerFromDockManager(FormName: string;
              FromList: Boolean = True;
              ScanKind: TListScanKind = lskForward): TCustomForm;
function Cn_FindDockClientFromDockManager(FormName: string;
              FromList: Boolean = True;
              ScanKind: TListScanKind = lskForward): TCustomForm;
function Cn_FindDockFormFromScreen(FormName: string;
              ScanKind: TListScanKind = lskForward): TCustomForm;
{ ---------------------------------------------------------------------------- }
function Cn_GetMinOffset(TBDockSize, ControlSize: Integer; Scale: Real): Integer;
{ ---------------------------------------------------------------------------- }
function Cn_GetNoNClientMetrics: TNONCLIENTMETRICS;
{ 获得系统的标题栏的高度 }
function Cn_GetSysCaptionHeight: Integer;
{ 获得系统的窗体的边框 }
function Cn_GetSysBorderWidth: Integer;
function Cn_GetSysCaptionHeightAndBorderWidth: Integer;
{ ---------------------------------------------------------------------------- }
{ 获得活动的标题栏的开始颜色 }
function Cn_GetActiveTitleBeginColor: TColor;
{ 获得活动的标题栏的结束颜色 }
function Cn_GetActiveTitleEndColor: TColor;
{ 获得非活动的标题栏的开始颜色 }
function Cn_GetInactiveTitleBeginColor: TColor;
{ 获得非活动的标题栏的结束颜色 }
function Cn_GetInactiveTitleEndColor: TColor;
{ 获得标题栏的字体颜色，Active指示是否是获得焦点 }
function Cn_GetTitleFontColor(Active: Boolean): TColor;
{ 获得活动的标题栏的字体颜色 }
function Cn_GetActiveTitleFontColor: TColor;
{ 获得非活动的标题栏的字体颜色 }
function Cn_GetInactiveTitleFontColor: TColor;
{ 获得标题栏的字体 }
function Cn_GetTitleFont: TFont;
{ 锁住窗体 }
procedure Cn_LockWindow(Control: TWinControl);
{ 解锁窗体 }
procedure Cn_UnLockWindow;
{ ---------------------------------------------------------------------------- }
{ 输入一些值创建一个TWMNCHitMessage结构并且返回 }
function Cn_CreateNCMessage(Control: TControl; Msg: Cardinal; HTFlag: Integer; Pos: TPoint): TWMNCHitMessage;
{ 交换参数Orient的值 }
function Cn_ExchangeOrient(Orient: TDockOrientation): TDockOrientation;
{ 根据输入的Control的Align属性得到它的方向 }
function Cn_GetControlOrient(AControl: TControl): TDockOrientation;
{ 根据输入的Control的Align属性得到它的宽度或者高度 }
function Cn_GetControlSize(AControl: TControl): Integer;

implementation

uses
  Math, CnDockFormControl, CnDockGlobal;

var
  Cn_TitleFont: TFont;

function Cn_StreamDataToString(Stream: TStream): string;
var
  B: Byte;
begin
  Result := '';
  Stream.Position := 0;
  while Stream.Position < Stream.Size do
  begin
    Stream.Read(B, SizeOf(B));
    Result := Result + IntToHex(B, 2);
  end;
end;

procedure Cn_StringToStreamData(Stream: TStream; Data: string);
var
  i: Integer;
  B: Byte;
begin
  i := 1;
  while i < Length(Data) do
  begin
    B := StrToInt('$' + Copy(Data, i, 2));
    Stream.Write(B, SizeOf(B));
    Inc(i, 2);
  end;
end;

function Cn_FindDockFormWithName(FormName: string;
              FromDockManager: Boolean;
              FromList: Boolean;
              ScanKind: TListScanKind): TCustomForm;
begin
  Result := Cn_FindDockClientFormWithName(FormName, FromDockManager, FromList, ScanKind);
  if Result = nil then
    Result := Cn_FindDockServerFormWithName(FormName, FromDockManager, FromList, ScanKind);
end;

function Cn_FindDockServerFormWithName(FormName: string;
  FromDockManager: Boolean;
  FromList: Boolean;
  ScanKind: TListScanKind): TCustomForm;
begin
  if FromDockManager then
    Result := Cn_FindDockServerFromDockManager(FormName, FromList, ScanKind)
  else Result := Cn_FindDockFormFromScreen(FormName, ScanKind);
end;

function Cn_FindDockClientFormWithName(FormName: string;
  FromDockManager: Boolean;
  FromList: Boolean;
  ScanKind: TListScanKind): TCustomForm;
begin
  if FromDockManager then
    Result := Cn_FindDockClientFromDockManager(FormName, FromList, ScanKind)
  else Result := Cn_FindDockFormFromScreen(FormName, ScanKind);
end;

function Cn_FindDockServerFromDockManager(FormName: string;
              FromList: Boolean;
              ScanKind: TListScanKind): TCustomForm;
var
  i: Integer;
begin
  case ScanKind of
    lskForward:
    begin
      for i := 0 to CnGlobalDockPresident.DockServersList.Count - 1 do
        if FormName = TCustomForm(CnGlobalDockPresident.DockServersList[i]).Name then
        begin
          Result := TCustomForm(CnGlobalDockPresident.DockServersList[i]);
          Exit;
        end;
    end;
    lskBackward:
    begin
      for i := CnGlobalDockPresident.DockServersList.Count - 1 downto 0 do
        if FormName = TCustomForm(CnGlobalDockPresident.DockServersList[i]).Name then
        begin
          Result := TCustomForm(CnGlobalDockPresident.DockServersList[i]);
          Exit;
        end;
    end;
  end;
  Result := nil;
end;

function Cn_FindDockClientFromDockManager(FormName: string;
              FromList: Boolean;
              ScanKind: TListScanKind): TCustomForm;
var
  i: Integer;
begin
  case ScanKind of
    lskForward:
    begin
      for i := 0 to CnGlobalDockPresident.DockClientsList.Count - 1 do
        if FormName = TCustomForm(CnGlobalDockPresident.DockClientsList[i]).Name then
        begin
          Result := TCustomForm(CnGlobalDockPresident.DockClientsList[i]);
          Exit;
        end;
    end;
    lskBackward:
    begin
      for i := CnGlobalDockPresident.DockClientsList.Count - 1 downto 0 do
        if FormName = TCustomForm(CnGlobalDockPresident.DockClientsList[i]).Name then
        begin
          Result := TCustomForm(CnGlobalDockPresident.DockClientsList[i]);
          Exit;
        end;
    end;
  end;
  Result := nil;
end;

function Cn_FindDockFormFromScreen(FormName: string;
              ScanKind: TListScanKind): TCustomForm;
var
  i: Integer;
begin
  case ScanKind of
    lskForward:
    begin
      for i := 0 to Screen.CustomFormCount - 1 do
        if FormName = Screen.CustomForms[i].Name then
        begin
          Result := Screen.CustomForms[i];
          Exit;
        end;
    end;
    lskBackward:
    begin
      for i := Screen.CustomFormCount - 1 downto 0 do
        if FormName = Screen.CustomForms[i].Name then
        begin
          Result := Screen.CustomForms[i];
          Exit;
        end;
    end;
  end;
  Result := nil;
end;

function Cn_GetMinOffset(TBDockSize, ControlSize: Integer; Scale: Real): Integer;
begin
  if (Scale < 0) or (Scale > 1) then
    Scale := 1;
  Result := Min(TBDockSize, Round(ControlSize * Scale));
end;

function Cn_GetNoNClientMetrics: TNONCLIENTMETRICS;
begin
  Result.cbSize := Sizeof(TNONCLIENTMETRICS);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, Result.cbSize,
    @Result, 0);
end;

function Cn_GetSysCaptionHeight: Integer;
begin
  Result := Cn_GetNoNClientMetrics.iCaptionHeight
end;

function Cn_GetSysBorderWidth: Integer;
begin
  Result := Cn_GetNoNClientMetrics.iBorderWidth;
end;

function Cn_GetSysCaptionHeightAndBorderWidth: Integer;
var NoNCM: TNONCLIENTMETRICS;
begin
  NoNCM := Cn_GetNoNClientMetrics;
  Result := NoNCM.iBorderWidth + NoNCM.iCaptionHeight;
end;

function Cn_GetActiveTitleBeginColor: TColor;
begin
  Result := GetSysColor(COLOR_ACTIVECAPTION);
end;

function Cn_GetActiveTitleEndColor: TColor;
begin
  Result := GetSysColor(COLOR_GRADIENTACTIVECAPTION);
end;

function Cn_GetInactiveTitleBeginColor: TColor;
begin
  Result := GetSysColor(COLOR_INACTIVECAPTION);
end;

function Cn_GetInactiveTitleEndColor: TColor;
begin
  Result := GetSysColor(COLOR_GRADIENTINACTIVECAPTION);
end;

function Cn_GetTitleFontColor(Active: Boolean): TColor;
begin
  if Active then
    Result := Cn_GetActiveTitleFontColor
  else Result := Cn_GetInactiveTitleFontColor;
end;

function Cn_GetActiveTitleFontColor: TColor;
begin
  Result := GetSysColor(COLOR_CAPTIONTEXT);
end;

function Cn_GetInactiveTitleFontColor: TColor;
begin
  Result := GetSysColor(COLOR_INACTIVECAPTIONTEXT);
end;

{ 获得标题栏的字体 }
function Cn_GetTitleFont: TFont;
var
  NoNCM: TNONCLIENTMETRICS;
begin
  Result := Cn_TitleFont;
  NoNCM := Cn_GetNoNClientMetrics;
  Result.Handle := CreateFontIndirect(NoNCM.lfCaptionFont);
end;

procedure Cn_LockWindow(Control: TWinControl);
var
  Handle: HWND;
begin
  if Control = nil then
    Handle := GetDesktopWindow
  else
    Handle := Control.Handle;
  LockWindowUpdate(Handle);
end;

procedure Cn_UnLockWindow;
begin
  LockWindowUpdate(0);
end;

function Cn_CreateNCMessage(Control: TControl; Msg: Cardinal; 
  HTFlag: Integer; Pos: TPoint): TWMNCHitMessage;
begin
  { 下面的五条语句给TWMNCHitMessage赋值 }
  Result.Msg := Msg;
  Result.HitTest := HTFlag;
  Pos := Control.ClientToScreen(Pos);
  Result.XCursor := Pos.X;
  Result.YCursor := Pos.Y;
end;

function Cn_ExchangeOrient(Orient: TDockOrientation): TDockOrientation;
begin
  case Orient of
    doHorizontal: Result := doVertical;
    doVertical: Result := doHorizontal;
  else
    Result := doNoOrient;
  end;
end;

function Cn_GetControlOrient(AControl: TControl): TDockOrientation;
begin
  Assert(AControl <> nil);
  Result := doNoOrient;
  case AControl.Align of
    alClient, alNone: Result := doNoOrient;
    alLeft, alRight:  Result := doVertical;
    alTop, alBottom:  Result := doHorizontal;
  end;
end;

function Cn_GetControlSize(AControl: TControl): Integer;
begin
  case Cn_GetControlOrient(AControl) of
    doVertical: Result := AControl.Width;
    doHorizontal: Result := AControl.Height;
  else
    raise Exception.Create(gs_CannotGetValueWithNoOrient);
  end;
end;

initialization
  Cn_TitleFont := TFont.Create;

finalization
  Cn_TitleFont.Free;

end.
