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

unit CnVclFmxMixed;
{* |<PRE>
================================================================================
* 软件名称：CnPack IDE 专家包
* 单元名称：引用了 FMX 相关功能的过程库混合单元
* 单元作者：CnPack 开发组
* 备    注：该单元从 CnCommon 分离出来，放置那些需要同时引用 VCL 与 FMX 的内容，
*           需要和 CnFmxUtils 配合使用，以减少用 CnCommon 但又不用 FMX 时的体积。
* 开发平台：WinXP + Delphi XE2
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2018.12.24 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Controls, Windows
{$IFDEF SUPPORT_FMX}
  , CnFmxUtils
{$ENDIF};

function GetControlScreenRect(AControl: TComponent): TRect;
{* 返回控件在屏幕上的坐标区域，如果是 FMX，需要返回的是相对于最外层容器的坐标。但目前实现有 Bug，只能使用相对坐标。 }

procedure SetControlScreenRect(AControl: TComponent; ARect: TRect);
{* 设置控件在屏幕上的坐标区域，如果是 FMX，需要传入的是相对于最外层容器的坐标。但目前实现有 Bug，只能使用相对坐标。 }

function GetControlParent(AControl: TComponent): TComponent;
{* 封装的获取 Conrol 的 Parent 的过程，封装了 FMX 的实现}

function GetControlTop(AControl: TComponent): Integer;
{* 封装的获取 Control 的 Top 的过程，封装了 FMX 的实现}

function GetControlLeft(AControl: TComponent): Integer;
{* 封装的获取 Control 的 Left 的过程，封装了 FMX 的实现}

function GetControlWidth(AControl: TComponent): Integer;
{* 封装的获取 Control 的 Width 的过程，封装了 FMX 的实现}

function GetControlHeight(AControl: TComponent): Integer;
{* 封装的获取 Control 的 Height 的过程，封装了 FMX 的实现}

procedure SetControlTop(AControl: TComponent; AValue: Integer);
{* 封装的设置 Control 的 Top 的过程，封装了 FMX 的实现}

procedure SetControlLeft(AControl: TComponent; AValue: Integer);
{* 封装的设置 Control 的 Left 的过程，封装了 FMX 的实现}

procedure SetControlWidth(AControl: TComponent; AValue: Integer);
{* 封装的设置 Control 的 Width 的过程，封装了 FMX 的实现}

procedure SetControlHeight(AControl: TComponent; AValue: Integer);
{* 封装的设置 Control 的 Height 的过程，封装了 FMX 的实现}

procedure ControlBringToFront(AControl: TComponent);
{* 封装的设置 Control 的 BringToFront 的过程，封装了 FMX 的实现}

procedure ControlSendToBack(AControl: TComponent);
{* 封装的设置 Control 的 SendToBack 的过程，封装了 FMX 的实现}

implementation

// 返回控件在屏幕上的坐标区域
function GetControlScreenRect(AControl: TComponent): TRect;
var
  AParent: TWinControl;
begin
  Assert(Assigned(AControl));
  if AControl is TControl then
  begin
    AParent := TControl(AControl).Parent;
    Assert(Assigned(AParent));
    with TControl(AControl) do
    begin
      Result.TopLeft := AParent.ClientToScreen(Point(Left, Top));
      Result.BottomRight := AParent.ClientToScreen(Point(Left + Width, Top + Height));
    end;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  if CnFmxIsInheritedFromControl(AControl) then
    Result := CnFmxGetControlRect(AControl);
{$ENDIF}
end;

// 设置控件在屏幕上的坐标区域
procedure SetControlScreenRect(AControl: TComponent; ARect: TRect);
var
  AParent: TWinControl;
  P1, P2: TPoint;
begin
  Assert(Assigned(AControl));
  if AControl is TControl then
  begin
    AParent := TControl(AControl).Parent;
    Assert(Assigned(AParent));
    P1 := AParent.ScreenToClient(ARect.TopLeft);
    P2 := AParent.ScreenToClient(ARect.BottomRight);
    TControl(AControl).SetBounds(P1.x, P1.y, P2.x - P1.x, P2.y - P1.y);
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  if CnFmxIsInheritedFromControl(AControl) then
    CnFmxSetControlRect(AControl, ARect);
{$ENDIF}
end;

// 封装的获取 Conrol 的 Parent 的过程，封装了 FMX 的实现
function GetControlParent(AControl: TComponent): TComponent;
begin
  if AControl is TControl then
  begin
    Result := TControl(AControl).Parent;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  Result := CnFmxGetControlParent(AControl);
{$ELSE}
  Result := nil;
{$ENDIF}
end;

// 封装的获取 Control 的 Top 的过程，封装了 FMX 的实现
function GetControlTop(AControl: TComponent): Integer;
begin
  if AControl is TControl then
  begin
    Result := TControl(AControl).Top;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  Result := CnFmxGetControlPositionValue(AControl, fptTop);
{$ELSE}
  Result := -1;
{$ENDIF}
end;

// 封装的获取 Control 的 Left 的过程，封装了 FMX 的实现
function GetControlLeft(AControl: TComponent): Integer;
begin
  if AControl is TControl then
  begin
    Result := TControl(AControl).Left;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  Result := CnFmxGetControlPositionValue(AControl, fptLeft);
{$ELSE}
  Result := -1;
{$ENDIF}
end;

// 封装的获取 Control 的 Width 的过程，封装了 FMX 的实现}
function GetControlWidth(AControl: TComponent): Integer;
begin
  if AControl is TControl then
  begin
    Result := TControl(AControl).Width;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  Result := CnFmxGetControlPositionValue(AControl, fptWidth);
{$ELSE}
  Result := -1;
{$ENDIF}
end;

// 封装的获取 Control 的 Height 的过程，封装了 FMX 的实现}
function GetControlHeight(AControl: TComponent): Integer;
begin
  if AControl is TControl then
  begin
    Result := TControl(AControl).Height;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  Result := CnFmxGetControlPositionValue(AControl, fptHeight);
{$ELSE}
  Result := -1;
{$ENDIF}
end;

// 封装的设置 Control 的 Top 的过程，封装了 FMX 的实现}
procedure SetControlTop(AControl: TComponent; AValue: Integer);
begin
  if AControl is TControl then
  begin
    TControl(AControl).Top := AValue;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  CnFmxSetControlPositionValue(AControl, AValue, fptTop);
{$ENDIF}
end;

{* 封装的设置 Control 的 Left 的过程，封装了 FMX 的实现}
procedure SetControlLeft(AControl: TComponent; AValue: Integer);
begin
  if AControl is TControl then
  begin
    TControl(AControl).Left := AValue;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  CnFmxSetControlPositionValue(AControl, AValue, fptLeft);
{$ENDIF}
end;

{* 封装的设置 Control 的 Width 的过程，封装了 FMX 的实现}
procedure SetControlWidth(AControl: TComponent; AValue: Integer);
begin
  if AControl is TControl then
  begin
    TControl(AControl).Width := AValue;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  CnFmxSetControlPositionValue(AControl, AValue, fptWidth);
{$ENDIF}
end;

{* 封装的设置 Control 的 Height 的过程，封装了 FMX 的实现}
procedure SetControlHeight(AControl: TComponent; AValue: Integer);
begin
  if AControl is TControl then
  begin
    TControl(AControl).Height := AValue;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  CnFmxSetControlPositionValue(AControl, AValue, fptHeight);
{$ENDIF}
end;

// 封装的设置 Control 的 BringToFront 的过程，封装了 FMX 的实现
procedure ControlBringToFront(AControl: TComponent);
begin
  if AControl is TControl then
  begin
    TControl(AControl).BringToFront;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  CnFmxControlBringToFront(AControl);
{$ENDIF}
end;

// 封装的设置 Control 的 SendToBack 的过程，封装了 FMX 的实现
procedure ControlSendToBack(AControl: TComponent);
begin
  if AControl is TControl then
  begin
    TControl(AControl).SendToBack;
    Exit;
  end;
{$IFDEF SUPPORT_FMX}
  CnFmxControlSendToBack(AControl);
{$ENDIF}
end;

end.
