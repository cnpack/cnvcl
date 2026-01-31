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

unit CnFmxUtils;
{* |<PRE>
================================================================================
* 软件名称：CnPack IDE 专家包
* 单元名称：FMX 相关的过程库单元
* 单元作者：CnPack 开发组
* 备    注：该单元定义了 XE2 或以上版本中与 FMX 相关的一些内容。
*           本单元不使用 VCL 的 TControl 框架，只使用 FMX 的。
*           其他单元均只使用 VCL 的 TControl 框架，如需进行 FMX 的相关操作，
*           便需要跳入此单元，以实现俩框架隔离的目的。
*
*           注意，由于 FMX 框架一直在升级，部分兼容代码较为复杂，比如字体相关：
*
*   XE2/3，继承于 FMX.Controls 里的 TTextControl 的组件有 Font属性，没 ParentFont 控制。
*   XE4/5，继承于 FMX.Controls 里的 TTextControl 的组件有 Font属性，有 StyledSettings 可模拟 ParentFont，StyledSettings 类型 ss 开头
*   XE6/7，继承于 FMX.Controls 里的 TTextControl 的组件有 TextSettings.Font 属性，有 StyledSettings 可模拟 ParentFont，StyledSettings 类型不用 ss 开头
*   XE8/~，继承于 FMX.StdCtrls 里的 TPresentedTextControl 的组件有 TextSettings.Font 属性，有 StyledSettings 可模拟 ParentFont，之后走上正规没变了。
*
* 开发平台：WinXP + Delphi XE2
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2011.10.02 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, {$IFDEF FMX_HAS_GRAPHICS} FMX.Graphics, {$ENDIF} FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.Grid {$IFDEF DELPHIXE8_UP}, FMX.StdCtrls {$ENDIF}
  {$IFDEF MSWINDOWS}, FMX.Platform.Win {$ENDIF} {$IFDEF MACOS}, FMX.Platform.Mac {$ENDIF};

type
  TCnFmxPosType = (fptLeft, fptTop, fptRight, fptBottom, fptWidth, fptHeight);

function CnFmxGetObjectParent(AObject: TComponent): TComponent;
{* 获取一个 FMX 的 Component 的 Parent。如果 AObject 并非 TFmxObject 的子类，返回 nil}

function CnFmxGetControlParent(AControl: TComponent): TComponent;
{* 获取一个 FMX 的 Control 的 Parent。如果 AControl 并非 FMX.TControl 的子类，返回 nil}

function CnFmxGetChildrenCount(AComp: TComponent): Integer;
{* 获取一个 FmxObject 的 Child 数量。如果 AComp 并非 FmxObject 的子类，返回 -1}

function CnFmxGetChildByIndex(AComp: TComponent; Index: Integer): TComponent;
{* 获取一个 FmxObject 的第 Index 个 Child。如果 AComp 并非 FmxObject 的子类，返回 nil}

function CnFmxGetControlsCount(AControl: TComponent): Integer;
{* 获取一个 FMX 的 Control 的子 Control 数量。如果 AControl 并非 FMX.TControl 的子类，返回 -1}

function CnFmxGetControlByIndex(AControl: TComponent; Index: Integer): TComponent;
{* 获取一个 FMX 的 Control 的第 Index 个子 Control。如果 AControl 并非 FMX.TControl 的子类，返回 nil}

function CnFmxIsInheritedFromClassByName(AObject: TObject; AClassName: string): Boolean;
{* 获取一个 Object 是否继承自 AClassName 名字标识的 FMX 类}

function CnFmxIsInheritedFromControl(AObject: TObject): Boolean;
{* 获取一个 Object 是否继承自 FMX.TControl 类}

function CnFmxClassIsInheritedFromControl(AClass: TClass): Boolean;
{* 获取一个 Class 是否继承自 FMX.TControl 类}

function CnFmxIsInheritedFromForm(AObject: TObject): Boolean;
{* 获取一个 Object 是否继承自 FMX.TForm 类}

function CnFmxClassIsInheritedFromForm(AClass: TClass): Boolean;
{* 获取一个 Class 是否继承自 FMX.TForm 类}

function CnFmxIsInheritedFromCommonCustomForm(AObject: TObject): Boolean;
{* 获取一个 Object 是否继承自 FMX.TCommonCustomForm 类}

function CnFmxIsInheritedFromFrame(AObject: TObject): Boolean;
{* 获取一个 Object 是否继承自 FMX.TFrame 类}

function CnFmxGetControlRect(AControl: TComponent): TRect;
{* 获取一个 FMX 的 Control 的基于其 Parent 的 Rect，内部手工计算}

procedure CnFmxSetControlRect(AControl: TComponent; ARect: TRect);
{* 设置一个 FMX 的 Control 的基于其 Parent 的 Rect，内部手工计算}

function CnFmxGetControlScreenRect(AControl: TComponent): TRect;
{* 获取一个 FMX 的 Control 的基于屏幕坐标的 Rect，内部手工计算}

procedure CnFmxSetControlScreenRect(AControl: TComponent; ARect: TRect);
{* 设置一个 FMX 的 Control 的基于屏幕坐标的 Rect，内部手工计算}

function CnFmxGetControlPositionValue(AControl: TComponent;
  PosType: TCnFmxPosType): Integer;
{* 获取一个 FMX 的 Control 的位置、尺寸等，所需内容由 PosType 参数指定}

procedure CnFmxSetControlPositionValue(AControl: TComponent; AValue: Single;
  PosType: TCnFmxPosType);
{* 设置一个 FMX 的 Control 的位置、尺寸等，所设置的内容由 PosType 参数指定}

procedure CnFmxControlBringToFront(AControl: TComponent);
{* 将一个 FMX 的 Control 提至前面}

procedure CnFmxControlSendToBack(AControl: TComponent);
{* 将一个 FMX 的 Control 放至后面}

procedure CnFmxGetFormClientSize(AForm: TComponent; out AClientWidth, AClientHeight: Integer);
{* 获取一个 FMX 的 Form 的 ClientWidth, ClientHeight}

function CnFmxGetCommonCustomFormCaption(AForm: TComponent): string;
{* 获取一个 FMX 的 Form 的标题文字}

function CnFmxFixSetValue(const PType: string; const PValue: string): string;
{* 为高版本语法的 set 赋值增加类名，如 [seTop] 变成 [TSide.seTop]}

function CnInputQuery(const ACaption, APrompt: string;
  var Value: string): Boolean;
{* FMX 版本的 InputQuery，直接调用 FMX 自带的}

procedure CnFmxSetStringGridColumnCount(Grid: TStringGrid; ColCount: Integer;
  ColWidth: Integer = 64);
{* 设置一 FMX 的 StringGrid 的列数功能的封装，内部需要增删 StringColumn
  如增，需要外部指定宽度 ColWidth，默认 64}

procedure CnFmxMoveSubControl(FromControl, ToControl: TComponent);
{* 将 FromControl 的所有子 Control 按顺序移动至 ToControl 下}

function CnFmxGetControlPosition(AControl: TComponent): TSmallPoint;
{* 返回 FMX Control 的位置}

procedure CnFmxGetScreenFormsWithName(const Name: string; OutForms: TList);
{* 遍历 Screen 的所有 Form 实例，将名称等于该名字的实例加入列表，名字为空时全加}

procedure CnFmxGetScreenFormsWithClassName(const ClsName: string; OutForms: TList);
{* 遍历 Screen 的所有 Form 实例，将类名等于该名字的实例加入列表}

function CnFmxGetFmxApplication: TComponent;
{* 返回 FMX 框架里的 Application 实例}

function CnFmxGetWindowPlatformHandle(AComp: TComponent): THandle;
{* 返回 FMX 框架里的封装的 Window Handle，如 AComp 不是相关类则返回 0}

function CnFmxGetControlVisible(AControl: TComponent): Boolean;
{* 返回 FMX Control 是否可见，如非 Control，返回 False}

procedure CnFmxSetControlVisible(AControl: TComponent; AVisible: Boolean);
{* 设置 FMX Control 是否可见，如非 Control 则什么也不做}

procedure CnFmxInvalidateControl(AControl: TComponent);
{* 让 FMX Control 重画，如非 Control 则什么也不做}

procedure CnFmxControlCanvasFillRect(AControl: TComponent; ARect: TRect; AColor: TColor);
{* 以指定矩形和颜色来填充 Control 的画布，如非 Control 则什么也不做}

function CnFmxGetScreenFormCount: Integer;
{* 获取 Screen 的 FormCount 数}

function CnFmxGetScreenForms(Index: Integer): TCommonCustomForm;
{* 获取 Screen 的第 Index 个 Form}

function CnFmxGetIsParentFont(AControl: TComponent): Boolean;
{* 获取 FMX Control 是否 ParentFont，封装 StyledSettings 的处理，
  注意不支持文字会返回 True，表示自身不处理}

procedure CnFmxSetIsParentFont(AControl: TComponent; AParentFont: Boolean);
{* 设置 FMX Control 是否 ParentFont，封装 StyledSettings 的处理}

function CnFmxGetControlFont(AControl: TComponent): TFont;
{* 获取 FMX Control 的 Font}

procedure CnFmxSetControlFont(AControl: TComponent; AFont: TFont);
{* 设置 FMX Control 的 Font}

function CnFmxGetControlParentFont(AControl: TComponent): TFont;
{* 获取 FMX Control 的 Parent 的 Font}

implementation

const
  CN_FMX_FIX_SET_COUNT = 10;
  CnFmxFixSetTypeArray: array[0..CN_FMX_FIX_SET_COUNT - 1] of string = (
    'TCorners', 'TSides', 'TStyledSettings', 'TInteractiveGestureFlags',
    'TFillTextFlags', 'TStandardGestures', 'TInteractiveGestures',
    'TGestureTypes', 'TGestureOptions', 'TGestureEngineFlags'
    );

  CnFmxFixEnumTypeArray: array[0..CN_FMX_FIX_SET_COUNT - 1] of string = (
    'TCorner', 'TSide', 'TStyledSetting', 'TInteractiveGestureFlag',
    'TFillTextFlag', 'TStandardGesture', 'TInteractiveGesture', 'TGestureType',
    'TGestureOption', 'TGestureEngineFlag'
    );

var
  FCnFmxFixEnumNameArray: array[0..CN_FMX_FIX_SET_COUNT - 1] of TStrings;

function CnFmxGetObjectParent(AObject: TComponent): TComponent;
begin
  if AObject.InheritsFrom(TFmxObject) then
    Result := TFmxObject(AObject).Parent
  else
    Result := nil;
end;

function CnFmxGetControlParent(AControl: TComponent): TComponent;
begin
  if AControl.InheritsFrom(TControl) then
    Result := TControl(AControl).Parent
  else
    Result := nil;
end;

function CnFmxGetChildrenCount(AComp: TComponent): Integer;
begin
  if (AComp = nil) or not (AComp is TFmxObject) then
    Result := -1
  else
    Result := TFmxObject(AComp).ChildrenCount;
end;

function CnFmxGetChildByIndex(AComp: TComponent; Index: Integer): TComponent;
begin
  if (AComp = nil) or not (AComp is TFmxObject) then
    Result := nil
  else
    Result := TFmxObject(AComp).Children[Index];
end;

function CnFmxGetControlsCount(AControl: TComponent): Integer;
begin
  if (AControl = nil) or not (AControl is TControl) then
    Result := -1
  else
  begin
{$IFDEF DELPHIXE3_UP}
    Result := TControl(AControl).ControlsCount;
{$ELSE}
    Result := TControl(AControl).ChildrenCount;
{$ENDIF}
  end;
end;

function CnFmxGetControlByIndex(AControl: TComponent; Index: Integer): TComponent;
begin
  if (AControl = nil) or not (AControl is TControl) then
    Result := nil
  else
  begin
{$IFDEF DELPHIXE3_UP}
    Result := TControl(AControl).Controls[Index];
{$ELSE}
    Result := TControl(AControl).Children[Index];
{$ENDIF}
  end;
end;

function CnFmxIsInheritedFromClassByName(AObject: TObject; AClassName: string): Boolean;
var
  AClass: TPersistentClass;
begin
  Result := False;
  AClass := GetClass(AClassName);
  if AClass = nil then
    Exit;

  Result := AObject.InheritsFrom(AClass);
end;

function CnFmxIsInheritedFromControl(AObject: TObject): Boolean;
begin
  Result := AObject.InheritsFrom(TControl);
end;

function CnFmxClassIsInheritedFromControl(AClass: TClass): Boolean;
begin
  Result := AClass.InheritsFrom(TControl);
end;

function CnFmxIsInheritedFromForm(AObject: TObject): Boolean;
begin
  Result := AObject.InheritsFrom(FMX.Forms.TForm);
end;

function CnFmxClassIsInheritedFromForm(AClass: TClass): Boolean;
begin
  Result := AClass.InheritsFrom(FMX.Forms.TForm);
end;

function CnFmxIsInheritedFromCommonCustomForm(AObject: TObject): Boolean;
begin
  Result := AObject.InheritsFrom(FMX.Forms.TCommonCustomForm);
end;

function CnFmxIsInheritedFromFrame(AObject: TObject): Boolean;
begin
{$IFDEF SUPPORT_FMX_FRAME}
  Result := AObject.InheritsFrom(FMX.Forms.TFrame);
{$ELSE}
  Result := False;
{$ENDIF}
end;

function CnFmxGetControlRect(AControl: TComponent): TRect;
var
  P: TPointF;
  AParent: TFmxObject;
begin
  // Local 与 Absolute 坐标的转换会出 AV，因此没法支持，暂时全使用相对坐标
  // 也就是说 Rect 是基于 Parent 下的而非屏幕的
  if (AControl <> nil) and AControl.InheritsFrom(TControl) then
  begin
    AParent := TControl(AControl).Parent;
    if (AParent <> nil)
      and (AParent.InheritsFrom(TControl) or AParent.InheritsFrom(TForm)) then
    begin
      P.X := TControl(AControl).Position.X;
      P.Y := TControl(AControl).Position.Y;
      // P := TControl(AParent).LocalToAbsolute(P);
      Result.Left := Trunc(P.X);
      Result.Top := Trunc(P.Y);

      P.X := TControl(AControl).Position.X + TControl(AControl).Width;
      P.Y := TControl(AControl).Position.Y + TControl(AControl).Height;
      // P := TControl(AParent).LocalToAbsolute(P);
      Result.Right := Trunc(P.X);
      Result.Bottom := Trunc(P.Y);
    end;
  end;
end;

procedure CnFmxSetControlRect(AControl: TComponent; ARect: TRect);
var
  P1, P2: TPointF;
  AParent: TFmxObject;
begin
  if (AControl <> nil) and AControl.InheritsFrom(TControl) then
  begin
    AParent := TControl(AControl).Parent;
    if (AParent <> nil)
      and (AParent.InheritsFrom(TControl) or AParent.InheritsFrom(TForm)) then
    begin
      P1.X := ARect.Left;
      P1.Y := ARect.Top;
      P2.X := ARect.Right;
      P2.Y := ARect.Bottom;
      // P1 := TControl(AParent).AbsoluteToLocal(P1);
      // P2 := TControl(AParent).AbsoluteToLocal(P2);
      TControl(AControl).SetBounds(P1.X, P1.Y, P2.X - P1.X, P2.Y - P1.Y);
    end;
  end;
end;

function CnFmxGetControlScreenRect(AControl: TComponent): TRect;
var
  AParent: TFmxObject;
  R, PF: TPointF;
begin
  if (AControl <> nil) and (AControl.InheritsFrom(TControl)) then
  begin
    R.X := TControl(AControl).Position.X;
    R.Y := TControl(AControl).Position.Y;

    // 循环找到最顶层的相对坐标
    AParent := TControl(AControl).Parent;
    while (AParent <> nil) and (AParent is TControl) and not (AParent is TCommonCustomForm) do
    begin
      R.X := R.X + TControl(AParent).Position.X;
      R.Y := R.Y + TControl(AParent).Position.Y;
      AParent := TControl(AParent).Parent;
    end;

    // 再加上最顶层的左上角坐标，变成屏幕坐标
    if (AParent <> nil) and (AParent is TCommonCustomForm) then
    begin
      PF.X := 0;
      PF.Y := 0;
      PF := TCommonCustomForm(AParent).ClientToScreen(PF);

      R.X := R.X + PF.X;
      R.Y := R.Y + PF.Y;
    end;

    Result.Left := Trunc(R.X);
    Result.Top := Trunc(R.Y);
{$IFDEF FMX_CONTROL_HAS_SIZE}
    Result.Right := Result.Left + Trunc(TControl(AControl).Size.Width);
    Result.Bottom := Result.Top + Trunc(TControl(AControl).Size.Height);
{$ELSE}
    Result.Right := Result.Left + Trunc(TControl(AControl).Width);
    Result.Bottom := Result.Top + Trunc(TControl(AControl).Height);
{$ENDIF}
  end;
end;

procedure CnFmxSetControlScreenRect(AControl: TComponent; ARect: TRect);
var
  AParent: TFmxObject;
  R, PF: TPointF;
  BRect: TRect;
begin
  if (AControl <> nil) and (AControl.InheritsFrom(TControl)) then
  begin
    R.X := 0; // 该 Control 在 Parent 左上角时的相对坐标
    R.Y := 0;

    // 循环找到最顶层的相对坐标
    AParent := TControl(AControl).Parent;
    while (AParent <> nil) and (AParent is TControl) and not (AParent is TCommonCustomForm) do
    begin
      R.X := R.X + TControl(AParent).Position.X;
      R.Y := R.Y + TControl(AParent).Position.Y;
      AParent := TControl(AParent).Parent;
    end;

    // 再加上最顶层的左上角坐标，变成屏幕坐标
    if (AParent <> nil) and (AParent is TCommonCustomForm) then
    begin
      PF.X := 0;
      PF.Y := 0;
      PF := TCommonCustomForm(AParent).ClientToScreen(PF);

      R.X := R.X + PF.X;
      R.Y := R.Y + PF.Y;
    end;

    // 基于屏幕的 Rect，都减去 Control 的 Parent 的左上角在屏幕上的坐标，
    // 就变成了基于 Control 的 Parent 的坐标

{$IFDEF IDE_SUPPORT_HDPI}
    BRect.Left := Round(ARect.Left - R.X);    // 不知道咋的要这么写
    BRect.Top := Round(ARect.Top - R.Y);
    BRect.Right := Round(ARect.Right - R.X);
    BRect.Bottom := Round(ARect.Bottom - R.Y);
{$ELSE}
    BRect.Left := Trunc(ARect.Left - R.X);
    BRect.Top := Trunc(ARect.Top - R.Y);
    BRect.Right := Trunc(ARect.Right - R.X);
    BRect.Bottom := Trunc(ARect.Bottom - R.Y);
{$ENDIF}

    TControl(AControl).SetBounds(BRect.Left, BRect.Top, BRect.Width, BRect.Height);
  end;
end;

function CnFmxGetControlPositionValue(AControl: TComponent;
  PosType: TCnFmxPosType): Integer;
begin
  Result := -1;
  if AControl <> nil then
  begin
    if AControl.InheritsFrom(TControl) then
    begin
      case PosType of
        fptLeft:
          Result := Trunc(TControl(AControl).Position.X);
        fptTop:
          Result := Trunc(TControl(AControl).Position.Y);
        fptRight:
          Result := Trunc(TControl(AControl).Position.X + TControl(AControl).Width);
        fptBottom:
          Result := Trunc(TControl(AControl).Position.Y + TControl(AControl).Height);
        fptWidth:
          Result := Trunc(TControl(AControl).Width);
        fptHeight:
          Result := Trunc(TControl(AControl).Height);
      end;
    end
    else if AControl.InheritsFrom(TCustomForm) then
    begin
      case PosType of
        fptLeft:
          Result := Trunc(TCustomForm(AControl).Left);
        fptTop:
          Result := Trunc(TCustomForm(AControl).Top);
        fptRight:
          Result := Trunc(TCustomForm(AControl).Left + TControl(AControl).Width);
        fptBottom:
          Result := Trunc(TCustomForm(AControl).Top + TControl(AControl).Height);
        fptWidth:
          Result := Trunc(TCustomForm(AControl).Width);
        fptHeight:
          Result := Trunc(TCustomForm(AControl).Height);
      end;
    end;
  end;
end;

procedure CnFmxSetControlPositionValue(AControl: TComponent; AValue: Single;
  PosType: TCnFmxPosType);
begin
  if AControl <> nil then
  begin
    if AControl.InheritsFrom(TControl) then
    begin
      case PosType of
        fptLeft:
          TControl(AControl).Position.X := Trunc(AValue);
        fptTop:
          TControl(AControl).Position.Y := Trunc(AValue);
        fptRight:
          TControl(AControl).Width := Trunc(AValue - TControl(AControl).Position.X);
        fptBottom:
          TControl(AControl).Height := Trunc(AValue - TControl(AControl).Position.Y);
        fptWidth:
          TControl(AControl).Width := Trunc(AValue);
        fptHeight:
          TControl(AControl).Height := Trunc(AValue);
      end;
    end
    else if AControl.InheritsFrom(TCustomForm) then
    begin
      case PosType of
        fptLeft:
          TCustomForm(AControl).Left := Trunc(AValue);
        fptTop:
          TCustomForm(AControl).Top := Trunc(AValue);
        fptRight:
          TCustomForm(AControl).Width := Trunc(AValue - TCustomForm(AControl).Left);
        fptBottom:
          TCustomForm(AControl).Height := Trunc(AValue - TCustomForm(AControl).Top);
        fptWidth:
          TCustomForm(AControl).Width := Trunc(AValue);
        fptHeight:
          TCustomForm(AControl).Height := Trunc(AValue);
      end;
    end;
  end;
end;

procedure CnFmxControlBringToFront(AControl: TComponent);
begin
  if (AControl <> nil) and AControl.InheritsFrom(TFmxObject) then
    TFmxObject(AControl).BringToFront;
end;

procedure CnFmxControlSendToBack(AControl: TComponent);
begin
  if (AControl <> nil) and AControl.InheritsFrom(TFmxObject) then
    TFmxObject(AControl).SendToBack;
end;

procedure CnFmxGetFormClientSize(AForm: TComponent; out AClientWidth, AClientHeight: Integer);
begin
  if (AForm <> nil) and CnFmxIsInheritedFromCommonCustomForm(AForm) then
  begin
    AClientWidth := FMX.Forms.TCommonCustomForm(AForm).ClientWidth;
    AClientHeight := FMX.Forms.TCommonCustomForm(AForm).ClientHeight;
  end;
end;

function CnFmxGetCommonCustomFormCaption(AForm: TComponent): string;
begin
  Result := '';
  if (AForm <> nil) and CnFmxIsInheritedFromCommonCustomForm(AForm) then
    Result := FMX.Forms.TCommonCustomForm(AForm).Caption;
end;

function CnFmxFixSetValue(const PType: string; const PValue: string): string;
var
  I, Idx: Integer;
begin
  Result := PValue;
  if (PType = '') or (PValue = '') then
    Exit
  else if Length(PValue) <= 2 then
    Exit
  else if PValue[1] <> '['  then
    Exit
  else
  begin
    Idx := -1;
    for I := Low(CnFmxFixSetTypeArray) to High(CnFmxFixSetTypeArray) do
    begin
      if PType = CnFmxFixSetTypeArray[I] then
      begin
        Idx := I;
        Break;
      end;
    end;

    if Idx >= 0 then
    begin
      for I := 0 to FCnFmxFixEnumNameArray[Idx].Count - 1 do
      begin
        Result := StringReplace(Result, FCnFmxFixEnumNameArray[Idx][I],
          CnFmxFixEnumTypeArray[Idx] + '.' + FCnFmxFixEnumNameArray[Idx][I],
          [rfReplaceAll]);
      end;
    end;
  end;
end;

function CnInputQuery(const ACaption, APrompt: string;
  var Value: string): Boolean;
begin
  Result := InputQuery(ACaption, APrompt, Value);
end;

procedure CnFmxSetStringGridColumnCount(Grid: TStringGrid; ColCount: Integer;
  ColWidth: Integer);
var
  I: Integer;
  Column: TStringColumn;
begin
  if (Grid = nil) or (ColCount < 0) then
    Exit;

  if Grid.ColumnCount > ColCount then
  begin
    for I := 1 to Grid.ColumnCount - ColCount do
      Grid.Columns[Grid.ColumnCount - 1].Free;
  end
  else if Grid.ColumnCount < ColCount then
  begin
    for I := 1 to ColCount - Grid.ColumnCount do
    begin
      Column := TStringColumn.Create(Grid);
      Column.Width := ColWidth;
      Column.Parent := Grid;
    end;
  end;
end;

procedure CnFmxMoveSubControl(FromControl, ToControl: TComponent);
var
  I, C: Integer;
  FromCtl, ToCtl, Ctl: TControl;
begin
  if (FromControl = nil) or (ToControl = nil) then
    Exit;

  if not FromControl.InheritsFrom(TControl) or not ToControl.InheritsFrom(TControl) then
    Exit;

  FromCtl := TControl(FromControl);
  ToCtl := TControl(ToControl);

  C := CnFmxGetControlsCount(FromCtl);
  for I := 0 to C - 1 do
  begin
    Ctl := TControl(CnFmxGetControlByIndex(FromCtl, 0));
    Ctl.Parent := ToCtl;
  end;
end;

function CnFmxGetControlPosition(AControl: TComponent): TSmallPoint;
begin
  Result.x := 0;
  Result.y := 0;

  if AControl = nil then
    Exit;

  if AControl.InheritsFrom(TControl) then
  begin
    Result.x := Trunc(TControl(AControl).Position.X);
    Result.y := Trunc(TControl(AControl).Position.Y);
  end;
end;

procedure CnFmxGetScreenFormsWithName(const Name: string; OutForms: TList);
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
  begin
    if (Name = '') or (Screen.Forms[I].Name = Name) then
      OutForms.Add(Screen.Forms[I]);
  end;
end;

procedure CnFmxGetScreenFormsWithClassName(const ClsName: string; OutForms: TList);
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I].ClassNameIs(ClsName) then
      OutForms.Add(Screen.Forms[I]);
  end;
end;

function CnFmxGetFmxApplication: TComponent;
begin
  Result := Application;
end;

function CnFmxGetWindowPlatformHandle(AComp: TComponent): THandle;
begin
  if AComp is TCommonCustomForm then
  begin
{$IFDEF DELPHIXE4_UP}
  {$IFDEF MACOS}
    Result := THandle(WindowHandleToPlatform(TCommonCustomForm(AComp).Handle).Wnd);
  {$ELSE}
    Result := WindowHandleToPlatform(TCommonCustomForm(AComp).Handle).Wnd;
  {$ENDIF}
{$ELSE}
    Result := FmxHandleToHWND(TCommonCustomForm(AComp).Handle);
{$ENDIF}
  end
  else
    Result := 0;
end;

function CnFmxGetControlVisible(AControl: TComponent): Boolean;
begin
  if AControl.InheritsFrom(TControl) then
    Result := TControl(AControl).Visible
  else
    Result := False;
end;

procedure CnFmxSetControlVisible(AControl: TComponent; AVisible: Boolean);
begin
  if AControl.InheritsFrom(TControl) then
    TControl(AControl).Visible := AVisible;
end;

procedure CnFmxInvalidateControl(AControl: TComponent);
begin
  if AControl.InheritsFrom(TControl) then
    TControl(AControl).Repaint;
end;

procedure CnFmxControlCanvasFillRect(AControl: TComponent; ARect: TRect; AColor: TColor);
var
  R: TRectF;
  C: TCanvas;
  OldColor: TAlphaColor;
  OldStyle: TBrushKind;
begin
  if AControl.InheritsFrom(TControl) then
  begin
    C := TControl(AControl).Canvas;
    OldColor := C.Fill.Color;
    OldStyle := C.Fill.Kind;
    try
{$IFNDEF DELPHIXE6_UP}
      C.Fill.Kind := TBrushKind.bkSolid;
{$ELSE}
      C.Fill.Kind := TBrushKind.Solid;
{$ENDIF}
      C.Fill.Color := AColor;

      R.Top := ARect.Top;
      R.Left := ARect.Left;
      R.Bottom := ARect.Bottom;
      R.Right := ARect.Right;
      C.FillRect(R, 0, 0, [], 1);
    finally
      C.Fill.Color := OldColor;
      C.Fill.Kind := OldStyle;
    end;
  end;
end;

function CnFmxGetScreenFormCount: Integer;
begin
  Result := Screen.FormCount;
end;

function CnFmxGetScreenForms(Index: Integer): TCommonCustomForm;
begin
  Result := Screen.Forms[Index];
end;

type
{$IFDEF DELPHIXE8_UP}
  TCnFmxControlHasFont = TPresentedTextControl;
{$ELSE}
  TCnFmxControlHasFont = TTextControl;
{$ENDIF}

function CnFmxGetIsParentFont(AControl: TComponent): Boolean;
begin
  if AControl.InheritsFrom(TCnFmxControlHasFont) then
  begin
{$IFDEF DELPHIXE4_UP}
  {$IFDEF DELPHIXE6_UP}
    Result := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style,
      TStyledSetting.FontColor] >= TCnFmxControlHasFont(AControl).StyledSettings;
  {$ELSE}
    Result := [TStyledSetting.ssFamily, TStyledSetting.ssSize, TStyledSetting.ssStyle,
      TStyledSetting.ssFontColor] >= TCnFmxControlHasFont(AControl).StyledSettings;
  {$ENDIF}
{$ELSE}
    Result := True;
{$ENDIF}
  end
  else
    Result := True;
end;

procedure CnFmxSetIsParentFont(AControl: TComponent; AParentFont: Boolean);
begin
  if AControl.InheritsFrom(TCnFmxControlHasFont) then
  begin
{$IFDEF DELPHIXE4_UP}
  {$IFDEF DELPHIXE6_UP}
    TCnFmxControlHasFont(AControl).StyledSettings := TCnFmxControlHasFont(AControl).StyledSettings +
     [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style, TStyledSetting.FontColor];
  {$ELSE}
    TCnFmxControlHasFont(AControl).StyledSettings := TCnFmxControlHasFont(AControl).StyledSettings +
     [TStyledSetting.ssFamily, TStyledSetting.ssSize, TStyledSetting.ssStyle, TStyledSetting.ssFontColor];
  {$ENDIF}
{$ENDIF}
  end;
end;

function CnFmxGetControlFont(AControl: TComponent): TFont;
begin
  if AControl.InheritsFrom(TCnFmxControlHasFont) then
  begin
{$IFDEF DELPHIXE6_UP}
    Result := TCnFmxControlHasFont(AControl).TextSettings.Font;
{$ELSE}
    Result := TCnFmxControlHasFont(AControl).Font;
{$ENDIF}
  end
  else
    Result := nil;
end;

procedure CnFmxSetControlFont(AControl: TComponent; AFont: TFont);
begin
  if AControl.InheritsFrom(TCnFmxControlHasFont) then
  begin
{$IFDEF DELPHIXE6_UP}
    TCnFmxControlHasFont(AControl).TextSettings.Font := AFont;
{$ELSE}
    TCnFmxControlHasFont(AControl).Font := AFont;
{$ENDIF}
  end;
end;

function CnFmxGetControlParentFont(AControl: TComponent): TFont;
var
  P: TFmxObject;
begin
  Result := nil;
  if AControl.InheritsFrom(TControl) then
  begin
    P := TControl(AControl).Parent;
    if (P <> nil) and P.InheritsFrom(TCnFmxControlHasFont) then
    begin
{$IFDEF DELPHIXE6_UP}
      Result := TCnFmxControlHasFont(P).TextSettings.Font;
{$ELSE}
      Result := TCnFmxControlHasFont(P).Font;
{$ENDIF}
    end;
  end;
end;

procedure CreateFmxSetFixArray;
begin
  // TCorner
  FCnFmxFixEnumNameArray[0] := TStringList.Create();
  FCnFmxFixEnumNameArray[0].Add('crTopLeft');
  FCnFmxFixEnumNameArray[0].Add('crTopRight');
  FCnFmxFixEnumNameArray[0].Add('crBottomLeft');
  FCnFmxFixEnumNameArray[0].Add('crBottomRight');

  // TSide
  FCnFmxFixEnumNameArray[1] := TStringList.Create();
  FCnFmxFixEnumNameArray[1].Add('sdTop');
  FCnFmxFixEnumNameArray[1].Add('sdLeft');
  FCnFmxFixEnumNameArray[1].Add('sdBottom');
  FCnFmxFixEnumNameArray[1].Add('sdRight');

  // TStyledSetting
  FCnFmxFixEnumNameArray[2] := TStringList.Create();
  FCnFmxFixEnumNameArray[2].Add('ssFamily');
  FCnFmxFixEnumNameArray[2].Add('ssSize');
  FCnFmxFixEnumNameArray[2].Add('ssStyle');
  FCnFmxFixEnumNameArray[2].Add('ssFontColor');
  FCnFmxFixEnumNameArray[2].Add('ssOther');

  // TInteractiveGestureFlag
  FCnFmxFixEnumNameArray[3] := TStringList.Create();
  FCnFmxFixEnumNameArray[3].Add('gfBegin');
  FCnFmxFixEnumNameArray[3].Add('gfInertia');
  FCnFmxFixEnumNameArray[3].Add('gfEnd');

  // TFillTextFlag
  FCnFmxFixEnumNameArray[4] := TStringList.Create();
  FCnFmxFixEnumNameArray[4].Add('ftRightToLeft');

  // TStandardGesture
  FCnFmxFixEnumNameArray[5] := TStringList.Create();
  FCnFmxFixEnumNameArray[5].Add('sgLeft');
  FCnFmxFixEnumNameArray[5].Add('sgRight');
  FCnFmxFixEnumNameArray[5].Add('sgUp');
  FCnFmxFixEnumNameArray[5].Add('sgDown');
  FCnFmxFixEnumNameArray[5].Add('sgUpLeft');
  FCnFmxFixEnumNameArray[5].Add('sgUpRight');
  FCnFmxFixEnumNameArray[5].Add('sgDownLeft');
  FCnFmxFixEnumNameArray[5].Add('sgDownRight');
  FCnFmxFixEnumNameArray[5].Add('sgLeftUp');
  FCnFmxFixEnumNameArray[5].Add('sgLeftDown');
  FCnFmxFixEnumNameArray[5].Add('sgRightUp');
  FCnFmxFixEnumNameArray[5].Add('sgRightDown');
  FCnFmxFixEnumNameArray[5].Add('sgUpDown');
  FCnFmxFixEnumNameArray[5].Add('sgDownUp');
  FCnFmxFixEnumNameArray[5].Add('sgLeftRight');
  FCnFmxFixEnumNameArray[5].Add('sgRightLeft');
  FCnFmxFixEnumNameArray[5].Add('sgUpLeftLong');
  FCnFmxFixEnumNameArray[5].Add('sgUpRightLong');
  FCnFmxFixEnumNameArray[5].Add('sgDownLeftLong');
  FCnFmxFixEnumNameArray[5].Add('sgDownRightLong');
  FCnFmxFixEnumNameArray[5].Add('sgScratchout');
  FCnFmxFixEnumNameArray[5].Add('sgTriangle');
  FCnFmxFixEnumNameArray[5].Add('sgSquare');
  FCnFmxFixEnumNameArray[5].Add('sgCheck');
  FCnFmxFixEnumNameArray[5].Add('sgCurlicue');
  FCnFmxFixEnumNameArray[5].Add('sgDoubleCurlicue');
  FCnFmxFixEnumNameArray[5].Add('sgCircle');
  FCnFmxFixEnumNameArray[5].Add('sgDoubleCircle');
  FCnFmxFixEnumNameArray[5].Add('sgSemiCircleLeft');
  FCnFmxFixEnumNameArray[5].Add('sgSemiCircleRight');
  FCnFmxFixEnumNameArray[5].Add('sgChevronUp');
  FCnFmxFixEnumNameArray[5].Add('sgChevronDown');
  FCnFmxFixEnumNameArray[5].Add('sgChevronLeft');
  FCnFmxFixEnumNameArray[5].Add('sgChevronRight');

  // TInteractiveGesture
  FCnFmxFixEnumNameArray[6] := TStringList.Create();
  FCnFmxFixEnumNameArray[6].Add('igZoom');
  FCnFmxFixEnumNameArray[6].Add('igPan');
  FCnFmxFixEnumNameArray[6].Add('igRotate');
  FCnFmxFixEnumNameArray[6].Add('igTwoFingerTap');
  FCnFmxFixEnumNameArray[6].Add('igPressAndTap');

  // TGestureType
  FCnFmxFixEnumNameArray[7] := TStringList.Create();
  FCnFmxFixEnumNameArray[7].Add('gtStandard');
  FCnFmxFixEnumNameArray[7].Add('gtRecorded');
  FCnFmxFixEnumNameArray[7].Add('gtRegistered');
  FCnFmxFixEnumNameArray[7].Add('gtNone');

  // TGestureOption
  FCnFmxFixEnumNameArray[8] := TStringList.Create();
  FCnFmxFixEnumNameArray[8].Add('goUniDirectional');
  FCnFmxFixEnumNameArray[8].Add('goSkew');
  FCnFmxFixEnumNameArray[8].Add('goEndpoint');
  FCnFmxFixEnumNameArray[8].Add('goRotate');

  // TGestureEngineFlag
  FCnFmxFixEnumNameArray[9] := TStringList.Create();
  FCnFmxFixEnumNameArray[9].Add('efMouseEvents');
  FCnFmxFixEnumNameArray[9].Add('efTouchEvents');
end;

procedure FreeFmxSetFixArray;
var
  I: Integer;
begin
  for I := Low(FCnFmxFixEnumNameArray) to High(FCnFmxFixEnumNameArray) do
  begin
    FCnFmxFixEnumNameArray[I].Free;
    FCnFmxFixEnumNameArray[I] := nil;
  end;
end;

initialization
  CreateFmxSetFixArray;

finalization
  FreeFmxSetFixArray;

end.