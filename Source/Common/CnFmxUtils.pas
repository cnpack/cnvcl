{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2019 CnPack 开发组                       }
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
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs;

type
  TCnFmxPosType = (fptLeft, fptTop, fptRight, fptBottom, fptWidth, fptHeight);

function CnFmxGetObjectParent(AObject: TComponent): TComponent;

function CnFmxGetControlParent(AControl: TComponent): TComponent;

function CnFmxGetControlsCount(AControl: TComponent): Integer;

function CnFmxGetControlByIndex(AControl: TComponent; Index: Integer): TComponent;

function CnFmxIsInheritedFromClassByName(AObject: TObject; AClassName: string): Boolean;

function CnFmxIsInheritedFromControl(AObject: TObject): Boolean;

function CnFmxIsInheritedFromForm(AObject: TObject): Boolean;

function CnFmxIsInheritedFromCommonCustomForm(AObject: TObject): Boolean;

function CnFmxIsInheritedFromFrame(AObject: TObject): Boolean;

function CnFmxGetControlRect(AControl: TComponent): TRect;

procedure CnFmxSetControlRect(AControl: TComponent; ARect: TRect);

function CnFmxGetControlPositionValue(AControl: TComponent;
  PosType: TCnFmxPosType): Integer;

procedure CnFmxSetControlPositionValue(AControl: TComponent; AValue: Single;
  PosType: TCnFmxPosType);

procedure CnFmxControlBringToFront(AControl: TComponent);

procedure CnFmxControlSendToBack(AControl: TComponent);

function CnFmxGetCommonCustomFormCaption(AForm: TComponent): string;

// 为高版本语法的set赋值增加类名，如[seTop]变成[TSide.seTop]
function CnFmxFixSetValue(const PType: string; const PValue: string): string;

implementation

{$IFDEF DEBUG}
uses
  CnDebug;
{$ENDIF}

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

type
  TControlHack = class(TControl);

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

function CnFmxIsInheritedFromForm(AObject: TObject): Boolean;
begin
  Result := AObject.InheritsFrom(FMX.Forms.TForm);
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
  // 也就是说只支持同一 Parent 下的
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
    if  AControl.InheritsFrom(TControl) then
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