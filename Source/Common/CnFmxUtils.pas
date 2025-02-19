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

unit CnFmxUtils;
{* |<PRE>
================================================================================
* ������ƣ�CnPack IDE ר�Ұ�
* ��Ԫ���ƣ�FMX ��صĹ��̿ⵥԪ
* ��Ԫ���ߣ�CnPack ������
* ��    ע���õ�Ԫ������ XE2 �����ϰ汾���� FMX ��ص�һЩ���ݡ�
*           ����Ԫ��ʹ�� VCL �� TControl ��ܣ�ֻʹ�� FMX �ġ�
*           ������Ԫ��ֻʹ�� VCL �� TControl ��ܣ�������� FMX ����ز�����
*           ����Ҫ����˵�Ԫ����ʵ������ܸ����Ŀ�ġ�
* ����ƽ̨��WinXP + Delphi XE2
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2011.10.02 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Grid, FMX.Platform.Win;

type
  TCnFmxPosType = (fptLeft, fptTop, fptRight, fptBottom, fptWidth, fptHeight);

function CnFmxGetObjectParent(AObject: TComponent): TComponent;
{* ��ȡһ�� FMX �� Component �� Parent����� AObject ���� TFmxObject �����࣬���� nil}

function CnFmxGetControlParent(AControl: TComponent): TComponent;
{* ��ȡһ�� FMX �� Control �� Parent����� AControl ���� FMX.TControl �����࣬���� nil}

function CnFmxGetChildrenCount(AComp: TComponent): Integer;
{* ��ȡһ�� FmxObject �� Child ��������� AComp ���� FmxObject �����࣬���� -1}

function CnFmxGetChildByIndex(AComp: TComponent; Index: Integer): TComponent;
{* ��ȡһ�� FmxObject �ĵ� Index �� Child����� AComp ���� FmxObject �����࣬���� nil}

function CnFmxGetControlsCount(AControl: TComponent): Integer;
{* ��ȡһ�� FMX �� Control ���� Control ��������� AControl ���� FMX.TControl �����࣬���� -1}

function CnFmxGetControlByIndex(AControl: TComponent; Index: Integer): TComponent;
{* ��ȡһ�� FMX �� Control �ĵ� Index ���� Control����� AControl ���� FMX.TControl �����࣬���� nil}

function CnFmxIsInheritedFromClassByName(AObject: TObject; AClassName: string): Boolean;
{* ��ȡһ�� Object �Ƿ�̳��� AClassName ���ֱ�ʶ�� FMX ��}

function CnFmxIsInheritedFromControl(AObject: TObject): Boolean;
{* ��ȡһ�� Object �Ƿ�̳��� FMX.TControl ��}

function CnFmxClassIsInheritedFromControl(AClass: TClass): Boolean;
{* ��ȡһ�� Class �Ƿ�̳��� FMX.TControl ��}

function CnFmxIsInheritedFromForm(AObject: TObject): Boolean;
{* ��ȡһ�� Object �Ƿ�̳��� FMX.TForm ��}

function CnFmxClassIsInheritedFromForm(AClass: TClass): Boolean;
{* ��ȡһ�� Class �Ƿ�̳��� FMX.TForm ��}

function CnFmxIsInheritedFromCommonCustomForm(AObject: TObject): Boolean;
{* ��ȡһ�� Object �Ƿ�̳��� FMX.TCommonCustomForm ��}

function CnFmxIsInheritedFromFrame(AObject: TObject): Boolean;
{* ��ȡһ�� Object �Ƿ�̳��� FMX.TFrame ��}

function CnFmxGetControlRect(AControl: TComponent): TRect;
{* ��ȡһ�� FMX �� Control �Ļ����� Parent �� Rect���ڲ��ֹ�����}

procedure CnFmxSetControlRect(AControl: TComponent; ARect: TRect);
{* ����һ�� FMX �� Control �Ļ����� Parent �� Rect���ڲ��ֹ�����}

function CnFmxGetControlScreenRect(AControl: TComponent): TRect;
{* ��ȡһ�� FMX �� Control �Ļ�����Ļ����� Rect���ڲ��ֹ�����}

procedure CnFmxSetControlScreenRect(AControl: TComponent; ARect: TRect);
{* ����һ�� FMX �� Control �Ļ�����Ļ����� Rect���ڲ��ֹ�����}

function CnFmxGetControlPositionValue(AControl: TComponent;
  PosType: TCnFmxPosType): Integer;
{* ��ȡһ�� FMX �� Control ��λ�á��ߴ�ȣ����������� PosType ����ָ��}

procedure CnFmxSetControlPositionValue(AControl: TComponent; AValue: Single;
  PosType: TCnFmxPosType);
{* ����һ�� FMX �� Control ��λ�á��ߴ�ȣ������õ������� PosType ����ָ��}

procedure CnFmxControlBringToFront(AControl: TComponent);
{* ��һ�� FMX �� Control ����ǰ��}

procedure CnFmxControlSendToBack(AControl: TComponent);
{* ��һ�� FMX �� Control ��������}

procedure CnFmxGetFormClientSize(AForm: TComponent; out AClientWidth, AClientHeight: Integer);
{* ��ȡһ�� FMX �� Form �� ClientWidth, ClientHeight}

function CnFmxGetCommonCustomFormCaption(AForm: TComponent): string;
{* ��ȡһ�� FMX �� Form �ı�������}

function CnFmxFixSetValue(const PType: string; const PValue: string): string;
{* Ϊ�߰汾�﷨�� set ��ֵ������������ [seTop] ��� [TSide.seTop]}

function CnInputQuery(const ACaption, APrompt: string;
  var Value: string): Boolean;
{* FMX �汾�� InputQuery��ֱ�ӵ��� FMX �Դ���}

procedure CnFmxSetStringGridColumnCount(Grid: TStringGrid; ColCount: Integer;
  ColWidth: Integer = 64);
{* ����һ FMX �� StringGrid ���������ܵķ�װ���ڲ���Ҫ��ɾ StringColumn
  ��������Ҫ�ⲿָ����� ColWidth��Ĭ�� 64}

procedure CnFmxMoveSubControl(FromControl, ToControl: TComponent);
{* �� FromControl �������� Control ��˳���ƶ��� ToControl ��}

function CnFmxGetControlPosition(AControl: TComponent): TSmallPoint;
{* ���� FMX Control ��λ��}

procedure CnFmxGetScreenFormsWithName(const Name: string; OutForms: TList);
{* ���� Screen ������ Form ʵ���������Ƶ��ڸ����ֵ�ʵ�������б�����Ϊ��ʱȫ��}

procedure CnFmxGetScreenFormsWithClassName(const ClsName: string; OutForms: TList);
{* ���� Screen ������ Form ʵ�������������ڸ����ֵ�ʵ�������б�}

function CnFmxGetFmxApplication: TComponent;
{* ���� FMX ������ Application ʵ��}

function CnFmxGetWindowPlatformHandle(AComp: TComponent): THandle;
{* ���� FMX �����ķ�װ�� Window Handle���� AComp ����������򷵻� 0}

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
  // Local �� Absolute �����ת����� AV�����û��֧�֣���ʱȫʹ���������
  // Ҳ����˵ Rect �ǻ��� Parent �µĶ�����Ļ��
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

    // ѭ���ҵ������������
    AParent := TControl(AControl).Parent;
    while (AParent <> nil) and (AParent is TControl) and not (AParent is TCommonCustomForm) do
    begin
      R.X := R.X + TControl(AParent).Position.X;
      R.Y := R.Y + TControl(AParent).Position.Y;
      AParent := TControl(AParent).Parent;
    end;

    // �ټ����������Ͻ����꣬�����Ļ����
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
    R.X := 0; // �� Control �� Parent ���Ͻ�ʱ���������
    R.Y := 0;

    // ѭ���ҵ������������
    AParent := TControl(AControl).Parent;
    while (AParent <> nil) and (AParent is TControl) and not (AParent is TCommonCustomForm) do
    begin
      R.X := R.X + TControl(AParent).Position.X;
      R.Y := R.Y + TControl(AParent).Position.Y;
      AParent := TControl(AParent).Parent;
    end;

    // �ټ����������Ͻ����꣬�����Ļ����
    if (AParent <> nil) and (AParent is TCommonCustomForm) then
    begin
      PF.X := 0;
      PF.Y := 0;
      PF := TCommonCustomForm(AParent).ClientToScreen(PF);

      R.X := R.X + PF.X;
      R.Y := R.Y + PF.Y;
    end;

    // ������Ļ�� Rect������ȥ Control �� Parent �����Ͻ�����Ļ�ϵ����꣬
    // �ͱ���˻��� Control �� Parent ������

{$IFDEF IDE_SUPPORT_HDPI}
    BRect.Left := Round(ARect.Left - R.X);    // ��֪��զ��Ҫ��ôд
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
    Result := WindowHandleToPlatform(TCommonCustomForm(AComp).Handle).Wnd;
{$ELSE}
    Result := FmxHandleToHWND(TCommonCustomForm(AComp).Handle);
{$ENDIF}
  end
  else
    Result := 0;
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