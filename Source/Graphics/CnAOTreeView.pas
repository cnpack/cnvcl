{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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

unit CnAOTreeView;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：自动参数设置 TreeView 组件单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 开发平台：PWin2000 SP4 + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 备    注：该单元定义了自动参数设置 TreeView 组件
*           该组件用于在运行时使用树状结构根据设置信息对象显示通用的设置界面。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, ComCtrls, TypInfo,
  {$IFDEF COMPILER6_UP} Variants, {$ENDIF COMPILER6_UP}
  StdCtrls, ImgList, CnSpin, Dialogs, Menus, Math, Forms, CnGraphConsts,
  CnAutoOption;

type

{ TCnAOTreeView }

  EUnsupportedPropKind = class(Exception);

  TCnOptionKind = (
    okUnknown, okGroup, okCustom, okBoolFalse, okBoolTrue, okString,
    okStringCombo, okInteger, okIntegerCombo, okFloat, okDateTime,
    okDate, okTime, okEnum, okSet, okVariant, okFont, okColor, okShortCut,
    okStrings);

  TCreateInplaceEditEvent = procedure(Sender: TObject; InplaceEdit: TControl;
    AOption: TCnBaseOption) of object;
  TGetItemTextEvent = procedure(Sender: TObject; AOption: TCnOptionItem;
    var AText: string) of object; 

  TCnAOTreeView = class(TCustomTreeView)
  private
    FImageList: TImageList;
    FInplaceEdit: TControl;
    FModified: Boolean;
    FOptions: TCnOptionGroup;
    FOnCreateInplaceEdit: TCreateInplaceEditEvent;
    FOnGetItemText: TGetItemTextEvent;
    procedure ApplyInplaceEdit;
    procedure ComboBoxDropDown(Sender: TObject);
    procedure CreateInplaceEdit;
    procedure FreeInplaceEdit;
    procedure OnColorClick(Sender: TObject);
    procedure OnFontClick(Sender: TObject);
    procedure OnInplaceEditEnterExit(Sender: TObject);
    procedure SetNodeImageIndex(Node: TTreeNode; Index: Integer);
    procedure UpdateInplaceEdit;
    procedure SetOptions(Value: TCnOptionGroup);
  protected
    function CanEdit(Node: TTreeNode): Boolean; override;
    procedure Change(Node: TTreeNode); override;
    procedure Click; override;
    function DoClickNode(Node: TTreeNode): Boolean;
    procedure DoCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure DoEnter; override;
    procedure DoExit; override;
    function GetOptionKind(Option: TCnBaseOption; RaiseError: Boolean = False):
      TCnOptionKind;
    procedure GetSelectedIndex(Node: TTreeNode); override;
    procedure UpdateNode(Node: TTreeNode);
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    property ImageList: TImageList read FImageList;
  public
    constructor Create(AOwner: TComponent); override;
    {* 类构造器 }
    destructor Destroy; override;
    {* 类析构器 }
    procedure ApplyOption;
    {* 应用当前的设置到对象属性 }
    procedure DefaultOption;
    {* 恢复设置为原对象属性的默认值 }
    procedure ResetOption;
    {* 恢复设置为原对象属性的当前值 }
    procedure UpdateTreeView;
    {* 更新设置树 }
    property Modified: Boolean read FModified;
    {* 标识是否有属性被修改 }
    property Options: TCnOptionGroup read FOptions write SetOptions;
    {* 用于设置的参数组对象 }
  published
    property Align;
    property Anchors;
    property AutoExpand;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property HotTrack;
    property Indent;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnContextPopup;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons default False;
    property ShowHint;
    property ShowLines default False;
    property ShowRoot;
    property TabOrder;
    property TabStop default True;
    property ToolTips;
    property Visible;

    property OnCreateInplaceEdit: TCreateInplaceEditEvent
      read FOnCreateInplaceEdit write FOnCreateInplaceEdit;
    property OnGetItemText: TGetItemTextEvent read FOnGetItemText write FOnGetItemText;
  end;

implementation

{$IFDEF DEBUG}
uses
  CnDebug;
{$ENDIF}

{$R *.res}

const
  csIdxGroup = 0;
  csIdxCustom = 1;
  csIdxUnChecked = 2;
  csIdxChecked = 3;
  csIdxUnSelected = 4;
  csIdxSelected = 5;
  csIdxString = 6;
  csIdxStringCombo = 7;
  csIdxInteger = 8;
  csIdxIntegerCombo = 9;
  csIdxFloat = 10;
  csIdxEnum = 11;
  csIdxSet = 12;
  csIdxVariant = 13;
  csIdxFont = 14;
  csIdxDateTime = 15;
  csIdxDate = 16;
  csIdxTime = 17;
  csIdxColor = 18;
  csIdxShortCut = 19;
  csIdxStrings = 20;
  csSepStr = ': ';

  csInplaceEditHeight = 20;
  csInplaceEditWidth = 120;
  csInplaceButtonHeight = 20;
  csInplaceButtonWidth = 60;
  csInplaceMemoHeight = 60;
  csInplaceMemoWidth = 120;

  csInplaceSpace = 4;
  csMaxStrLength = 15;

  csImageIndexs: array[TCnOptionKind] of Integer = (
    -1, csIdxGroup, csIdxCustom, csIdxUnChecked, csIdxChecked, csIdxString,
    csIdxStringCombo, csIdxInteger, csIdxIntegerCombo, csIdxFloat, csIdxDateTime,
    csIdxDate, csIdxTime, csIdxEnum, csIdxSet, csIdxVariant, csIdxFont, csIdxColor,
    csIdxShortCut, csIdxStrings);

type
  TWinControlHack = class(TWinControl);

constructor TCnAOTreeView.Create(AOwner: TComponent);
var
  Bitmap: TBitmap;
begin
  inherited;
  FImageList := TImageList.Create(Self);
  Images := FImageList;
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromResourceName(HInstance, 'CNAOTREEVIEW');
    FImageList.AddMasked(Bitmap, Bitmap.TransparentColor);
  finally
    Bitmap.Free;
  end;
  
  ShowLines := False;
  ShowButtons := False;
  ReadOnly := True;
  OnCustomDrawItem := DoCustomDrawItem;
end;

destructor TCnAOTreeView.Destroy;
begin
  FImageList.Free;
  inherited;
end;

procedure TCnAOTreeView.ApplyInplaceEdit;
var
  Node: TTreeNode;
  Item: TCnOptionItem;
  Obj: TObject;

  // 将字符串转换为匹配的格式
  function GetString(PropKind: TTypeKind; const Value: string): Variant;
  var
    C: Char;
    WC: WideChar;
    WS: WideString;
  begin
    case PropKind of
      tkChar:
        begin
          if Value <> '' then
            C := Value[1]
          else
            C := #0;
          Result := C;
        end;
      tkWChar:
        begin
          WS := Value;
          if WS <> '' then
            WC := WS[1]
          else
            WC := #0;
          Result := WC;
        end;
      tkWString {$IFDEF UNICODE}, tkUString{$ENDIF}:
        begin
          WS := Value;
          Result := WS;
        end;
    else
      Result := Value;
    end;
  end;
begin
  if FInplaceEdit = nil then Exit;

  Node := TTreeNode(FInplaceEdit.Tag);
  Item := TCnOptionItem(Node.Data);
  
  if FInplaceEdit is TDateTimePicker then
  begin
    Item.Value := TDateTimePicker(FInplaceEdit).DateTime;
    FModified := True;
  end
  else if FInplaceEdit is TCnSpinEdit then
  begin
    with TCnSpinEdit(FInplaceEdit) do
    begin
      if (MaxValue > MinValue) and ((MaxValue <> 0) or (MinValue <> 0)) then
      begin
        if Value > MaxValue then
          Value := MaxValue;
        if Value < MinValue then
          Value := MinValue;
      end;
      Item.Value := Value;
    end;
    FModified := True;
  end
  else if FInplaceEdit is TComboBox then
  begin
    case GetOptionKind(Item) of
      okIntegerCombo:
        Item.Value := TComboBox(FInplaceEdit).ItemIndex;
      okStringCombo:
        Item.Value := GetString(Item.PropKind, TComboBox(FInplaceEdit).Text);
    else
      Assert(False);
    end;
    FModified := True;
  end
  else if FInplaceEdit is THotKey then
  begin
    Item.Value := THotKey(FInplaceEdit).HotKey;
    FModified := True;
  end
  else if FInplaceEdit is TEdit then
  begin
    try
      case GetOptionKind(Item) of
        okFloat:
          Item.Value := StrToFloat(TEdit(FInplaceEdit).Text);
        okDateTime:
          Item.Value := StrToDateTime(TEdit(FInplaceEdit).Text);
        okString:
          Item.Value := GetString(Item.PropKind, TEdit(FInplaceEdit).Text);
        okVariant:
          Item.Value := TEdit(FInplaceEdit).Text;
      else
        Assert(False);
      end;
      FModified := True;
    except
      ;
    end;
  end
  else if FInplaceEdit is TMemo then
  begin
{$IFDEF WIN64}
    Obj := TObject(Integer(Item.Value));
{$ELSE}
    Integer(Obj) := Item.Value;
{$ENDIF}
    if Obj is TStrings then
      TStrings(Obj).Text := TMemo(FInplaceEdit).Lines.Text;
    FModified := True;
  end
  else if FInplaceEdit is TButton then
  begin
    // None
  end
  else
  begin
    // None
  end;

  UpdateNode(Node);
  Repaint;
end;

procedure TCnAOTreeView.ApplyOption;

  procedure DoApplyOption(Option: TCnBaseOption);
  var
    i: Integer;
  begin
    if Option is TCnOptionItem then
      TCnOptionItem(Option).ApplyOption
    else if Option is TCnOptionGroup then
      for i := 0 to TCnOptionGroup(Option).Count - 1 do
        DoApplyOption(TCnOptionGroup(Option)[i]);
  end;
begin
  ApplyInplaceEdit;
  DoApplyOption(FOptions);
end;

function TCnAOTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

procedure TCnAOTreeView.Change(Node: TTreeNode);
begin
  inherited;
  CreateInplaceEdit;
end;

procedure TCnAOTreeView.Click;
var
  P: TPoint;
  Node: TTreeNode;
begin
  inherited;
  GetCursorPos(P);
  P := ScreenToClient(P);
  Node := GetNodeAt(P.X, P.Y);
  if Node <> nil then
    DoClickNode(Node);
end;

procedure TCnAOTreeView.ComboBoxDropDown(Sender: TObject);
var
  i: Integer;
  MaxWidth: Integer;
  Bitmap: Graphics.TBitmap;
  ComboBox: TComboBox;
begin
  if not (Sender is TComboBox) then
    Exit;

  ComboBox := TComboBox(Sender);
  MaxWidth := ComboBox.Width;
  Bitmap := Graphics.TBitmap.Create;
  try
    Bitmap.Canvas.Font.Assign(ComboBox.Font);
    for i := 0 to ComboBox.Items.Count - 1 do
      MaxWidth := Max(MaxWidth, Bitmap.Canvas.TextWidth(ComboBox.Items[i]) + 10);
  finally;
    Bitmap.Free;
  end;
  if ComboBox.Items.Count > ComboBox.DropDownCount then
    Inc(MaxWidth,  GetSystemMetrics(SM_CXVSCROLL));
  MaxWidth := Min(400, MaxWidth);
  if MaxWidth > ComboBox.Width then
    SendMessage(ComboBox.Handle, CB_SETDROPPEDWIDTH, MaxWidth, 0)
  else
    SendMessage(ComboBox.Handle, CB_SETDROPPEDWIDTH, 0, 0)
end;

procedure TCnAOTreeView.CreateInplaceEdit;
var
  Option: TCnBaseOption;
  Item: TCnOptionItem;

  procedure CreateSpin(Value, MinValue, MaxValue: Integer);
  begin
    FInplaceEdit := TCnSpinEdit.Create(Self);
    FInplaceEdit.Height := csInplaceEditHeight;
    FInplaceEdit.Width := csInplaceEditWidth;
    TCnSpinEdit(FInplaceEdit).Value := Value;
    TCnSpinEdit(FInplaceEdit).MinValue := MinValue;
    TCnSpinEdit(FInplaceEdit).MaxValue := MaxValue;
    UpdateInplaceEdit;
  end;

  procedure CreateDateTimePicker(Kind: TDateTimeKind; Value: TDateTime);
  begin
    FInplaceEdit := TDateTimePicker.Create(Self);
    FInplaceEdit.Height := csInplaceEditHeight;
    FInplaceEdit.Width := csInplaceEditWidth;
    TDateTimePicker(FInplaceEdit).Kind := Kind;
    TDateTimePicker(FInplaceEdit).DateTime := Value;
    UpdateInplaceEdit;
  end;

  procedure CreateEdit(const Value: string);
  begin
    FInplaceEdit := TEdit.Create(Self);
    FInplaceEdit.Height := csInplaceEditHeight;
    FInplaceEdit.Width := csInplaceEditWidth;
    UpdateInplaceEdit;
    TEdit(FInplaceEdit).Text := Value;
  end;

  procedure CreateComboBox(const Value: string; List: TStrings; DropDownList: Boolean);
  begin
    FInplaceEdit := TComboBox.Create(Self);
    FInplaceEdit.Height := csInplaceEditHeight;
    FInplaceEdit.Width := csInplaceEditWidth;
    UpdateInplaceEdit;
    TComboBox(FInplaceEdit).Items.Assign(List);
    TComboBox(FInplaceEdit).OnDropDown := ComboBoxDropDown;
    if DropDownList then
    begin
      TComboBox(FInplaceEdit).Style := csDropDownList;
      TComboBox(FInplaceEdit).ItemIndex := List.IndexOf(Value);
    end
    else
    begin
      TComboBox(FInplaceEdit).Style := csDropDown;
      TComboBox(FInplaceEdit).Text := Value;
    end;
  end;

  procedure CreateButton(Caption: string; OnClick: TNotifyEvent);
  begin
    FInplaceEdit := TButton.Create(Self);
    FInplaceEdit.Height := csInplaceButtonHeight;
    FInplaceEdit.Width := csInplaceButtonWidth;
    UpdateInplaceEdit;
    if Caption = '' then
      Caption := SCnAOCaptionOption;
    TButton(FInplaceEdit).Caption := Caption;
    TButton(FInplaceEdit).OnClick := OnClick;
  end;

  procedure CreateHotKey(ShortCut: TShortCut);
  begin
    FInplaceEdit := THotKey.Create(Self);
    FInplaceEdit.Height := csInplaceEditHeight;
    FInplaceEdit.Width := csInplaceEditWidth;
    UpdateInplaceEdit;
    THotKey(FInplaceEdit).HotKey := ShortCut;
  end;

  procedure CreateMemo(Value: Variant);
  var
    Obj: TPersistent;
  begin
{$IFDEF WIN64}
    Obj := TPersistent(Integer(Value));
{$ELSE}
    Integer(Obj) := Value;
{$ENDIF}
    Assert(Obj is TStrings);
    FInplaceEdit := TMemo.Create(Self);
    FInplaceEdit.Width := csInplaceMemoWidth;
    FInplaceEdit.Height := csInplaceMemoHeight;
    UpdateInplaceEdit;
    TMemo(FInplaceEdit).Lines.Assign(Obj);
  end;
begin
  FreeInplaceEdit;
  if (Selected = nil) or (Selected.Data = nil) then Exit;

  Option := TCnBaseOption(Selected.Data);
  Item := TCnOptionItem(Selected.Data);
  case GetOptionKind(Option) of
    okCustom:
      CreateButton(TCnOptionCustom(Option).Caption, TCnOptionCustom(Option).OnClick);
    okString:
      CreateEdit(Item.Value);
    okStringCombo:
      CreateComboBox(Item.Value, Item.List, False);
    okInteger:
      CreateSpin(Item.Value, Item.MinValue, Item.MaxValue);
    okIntegerCombo:
      CreateComboBox(Item.List[Item.Value], Item.List, True);
    okFloat:
      CreateEdit(FloatToStr(Item.Value));
    okDateTime:
      CreateEdit(DateTimeToStr(TDateTime(Item.Value)));
    okDate:
      CreateDateTimePicker(dtkDate, TDate(Item.Value));
    okTime:
      CreateDateTimePicker(dtkTime, TTime(Item.Value));
    okVariant:
      CreateEdit(VarToStr(Item.Value));
    okFont:
      CreateButton(SCnAOCaptionFont, OnFontClick);
    okColor:
      CreateButton(SCnAOCaptionColor, OnColorClick);
    okShortCut:
      CreateHotKey(Item.Value);
    okStrings:
      CreateMemo(Item.Value);
  else
    Exit;
  end;

  if Assigned(FOnCreateInplaceEdit) then
    FOnCreateInplaceEdit(Self, FInplaceEdit, Option);
end;

procedure TCnAOTreeView.DefaultOption;
  procedure DoDefaultOption(Option: TCnBaseOption);
  var
    i: Integer;
  begin
    if Option is TCnOptionItem then
      TCnOptionItem(Option).DefaultOption
    else if Option is TCnOptionGroup then
      for i := 0 to TCnOptionGroup(Option).Count - 1 do
        DoDefaultOption(TCnOptionGroup(Option)[i]);
  end;
begin
  DoDefaultOption(FOptions);
  UpdateTreeView;
end;

function TCnAOTreeView.DoClickNode(Node: TTreeNode): Boolean;
var
  i, Min, Max: Integer;
  Item: TCnOptionItem;
  BoolValue: Boolean;
  EnumInfo: PTypeInfo;
  SetValue: TIntegerSet;
begin
  Result := False;
  if Node = nil then Exit;
  
  if Node.Data = nil then  // 是集合或枚举子项
  begin
    Item := TCnOptionItem(Node.Parent.Data);
    Assert(Item is TCnOptionItem);
    Assert(GetOptionKind(Item) in [okEnum, okSet]);

    case GetOptionKind(Item) of
      okEnum:
        begin
          Item.Value := Node.Index + Item.MinValue;
          for i := 0 to Node.Parent.Count - 1 do
            if i = Node.Index then
              SetNodeImageIndex(Node.Parent.Item[i], csIdxSelected)
            else
              SetNodeImageIndex(Node.Parent.Item[i], csIdxUnSelected)
        end;
      okSet:
        begin
          EnumInfo := GetTypeData(Item.PropInfo^.PropType^)^.CompType^;
          if Node.ImageIndex = csIdxChecked then
            SetNodeImageIndex(Node, csIdxUnChecked)
          else
            SetNodeImageIndex(Node, csIdxChecked);

          SetValue := [];
          Min := GetTypeData(EnumInfo).MinValue;
          Max := GetTypeData(EnumInfo).MaxValue;
          for i := Min to Max do
            if Node.Parent.Item[i].ImageIndex = csIdxChecked then
              Include(SetValue, i + Min);
          Item.Value := Integer(SetValue);
        end;
    end;

    FModified := True;
    Result := True;
  end
  else if GetOptionKind(TCnBaseOption(Node.Data)) in [okBoolFalse, okBoolTrue] then
  begin
    Item := TCnOptionItem(Node.Data);
    
    BoolValue := not Item.Value;
    Item.Value := BoolValue;
    if BoolValue then
      SetNodeImageIndex(Node, csIdxChecked)
    else
      SetNodeImageIndex(Node, csIdxUnChecked);

    FModified := True;
    Result := True;
  end;
end;

procedure TCnAOTreeView.DoCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  // Todo: 自绘制颜色属性及字体等
  DefaultDraw := True;
end;

procedure TCnAOTreeView.DoEnter;
begin
  inherited;
  Change(Selected);
end;

procedure TCnAOTreeView.DoExit;
begin
  inherited;
  FreeInplaceEdit;
end;

procedure TCnAOTreeView.FreeInplaceEdit;
begin
  if FInplaceEdit <> nil then
  begin
    ApplyInplaceEdit;
    FreeAndNil(FInplaceEdit);
  end;
end;

function TCnAOTreeView.GetOptionKind(Option: TCnBaseOption; RaiseError: Boolean
  = False): TCnOptionKind;
var
  Item: TCnOptionItem;
  BoolValue: Boolean;
  Obj: TObject;
begin
  Result := okUnknown;
  
  if Option is TCnOptionCustom then
    Result := okCustom
  else if Option is TCnOptionGroup then
    Result := okGroup
  else if Option is TCnOptionItem then
  begin
    Item := TCnOptionItem(Option);
    case Item.PropKind of
      tkInteger, tkInt64:
        begin
          if Item.PropInfo^.PropType^ = TypeInfo(TColor) then
            Result := okColor
          else if Item.PropInfo^.PropType^ = TypeInfo(TShortCut) then
            Result := okShortCut
          else if (Item.List.Count > 0) and (Item.Value >= 0) and
            (Item.Value < Item.List.Count) then
            Result := okIntegerCombo
          else
            Result := okInteger;
        end;
      tkFloat:
        begin
          if Item.PropInfo^.PropType^ = TypeInfo(TDateTime) then
            Result := okDateTime
          else if Item.PropInfo^.PropType^ = TypeInfo(TDate) then
            Result := okDate
          else if Item.PropInfo^.PropType^ = TypeInfo(TTime) then
            Result := okTime
          else
            Result := okFloat;
        end;
      tkChar, tkString, tkWChar, tkLString, tkWString{$IFDEF UNICODE}, tkUString{$ENDIF}:
        begin
          if Item.List.Count > 0 then
            Result := okStringCombo
          else
            Result := okString
        end;
      tkEnumeration:
        begin
          if IsBooleanType(Item.PropInfo^.PropType^) or
            IsBoolType(Item.PropInfo^.PropType^) then
          begin
            BoolValue := Item.Value;
            if BoolValue then
              Result := okBoolTrue
            else
              Result := okBoolFalse;
          end
          else
            Result := okEnum;
        end;
      tkSet:
        begin
          Result := okSet;
        end;
      tkClass:
        begin
{$IFDEF WIN64}
          Obj := TObject(Integer(Item.Value));
{$ELSE}
          Integer(Obj) := Item.Value;
{$ENDIF}
          if Obj is TFont then
            Result := okFont
          else if Obj is TStrings then
            Result := okStrings
          else if RaiseError then
            raise EUnsupportedPropKind.Create('Unsupported Property Kind: ' +
              Obj.ClassName);
        end;
      tkVariant:
        begin
          Result := okVariant;
        end;
    end;
  end;

  if (Result = okUnknown) and RaiseError then
    if Option is TCnOptionItem then
      raise EUnsupportedPropKind.Create('Unsupported Property Kind: ' +
        GetEnumName(TypeInfo(TTypeKind), Ord(TCnOptionItem(Option).PropKind)))
    else
      raise EUnsupportedPropKind.Create('Unsupported Property Define: ' + Option.Text);
end;

procedure TCnAOTreeView.GetSelectedIndex(Node: TTreeNode);
begin
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TCnAOTreeView.OnColorClick(Sender: TObject);
var
  Node: TTreeNode;
  Item: TCnOptionItem;
begin
  if FInplaceEdit = nil then Exit;

  Node := TTreeNode(FInplaceEdit.Tag);
  Item := TCnOptionItem(Node.Data);
  with TColorDialog.Create(Self) do
  try
    Color := ColorToRGB(Item.Value);
    if Execute then
      Item.Value := Color;
  finally
    Free;
  end;
end;

procedure TCnAOTreeView.OnFontClick(Sender: TObject);
var
  Node: TTreeNode;
  Item: TCnOptionItem;
  Obj: TFont;
begin
  if FInplaceEdit = nil then Exit;

  Node := TTreeNode(FInplaceEdit.Tag);
  Item := TCnOptionItem(Node.Data);
{$IFDEF WIN64}
  Obj := TFont(Integer(Item.Value));
{$ELSE}
  Integer(Obj) := Item.Value;
{$ENDIF}
  with TFontDialog.Create(Self) do
  try
    Font.Assign(Obj);
    if Execute then
      Obj.Assign(Font);
  finally
    Free;
  end;
end;

procedure TCnAOTreeView.OnInplaceEditEnterExit(Sender: TObject);
var
  i: Integer;
begin
  if FInplaceEdit <> nil then
  begin
    FInplaceEdit.Invalidate;
    if FInplaceEdit is TWinControl then
      for i := 0 to TWinControl(FInplaceEdit).ControlCount - 1 do
        TWinControl(FInplaceEdit).Controls[i].Invalidate;
  end;
end;

procedure TCnAOTreeView.ResetOption;

  procedure DoResetOption(Option: TCnBaseOption);
  var
    i: Integer;
  begin
    if Option is TCnOptionItem then
      TCnOptionItem(Option).ResetOption
    else if Option is TCnOptionGroup then
      for i := 0 to TCnOptionGroup(Option).Count - 1 do
      try
        DoResetOption(TCnOptionGroup(Option)[i]);
      except
        Application.HandleException(nil);
      end;
  end;
begin
  DoResetOption(FOptions);
  UpdateTreeView;
end;

procedure TCnAOTreeView.SetNodeImageIndex(Node: TTreeNode; Index: Integer);
begin
  Node.ImageIndex := Index;
  Node.SelectedIndex := Index;
end;

procedure TCnAOTreeView.UpdateInplaceEdit;
var
  R1, R2: TRect;
begin
  if (Selected <> nil) and (TopItem <> nil) and (FInplaceEdit <> nil) then
  begin
    FInplaceEdit.Tag := Integer(Selected);
    FInplaceEdit.Parent := Self;
    if FInplaceEdit is TWinControl then
    begin
      TWinControlHack(FInplaceEdit).OnEnter := OnInplaceEditEnterExit;
      TWinControlHack(FInplaceEdit).OnExit := OnInplaceEditEnterExit;
    end;
    Selected.Text := TCnBaseOption(Selected.Data).Text + csSepStr;
    R1 := Selected.DisplayRect(True);
    R2 := TopItem.DisplayRect(True);
    FInplaceEdit.Top := R1.Top - R2.Top;
    FInplaceEdit.Left := R1.Right + csInplaceSpace;
    FInplaceEdit.Invalidate;
    //Invalidate;
    // Todo: 滚动时刷新有时不正常
  end;
end;

procedure TCnAOTreeView.UpdateNode(Node: TTreeNode);
var
  Idx: Integer;
  Item: TCnOptionItem;
  OptionKind: TCnOptionKind;
  OrdValue: Integer;
  EnumInfo: PTypeInfo;

  function FontToStr(Value: Variant): string;
  var
    Obj: TObject;
  begin
{$IFDEF WIN64}
    Obj := TObject(Integer(Value));
{$ELSE}
    Integer(Obj) := Value;
{$ENDIF}
    if Obj is TFont then
      Result := Format('%s,%d', [TFont(Obj).Name, TFont(Obj).Size])
    else
      Result := '';
  end;

  function StringsToStr(Value: Variant): string;
  var
    Obj: TObject;
  begin
{$IFDEF WIN64}
    Obj := TObject(Integer(Value));
{$ELSE}
    Integer(Obj) := Value;
{$ENDIF}
    if Obj is TStrings then
      Result := StringReplace(TStrings(Obj).Text, #13#10, ' ', [rfReplaceAll])
    else
      Result := '';
    if Length(Result) > csMaxStrLength - 3 then
      Result := Copy(Result, 1, csMaxStrLength - 3) + '...';
  end;

  procedure SetNodeText(AItem: TCnOptionItem; Text: string);
  begin
    if Assigned(FOnGetItemText) then
      FOnGetItemText(Self, AItem, Text);
    Node.Text := AItem.Text + csSepStr + Text;
  end;
begin
  Assert(Node <> nil);

  if Node.Data = nil then  // 是集合或枚举子项
  begin
    Item := TCnOptionItem(Node.Parent.Data);
    Assert(GetOptionKind(Item) in [okEnum, okSet]);

    case GetOptionKind(Item) of
      okEnum:
        begin
          OrdValue := Item.Value;
          Idx := Node.Index;

          if Idx < Item.List.Count then
            Node.Text := Item.List[Idx - Item.MinValue]
          else
            Node.Text := GetEnumName(Item.PropInfo^.PropType^, Idx);

          if Idx = OrdValue then
            SetNodeImageIndex(Node, csIdxSelected)
          else
            SetNodeImageIndex(Node, csIdxUnSelected);
        end;
      okSet:
        begin
          OrdValue := Item.Value;
          EnumInfo := GetTypeData(Item.PropInfo^.PropType^)^.CompType^;
          Idx := Node.Index;

          if Idx < Item.List.Count then
            Node.Text := Item.List[Idx - GetTypeData(EnumInfo).MinValue]
          else
            Node.Text := GetEnumName(EnumInfo, Idx);

          if Idx in TIntegerSet(OrdValue) then
            SetNodeImageIndex(Node, csIdxChecked)
          else
            SetNodeImageIndex(Node, csIdxUnChecked);
        end;
    end;
  end
  else
  begin
    Item := TCnOptionItem(Node.Data);
    OptionKind := GetOptionKind(TCnBaseOption(Node.Data));
    SetNodeImageIndex(Node, csImageIndexs[OptionKind]);
    case OptionKind of
      okString, okStringCombo:
        SetNodeText(Item, Item.Value);
      okInteger:
        SetNodeText(Item, IntToStr(Item.Value));
      okIntegerCombo:
        SetNodeText(Item, Item.List[Item.Value]);
      okFloat:
        SetNodeText(Item, FloatToStr(Item.Value));
      okDateTime:
        SetNodeText(Item, DateTimeToStr(Item.Value));
      okDate:
        SetNodeText(Item, DateToStr(Item.Value));
      okTime:
        SetNodeText(Item, TimeToStr(Item.Value));
      okVariant:
        SetNodeText(Item, VarToStr(Item.Value));
      okFont:
        SetNodeText(Item, FontToStr(Item.Value));
      okColor:
        SetNodeText(Item, IntToHex(ColorToRGB(Item.Value), 8));
      okShortCut:
        SetNodeText(Item, ShortCutToText(Item.Value));
      okStrings:
        SetNodeText(Item, StringsToStr(Item.Value));
    else
      Node.Text := TCnBaseOption(Node.Data).Text;
    end;
  end;
end;

procedure TCnAOTreeView.UpdateTreeView;

  function AddNode(ParentNode: TTreeNode; AOption: TCnBaseOption): TTreeNode;
  var
    i: Integer;
    EnumInfo: PTypeInfo;
  begin
    Result := Items.AddChildObject(ParentNode, AOption.Text, AOption);
    try
      UpdateNode(Result);
      case GetOptionKind(AOption, True) of
        okGroup:
          begin
            with TCnOptionGroup(AOption) do
              for i := 0 to Count - 1 do
                AddNode(Result, Items[i]);
          end;
        okEnum:
          with TCnOptionItem(AOption) do
          begin
            for i := MinValue to MaxValue do
            begin
              UpdateNode(Items.AddChildObject(Result, '', nil));
            end;
          end;
        okSet:
          with TCnOptionItem(AOption) do
          begin
            EnumInfo := GetTypeData(PropInfo^.PropType^)^.CompType^;
            for i := GetTypeData(EnumInfo).MinValue to GetTypeData(EnumInfo).MaxValue do
              UpdateNode(Items.AddChildObject(Result, '', nil));
          end;
      end;
    except
      Result.Free;
      Application.HandleException(Self);
    end;
  end;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    AddNode(nil, Options);
    Selected := Items.GetFirstNode;
    FullExpand;
    TopItem := Selected;
    FModified := False;
  finally
    Items.EndUpdate;
  end;
end;

procedure TCnAOTreeView.SetOptions(Value: TCnOptionGroup);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    ResetOption;
  end;
end;

procedure TCnAOTreeView.WMChar(var Message: TWMChar);
begin
  if (Char(Message.CharCode) <> ' ') or not DoClickNode(Selected) then
    inherited;
end;

procedure TCnAOTreeView.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
  UpdateInplaceEdit;
end;

procedure TCnAOTreeView.WMMouseWheel(var Message: TWMMouseWheel);
begin
  inherited;
  UpdateInplaceEdit;
end;

procedure TCnAOTreeView.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  UpdateInplaceEdit;
end;

end.
