{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2016 CnPack 开发组                       }
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

unit CnPropSheetFrm;
{ |<PRE>
================================================================================
* 软件名称：CnPack 公用单元
* 单元名称：对象 RTTI 信息显示窗体单元
* 单元作者：刘啸（LiuXiao） liuxiao@cnpack.org
* 备    注：
* 开发平台：PWinXP + Delphi 5
* 兼容测试：未测试
* 本 地 化：该窗体中的字符串暂不符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2016.04.10
*               加入修改属性的功能
*           2012.03.10
*               加入图片对象的可视化显示
*           2006.11.23
*               加入对象类继承关系的显示
*           2006.11.07
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  Grids, StdCtrls, ExtCtrls, TypInfo, Contnrs, Buttons, ComCtrls, Tabs, Commctrl,
  Clipbrd {$IFDEF VER130}{$ELSE}, Variants{$ENDIF};

const
  CN_INSPECTOBJECT = WM_USER + $C10; // Cn Inspect Object

type
  TCnPropContentType = (pctProps, pctEvents, pctCollectionItems, pctMenuItems,
    pctStrings, pctGraphics, pctComponents, pctControls, pctHierarchy);
  TCnPropContentTypes = set of TCnPropContentType;

  TCnDisplayObject = class(TObject)
  {* 描述一供显示内容的基类 }
  private
    FChanged: Boolean;
    FDisplayValue: string;
    FObjStr: string;
    FObjValue: TObject;
    FObjClassName: string;
  public
    property Changed: Boolean read FChanged write FChanged;
    property DisplayValue: string read FDisplayValue write FDisplayValue;
    property ObjClassName: string read FObjClassName write FObjClassName;
    property ObjValue: TObject read FObjValue write FObjValue;
    property ObjStr: string read FObjStr write FObjStr;
  end;

  TCnPropertyObject = class(TCnDisplayObject)
  {* 描述一属性 }
  private
    FIsObject: Boolean;
    FPropName: string;
    FPropType: TTypeKind;
    FPropValue: Variant;
    FCanModify: Boolean;
  public
    property PropName: string read FPropName write FPropName;
    property PropType: TTypeKind read FPropType write FPropType;
    property IsObject: Boolean read FIsObject write FIsObject;
    property PropValue: Variant read FPropValue write FPropValue;
    property CanModify: Boolean read FCanModify write FCanModify;
  end;

  TCnEventObject = class(TCnDisplayObject)
  {* 描述一事件及其处理函数 }
  private
    FHandlerName: string;
    FEventType: string;
    FEventName: string;
  public
    property EventName: string read FEventName write FEventName;
    property EventType: string read FEventType write FEventType;
    property HandlerName: string read FHandlerName write FHandlerName;
  end;

  TCnStringsObject = class(TCnDisplayObject)
  {* 描述一 TStrings}
  private

  public
    procedure Clear;
  end;

  TCnGraphicsObject = class(TCnDisplayObject)
  {* 描述一图片}
  private
    FGraphic: TObject;
  public
    property Graphic: TObject read FGraphic write FGraphic;
  end;

  TCnCollectionItemObject = class(TCnDisplayObject)
  {* 描述一 Collection Item }
  private
    FIndex: Integer;
    FItemName: string;
  public
    property ItemName: string read FItemName write FItemName;
    property Index: Integer read FIndex write FIndex;
  end;

  TCnMenuItemObject = class(TCnDisplayObject)
  {* 描述一 MenuItem }
  private
    FIndex: Integer;
    FItemName: string;
  public
    property ItemName: string read FItemName write FItemName;
    property Index: Integer read FIndex write FIndex;
  end;

  TCnComponentObject = class(TCnDisplayObject)
  {* 描述一 Component }
  private
    FIndex: Integer;
    FCompName: string;
    FDisplayName: string;
  public
    property DisplayName: string read FDisplayName write FDisplayName;
    property CompName: string read FCompName write FCompName;
    property Index: Integer read FIndex write FIndex;
  end;

  TCnControlObject = class(TCnDisplayObject)
  {* 描述一 Component }
  private
    FIndex: Integer;
    FCtrlName: string;
    FDisplayName: string;
  public
    property DisplayName: string read FDisplayName write FDisplayName;
    property CtrlName: string read FCtrlName write FCtrlName;
    property Index: Integer read FIndex write FIndex;
  end;

  TCnObjectInspector = class(TObject)
  {* 对象属性方法的管理基础类 }
  private
    FObjectAddr: Pointer;
    FProperties: TObjectList;
    FEvents: TObjectList;
    FInspectComplete: Boolean;
    FObjClassName: string;
    FContentTypes: TCnPropContentTypes;
    FComponents: TObjectList;
    FControls: TObjectList;
    FStrings: TCnStringsObject;
    FIsRefresh: Boolean;
    FCollectionItems: TObjectList;
    FMenuItems: TObjectList;
    FHierarchy: string;
    FOnAfterEvaluateHierarchy: TNotifyEvent;
    FOnAfterEvaluateCollections: TNotifyEvent;
    FOnAfterEvaluateMenuItems: TNotifyEvent;
    FOnAfterEvaluateControls: TNotifyEvent;
    FOnAfterEvaluateProperties: TNotifyEvent;
    FOnAfterEvaluateComponents: TNotifyEvent;
    FGraphics: TCnGraphicsObject;
    function GetEventCount: Integer;
    function GetPropCount: Integer;
    function GetInspectComplete: Boolean;
    function GetCompCount: Integer;
    function GetControlCount: Integer;
    function GetCollectionItemCount: Integer;
    procedure SetInspectComplete(const Value: Boolean);
    function GetMenuItemCount: Integer;
  protected
    procedure SetObjectAddr(const Value: Pointer); virtual;
    procedure DoEvaluate; virtual; abstract;
    procedure DoAfterEvaluateComponents; virtual;
    procedure DoAfterEvaluateControls; virtual;
    procedure DoAfterEvaluateCollections; virtual;
    procedure DoAfterEvaluateMenuItems; virtual;
    procedure DoAfterEvaluateProperties; virtual;
    procedure DoAfterEvaluateHierarchy; virtual;    

    function IndexOfProperty(AProperties: TObjectList;
      const APropName: string): TCnPropertyObject;
    function IndexOfEvent(AEvents: TObjectList;
      const AEventName: string): TCnEventObject;
  public
    constructor Create(Data: Pointer); virtual;
    destructor Destroy; override;

    procedure InspectObject;
    procedure Clear;

    function ChangePropertyValue(const PropName, Value: string): Boolean; virtual;
    property ObjectAddr: Pointer read FObjectAddr write SetObjectAddr;
    {* 主要供外部写，写入 Object，或 String }

    property Properties: TObjectList read FProperties;
    property Events: TObjectList read FEvents;
    property Strings: TCnStringsObject read FStrings;
    property Graphics: TCnGraphicsObject read FGraphics;
    property Components: TObjectList read FComponents;
    property Controls: TObjectList read FControls;
    property CollectionItems: TObjectList read FCollectionItems;
    property MenuItems: TObjectList read FMenuItems;

    property PropCount: Integer read GetPropCount;
    property EventCount: Integer read GetEventCount;
    property CompCount: Integer read GetCompCount;
    property ControlCount: Integer read GetControlCount;
    property CollectionItemCount: Integer read GetCollectionItemCount;
    property MenuItemCount: Integer read GetMenuItemCount;

    property IsRefresh: Boolean read FIsRefresh write FIsRefresh;
    property InspectComplete: Boolean read GetInspectComplete
      write SetInspectComplete;

    property ContentTypes: TCnPropContentTypes read FContentTypes
      write FContentTypes;
    property ObjClassName: string read FObjClassName write FObjClassName;
    property Hierarchy: string read FHierarchy write FHierarchy;

    property OnAfterEvaluateProperties: TNotifyEvent
      read FOnAfterEvaluateProperties write FOnAfterEvaluateProperties;
    property OnAfterEvaluateComponents: TNotifyEvent
      read FOnAfterEvaluateComponents write FOnAfterEvaluateComponents;
    property OnAfterEvaluateControls: TNotifyEvent
      read FOnAfterEvaluateControls write FOnAfterEvaluateControls;
    property OnAfterEvaluateCollections: TNotifyEvent
      read FOnAfterEvaluateCollections write FOnAfterEvaluateCollections;
    property OnAfterEvaluateMenuItems: TNotifyEvent
      read FOnAfterEvaluateMenuItems write FOnAfterEvaluateMenuItems;
    property OnAfterEvaluateHierarchy: TNotifyEvent
      read FOnAfterEvaluateHierarchy write FOnAfterEvaluateHierarchy;
  end;

  TCnObjectInspectorClass = class of TCnObjectInspector;

  TCnLocalObjectInspector = class(TCnObjectInspector)
  {* 同一进程内的对象属性方法的管理类 }
  private
    FObjectInstance: TObject;
  protected
    procedure SetObjectAddr(const Value: Pointer); override;

    procedure DoEvaluate; override;
  public
    function ChangePropertyValue(const PropName, Value: string): Boolean; override;
    property ObjectInstance: TObject read FObjectInstance;
  end;

  TCnPropSheetForm = class(TForm)
    pnlTop: TPanel;
    tsSwitch: TTabSet;
    pnlMain: TPanel;
    lvProp: TListView;
    mmoText: TMemo;
    lvEvent: TListView;
    pnlInspectBtn: TPanel;
    btnInspect: TSpeedButton;
    lvCollectionItem: TListView;
    lvComp: TListView;
    lvControl: TListView;
    btnRefresh: TSpeedButton;
    btnTop: TSpeedButton;
    edtObj: TEdit;
    lblDollar: TLabel;
    btnEvaluate: TSpeedButton;
    pnlHierarchy: TPanel;
    pnlGraphic: TPanel;
    imgGraphic: TImage;
    lvMenuItem: TListView;
    edtClassName: TEdit;
    btnLocate: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tsSwitchChange(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure btnInspectClick(Sender: TObject);
    procedure lvPropCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvPropSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvPropCustomDrawSubItem(Sender: TCustomListView;
      Item: TListItem; SubItem: Integer; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnTopClick(Sender: TObject);
    procedure btnEvaluateClick(Sender: TObject);
    procedure edtObjKeyPress(Sender: TObject; var Key: Char);
    procedure lvPropDblClick(Sender: TObject);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnLocateClick(Sender: TObject);
  private
    FListViewHeaderHeight: Integer;
    FContentTypes: TCnPropContentTypes;
    FPropListPtr: PPropList;
    FPropCount: Integer;
    FObjectPointer: Pointer;
    // 指向 Object 实例或 标识字符串
    FInspector: TCnObjectInspector;
    FInspectParam: Pointer;
    FCurrObj: TObject;
    FParentSheetForm: TCnPropSheetForm;
    FHierarchys: TStrings;
    FGraphicObject: TObject;
    FHierPanels: TComponentList;
    FHierLines: TComponentList;
    FOnEvaluateBegin: TNotifyEvent;
    FOnEvaluateEnd: TNotifyEvent;
    FOnAfterEvaluateHierarchy: TNotifyEvent;
    FOnAfterEvaluateCollections: TNotifyEvent;
    FOnAfterEvaluateControls: TNotifyEvent;
    FOnAfterEvaluateProperties: TNotifyEvent;
    FOnAfterEvaluateComponents: TNotifyEvent;

    procedure SetContentTypes(const Value: TCnPropContentTypes);
    procedure SetParentSheetForm(const Value: TCnPropSheetForm);

    procedure UpdateContentTypes;
    procedure UpdateUIStrings;
    procedure UpdateHierarchys;
    procedure UpdatePanelPositions;

    procedure MsgInspectObject(var Msg: TMessage); message CN_INSPECTOBJECT;
    procedure DoEvaluateBegin; virtual;
    procedure DoEvaluateEnd; virtual;

    // 事件转移导出到外面
    procedure AfterEvaluateComponents(Sender: TObject);
    procedure AfterEvaluateControls(Sender: TObject);
    procedure AfterEvaluateCollections(Sender: TObject);
    procedure AfterEvaluateProperties(Sender: TObject);
    procedure AfterEvaluateHierarchy(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure SetPropListSize(const Value: Integer);
    procedure InspectObject(Data: Pointer);
    procedure Clear;
    property ObjectPointer: Pointer read FObjectPointer write FObjectPointer;
    property ContentTypes: TCnPropContentTypes read FContentTypes write SetContentTypes;
    property ParentSheetForm: TCnPropSheetForm read FParentSheetForm write SetParentSheetForm;

    property OnEvaluateBegin: TNotifyEvent read FOnEvaluateBegin write FOnEvaluateBegin;
    property OnEvaluateEnd: TNotifyEvent read FOnEvaluateEnd write FOnEvaluateEnd;
    property OnAfterEvaluateProperties: TNotifyEvent
      read FOnAfterEvaluateProperties write FOnAfterEvaluateProperties;
    property OnAfterEvaluateComponents: TNotifyEvent
      read FOnAfterEvaluateComponents write FOnAfterEvaluateComponents;
    property OnAfterEvaluateControls: TNotifyEvent
      read FOnAfterEvaluateControls write FOnAfterEvaluateControls;
    property OnAfterEvaluateCollections: TNotifyEvent
      read FOnAfterEvaluateCollections write FOnAfterEvaluateCollections;
    property OnAfterEvaluateHierarchy: TNotifyEvent
      read FOnAfterEvaluateHierarchy write FOnAfterEvaluateHierarchy;
  end;

function EvaluatePointer(Address: Pointer; Data: Pointer = nil;
  AForm: TCnPropSheetForm = nil; SyncMode: Boolean = False;
  AParentSheet: TCnPropSheetForm = nil): TCnPropSheetForm;
{* 执行真正的查看，SyncMode 指是否同步查看。默认异步，Form 内自己发消息查看}

function GetPropValueStr(Instance: TObject; PropInfo: PPropInfo): string;

function GetObjValueStr(AObj: TObject): string;

var
  ObjectInspectorClass: TCnObjectInspectorClass = nil;

implementation

{$R *.DFM}

type
  PParamData = ^TParamData;
  TParamData = record
  // Copy from TypInfo
    Flags: TParamFlags;
    ParamName: ShortString;
    TypeName: ShortString;
  end;

  TGraphicConrolAccess = class(TGraphicControl);

  TCnByteSet = set of 0..SizeOf(Byte) * 8 - 1;
  TCnWordSet = set of 0..SizeOf(Word) * 8 - 1;
  TCnDWordSet = set of 0..SizeOf(DWORD) * 8 - 1;

const
  SCnPropContentType: array[TCnPropContentType] of string =
    ('Properties', 'Events', 'CollectionItems', 'MenuItems', 'Strings', 'Graphics',
     'Components', 'Controls', 'Hierarchy');

  SCnInputNewValueCaption = 'Modify Value';
  SCnInputNewValuePrompt = 'Enter a New Value for %s:';
  SCnErrorChangeValue = 'Change Property Value Failed!';

  CnCanModifyPropTypes: TTypeKinds =
    [tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet, tkWChar,
    tkLString, tkWString, tkInt64];

var
  FSheetList: TComponentList = nil;

  CnFormLeft: Integer = 50;
  CnFormTop: Integer = 50;
  Closing: Boolean = False;

// 根据 set 值与 set 的类型获得 set 的字符串，TypInfo 参数必须是枚举的类型，
// 而不能是 set of 后的类型，如无 TypInfo，则返回数值
function GetSetStr(TypInfo: PTypeInfo; Value: Integer): string;
var
  I: Integer;
  S: TIntegerSet;
begin
  Result := '';
  if Value = 0 then
    Exit;

  Integer(S) := Value;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
  begin
    if I in S then
    begin
      if Result <> '' then
        Result := Result + ',';

      if TypInfo = nil then
        Result := Result + IntToStr(I)
      else
        Result := Result + GetEnumName(TypInfo, I);
    end;
  end;
end;

function IndexOfContentTypeStr(const AStr: string): TCnPropContentType;
var
  I: TCnPropContentType;
begin
  Result := pctProps;
  for I := Low(TCnPropContentType) to High(TCnPropContentType) do
    if AStr = SCnPropContentType[I] then
    begin
      Result := I;
      Exit;
    end;
end;

function EvaluatePointer(Address: Pointer; Data: Pointer = nil;
  AForm: TCnPropSheetForm = nil; SyncMode: Boolean = False;
  AParentSheet: TCnPropSheetForm = nil): TCnPropSheetForm;
begin
  Result := nil;
  if Address = nil then Exit;

  if AForm = nil then
    AForm := TCnPropSheetForm.Create(nil);

  AForm.ObjectPointer := Address;
  AForm.Clear;
  AForm.ParentSheetForm := AParentSheet;

  if SyncMode then
  begin
    AForm.DoEvaluateBegin;
    try
      AForm.FInspectParam := Data;
      AForm.InspectObject(AForm.FInspectParam);
    finally
      AForm.DoEvaluateEnd;
      AForm.Show;  // After Evaluation. Show the form.
    end;
  end
  else
    PostMessage(AForm.Handle, CN_INSPECTOBJECT, WPARAM(Data), 0);

  Result := AForm;
end;

function PropInfoName(PropInfo: PPropInfo): string;
begin
  Result := string(PropInfo^.Name);
end;

function GetObjValueStr(AObj: TObject): string;
var
  S: string;
begin
  if AObj <> nil then
  begin
    try
      S := AObj.ClassName;
    except
      S := 'Unknown Object';
    end;
    Result := Format('(%s.$%8.8x)', [S, Integer(AObj)])
  end
  else
    Result := 'nil';
end;

function GetPropValueStr(Instance: TObject; PropInfo: PPropInfo): string;
var
  iTmp: Integer;
  S: string;
  IntToId: TIntToIdent;

  function GetParamFlagsName(AParamFlags: TParamFlags): string;
  const
    SParamFlag: array[TParamFlag] of string
      = ('var', 'const', 'array of', 'address', '', 'out'{$IFDEF COMPILER14_UP}, 'result'{$ENDIF});
  var
    I: TParamFlag;
  begin
    Result := '';
    for I := Low(TParamFlag) to High(TParamFlag) do
    begin
      if (I <> pfAddress) and (I in AParamFlags) then
        Result := Result + SParamFlag[I];
    end;
  end;

  function GetMethodDeclare: string;
  var
    CompName, MthName: string;
    TypeStr: PShortString;
    T: PTypeData;
    P: PParamData;
    I: Integer;
    AMethod: TMethod;
  begin
    CompName := '*';
    if Instance is TComponent then
      CompName := (Instance as TComponent).Name;
    MthName := PropInfoName(PropInfo);

    AMethod := GetMethodProp(Instance, PropInfo);
    if AMethod.Data <> nil then
    begin
      try
        MthName := TObject(AMethod.Data).MethodName(AMethod.Code);
        if TObject(AMethod.Data) is TComponent then
          CompName := (TObject(AMethod.Data) as TComponent).Name;
      except
        ;
      end;
    end;

    T := GetTypeData(PropInfo^.PropType^);

    if T^.MethodKind = mkFunction then
      Result := Result + 'function ' + CompName + '.' + MthName + '('
    else
      Result := Result + 'procedure ' + CompName + '.' + MthName + '(';

    P := PParamData(@T^.ParamList);
    for I := 1 to T^.ParamCount do
    begin
      TypeStr := Pointer(Integer(@P^.ParamName) + Length(P^.ParamName) + 1);
      if Pos('array of', GetParamFlagsName(P^.Flags)) > 0 then
        Result := Result + Trim(Format('%s: %s %s;', [(P^.ParamName),
          (GetParamFlagsName(P^.Flags)), TypeStr^]))
      else
        Result := Result + trim(Format('%s %s: %s;', [(GetParamFlagsName(P^.Flags)),
          (P^.ParamName), TypeStr^]));
      P := PParamData(Integer(P) + SizeOf(TParamFlags) +
        Length(P^.ParamName) + Length(TypeStr^) + 2);
    end;

    Delete(Result, Length(Result), 1);
    Result := Result + ')';
    if T^.MethodKind = mkFunction then
      Result := Result + ': ' + string(PShortString(P)^);
    Result := Result + ';';
  end;

begin
  Result := '';
  case PropInfo^.PropType^^.Kind of
    tkInteger:
      begin
        S := IntToStr(GetOrdProp(Instance, PropInfo));
        IntToId := FindIntToIdent(PropInfo^.PropType^);
        if Assigned(IntToId) and IntToId(GetOrdProp(Instance, PropInfo), S) then
        else
        begin
          if PropInfo^.PropType^^.Name = 'TColor' then
            S := Format('$%8.8x', [GetOrdProp(Instance, PropInfo)])
          else
            S := IntToStr(GetOrdProp(Instance, PropInfo));
        end;
      end;
    tkChar, tkWChar:
      S := IntToStr(GetOrdProp(Instance, PropInfo));
    tkClass:
      begin
        iTmp := GetOrdProp(Instance, PropInfo);
        S := GetObjValueStr(TObject(iTmp));
      end;
    tkEnumeration:
      S := GetEnumProp(Instance, PropInfo);
    tkSet:
      S := GetSetProp(Instance, PropInfo);
    tkFloat:
      S := FloatToStr(GetFloatProp(Instance, PropInfo));
    tkMethod:
      begin
        iTmp := GetOrdProp(Instance, PropInfo);
        if iTmp <> 0 then
          S := Format('%s: ($%8.8x, $%8.8x): %s', [PropInfo^.PropType^^.Name,
            Integer(GetMethodProp(Instance, PropInfo).Code),
            Integer(GetMethodProp(Instance, PropInfo).Data),
             GetMethodDeclare()])
        else
          S := 'nil';
      end;
    tkString, tkLString, tkWString{$IFDEF UNICODE_STRING}, tkUString{$ENDIF}:
      S := GetStrProp(Instance, PropInfo);
    tkVariant:
      S := VarToStr(GetVariantProp(Instance, PropInfo));
    tkInt64:
      S := FloatToStr(GetInt64Prop(Instance, PropInfo) + 0.0);
  end;
  Result := S;
end;

{ TCnStringsObject }

procedure TCnStringsObject.Clear;
begin
  FDisplayValue := '';
  Changed := False;
end;

{ TCnObjectInspector }

procedure TCnObjectInspector.Clear;
begin
  FStrings.DisplayValue := '';
  FObjClassName := '';
  FInspectComplete := False;
  FContentTypes := [];
end;

constructor TCnObjectInspector.Create(Data: Pointer);
begin
  inherited Create;
  FProperties := TObjectList.Create(True);
  FEvents := TObjectList.Create(True);
  FStrings := TCnStringsObject.Create;
  FGraphics := TCnGraphicsObject.Create;
  FComponents := TObjectList.Create(True);
  FControls := TObjectList.Create(True);
  FCollectionItems := TObjectList.Create(True);
  FMenuItems := TObjectList.Create(True);
end;

destructor TCnObjectInspector.Destroy;
begin
  FMenuItems.Free;
  FCollectionItems.Free;
  FControls.Free;
  FComponents.Free;
  FGraphics.Free;
  FStrings.Free;
  FEvents.Free;
  FProperties.Free;
  inherited;
end;

function TCnObjectInspector.GetCompCount: Integer;
begin
  Result := FComponents.Count;
end;

function TCnObjectInspector.GetControlCount: Integer;
begin
  Result := FControls.Count;
end;

function TCnObjectInspector.GetEventCount: Integer;
begin
  Result := FEvents.Count;
end;

function TCnObjectInspector.GetPropCount: Integer;
begin
  Result := FProperties.Count;
end;

function TCnObjectInspector.GetCollectionItemCount: Integer;
begin
  Result := FCollectionItems.Count;
end;

function TCnObjectInspector.GetInspectComplete: Boolean;
begin
  Result := FInspectComplete;
end;

procedure TCnObjectInspector.InspectObject;
begin
  if FObjectAddr <> nil then
  begin
    Clear;
    try
      DoEvaluate;
    except
      Include(FContentTypes, pctProps);
      FInspectComplete := True;
    end;
  end;
end;

procedure TCnObjectInspector.SetObjectAddr(const Value: Pointer);
begin
  if FObjectAddr <> Value then
  begin
    FObjectAddr := Value;
    Clear;
  end;
end;

procedure TCnObjectInspector.SetInspectComplete(const Value: Boolean);
begin
  FInspectComplete := Value;
end;

function TCnObjectInspector.IndexOfEvent(AEvents: TObjectList;
  const AEventName: string): TCnEventObject;
var
  I: Integer;
  AEvent: TCnEventObject;
begin
  Result := nil;
  if AEvents <> nil then
  begin
    for I := 0 to AEvents.Count - 1 do
    begin
      AEvent := TCnEventObject(AEvents.Items[I]);
      if AEvent.EventName = AEventName then
      begin
        Result := AEvent;
        Exit;
      end;
    end;
  end;
end;

function TCnObjectInspector.IndexOfProperty(AProperties: TObjectList;
  const APropName: string): TCnPropertyObject;
var
  I: Integer;
  AProp: TCnPropertyObject;
begin
  Result := nil;
  if AProperties <> nil then
  begin
    for I := 0 to AProperties.Count - 1 do
    begin
      AProp := TCnPropertyObject(AProperties.Items[I]);
      if AProp.PropName = APropName then
      begin
        Result := AProp;
        Exit;
      end;
    end;
  end;
end;

procedure TCnObjectInspector.DoAfterEvaluateCollections;
begin
  if Assigned(FOnAfterEvaluateCollections) then
    FOnAfterEvaluateCollections(Self);
end;

procedure TCnObjectInspector.DoAfterEvaluateComponents;
begin
  if Assigned(FOnAfterEvaluateComponents) then
    FOnAfterEvaluateComponents(Self);
end;

procedure TCnObjectInspector.DoAfterEvaluateControls;
begin
  if Assigned(FOnAfterEvaluateControls) then
    FOnAfterEvaluateControls(Self);
end;

procedure TCnObjectInspector.DoAfterEvaluateHierarchy;
begin
  if Assigned(FOnAfterEvaluateHierarchy) then
    FOnAfterEvaluateHierarchy(Self);
end;

procedure TCnObjectInspector.DoAfterEvaluateProperties;
begin
  if Assigned(FOnAfterEvaluateProperties) then
    FOnAfterEvaluateProperties(Self);
end;

function TCnObjectInspector.GetMenuItemCount: Integer;
begin
  Result := FMenuItems.Count;
end;

procedure TCnObjectInspector.DoAfterEvaluateMenuItems;
begin
  if Assigned(FOnAfterEvaluateMenuItems) then
    FOnAfterEvaluateMenuItems(Self);
end;

function TCnObjectInspector.ChangePropertyValue(const PropName,
  Value: string): Boolean;
begin
  Result := False;
end;

{ TCnLocalObjectInspector }

procedure TCnLocalObjectInspector.SetObjectAddr(const Value: Pointer);
begin
  IsRefresh := (Value = FObjectAddr);
  inherited;

  try
    FObjectInstance := TObject(Value);
  except
    FObjectInstance := nil;
    FObjClassName := 'Unknown Object';
  end;
end;

procedure TCnLocalObjectInspector.DoEvaluate;
var
  PropListPtr: PPropList;
  I, APropCount: Integer;
  PropInfo: PPropInfo;
  AProp: TCnPropertyObject;
  AEvent: TCnEventObject;
  ACollection: TCollection;
  AMenuItem: TMenuItem;
  AComp: TComponent;
  AControl: TWinControl;
  AItemObj: TCnCollectionItemObject;
  AMenuObj: TCnMenuItemObject;
  ACompObj: TCnComponentObject;
  AControlObj: TCnControlObject;
  S: string;
  IsExisting: Boolean;
  Hies: TStrings;
  ATmpClass: TClass;
  ByteSet: TCnByteSet;
  WordSet: TCnWordSet;

  procedure AddNewProp(Str: string; AProperty: TCnPropertyObject);
  begin
    if Str <> AProperty.DisplayValue then
    begin
      AProperty.DisplayValue := Str;
      AProperty.Changed := True;
    end
    else
      AProperty.Changed := False;

    if not IsRefresh then
      Properties.Add(AProperty);
  end;

begin
  if ObjectInstance <> nil then
  begin
    if not IsRefresh then
    begin
      Properties.Clear;
      Events.Clear;
      Components.Clear;
      Controls.Clear;
      CollectionItems.Clear;
      Strings.Clear;
      MenuItems.Clear;
      Graphics.Graphic := nil;
    end;

    ContentTypes := [pctHierarchy];

    ObjClassName := FObjectInstance.ClassName;

    Hies := TStringList.Create;
    ATmpClass := ObjectInstance.ClassType;
    Hies.Add(ATmpClass.ClassName);
    while ATmpClass.ClassParent <> nil do
    begin
      ATmpClass := ATmpClass.ClassParent;
      Hies.Add(ATmpClass.ClassName);
    end;
    Hierarchy := Hies.Text;
    Hies.Free;

    DoAfterEvaluateHierarchy;

    if ObjectInstance is TStrings then
    begin
      Include(FContentTypes, pctStrings);
      if Strings.DisplayValue <> (FObjectInstance as TStrings).Text then
      begin
        Strings.Changed := True;
        Strings.DisplayValue := (FObjectInstance as TStrings).Text;
      end;
    end;

    APropCount := GetTypeData(PTypeInfo(FObjectInstance.ClassInfo))^.PropCount;
    GetMem(PropListPtr, APropCount * SizeOf(Pointer));
    GetPropList(PTypeInfo(FObjectInstance.ClassInfo), tkAny, PropListPtr);

    for I := 0 to APropCount - 1 do
    begin
      PropInfo := PropListPtr^[I];
      if PropInfo^.PropType^^.Kind in tkProperties then
      begin
        if not IsRefresh then
          AProp := TCnPropertyObject.Create
        else
          AProp := IndexOfProperty(Properties, PropInfoName(PropInfo));

        AProp.PropName := PropInfoName(PropInfo);
        AProp.PropType := PropInfo^.PropType^^.Kind;
        AProp.IsObject := AProp.PropType = tkClass;

        // 有写入权限，并且指定类型，才可修改，否则界面上没法整
        AProp.CanModify := (PropInfo^.SetProc <> nil) and (PropInfo^.PropType^^.Kind
          in CnCanModifyPropTypes);

        AProp.PropValue := GetPropValue(FObjectInstance, PropInfoName(PropInfo));
        if AProp.IsObject then
          AProp.ObjValue := GetObjectProp(FObjectInstance, PropInfo)
        else
          AProp.ObjValue := nil;

        S := GetPropValueStr(FObjectInstance, PropInfo);
        if S <> AProp.DisplayValue then
        begin
          AProp.DisplayValue := S;
          AProp.Changed := True;
        end
        else
          AProp.Changed := False;

        if not IsRefresh then
          Properties.Add(AProp);

        Include(FContentTypes, pctProps);
      end;

      if PropInfo^.PropType^^.Kind = tkMethod then
      begin
        if not IsRefresh then
          AEvent := TCnEventObject.Create
        else
          AEvent := IndexOfEvent(FEvents, PropInfoName(PropInfo));

        AEvent.EventName := PropInfoName(PropInfo);
        AEvent.EventType := VarToStr(GetPropValue(FObjectInstance, PropInfoName(PropInfo)));
        S := GetPropValueStr(FObjectInstance, PropInfo);
        if S <> AEvent.DisplayValue then
        begin
          AEvent.DisplayValue := S;
          AEvent.Changed := True;
        end
        else
          AEvent.Changed := False;

        if not IsRefresh then
          FEvents.Add(AEvent);

        Include(FContentTypes, pctEvents);
      end;
    end;
    FreeMem(PropListPtr);

    // 额外添加显示不在 published 域的一些已知的公用属性
    if ObjectInstance is TComponent then
    begin
      // 添加 Component 的 Owner
      if not IsRefresh then
        AProp := TCnPropertyObject.Create
      else
        AProp := IndexOfProperty(Properties, 'Owner');

      AProp.PropName := 'Owner';
      AProp.PropType := tkClass;
      AProp.IsObject := True;
      AProp.PropValue := Integer((FObjectInstance as TComponent).Owner);
      AProp.ObjValue := (FObjectInstance as TComponent).Owner;

      S := GetObjValueStr(AProp.ObjValue);
      if S <> AProp.DisplayValue then
      begin
        AProp.DisplayValue := S;
        AProp.Changed := True;
      end
      else
        AProp.Changed := False;

      if not IsRefresh then
        Properties.Add(AProp);

      // 添加 Component 的 ComponentIndex
      if not IsRefresh then
        AProp := TCnPropertyObject.Create
      else
        AProp := IndexOfProperty(Properties, 'ComponentIndex');

      AProp.PropName := 'ComponentIndex';
      AProp.PropType := tkInteger;
      AProp.IsObject := False;
      AProp.PropValue := (FObjectInstance as TComponent).ComponentIndex;
      AProp.ObjValue := nil;

      S := IntToStr(AProp.PropValue);
      AddNewProp(S, AProp);

      // 添加 Component 的 ComponentState
      if not IsRefresh then
        AProp := TCnPropertyObject.Create
      else
        AProp := IndexOfProperty(Properties, 'ComponentState');

      AProp.PropName := 'ComponentState';
      AProp.PropType := tkSet;
      AProp.IsObject := False;
      WordSet := TCnWordSet((FObjectInstance as TComponent).ComponentState);
      AProp.PropValue := Word(WordSet);
      AProp.ObjValue := nil;

      S := GetSetStr(nil, AProp.PropValue);
      AddNewProp(S, AProp);

      // 添加 Component 的 ComponentStyle
      if not IsRefresh then
        AProp := TCnPropertyObject.Create
      else
        AProp := IndexOfProperty(Properties, 'ComponentStyle');

      AProp.PropName := 'ComponentStyle';
      AProp.PropType := tkSet;
      AProp.IsObject := False;
      ByteSet := TCnByteSet((FObjectInstance as TComponent).ComponentStyle);
      AProp.PropValue := Byte(ByteSet);
      AProp.ObjValue := nil;

      S := GetSetStr(nil, AProp.PropValue);
      AddNewProp(S, AProp);
    end;

    if ObjectInstance is TControl then
    begin
      if not IsRefresh then
        AProp := TCnPropertyObject.Create
      else
        AProp := IndexOfProperty(Properties, 'Parent');

      AProp.PropName := 'Parent';
      AProp.PropType := tkClass;
      AProp.IsObject := True;
      AProp.PropValue := Integer((FObjectInstance as TControl).Parent);
      AProp.ObjValue := (FObjectInstance as TControl).Parent;

      S := GetObjValueStr(AProp.ObjValue);
      if S <> AProp.DisplayValue then
      begin
        AProp.DisplayValue := S;
        AProp.Changed := True;
      end
      else
        AProp.Changed := False;

      if not IsRefresh then
        Properties.Add(AProp);
    end;

    DoAfterEvaluateProperties;

    // 处理 CollectionItem，Components 和 Controls，取来直接比较是否 Changed 即可。
    if ObjectInstance is TCollection then
    begin
      // 获得其 Items
      ACollection := (FObjectInstance as TCollection);
      for I := 0 to ACollection.Count - 1 do
      begin
        IsExisting := IsRefresh and (I < FCollectionItems.Count);
        if IsExisting then
          AItemObj := TCnCollectionItemObject(FCollectionItems.Items[I])
        else
          AItemObj := TCnCollectionItemObject.Create;

        AItemObj.ObjClassName := ACollection.ItemClass.ClassName;
        AItemObj.Index := I;
        if AItemObj.ObjValue <> ACollection.Items[I] then
        begin
          AItemObj.ObjValue := ACollection.Items[I];
          AItemObj.Changed := True;
        end
        else
          AItemObj.Changed := False;

        S := ACollection.GetNamePath;
        if S = '' then S := '*';
        AItemObj.ItemName := Format('%s.Item[%d]', [S, I]);
        AItemObj.DisplayValue := Format('%s: $%8.8x', [AItemObj.ObjClassName, Integer(AItemObj.ObjValue)]);

        if not IsExisting then
          CollectionItems.Add(AItemObj);

        Include(FContentTypes, pctCollectionItems);
      end;

      DoAfterEvaluateCollections;
    end
    else if ObjectInstance is TMenuItem then
    begin
      // 获得其 Items
      AMenuItem := (FObjectInstance as TMenuItem);
      for I := 0 to AMenuItem.Count - 1 do
      begin
        IsExisting := IsRefresh and (I < FMenuItems.Count);
        if IsExisting then
          AMenuObj := TCnMenuItemObject(FMenuItems.Items[I])
        else
          AMenuObj := TCnMenuItemObject.Create;

        AMenuObj.ObjClassName := AMenuItem.ClassName;
        AMenuObj.Index := I;
        if AMenuObj.ObjValue <> AMenuItem.Items[I] then
        begin
          AMenuObj.ObjValue := AMenuItem.Items[I];
          AMenuObj.Changed := True;
        end
        else
          AMenuObj.Changed := False;

        S := AMenuItem.GetNamePath;
        if S = '' then S := '(noname)';
        AMenuObj.ItemName := Format('%s.Item[%d]', [S, I]);
        AMenuObj.DisplayValue := Format('%s: $%8.8x', [AMenuObj.ObjClassName, Integer(AMenuObj.ObjValue)]);

        if not IsExisting then
          FMenuItems.Add(AMenuObj);

        Include(FContentTypes, pctMenuItems);
      end;

      DoAfterEvaluateMenuItems;
    end
    else if ObjectInstance is TComponent then
    begin
      // 获得其 Componets
      AComp := (FObjectInstance as TComponent);
      for I := 0 to AComp.ComponentCount - 1 do
      begin
        IsExisting := IsRefresh and (I < FComponents.Count);
        if IsExisting then
          ACompObj := TCnComponentObject(FComponents.Items[I])
        else
          ACompObj := TCnComponentObject.Create;

        ACompObj.ObjClassName := AComp.Components[I].ClassName;
        ACompObj.CompName := AComp.Components[I].Name;
        ACompObj.Index := I;
        if ACompObj.ObjValue <> AComp.Components[I] then
        begin
          ACompObj.ObjValue := AComp.Components[I];
          ACompObj.Changed := True;
        end
        else
          ACompObj.Changed := False;

        ACompObj.DisplayName := Format('%s.Components[%d]', [AComp.Name, I]);
        ACompObj.DisplayValue := Format('%s: %s: $%8.8x', [ACompObj.CompName,
          ACompObj.ObjClassName, Integer(ACompObj.ObjValue)]);

        if not IsExisting then
          Components.Add(ACompObj);

        Include(FContentTypes, pctComponents);
      end;

      DoAfterEvaluateComponents;

      // 获得其 Controls
      if ObjectInstance is TWinControl then
      begin
        AControl:= (FObjectInstance as TWinControl);
        for I := 0 to AControl.ControlCount - 1 do
        begin
          IsExisting := IsRefresh and (I < FControls.Count);
          if IsExisting then
            AControlObj := TCnControlObject(FControls.Items[I])
          else
            AControlObj := TCnControlObject.Create;

          AControlObj.ObjClassName := AControl.Controls[I].ClassName;
          AControlObj.CtrlName := AControl.Controls[I].Name;
          AControlObj.Index := I;
          if AControlObj.ObjValue <> AControl.Controls[I] then
          begin
            AControlObj.ObjValue := AControl.Controls[I];
            AControlObj.Changed := True;
          end
          else
            AControlObj.Changed := False;

          AControlObj.DisplayName := Format('%s.Controls[%d]', [AControl.Name, I]);
          AControlObj.DisplayValue := Format('%s: %s: $%8.8x', [AControlObj.CtrlName,
            AControlObj.ObjClassName, Integer(AControlObj.ObjValue)]);

          if not IsExisting then
            Controls.Add(AControlObj);

          Include(FContentTypes, pctControls);
        end;

        DoAfterEvaluateControls;
      end;

      // 如果是 ImageList，画其子图片
      if ObjectInstance is TImageList then
      begin
        FGraphics.Graphic := ObjectInstance;
        Include(FContentTypes, pctGraphics);
      end;
    end
    else if (ObjectInstance is TGraphic) or (ObjectInstance is TPicture) then
    begin // 处理图像数据
      FGraphics.Graphic := ObjectInstance;
      Include(FContentTypes, pctGraphics);
    end;
  end;

  if FContentTypes = [] then
    Include(FContentTypes, pctProps);
  FInspectComplete := True;
end;

function TCnLocalObjectInspector.ChangePropertyValue(const PropName,
  Value: string): Boolean;
var
  PropInfo: PPropInfo;
  VInt: Integer;
  VInt64: Int64;
  VFloat: Double;
begin
  Result := False;
  if ObjectInstance = nil then
    Exit;

  PropInfo := GetPropInfo(ObjectInstance, PropName);
  if (PropInfo = nil) or (PropInfo^.SetProc = nil) then
    Exit;

  case PropInfo^.PropType^^.Kind of
    tkInteger:
      begin
        try
          VInt := StrToInt(Value);
          SetOrdProp(ObjectInstance, PropName, VInt);
        except
          Exit;
        end;
      end;
    tkInt64:
      begin
        try
          VInt64 := StrToInt(Value);
          SetOrdProp(ObjectInstance, PropName, VInt64);
        except
          Exit;
        end;
      end;
    tkFloat:
      begin
        try
          VFloat := StrToFloat(Value);
          SetFloatProp(ObjectInstance, PropName, VFloat);
        except
          Exit;
        end;
      end;
    tkChar,
    tkWChar,
    tkLString,
    tkWString,
    tkString:
      begin
        SetStrProp(ObjectInstance, PropName, Value);
      end;
    tkEnumeration:
      begin
        SetEnumProp(ObjectInstance, PropName, Value);
      end;
    tkSet:
      begin
        SetSetProp(ObjectInstance, PropName, Value);
      end;
    else
      Exit;
  end;
  Result := True;
end;

{ TCnPropSheetForm }

procedure TCnPropSheetForm.FormCreate(Sender: TObject);
begin
  FSheetList.Add(Self);
  FContentTypes := [];
  FHierarchys := TStringList.Create;
  FHierPanels := TComponentList.Create(True);
  FHierLines := TComponentList.Create(True);
  btnInspect.Height := lvProp.Canvas.TextHeight('lj') - 1;
  btnInspect.Width := btnInspect.Height;
  UpdateUIStrings;

  // FListViewHeaderHeight := ListView_GetItemSpacing(lvProp.Handle, 1);
  FListViewHeaderHeight := 8; // 列头高度
  
  Left := CnFormLeft;
  Top := CnFormTop;
  Inc(CnFormLeft, 20);
  Inc(CnFormTop, 20);
  if CnFormLeft >= Screen.Width - Self.Width - 30 then CnFormLeft := 50;
  if CnFormTop >= Screen.Height - Self.Height - 30 then CnFormTop := 50;
end;

procedure TCnPropSheetForm.InspectObject(Data: Pointer);
const
  IMG_MARGIN = 15;
  IMG_INTERVAL = 30;
var
  I, ImgTop, ImgLeft: Integer;
  ImageList: TImageList;

  procedure InternalDrawGraphic(AGraphic: TGraphic);
  const
    EMPTY_STR = '<Empty>';
    MARGIN = 10;
  var
    W, H: Integer;
    S: string;
  begin
    if AGraphic = nil then
      Exit;

    imgGraphic.Canvas.Draw(0, 0, AGraphic);
    imgGraphic.Canvas.Pen.Color := clBtnShadow;
    if AGraphic.Empty then
      S := EMPTY_STR
    else
      S := Format('Width: %d, Height: %d.', [AGraphic.Width, AGraphic.Height]);

    W := imgGraphic.Canvas.TextWidth(S);
    H := imgGraphic.Canvas.TextHeight(S);

    imgGraphic.Canvas.TextOut(imgGraphic.Width - MARGIN - W,
      imgGraphic.Height - MARGIN - H, S);
  end;

begin
  if ObjectInspectorClass = nil then
    Exit;

  if FInspector = nil then
  begin
    FInspector := TCnObjectInspector(ObjectInspectorClass.NewInstance);
    FInspector.Create(Data);
  end;

  // 接收内部事件
  FInspector.OnAfterEvaluateProperties := AfterEvaluateProperties;
  FInspector.OnAfterEvaluateComponents := AfterEvaluateComponents;
  FInspector.OnAfterEvaluateControls := AfterEvaluateControls;
  FInspector.OnAfterEvaluateCollections := AfterEvaluateCollections;
  FInspector.OnAfterEvaluateHierarchy := AfterEvaluateHierarchy;
  
  FInspector.ObjectAddr := FObjectPointer;
  FInspector.InspectObject;

  while not FInspector.InspectComplete do
    Application.ProcessMessages;

  lvProp.Items.Clear;
  lvEvent.Items.Clear;
  lvCollectionItem.Items.Clear;
  lvMenuItem.Items.Clear;
  lvComp.Items.Clear;
  lvControl.Items.Clear;

  if FInspector.ObjClassName <> '' then
    edtClassName.Text := FInspector.ObjClassName
  else
    edtClassName.Text := 'Unknown Object';

  edtObj.Text := Format('%8.8x', [Integer(FInspector.ObjectAddr)]);
  edtClassName.Text := Format('%s: $%8.8x', [edtClassName.Text, Integer(FInspector.ObjectAddr)]);

  for I := 0 to FInspector.PropCount - 1 do
  begin
    with lvProp.Items.Add do
    begin
      Data := FInspector.Properties.Items[I];
      Caption := TCnPropertyObject(FInspector.Properties.Items[I]).PropName;
      SubItems.Add(TCnPropertyObject(FInspector.Properties.Items[I]).DisplayValue);
    end;
  end;

  for I := 0 to FInspector.EventCount - 1 do
  begin
    with lvEvent.Items.Add do
    begin
      Data := FInspector.Events.Items[I];
      Caption := TCnEventObject(FInspector.Events.Items[I]).EventName;
      SubItems.Add(TCnEventObject(FInspector.Events.Items[I]).DisplayValue);
    end;
  end;

  for I := 0 to FInspector.CollectionItemCount - 1 do
  begin
    with lvCollectionItem.Items.Add do
    begin
      Data := FInspector.CollectionItems.Items[I];
      Caption := TCnCollectionItemObject(FInspector.CollectionItems.Items[I]).ItemName;
      SubItems.Add(TCnCollectionItemObject(FInspector.CollectionItems.Items[I]).DisplayValue);
    end;
  end;

  for I := 0 to FInspector.MenuItemCount - 1 do
  begin
    with lvMenuItem.Items.Add do
    begin
      Data := FInspector.MenuItems.Items[I];
      Caption := TCnMenuItemObject(FInspector.MenuItems.Items[I]).ItemName;
      SubItems.Add(TCnMenuItemObject(FInspector.MenuItems.Items[I]).DisplayValue);
    end;
  end;

  for I := 0 to FInspector.CompCount - 1 do
  begin
    with lvComp.Items.Add do
    begin
      Data := FInspector.Components.Items[I];
      Caption := TCnComponentObject(FInspector.Components.Items[I]).DisplayName;
      SubItems.Add(TCnComponentObject(FInspector.Components.Items[I]).DisplayValue);
    end;
  end;

  for I := 0 to FInspector.ControlCount - 1 do
  begin
    with lvControl.Items.Add do
    begin
      Data := FInspector.Controls.Items[I];
      Caption := TCnControlObject(FInspector.Controls.Items[I]).DisplayName;
      SubItems.Add(TCnControlObject(FInspector.Controls.Items[I]).DisplayValue);
    end;
  end;

  mmoText.Lines.Text := FInspector.Strings.DisplayValue;
  FHierarchys.Text := FInspector.Hierarchy;
  FGraphicObject := FInspector.Graphics.Graphic;

  imgGraphic.Canvas.FillRect(imgGraphic.Canvas.ClipRect);
  if FGraphicObject <> nil then
  begin
    if FGraphicObject is TPicture then
    begin
      if (FGraphicObject as TPicture).Graphic <> nil then
        InternalDrawGraphic((FGraphicObject as TPicture).Graphic);
    end
    else if FGraphicObject is TGraphic then
    begin
      InternalDrawGraphic(FGraphicObject as TGraphic);
    end
    else if FGraphicObject is TImageList then
    begin
      if (FGraphicObject as TImageList).Count > 0 then
      begin
        // TODO: 根据 ImageList 尺寸以及 Image 尺寸来排版绘制
        ImageList := FGraphicObject as TImageList;
        ImgLeft := IMG_MARGIN;
        ImgTop := IMG_MARGIN;

        for I := 0 to ImageList.Count - 1 do
        begin
          ImageList.Draw(imgGraphic.Canvas, ImgLeft, ImgTop, I);
          Inc(ImgLeft, ImageList.Width + IMG_INTERVAL);
          if ImgLeft > imgGraphic.Width then
          begin
            Inc(ImgTop, ImageList.Height + IMG_INTERVAL);
            ImgLeft := IMG_MARGIN;
          end;
        end;
      end;
    end;
  end;

  UpdateHierarchys;
  ContentTypes := FInspector.ContentTypes;

  btnLocate.Visible := (TObject(FInspector.ObjectAddr) is TGraphicControl) or
    (TObject(FInspector.ObjectAddr) is TWinControl); 
end;

procedure TCnPropSheetForm.SetContentTypes(const Value: TCnPropContentTypes);
begin
  if Value <> FContentTypes then
  begin
    FContentTypes := Value;
    UpdateContentTypes;
  end;
end;

procedure TCnPropSheetForm.SetPropListSize(const Value: Integer);
begin
  if (FPropListPtr <> nil) and (FPropCount > 0) then
  begin
    FreeMem(FPropListPtr, FPropCount * SizeOf(Pointer));
    FPropCount := 0;
    FPropListPtr := nil;
  end;

  if Value > 0 then
  begin
    GetMem(FPropListPtr, Value * SizeOf(Pointer));
    FPropCount := Value;
  end;
end;

procedure TCnPropSheetForm.UpdateContentTypes;
var
  AType: TCnPropContentType;
begin
  tsSwitch.Tabs.Clear;
  for AType := Low(TCnPropContentType) to High(TCnPropContentType) do
    if AType in FContentTypes then
      tsSwitch.Tabs.Add(SCnPropContentType[AType]);

  tsSwitch.TabIndex := 0;
end;

procedure TCnPropSheetForm.UpdateUIStrings;
begin

end;

procedure TCnPropSheetForm.FormResize(Sender: TObject);
var
  FixWidth: Integer;
begin
  if Parent <> nil then
    FixWidth := 20
  else
    FixWidth := 28;

  lvProp.Columns[1].Width := Self.Width - lvProp.Columns[0].Width - FixWidth;
  lvEvent.Columns[1].Width := Self.Width - lvEvent.Columns[0].Width - FixWidth;
  lvCollectionItem.Columns[1].Width := Self.Width - lvCollectionItem.Columns[0].Width - FixWidth;
  lvMenuItem.Columns[1].Width := Self.Width - lvMenuItem.Columns[0].Width - FixWidth;
  lvComp.Columns[1].Width := Self.Width - lvComp.Columns[0].Width - FixWidth;
  lvControl.Columns[1].Width := Self.Width - lvControl.Columns[0].Width - FixWidth;
  UpdatePanelPositions;
end;

procedure TCnPropSheetForm.FormDestroy(Sender: TObject);
begin
  FHierarchys.Free;
  FHierLines.Free;
  FHierPanels.Free;
  if FInspector <> nil then
    FreeAndNil(FInspector);
end;

procedure TCnPropSheetForm.Clear;
begin
  mmoText.Lines.Clear;
  UpdateUIStrings;
end;

procedure TCnPropSheetForm.UpdateHierarchys;
var
  I: Integer;
  APanel: TPanel;
  ABevel: TBevel;
begin
  // 根据 FHierarchys 绘制 Hierarchy 图
  FHierPanels.Clear;
  for I := 0 to FHierarchys.Count - 1 do
  begin
    APanel := TPanel.Create(nil);
    APanel.Caption := FHierarchys.Strings[I];
    APanel.BevelOuter := bvNone;
    APanel.BevelInner := bvRaised;
    APanel.Parent := pnlHierarchy;
    APanel.Color := clBtnFace;
    FHierPanels.Add(APanel);

    ABevel := TBevel.Create(nil);
    ABevel.Shape := bsLeftLine;
    ABevel.Parent := pnlHierarchy;
    FHierLines.Add(ABevel);
  end;
  UpdatePanelPositions;
end;

procedure TCnPropSheetForm.tsSwitchChange(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
var
  AControl: TWinControl;
  NeedChangePanel: Boolean;
begin
  AControl := nil; NeedChangePanel := False;
  case IndexOfContentTypeStr(tsSwitch.Tabs.Strings[NewTab]) of
    pctProps:             AControl := lvProp;
    pctEvents:            AControl := lvEvent;
    pctCollectionItems:   AControl := lvCollectionItem;
    pctMenuItems:         AControl := lvMenuItem;
    pctStrings:           AControl := mmoText;
    pctGraphics:          AControl := pnlGraphic;
    pctComponents:        AControl := lvComp;
    pctControls:          AControl := lvControl;
    pctHierarchy:
      begin               AControl := pnlHierarchy;
                          NeedChangePanel := True;
      end;
  end;

  if AControl <> nil then
  begin
    AControl.BringToFront;
    AControl.Visible := True;
    AControl.Align := alClient;
    if NeedChangePanel then
      UpdatePanelPositions;
  end;
end;

procedure TCnPropSheetForm.btnInspectClick(Sender: TObject);
begin
  if FCurrObj <> nil then
    EvaluatePointer(FCurrObj, FInspectParam, nil, False, Self);
end;

procedure TCnPropSheetForm.lvPropCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  ARect: TRect;
  ALv: TListView;
begin
  DefaultDraw := True;
  if Sender is TListView then
  begin
    ALv := Sender as TListView;
    ListView_GetSubItemRect(ALv.Handle, Item.Index, 0, LVIR_BOUNDS, @ARect);
    ALv.Canvas.Brush.Color := $00FFBBBB;

    if ALv.Focused then
    begin
      if (Item <> nil) and (Item.Data <> nil) and (ALv.Selected = Item)
        and (TCnDisplayObject(Item.Data).ObjValue <> nil) then
      begin
        ARect := Item.DisplayRect(drSelectBounds);
        FCurrObj := TCnDisplayObject(Item.Data).ObjValue;

        if ARect.Top >= FListViewHeaderHeight then
        begin
          pnlInspectBtn.Parent := ALv;
          pnlInspectBtn.Left := ARect.Right - pnlInspectBtn.Width - 1;
          pnlInspectBtn.Top := ARect.Top + 1;
          pnlInspectBtn.Visible := True;
        end
        else
          pnlInspectBtn.Visible := False;
      end;
      Exit;
    end;

    pnlInspectBtn.Visible := False;
  end;
end;

procedure TCnPropSheetForm.lvPropSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if Sender is TListView then
  begin
    if (Item <> nil) and (Item.Data <> nil) and ((Sender as TListView).Selected = Item)
      and (TCnDisplayObject(Item.Data).ObjValue <> nil) then
    else
      pnlInspectBtn.Visible := False;
  end;
end;

procedure TCnPropSheetForm.lvPropCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState;
  var DefaultDraw: Boolean);
var
  ARect: TRect;
  ALv: TListView;
begin
  DefaultDraw := True;
  if Sender is TListView then
  begin
    ALv := Sender as TListView;
    ListView_GetSubItemRect(ALv.Handle, Item.Index, 1, LVIR_BOUNDS, @ARect);
    ALv.Canvas.Brush.Color := $00AAFFFF;

    if (Item <> nil) and (Item.Data <> nil) and TCnDisplayObject(Item.Data).Changed then
    begin
      ALv.Canvas.Font.Color := clRed;
      ALv.Canvas.Font.Style := [fsBold];
    end
    else
    begin
      ALv.Canvas.Font.Color := clBlack;
      ALv.Canvas.Font.Style := [];
    end;
  end;
end;

procedure TCnPropSheetForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
  KeyState: TKeyboardState;
  I: Integer;
begin
  if Closing then Exit;
  
  GetKeyboardState(KeyState);
  if (KeyState[VK_SHIFT] and $80) <> 0 then // 按 Shift 全关
  begin
    Closing := True;
    try
      for I := FSheetList.Count - 1 downto 0 do
        if FSheetList.Items[I] <> Self then
          FSheetList.Delete(I);
    finally
      Closing := False;
    end;
  end
  else
  begin
    // 关闭时，把来源的窗体调到前面
    if FParentSheetForm <> nil then
      FParentSheetForm.BringToFront;
  end;
end;

procedure TCnPropSheetForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TCnPropSheetForm.btnRefreshClick(Sender: TObject);
begin
  EvaluatePointer(FObjectPointer, FInspectParam, Self);
end;

procedure TCnPropSheetForm.btnTopClick(Sender: TObject);
begin
  if btnTop.Down then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TCnPropSheetForm.btnEvaluateClick(Sender: TObject);
var
  P: Integer;
begin
  P := StrToIntDef('$' + edtObj.Text, 0);
  if P <> 0 then
    EvaluatePointer(Pointer(P), FInspectParam, Self);
end;

procedure TCnPropSheetForm.edtObjKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    btnEvaluate.Click;
end;

procedure TCnPropSheetForm.UpdatePanelPositions;
const
  PanelMargin = 20;
  PanelStep = 45;
var
  I: Integer;
  APanel: TPanel;
  ABevel: TBevel;
begin
  for I := 0 to FHierPanels.Count - 1 do
  begin
    APanel := TPanel(FHierPanels.Items[I]);
    APanel.Left := PanelMargin;
    APanel.Width := Width - PanelMargin * 2;
    APanel.Top := PanelMargin + I * PanelStep;
    APanel.Height := PanelStep - PanelMargin;
    APanel.Color := clBtnFace;

    ABevel := TBevel(FHierLines.Items[I]);
    ABevel.Left := pnlHierarchy.Width div 2;
    ABevel.Top := APanel.Top + APanel.Height;
    ABevel.Height := PanelMargin;
    ABevel.Visible := I <> FHierLines.Count - 1;
  end;
end;

procedure TCnPropSheetForm.MsgInspectObject(var Msg: TMessage);
begin
  DoEvaluateBegin;
  try
    FInspectParam := Pointer(Msg.WParam);
    InspectObject(FInspectParam);
  finally
    DoEvaluateEnd;
    Show;  // After Evaluation. Show the form.
  end;
end;

procedure TCnPropSheetForm.DoEvaluateBegin;
begin
  if Assigned(FOnEvaluateBegin) then
    FOnEvaluateBegin(Self);
end;

procedure TCnPropSheetForm.DoEvaluateEnd;
begin
  if Assigned(FOnEvaluateEnd) then
    FOnEvaluateEnd(Self);
end;

procedure TCnPropSheetForm.AfterEvaluateCollections(Sender: TObject);
begin
  if Assigned(FOnAfterEvaluateCollections) then
    FOnAfterEvaluateCollections(Sender);
end;

procedure TCnPropSheetForm.AfterEvaluateComponents(Sender: TObject);
begin
  if Assigned(FOnAfterEvaluateComponents) then
    FOnAfterEvaluateComponents(Sender);
end;

procedure TCnPropSheetForm.AfterEvaluateControls(Sender: TObject);
begin
  if Assigned(FOnAfterEvaluateControls) then
    FOnAfterEvaluateControls(Sender);
end;

procedure TCnPropSheetForm.AfterEvaluateHierarchy(Sender: TObject);
begin
  if Assigned(FOnAfterEvaluateHierarchy) then
    FOnAfterEvaluateHierarchy(Sender);
end;

procedure TCnPropSheetForm.AfterEvaluateProperties(Sender: TObject);
begin
  if Assigned(FOnAfterEvaluateProperties) then
    FOnAfterEvaluateProperties(Sender);
end;

procedure TCnPropSheetForm.lvPropDblClick(Sender: TObject);
var
  Prop: TCnPropertyObject;
  S: string;
begin
  if lvProp.Selected = nil then
    Exit;

  Prop := TCnPropertyObject(lvProp.Selected.Data);
  if (Prop = nil) or not Prop.CanModify then
    Exit;

  S := Prop.DisplayValue;
  if InputQuery(SCnInputNewValueCaption, Format(SCnInputNewValuePrompt, [Prop.PropName]), S) then
  begin
    if FInspector.ChangePropertyValue(Prop.PropName, S) then
      btnRefresh.Click
    else
      ShowMessage(SCnErrorChangeValue);
  end;
end;

procedure TCnPropSheetForm.SetParentSheetForm(const Value: TCnPropSheetForm);
begin
  if FParentSheetForm <> Value then
  begin
    if FParentSheetForm <> nil then
      FParentSheetForm.RemoveFreeNotification(Self);
    FParentSheetForm := Value;
    if FParentSheetForm <> nil then
      FParentSheetForm.FreeNotification(Self);
  end;
end;

procedure TCnPropSheetForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FParentSheetForm) and (Operation = opRemove) then
    FParentSheetForm := nil;
end;

procedure TCnPropSheetForm.ListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Item: TListItem;
  S: string;
  I: Integer;
begin
  if (Sender is TListView) and (Shift = [ssCtrl]) and (Key = Ord('C')) then
  begin
    Item := (Sender as TListView).Selected;
    if Item <> nil then
    begin
      S := Item.Caption;
      for I := 0 to Item.SubItems.Count - 1 do
        S := S + ' ' + Item.SubItems[I];
      ClipBoard.AsText := S;
    end;
  end;
end;

procedure TCnPropSheetForm.btnLocateClick(Sender: TObject);
var
  GraphicCtrl: TGraphicConrolAccess;
  WinCtrl: TWinControl;
  DC: HDC;
  ACanvas: TCanvas;
  AHandle: THandle;
  OldColor: TColor;
  OldStyle: TBrushStyle;
begin
  // Paint GraphicControl using its Canvas
  if TObject(FInspector.ObjectAddr) is TGraphicControl then
  begin
    GraphicCtrl := TGraphicConrolAccess(FInspector.ObjectAddr);
    ACanvas := GraphicCtrl.Canvas;

    OldColor := ACanvas.Brush.Color;
    OldStyle := ACanvas.Brush.Style;
    try
      try
        ACanvas.Brush.Color := clRed;
        ACanvas.Brush.Style := bsSolid;
        ACanvas.FillRect(Rect(0, 0, GraphicCtrl.Width, GraphicCtrl.Height));
      except
        ;
      end;
    finally
      ACanvas.Brush.Color := OldColor;
      ACanvas.Brush.Style := OldStyle;
    end;
  end
  else if TObject(FInspector.ObjectAddr) is TWinControl then
  begin
    // Paint WinControl using its Window DC
    WinCtrl := TWinControl(FInspector.ObjectAddr);
    AHandle := WinCtrl.Handle;

    try
      DC := GetWindowDC(AHandle);
      ACanvas := TCanvas.Create;
      try
        ACanvas.Handle := DC;

        ACanvas.Brush.Color := clRed;
        ACanvas.Brush.Style := bsSolid;
        ACanvas.FillRect(Rect(0, 0, WinCtrl.Width, WinCtrl.Height));
      finally
        ACanvas.Free;
      end;
    except
      ;
    end;
  end;
end;

initialization
  FSheetList := TComponentList.Create(True);
  ObjectInspectorClass := TCnLocalObjectInspector;

finalization
  // Free All Form Instances.
  FreeAndNil(FSheetList);

end.
