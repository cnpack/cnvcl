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

unit CnPropSheetFrm;
{ |<PRE>
================================================================================
* ������ƣ�CnPack ���õ�Ԫ
* ��Ԫ���ƣ����� RTTI ��Ϣ��ʾ���嵥Ԫ
* ��Ԫ���ߣ�CnPack ������ master@cnpack.org
* ��    ע���������ڱ��������Բ�������ֵʱ���ڲ��ᷢ���Ķ�����߰汾�� TPicture
*           �� Bitmap/Icon/Metafile ����ʱ��ʲô�ڲ��ͻ�ǿ��ת��ʲô����ԭ�����ݶ�ʧ
*           ���ั��������ʱҪע��
* ����ƽ̨��PWinXP + Delphi 5
* ���ݲ��ԣ�δ����
* �� �� �����ô����е��ַ����ݲ����ϱ��ػ�����ʽ
* �޸ļ�¼��2016.04.10
*               �����޸����ԵĹ���
*           2012.03.10
*               ����ͼƬ����Ŀ��ӻ���ʾ
*           2006.11.23
*               ���������̳й�ϵ����ʾ
*           2006.11.07
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

// ���ڸ߰汾�±��룬��Ҫ�ڹ��̱��������м��� ENABLE_UIINTERACT ��֧�����ṹ
// �Ҳ�����ԪʱҪ�� UnitScope �м� Vcl����ʹ���� FMX ��ϵ��

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Menus,
  Grids, StdCtrls, ExtCtrls, TypInfo, Contnrs, Buttons, ComCtrls, {$IFNDEF FPC} Tabs, {$ENDIF} Commctrl,
  Clipbrd, ImgList, CnTree {$IFDEF VER130}{$ELSE}, Variants{$ENDIF}
  {$IFDEF SUPPORT_ENHANCED_RTTI}, Rtti {$ENDIF};

const
  CN_INSPECTOBJECT = WM_USER + $C10; // Cn Inspect Object

  CnCanModifyPropTypes: TTypeKinds =
    [tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet, tkWChar,
    tkLString, tkWString, {$IFDEF UNICODE} tkUString, {$ENDIF} tkInt64];

  SCnCanNotReadValue = '<Can NOT Read Value>';

type
  TCnPropContentType = (pctProps, pctFields, pctEvents, pctMethods, pctCollectionItems, pctMenuItems,
    pctStrings, pctGraphics, pctComponents, pctControls, pctHierarchy);
  TCnPropContentTypes = set of TCnPropContentType;

  TCnDisplayObject = class(TObject)
  {* ����һ����ʾ���ݵĻ��� }
  private
    FChanged: Boolean;
    FDisplayValue: string;
    FObjStr: string;
    FObjValue: TObject;
    FObjClassName: string;
    FIsNewRTTI: Boolean;
    FIntfValue: IUnknown;
    FIsObjOrIntf: Boolean;
  public
    property Changed: Boolean read FChanged write FChanged;
    property DisplayValue: string read FDisplayValue write FDisplayValue;
    property ObjClassName: string read FObjClassName write FObjClassName;
    property ObjValue: TObject read FObjValue write FObjValue;
    property IntfValue: IUnknown read FIntfValue write FIntfValue;
    property IsObjOrIntf: Boolean read FIsObjOrIntf write FIsObjOrIntf;
    property ObjStr: string read FObjStr write FObjStr;
    property IsNewRTTI: Boolean read FIsNewRTTI write FIsNewRTTI;
  end;

  TCnPropertyObject = class(TCnDisplayObject)
  {* ����һ���� }
  private
    FPropName: string;
    FPropType: TTypeKind;
    FPropTypeName: string;
    FPropValue: Variant;
{$IFDEF SUPPORT_ENHANCED_RTTI}
    FPropRttiValue: TValue;
  {$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
    FIndexParamCount: Integer;
    FIndexNames: TStringList;
  {$ENDIF}
{$ENDIF}
    FCanModify: Boolean;
  protected
{$IFDEF SUPPORT_ENHANCED_RTTI}
  {$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
    function GetIndexNames(Index: Integer): string;
  {$ENDIF}
{$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;

{$IFDEF SUPPORT_ENHANCED_RTTI}
  {$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
    function AddIndexName(const AName: string): Integer;
  {$ENDIF}
{$ENDIF}

    property PropName: string read FPropName write FPropName;
    property PropType: TTypeKind read FPropType write FPropType;
    property PropTypeName: string read FPropTypeName write FPropTypeName;
    property PropValue: Variant read FPropValue write FPropValue;
{$IFDEF SUPPORT_ENHANCED_RTTI}
    property PropRttiValue: TValue read FPropRttiValue write FPropRttiValue;

  {$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
    // Indexed Property Support
    property IndexParamCount: Integer read FIndexParamCount write FIndexParamCount;
    property IndexNames[Index: Integer]: string read GetIndexNames;
  {$ENDIF}
{$ENDIF}
    property CanModify: Boolean read FCanModify write FCanModify;
  end;

  TCnEventObject = class(TCnDisplayObject)
  {* ����һ�¼����䴦���� }
  private
    FHandlerName: string;
    FEventType: string;
    FEventName: string;
  public
    property EventName: string read FEventName write FEventName;
    property EventType: string read FEventType write FEventType;
    property HandlerName: string read FHandlerName write FHandlerName;
  end;

  TCnMethodObject = class(TCnDisplayObject)
  {* ����һ���� }
  private
    FMethodSimpleName: string;
    FFullName: string;
    FAddress: string;
    FFullNameWithAddress: string;
  public
    property MethodSimpleName: string read FMethodSimpleName write FMethodSimpleName;
    {* ������}
    property Address: string read FAddress write FAddress;
    {* ��ַ}
    property FullName: string read FFullName write FFullName;
    {* ������ַ����������}

    property FullNameWithAddress: string read FFullNameWithAddress write FFullNameWithAddress;
    {* ����ַ���������������ڲ���ѯ����ʹ��}
  end;

{$IFDEF SUPPORT_ENHANCED_RTTI}

  TCnFieldObject = class(TCnDisplayObject)
  {* ����һ Field��ֻ����ǿ RTTI ������}
  private
    FOffset: Integer;
    FFieldName: string;
    FFieldType: TRttiType;
    FFieldTypeName: string;
    FCanModify: Boolean;
    FFieldValue: TValue;
  public
    constructor Create;
    {* ���캯��}
    property FieldName: string read FFieldName write FFieldName;
    {* Field ������}
    property FieldTypeName: string read FFieldTypeName write FFieldTypeName;
    {* Field ����������}
    property FieldType: TRttiType read FFieldType write FFieldType;
    {* Field ������}
    property FieldValue: TValue read FFieldValue write FFieldValue;
    {* Field ��ֵ}
    property Offset: Integer read FOffset write FOffset;
    {* Field ��ƫ����}
    property CanModify: Boolean read FCanModify write FCanModify;
    {* �� Field �ܷ��޸ģ�һ�㶼Ϊ True}
  end;

{$ENDIF}

  TCnStringsObject = class(TCnDisplayObject)
  {* ����һ TStrings}
  private

  public
    procedure Clear;
  end;

  TCnGraphicsObject = class(TCnDisplayObject)
  {* ����һͼƬ}
  private
    FGraphic: TObject;
  public
    property Graphic: TObject read FGraphic write FGraphic;
  end;

  TCnCollectionItemObject = class(TCnDisplayObject)
  {* ����һ Collection Item }
  private
    FIndex: Integer;
    FItemName: string;
  public
    property ItemName: string read FItemName write FItemName;
    property Index: Integer read FIndex write FIndex;
  end;

  TCnMenuItemObject = class(TCnDisplayObject)
  {* ����һ MenuItem }
  private
    FIndex: Integer;
    FItemName: string;
  public
    property ItemName: string read FItemName write FItemName;
    property Index: Integer read FIndex write FIndex;
  end;

  TCnComponentObject = class(TCnDisplayObject)
  {* ����һ Component }
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
  {* ����һ Component }
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
  {* �������Է����Ĺ�������� }
  private
    FObjectAddr: Pointer;
    FProperties: TObjectList;
    FFields: TObjectList;
    FEvents: TObjectList;
    FMethods: TObjectList;
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
    FLazyInspect: Boolean;
    function GetEventCount: Integer;
    function GetPropCount: Integer;
    function GetInspectComplete: Boolean;
    function GetCompCount: Integer;
    function GetControlCount: Integer;
    function GetCollectionItemCount: Integer;
    procedure SetInspectComplete(const Value: Boolean);
    function GetMenuItemCount: Integer;
    function GetMethodCount: Integer;
    function GetFieldCount: Integer;
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
    function IndexOfMethod(AMethods: TObjectList;
      const AMethodNameWithAddress: string): TCnMethodObject;
{$IFDEF SUPPORT_ENHANCED_RTTI}
    function IndexOfField(AFields: TObjectList;
      const AFieldName: string): TCnFieldObject;
{$ENDIF}
  public
    constructor Create(Data: Pointer); virtual;
    destructor Destroy; override;

    procedure InspectObject;
    procedure Clear;
{$IFDEF SUPPORT_ENHANCED_RTTI}
    function ChangeFieldValue(const FieldName, Value: string;
      FieldObj: TCnFieldObject): Boolean; virtual;
{$ENDIF}
    function ChangePropertyValue(const PropName, Value: string;
      PropObj: TCnPropertyObject): Boolean; virtual;
    property ObjectAddr: Pointer read FObjectAddr write SetObjectAddr;
    {* ��Ҫ���ⲿд��д�� Object���� String }

    property Properties: TObjectList read FProperties;
    property Fields: TObjectList read FFields;
    property Events: TObjectList read FEvents;
    property Methods: TObjectList read FMethods;
    property Strings: TCnStringsObject read FStrings;
    property Graphics: TCnGraphicsObject read FGraphics;
    property Components: TObjectList read FComponents;
    property Controls: TObjectList read FControls;
    property CollectionItems: TObjectList read FCollectionItems;
    property MenuItems: TObjectList read FMenuItems;

    property PropCount: Integer read GetPropCount;
    property FieldCount: Integer read GetFieldCount;
    property EventCount: Integer read GetEventCount;
    property MethodCount: Integer read GetMethodCount;
    property CompCount: Integer read GetCompCount;
    property ControlCount: Integer read GetControlCount;
    property CollectionItemCount: Integer read GetCollectionItemCount;
    property MenuItemCount: Integer read GetMenuItemCount;

    property LazyInspect: Boolean read FLazyInspect write FLazyInspect;
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
  {* ͬһ�����ڵĶ������Է����Ĺ����� }
  private
    FObjectInstance: TObject;
  protected
    procedure SetObjectAddr(const Value: Pointer); override;

    procedure DoEvaluate; override;
  public
    constructor Create(Data: Pointer); override;
    destructor Destroy; override;

{$IFDEF SUPPORT_ENHANCED_RTTI}
    function ChangeFieldValue(const FieldName, Value: string;
      FieldObj: TCnFieldObject): Boolean; override;
{$ENDIF}
    function ChangePropertyValue(const PropName, Value: string;
      PropObj: TCnPropertyObject): Boolean; override;
    property ObjectInstance: TObject read FObjectInstance;
  end;

  TCnPropSheetForm = class(TForm)
    pnlTop: TPanel;
    pnlTree: TPanel;
    pnlTreeTab: TPanel;
    TreeView: TTreeView;
    pnlSwitchTab: TPanel;
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
    lvMenuItem: TListView;
    edtClassName: TEdit;
    btnLocate: TSpeedButton;
    lvMethod: TListView;
    lvField: TListView;
    pnlGraphicInfo: TPanel;
    bxGraphic: TScrollBox;
    pbGraphic: TPaintBox;
    lblGraphicInfo: TLabel;
    lblPixel: TLabel;
    btnTree: TSpeedButton;
    pnlSearch: TPanel;
    edtSearch: TEdit;
    btnSearch: TSpeedButton;
    pmSheet: TPopupMenu;
    Copy1: TMenuItem;
    CopyAll1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tsSwitchChange(Sender: TObject {$IFNDEF FPC}; NewTab: Integer;
      var AllowChange: Boolean {$ENDIF});
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
    procedure pbGraphicMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnTreeClick(Sender: TObject);
    procedure tsTreeChange(Sender: TObject {$IFNDEF FPC}; NewTab: Integer;
      var AllowChange: Boolean {$ENDIF});
    procedure TreeViewDblClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure edtSearchKeyPress(Sender: TObject; var Key: Char);
    procedure Copy1Click(Sender: TObject);
    procedure CopyAll1Click(Sender: TObject);
    procedure pbGraphicPaint(Sender: TObject);
    procedure lvFieldDblClick(Sender: TObject);
  private
{$IFDEF FPC}
    tsSwitch: TTabControl;
    tsTree: TTabControl;
{$ELSE}
    tsSwitch: TTabSet;
    tsTree: TTabSet;
{$ENDIF}
    FImgBk: TBitmap;
    FGraphicBmp: TBitmap;  // ������ʾ��
    FListViewHeaderHeight: Integer;
    FContentTypes: TCnPropContentTypes;
    FPropListPtr: PPropList;
    FPropCount: Integer;
    FObjectPointer: Pointer; // ָ�� Object ʵ��
    FInspector: TCnObjectInspector;
    FInspectParam: Pointer;
    FCurrObj: TObject;
    FCurrIntf: IUnknown;
{$IFDEF SUPPORT_ENHANCED_RTTI}
  {$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
    FCurrProp: TCnPropertyObject;  // Ŀǰ���� Indexed Property
  {$ENDIF}
{$ENDIF}
    FParentSheetForm: TCnPropSheetForm;
    FHierarchys: TStrings;
    FGraphicObject: TObject;
    FHierPanels: TComponentList;
    FHierLines: TComponentList;

    FComponentTree: TCnTree;
    FControlTree: TCnTree;
    FScreenTree: TCnTree;

    FOnEvaluateBegin: TNotifyEvent;
    FOnEvaluateEnd: TNotifyEvent;
    FOnAfterEvaluateHierarchy: TNotifyEvent;
    FOnAfterEvaluateCollections: TNotifyEvent;
    FOnAfterEvaluateControls: TNotifyEvent;
    FOnAfterEvaluateProperties: TNotifyEvent;
    FOnAfterEvaluateComponents: TNotifyEvent;
    FShowTree: Boolean;
    FSyncMode: Boolean;
    FObjectExpr: string; // Object ��ʶ�ַ���
    FInspectorClass: TCnObjectInspectorClass;

    procedure SetContentTypes(const Value: TCnPropContentTypes);
    procedure SetParentSheetForm(const Value: TCnPropSheetForm);
    procedure SetShowTree(const Value: Boolean);

    procedure UpdateContentTypes;
    procedure UpdateUIStrings;
    procedure UpdateHierarchys;
    procedure UpdatePanelPositions;

    // ���� FObjectPointer �����������ؼ���������
    procedure SearchTrees;
    procedure UpdateToTree(TreeType: Integer);
    procedure SaveATreeNode(ALeaf: TCnLeaf; ATreeNode: TTreeNode; var Valid: Boolean);

    procedure MsgInspectObject(var Msg: TMessage); message CN_INSPECTOBJECT;

    // �¼�ת�Ƶ���������
    procedure AfterEvaluateComponents(Sender: TObject);
    procedure AfterEvaluateControls(Sender: TObject);
    procedure AfterEvaluateCollections(Sender: TObject);
    procedure AfterEvaluateProperties(Sender: TObject);
    procedure AfterEvaluateHierarchy(Sender: TObject);
  protected
    procedure TileBkToImageBmp;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    procedure SetPropListSize(const Value: Integer);
    procedure InspectObject(Data: Pointer);
    procedure Clear;
    procedure DoEvaluateBegin; virtual;
    procedure DoEvaluateEnd; virtual;

    property ObjectPointer: Pointer read FObjectPointer write FObjectPointer;
    {* �����ڴ���ʾ�Ķ���ʵ����Ϊ nil ��ʾ���ڽ�������ֵģʽ}
    property ObjectExpr: string read FObjectExpr write FObjectExpr;
    {* Զ����ֵģʽ�Ķ����ַ�����Ϊ��ʱ��ʾ����Զ����ֵģʽ}

    property ContentTypes: TCnPropContentTypes read FContentTypes write SetContentTypes;
    property ParentSheetForm: TCnPropSheetForm read FParentSheetForm write SetParentSheetForm;
    property ShowTree: Boolean read FShowTree write SetShowTree;
    property SyncMode: Boolean read FSyncMode write FSyncMode;
    property InspectorClass: TCnObjectInspectorClass read FInspectorClass write FInspectorClass;
    property InspectParam: Pointer read FInspectParam write FInspectParam;

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
{* ִ�������Ĳ鿴��SyncMode ָ�Ƿ�ͬ���鿴��Ĭ���첽��Form ���Լ�����Ϣ�鿴}

function GetPropValueStr(Instance: TObject; PropInfo: PPropInfo): string;

function GetObjValueStr(AObj: TObject; WithType: Boolean = True): string;

{$IFDEF SUPPORT_ENHANCED_RTTI}

function GetRttiPropValueStr(Instance: TObject; RttiProperty: TRttiProperty): string;

{$ENDIF}

var
  ObjectInspectorClass: TCnObjectInspectorClass = nil;

implementation

{$R *.DFM}

{$R CnPropSheet.res}

uses
  CnDebug {$IFDEF ENABLE_FMX}, CnFmxUtils {$ENDIF};

const
  CN_TREE_TYPE_COMPONENT  = 0;
  CN_TREE_TYPE_CONTROL    = 1;
  CN_TREE_TYPE_SCREENFORM = 2;

type
  PParamData = ^TParamData;
  TParamData = record
  // Copy from TypInfo
    Flags: TParamFlags;
    ParamName: ShortString;
    TypeName: ShortString;
  end;

  TConrolAccess = class(TControl);
  TGraphicConrolAccess = class(TGraphicControl);

  TCnByteSet = set of 0..SizeOf(Byte) * 8 - 1;
  TCnWordSet = set of 0..SizeOf(Word) * 8 - 1;
  TCnDWordSet = set of 0..SizeOf(DWORD) * 8 - 1;

  // ������� TComponent.ComponentState ��������������Ϣ
  TCnComponentState = (csLoading, csReading, csWriting, csDestroying,
    csDesigning, csAncestor, csUpdating, csFixups, csFreeNotification,
    csInline, csDesignInstance);

  // ������� TComponent.ComponentStyle ��������������Ϣ
  TCnComponentStyle = (csInheritable, csCheckPropAvail, csSubComponent,
    csTransient);

  // ������� TControl.ControlState ��������������Ϣ
  TCnControlState = (csLButtonDown, csClicked, csPalette,
    csReadingState, csAlignmentNeeded, csFocusing, csCreating,
    csPaintCopy, csCustomPaint, csDestroyingHandle, csDocking,
    csDesignerHide, csPanning, csRecreating, csAligning, csGlassPaint,
    csPrintClient);

  // ������� TControl.ControlStyle ��������������Ϣ
  TCnControlStyle = (csAcceptsControls, csCaptureMouse,
    csDesignInteractive, csClickEvents, csFramed, csSetCaption, csOpaque,
    csDoubleClicks, csFixedWidth, csFixedHeight, csNoDesignVisible,
    csReplicatable, csNoStdEvents, csDisplayDragImage, csReflector,
    csActionClient, csMenuEvents, csNeedsBorderPaint, csParentBackground,
    csPannable, csAlignWithMargins, csGestures, csPaintBlackOpaqueOnGlass,
    csOverrideStylePaint, csNeedsDesignDisabledState);

{$IFNDEF SUPPORT_INTERFACE_AS_OBJECT}
  PPointer = ^Pointer;
  TObjectFromInterfaceStub = packed record
    Stub: Cardinal;
    case Integer of
      0: (ShortJmp: ShortInt);
      1: (LongJmp:  LongInt)
  end;
  PObjectFromInterfaceStub = ^TObjectFromInterfaceStub;
{$ENDIF}

const
  SCnPropContentType: array[TCnPropContentType] of string =
    ('Properties', 'Fields', 'Events', 'Methods', 'CollectionItems', 'MenuItems',
     'Strings', 'Graphics', 'Components', 'Controls', 'Hierarchy');

  SCnInputGetIndexedPropertyCaption = 'Get Indexed Property Value';
  SCnInputGetIndexedPropertyPrompt = 'Enter a Value for %s:';

  SCnInputNewValueCaption = 'Modify Value';
  SCnInputNewValuePrompt = 'Enter a New Value for %s:';
  SCnErrorChangeValue = 'Change Field/Property Value Failed!';
{$IFNDEF COMPILER6_UP}
  AC_SRC_ALPHA = $01;
{$ENDIF}

var
  FSheetList: TComponentList = nil;

  CnFormLeft: Integer = 50;
  CnFormTop: Integer = 50;
  Closing: Boolean = False;
  CnPnlTreeWidth: Integer = 400;

// ���� set ֵ�� set �����ͻ�� set ���ַ�����TypInfo ����������ö�ٵ����ͣ�
// �������� set of ������ͣ����� TypInfo���򷵻���ֵ
function GetSetStr(TypInfo: PTypeInfo; Value: Integer): string;
var
  I: Integer;
  S: TIntegerSet;
begin
  if Value = 0 then
  begin
    Result := '[]';
    Exit;
  end;

  Result := '';
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
  Result := '[' + Result + ']';
end;

function IndexOfContentTypeStr(const AStr: string): TCnPropContentType;
var
  I: TCnPropContentType;
begin
  Result := pctProps;
  for I := Low(TCnPropContentType) to High(TCnPropContentType) do
  begin
    if AStr = SCnPropContentType[I] then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function EvaluatePointer(Address: Pointer; Data: Pointer;
  AForm: TCnPropSheetForm; SyncMode: Boolean;
  AParentSheet: TCnPropSheetForm): TCnPropSheetForm;
begin
  Result := nil;
  if Address = nil then Exit;

  if AForm = nil then
    AForm := TCnPropSheetForm.Create(nil);

  AForm.ObjectPointer := Address;
  AForm.ObjectExpr := '';
  AForm.Clear;
  AForm.ParentSheetForm := AParentSheet;
  AForm.SyncMode := SyncMode;

  if SyncMode then
  begin
    AForm.DoEvaluateBegin;
    try
      AForm.InspectParam := Data;
      AForm.InspectObject(AForm.InspectParam);
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

function GetClassValueStr(AClass: TClass; WithType: Boolean = True): string;
var
 S: string;
begin
 if AClass <> nil then
 begin
   if WithType then
   begin
     try
       S := AClass.ClassName;
     except
       S := 'Unknown Class';
     end;

{$IFDEF WIN64}
     Result := Format('(%s.$%16.16x)', [S, NativeInt(AClass)]);
{$ELSE}
     Result := Format('(%s.$%8.8x)', [S, Integer(AClass)]);
{$ENDIF}
   end
   else
   begin
{$IFDEF WIN64}
     Result := Format('($%16.16x)', [NativeInt(AClass)]);
{$ELSE}
     Result := Format('($%8.8x)', [Integer(AClass)]);
{$ENDIF}
   end;
 end
 else
   Result := 'nil';
end;

function GetObjValueStr(AObj: TObject; WithType: Boolean): string;
var
  S: string;
begin
  if AObj <> nil then
  begin
    if WithType then
    begin
      try
        S := AObj.ClassName;
      except
        S := 'Unknown Object';
      end;

{$IFDEF WIN64}
      Result := Format('(%s.$%16.16x)', [S, NativeInt(AObj)]);
{$ELSE}
      Result := Format('(%s.$%8.8x)', [S, Integer(AObj)]);
{$ENDIF}
    end
    else
    begin
{$IFDEF WIN64}
      Result := Format('($%16.16x)', [NativeInt(AObj)]);
{$ELSE}
      Result := Format('($%8.8x)', [Integer(AObj)]);
{$ENDIF}
    end;
  end
  else
    Result := 'nil';
end;

function GetParamFlagsName(AParamFlags: TParamFlags): string;
const
{$IFDEF FPC}
  SParamFlag: array[TParamFlag] of string
    = ('var', 'const', 'array of', 'address', '', 'out', 'const ref', 'hidden', 'high', 'self', 'vmt', 'result');
{$ELSE}
  SParamFlag: array[TParamFlag] of string
    = ('var', 'const', 'array of', 'address', '', 'out'{$IFDEF COMPILER14_UP}, 'result'{$ENDIF});
{$ENDIF}
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

// ���ݺ���������Ϣ�����������PropInfo����Ϊ����������Ϣ
function GetMethodDeclare(Instance: TObject; PropInfo: PPropInfo): string;
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

{$IFDEF FPC}
  T := GetTypeData(PropInfo^.PropType);
{$ELSE}
  T := GetTypeData(PropInfo^.PropType^);
{$ENDIF}

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
        (GetParamFlagsName(P^.Flags)), TypeStr^])) + ' '
    else
      Result := Result + Trim(Format('%s %s: %s; ', [(GetParamFlagsName(P^.Flags)),
        (P^.ParamName), TypeStr^])) + ' ';
    P := PParamData(Integer(P) + SizeOf(TParamFlags) +
      Length(P^.ParamName) + Length(TypeStr^) + 2);
  end;

  if T^.ParamCount > 0 then
    Delete(Result, Length(Result) - 1, 2);
  Result := Result + ')';
  if T^.MethodKind = mkFunction then
    Result := Result + ': ' + string(PShortString(P)^);
  Result := Result + ';';
end;

{$IFDEF SUPPORT_ENHANCED_RTTI}

// ���ݺ���������Ϣ�����������2010���ϰ汾ʹ��
function GetRttiMethodDeclare(Instance: TObject; RttiProperty: TRttiProperty): string;
var
  CompName, MthName, S: string;
  TypeStr: PShortString;
  T: PTypeData;
  P: PParamData;
  I, DataSize: Integer;
  AMethod: TMethod;
begin
  Result := '';
  DataSize := RttiProperty.GetValue(Instance).DataSize;
  if DataSize <> SizeOf(TMethod) then
    Exit;

  RttiProperty.GetValue(Instance).ExtractRawData(@AMethod);

  CompName := '*';
  if Instance is TComponent then
    CompName := (Instance as TComponent).Name;
  MthName := RttiProperty.Name;

  if AMethod.Data <> nil then
  begin
    try
      S := TObject(AMethod.Data).MethodName(AMethod.Code);
      if S <> '' then
        MthName := S;
      if TObject(AMethod.Data) is TComponent then
        CompName := (TObject(AMethod.Data) as TComponent).Name;
    except
      ;
    end;
  end;

  T := GetTypeData(RttiProperty.PropertyType.Handle);

  if T^.MethodKind = mkFunction then
    Result := Result + 'function ' + CompName + '.' + MthName + '('
  else
    Result := Result + 'procedure ' + CompName + '.' + MthName + '(';

  P := PParamData(@T^.ParamList);
  for I := 1 to T^.ParamCount do
  begin
    TypeStr := Pointer(Integer(@P^.ParamName) + Length(P^.ParamName) + 1);
    if Pos('array of', GetParamFlagsName(P^.Flags)) > 0 then
      Result := Result + Trim(Format('%s: %s %s; ', [(P^.ParamName),
        (GetParamFlagsName(P^.Flags)), TypeStr^])) + ' '
    else
      Result := Result + Trim(Format('%s %s: %s; ', [(GetParamFlagsName(P^.Flags)),
        (P^.ParamName), TypeStr^])) + ' ';
    P := PParamData(Integer(P) + SizeOf(TParamFlags) +
      Length(P^.ParamName) + Length(TypeStr^) + 2);
  end;

  if T^.ParamCount > 0 then
    Delete(Result, Length(Result) - 1, 2);
  Result := Result + ')';
  if T^.MethodKind = mkFunction then
    Result := Result + ': ' + string(PShortString(P)^);
  Result := Result + ';';
end;

// ���� Field ����������Ϣ�����������2010���ϰ汾ʹ��
function GetRttiFieldMethodDeclare(Instance: TObject; RttiField: TRttiField): string;
var
  CompName, MthName, S: string;
  TypeStr: PShortString;
  T: PTypeData;
  P: PParamData;
  I, DataSize: Integer;
  AMethod: TMethod;
begin
  Result := '';
  DataSize := RttiField.GetValue(Instance).DataSize;
  if DataSize <> SizeOf(TMethod) then
    Exit;

  RttiField.GetValue(Instance).ExtractRawData(@AMethod);

  CompName := '*';
  if Instance is TComponent then
    CompName := (Instance as TComponent).Name;
  MthName := RttiField.Name;

  if AMethod.Data <> nil then
  begin
    try
      S := TObject(AMethod.Data).MethodName(AMethod.Code);
      if S <> '' then
        MthName := S;
      if TObject(AMethod.Data) is TComponent then
        CompName := (TObject(AMethod.Data) as TComponent).Name;
    except
      ;
    end;
  end;

  T := GetTypeData(RttiField.FieldType.Handle);

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
        (GetParamFlagsName(P^.Flags)), TypeStr^])) + ' '
    else
      Result := Result + Trim(Format('%s %s: %s;', [(GetParamFlagsName(P^.Flags)),
        (P^.ParamName), TypeStr^])) + ' ';
    P := PParamData(Integer(P) + SizeOf(TParamFlags) +
      Length(P^.ParamName) + Length(TypeStr^) + 2);
  end;

  if T^.ParamCount > 0 then
    Delete(Result, Length(Result) - 1 , 2);
  Result := Result + ')';
  if T^.MethodKind = mkFunction then
    Result := Result + ': ' + string(PShortString(P)^);
  Result := Result + ';';
end;

{$ENDIF}

function GetPropValueStr(Instance: TObject; PropInfo: PPropInfo): string;
var
  iTmp: Integer;
  Obj: TObject;
  S: string;
  IntToId: TIntToIdent;
  AMethod: TMethod;
  K: TTypeKind;
{$IFDEF COMPILER6_UP}
  Intf: IInterface;
{$ENDIF}
begin
  Result := '';
{$IFDEF FPC}
  K := PropInfo^.PropType^.Kind;
{$ELSE}
  K := PropInfo^.PropType^^.Kind;
{$ENDIF}
  case K of
{$IFDEF FPC}
      tkAString: // AString ���� FPC ����
      begin
        S := GetStrProp(Instance, PropInfo);
      end;
    tkBool:    // Bool ���� FPC ����
      begin
        iTmp := GetOrdProp(Instance, PropInfo);
        S := BooleanIdents[iTmp <> 0];
      end;
{$ENDIF}
    tkInteger:
      begin
        S := IntToStr(GetOrdProp(Instance, PropInfo));
{$IFDEF FPC}
        IntToId := FindIntToIdent(PropInfo^.PropType);
{$ELSE}
        IntToId := FindIntToIdent(PropInfo^.PropType^);
{$ENDIF}
        if Assigned(IntToId) and IntToId(GetOrdProp(Instance, PropInfo), S) then
        else
        begin
{$IFDEF FPC}
          if PropInfo^.PropType^.Name = 'TColor' then
            S := Format('$%8.8x', [GetOrdProp(Instance, PropInfo)])
          else
            S := IntToStr(GetOrdProp(Instance, PropInfo));
{$ELSE}
          if PropInfo^.PropType^^.Name = 'TColor' then
            S := Format('$%8.8x', [GetOrdProp(Instance, PropInfo)])
          else if PropInfo^.PropType^^.Name = 'TShortCut' then
          begin
            if GetOrdProp(Instance, PropInfo) = 0 then
              S := '0'
            else
              S := IntToStr(GetOrdProp(Instance, PropInfo)) + ' - ' + ShortCutToText(TShortCut(GetOrdProp(Instance, PropInfo)))
          end
          else
            S := IntToStr(GetOrdProp(Instance, PropInfo));
{$ENDIF}
        end;
      end;
    tkChar:
      S := Chr(GetOrdProp(Instance, PropInfo));
    tkWChar:
      begin
        iTmp := GetOrdProp(Instance, PropInfo);
        // �õ����� WideChar �� Unicode ˫�ֽ�ֵ��ת��������ʾ���ַ���
{$IFDEF UNICODE}
        S := Chr(iTmp);
{$ELSE}
//        SetLength(WS, 1);       // �� WideString ���� Unicode �ַ�
//        P := PByte(@WS[1]);
//        P^ := iTmp and $FF;
//        Inc(P);
//        P^ := ((iTmp shr 8) and $FF);
//        S := string(WS);        // ת AnsiString
        S := WideChar(iTmp);
{$ENDIF}
      end;
    tkClass:
      begin
        Obj := GetObjectProp(Instance, PropInfo);
        S := GetObjValueStr(Obj);
      end;
    tkEnumeration:
      S := GetEnumProp(Instance, PropInfo);
    tkSet:
      begin
        S := GetSetProp(Instance, PropInfo, True);
        if S = '' then
          S := '[]';
      end;
    tkFloat:
      S := FloatToStr(GetFloatProp(Instance, PropInfo));
    tkMethod:
      begin
        AMethod := GetMethodProp(Instance, PropInfo);
        if (AMethod.Code <> nil) and (AMethod.Data <> nil) then
        begin
{$IFDEF FPC}
  {$IFDEF WIN64}
          S := Format('%s: ($%16.16x, $%16.16x): %s', [PropInfo^.PropType^.Name,
            NativeInt(AMethod.Code), NativeInt(AMethod.Data),
            GetMethodDeclare(Instance, PropInfo)]);
  {$ELSE}
          S := Format('%s: ($%8.8x, $%8.8x): %s', [PropInfo^.PropType^.Name,
            Integer(AMethod.Code), Integer(AMethod.Data),
            GetMethodDeclare(Instance, PropInfo)]);
  {$ENDIF}
{$ELSE}
  {$IFDEF WIN64}
          S := Format('%s: ($%16.16x, $%16.16x): %s', [PropInfo^.PropType^^.Name,
            NativeInt(AMethod.Code), NativeInt(AMethod.Data),
            GetMethodDeclare(Instance, PropInfo)]);
  {$ELSE}
          S := Format('%s: ($%8.8x, $%8.8x): %s', [PropInfo^.PropType^^.Name,
            Integer(AMethod.Code), Integer(AMethod.Data),
            GetMethodDeclare(Instance, PropInfo)]);
  {$ENDIF}
{$ENDIF}
        end
        else
          S := 'nil';
      end;
    tkString, tkLString, tkWString{$IFDEF UNICODE}, tkUString{$ENDIF}:
      S := GetStrProp(Instance, PropInfo);
    tkVariant:
      S := VarToStr(GetVariantProp(Instance, PropInfo));
    tkInt64:
      S := FloatToStr(GetInt64Prop(Instance, PropInfo) + 0.0);
    tkInterface:
      begin
{$IFDEF COMPILER6_UP}
        Intf := GetInterfaceProp(Instance, PropInfo);
        {$IFDEF WIN64}
        S := Format('(Interface:$%16.16x)', [NativeInt(Intf)]);
        {$ELSE}
        S := Format('(Interface:$%8.8x)', [Integer(Intf)]);
        {$ENDIF}
{$ELSE}
        S := '(Interface:<...>)';
{$ENDIF}
      end;
  end;
  Result := S;
end;

{$IFDEF SUPPORT_ENHANCED_RTTI}

function GetRttiPropValueStr(Instance: TObject; RttiProperty: TRttiProperty): string;
var
  S: string;
  IntToId: TIntToIdent;
  PropTypeInfo, SetElementTypeInfo: PTypeInfo;
  DataSize: Integer;
  Buf: array[0..1] of Integer; // for x64?
  APtr: Pointer;
  Intf: IInterface;
  AMethod: TMethod;
  AClass: TClass;
begin
  Result := '';
  if not RttiProperty.IsReadable then
  begin
    Result := SCnCanNotReadValue;
    Exit;
  end;

  case RttiProperty.PropertyType.TypeKind of
    tkInteger:
      begin
        S := IntToStr(RttiProperty.GetValue(Instance).AsInteger);
        PropTypeInfo := RttiProperty.PropertyType.Handle;
        IntToId := FindIntToIdent(PropTypeInfo);
        if Assigned(IntToId) and IntToId(RttiProperty.GetValue(Instance).AsInteger, S) then
        else
        begin
          if RttiProperty.PropertyType.Name = 'TColor' then
            S := Format('$%8.8x', [RttiProperty.GetValue(Instance).AsInteger])
          else
            S := IntToStr(RttiProperty.GetValue(Instance).AsInteger);
        end;
      end;
    tkChar, tkWChar:
      S := RttiProperty.GetValue(Instance).AsString;
    tkClass:
      begin
        // һ�������� Class�����ܸ���������ȡ������ʵ����published ��ͷ���ã�
        // һ�������� Object��public ��ͷ���ã�
        if RttiProperty.GetValue(Instance).IsClass then
        begin
          AClass := RttiProperty.GetValue(Instance).AsClass;
          S := GetClassValueStr(AClass);
        end
        else if RttiProperty.GetValue(Instance).IsObject then
        begin
          S := GetObjValueStr(RttiProperty.GetValue(Instance).AsObject);
        end;
      end;
    tkEnumeration:
      begin
        PropTypeInfo := RttiProperty.PropertyType.Handle;
        S := GetEnumName(PropTypeInfo, RttiProperty.GetValue(Instance).AsOrdinal);
      end;
    tkSet:
      begin
        SetElementTypeInfo := nil;
        if RttiProperty.PropertyType.IsSet then
          SetElementTypeInfo := RttiProperty.PropertyType.AsSet.ElementType.Handle;

        DataSize := RttiProperty.GetValue(Instance).DataSize;
        if (DataSize <= SizeOf(Integer)) and (SetElementTypeInfo <> nil) then
        begin
          FillChar(Buf[0], SizeOf(Buf), 0);
          RttiProperty.GetValue(Instance).ExtractRawData((@Buf[0]));
          S := GetSetStr(SetElementTypeInfo, Buf[0]);
        end;
      end;
    tkFloat:
      S := FloatToStr(RttiProperty.GetValue(Instance).AsExtended);
    tkMethod:
      begin
        DataSize := RttiProperty.GetValue(Instance).DataSize;
        if DataSize = SizeOf(TMethod) then
        begin
          RttiProperty.GetValue(Instance).ExtractRawData(@AMethod);
          if AMethod.Code = nil then
            S := 'nil'
          else
          begin
{$IFDEF WIN64}
            S := Format('%s: ($%16.16x, $%16.16x): %s', [RttiProperty.PropertyType.Name,
              NativeInt(AMethod.Code), NativeInt(AMethod.Data),
              GetRttiMethodDeclare(Instance, RttiProperty)]);
{$ELSE}
            S := Format('%s: ($%8.8x, $%8.8x): %s', [RttiProperty.PropertyType.Name,
              Integer(AMethod.Code), Integer(AMethod.Data),
              GetRttiMethodDeclare(Instance, RttiProperty)]);
{$ENDIF}
          end;
        end;
      end;
    tkString, tkLString, tkWString{$IFDEF UNICODE}, tkUString{$ENDIF}:
      S := RttiProperty.GetValue(Instance).AsString;
    tkVariant:
      S := VarToStr(RttiProperty.GetValue(Instance).AsVariant);
    tkInt64:
      S := FloatToStr(RttiProperty.GetValue(Instance).AsInt64 + 0.0);
    tkInterface:
      begin
        try
          Intf := RttiProperty.GetValue(Instance).AsInterface;
          {$IFDEF WIN64}
          S := Format('(Interface:$%16.16x)', [NativeInt(Intf)]);
          {$ELSE}
          S := Format('(Interface:$%8.8x)', [Integer(Intf)]);
          {$ENDIF}
        except
          on E: Exception do
            S := Format('(Interface:<Exception: %s>)', [E.Message]);
        end;
      end;
    tkPointer:
      begin
        DataSize := RttiProperty.GetValue(Instance).DataSize;
        if DataSize = SizeOf(Pointer) then
        begin
          RttiProperty.GetValue(Instance).ExtractRawData(@APtr);
{$IFDEF WIN64}
          S := Format('(Pointer:$%16.16x)', [NativeInt(APtr)]);
{$ELSE}
          S := Format('(Pointer:$%8.8x)', [Integer(APtr)]);
{$ENDIF}
        end;
      end;
  end;
  Result := S;
end;

function GetRttiFieldValueStr(Instance: TObject; RttiField: TRttiField): string;
var
  S: string;
  IntToId: TIntToIdent;
  PropTypeInfo, SetElementTypeInfo: PTypeInfo;
  DataSize: Integer;
  Buf: array[0..1] of Integer; // for x64?
  APtr: Pointer;
  Intf: IInterface;
  AMethod: TMethod;
  AClass: TClass;
begin
  Result := '';
  if RttiField.FieldType = nil then
  begin
    Result := '<Rtti Exception>';
    Exit;
  end;

  case RttiField.FieldType.TypeKind of
    tkInteger:
      begin
        S := IntToStr(RttiField.GetValue(Instance).AsInteger);
        PropTypeInfo := RttiField.FieldType.Handle;
        IntToId := FindIntToIdent(PropTypeInfo);
        if Assigned(IntToId) and IntToId(RttiField.GetValue(Instance).AsInteger, S) then
        else
        begin
          if RttiField.FieldType.Name = 'TColor' then
            S := Format('$%8.8x', [RttiField.GetValue(Instance).AsInteger])
          else
            S := IntToStr(RttiField.GetValue(Instance).AsInteger);
        end;
      end;
    tkChar, tkWChar:
      S := RttiField.GetValue(Instance).AsString;
    tkClass:
      begin
        // һ�������� Class�����ܸ���������ȡ������ʵ����published ��ͷ���ã�
        // һ�������� Object��public ��ͷ���ã�
        if RttiField.GetValue(Instance).IsClass then
        begin
          AClass := RttiField.GetValue(Instance).AsClass;
          S := GetClassValueStr(AClass);
        end
        else if RttiField.GetValue(Instance).IsObject then
        begin
          S := GetObjValueStr(RttiField.GetValue(Instance).AsObject);
        end;
      end;
    tkEnumeration:
      begin
        PropTypeInfo := RttiField.FieldType.Handle;
        S := GetEnumName(PropTypeInfo, RttiField.GetValue(Instance).AsOrdinal);
      end;
    tkSet:
      begin
        SetElementTypeInfo := nil;
        if RttiField.FieldType.IsSet then
          SetElementTypeInfo := RttiField.FieldType.AsSet.ElementType.Handle;

        DataSize := RttiField.GetValue(Instance).DataSize;
        if (DataSize <= SizeOf(Integer)) and (SetElementTypeInfo <> nil) then
        begin
          FillChar(Buf[0], SizeOf(Buf), 0);
          RttiField.GetValue(Instance).ExtractRawData((@Buf[0]));
          S := GetSetStr(SetElementTypeInfo, Buf[0]);
        end;
      end;
    tkFloat:
      S := FloatToStr(RttiField.GetValue(Instance).AsExtended);
    tkMethod:
      begin
        DataSize := RttiField.GetValue(Instance).DataSize;
        if DataSize = SizeOf(TMethod) then
        begin
          RttiField.GetValue(Instance).ExtractRawData(@AMethod);
          if AMethod.Code = nil then
            S := 'nil'
          else
          begin
{$IFDEF WIN64}
            S := Format('%s: ($%16.16x, $%16.16x: %s)', [RttiField.FieldType.Name,
              NativeInt(AMethod.Code), NativeInt(AMethod.Data),
              GetRttiFieldMethodDeclare(Instance, RttiField)]);
{$ELSE}
            S := Format('%s: ($%8.8x, $%8.8x: %s)', [RttiField.FieldType.Name,
              Integer(AMethod.Code), Integer(AMethod.Data),
              GetRttiFieldMethodDeclare(Instance, RttiField)]);
{$ENDIF}
          end;
        end;
      end;
    tkString, tkLString, tkWString{$IFDEF UNICODE}, tkUString{$ENDIF}:
      S := RttiField.GetValue(Instance).AsString;
    tkVariant:
      S := VarToStr(RttiField.GetValue(Instance).AsVariant);
    tkInt64:
      S := FloatToStr(RttiField.GetValue(Instance).AsInt64 + 0.0);
    tkInterface:
      begin
        try
          Intf := RttiField.GetValue(Instance).AsInterface;
          {$IFDEF WIN64}
          S := Format('(Interface:$%16.16x)', [NativeInt(Intf)]);
          {$ELSE}
          S := Format('(Interface:$%8.8x)', [Integer(Intf)]);
          {$ENDIF}
        except
          on E: Exception do
            S := Format('(Interface:<Exception: %s>)', [E.Message]);
        end;
      end;
    tkPointer:
      begin
        DataSize := RttiField.GetValue(Instance).DataSize;
        if DataSize = SizeOf(Pointer) then
        begin
          RttiField.GetValue(Instance).ExtractRawData(@APtr);
{$IFDEF WIN64}
          S := Format('(Pointer:$%16.16x)', [NativeInt(APtr)]);
{$ELSE}
          S := Format('(Pointer:$%8.8x)', [Integer(APtr)]);
{$ENDIF}
        end;
      end;
  end;
  Result := S;
end;

{$ENDIF}

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
  FFields := TObjectList.Create(True);
  FEvents := TObjectList.Create(True);
  FMethods := TObjectList.Create(True);
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
  FMethods.Free;
  FEvents.Free;
  FFields.Free;
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

function TCnObjectInspector.GetFieldCount: Integer;
begin
  Result := FFields.Count;
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

{$IFDEF SUPPORT_ENHANCED_RTTI}

function TCnObjectInspector.IndexOfField(AFields: TObjectList;
  const AFieldName: string): TCnFieldObject;
var
  I: Integer;
  AField: TCnFieldObject;
begin
  Result := nil;
  if AFields <> nil then
  begin
    for I := 0 to AFields.Count - 1 do
    begin
      AField := TCnFieldObject(AFields.Items[I]);
      if AField.FieldName = AFieldName then
      begin
        Result := AField;
        Exit;
      end;
    end;
  end;
end;

{$ENDIF}

function TCnObjectInspector.IndexOfProperty(AProperties: TObjectList;
  const APropName: string): TCnPropertyObject;
var
  I, P: Integer;
  AProp: TCnPropertyObject;
  TmpPropName: string;
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
      end
      else
      begin
        P := Pos('[', AProp.PropName);
        if P > 0 then
        begin
          TmpPropName := Copy(AProp.PropName, 1, P - 1);
          if TmpPropName = APropName then
          begin
            Result := AProp;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

function TCnObjectInspector.IndexOfMethod(AMethods: TObjectList;
  const AMethodNameWithAddress: string): TCnMethodObject;
var
  I: Integer;
  AMethod: TCnMethodObject;
begin
  Result := nil;
  if AMethods <> nil then
  begin
    for I := 0 to AMethods.Count - 1 do
    begin
      AMethod := TCnMethodObject(AMethods.Items[I]);
      if AMethod.FullNameWithAddress = AMethodNameWithAddress then
      begin
        Result := AMethod;
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

{$IFDEF SUPPORT_ENHANCED_RTTI}

function TCnObjectInspector.ChangeFieldValue(const FieldName, Value: string;
  FieldObj: TCnFieldObject): Boolean;
begin
  Result := False;
end;

{$ENDIF}

function TCnObjectInspector.ChangePropertyValue(const PropName, Value: string;
  PropObj: TCnPropertyObject): Boolean;
begin
  Result := False;
end;

function TCnObjectInspector.GetMethodCount: Integer;
begin
  Result := FMethods.Count;
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
  K: TTypeKind;
  AProp: TCnPropertyObject;
  AEvent: TCnEventObject;
  ACollection: TCollection;
  AMenuItem: TMenuItem;
  AComp: TComponent;
  AControl: TWinControl;
{$IFDEF ENABLE_FMX}
  AFmxControl: TComponent;
{$ENDIF}
  AItemObj: TCnCollectionItemObject;
  AMenuObj: TCnMenuItemObject;
  ACompObj: TCnComponentObject;
  AControlObj: TCnControlObject;
  S: string;
  IsExisting: Boolean;
  Hies: TStrings;
  ATmpClass: TClass;
  IntfTable: PInterfaceTable;
  IntfEntry: PInterfaceEntry;
  IntSet: Integer;
{$IFDEF SUPPORT_ENHANCED_RTTI}
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProperty: TRttiProperty;
  RttiField: TRttiField;
  AField: TCnFieldObject;
{$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
  RttiIndexedProperty: TRttiIndexedProperty;
{$ENDIF}
  RttiMethod: TRttiMethod;
  AMethod: TCnMethodObject;

  function GetMethodFullName(ARttiMethod: TRttiMethod; WithAddress: Boolean): string;
  begin
    if WithAddress then
    begin
  {$IFDEF WIN64}
    Result := Format('$%16.16x: %s;', [NativeInt(ARttiMethod.CodeAddress),
      ARttiMethod.ToString]);
  {$ELSE}
    Result := Format('$%8.8x: %s;', [Integer(ARttiMethod.CodeAddress),
      ARttiMethod.ToString]);
  {$ENDIF}
    end
    else
      Result := ARttiMethod.ToString;
  end;
{$ENDIF}

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

  function AlreadyHasProperty(const APropName: string): Boolean;
  var
    I, P: Integer;
    PropObj: TCnPropertyObject;
    TmpPropName: string;
  begin
    Result := False;
    for I := 0 to Properties.Count - 1 do
    begin
      PropObj := TCnPropertyObject(Properties[I]);
      if PropObj <> nil then
      begin
        if PropObj.PropName = APropName then
        begin
          Result := True;
          Exit;
        end
        else // ���ִ洢���������� Items[I: Integer] ����ʽ
        begin
          P := Pos('[', PropObj.PropName);
          if P > 0 then
          begin
            TmpPropName := Copy(PropObj.PropName, 1, P - 1);
            if TmpPropName = APropName then
            begin
              Result := True;
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;

  function AlreadyHasEvent(AEventName: string): Boolean;
  var
    I: Integer;
    EventObj: TCnEventObject;
  begin
    Result := False;
    for I := 0 to Events.Count - 1 do
    begin
      EventObj := TCnEventObject(Events[I]);
      if EventObj <> nil then
        if EventObj.EventName = AEventName then
        begin
          Result := True;
          Exit;
        end;
    end;
  end;

{$IFDEF SUPPORT_ENHANCED_RTTI}

  function GetInterfaceNameByGUID(const AGUID: TGUID): string;
  var
    RttiContext: TRttiContext;
    RttiType: TRttiType;
  begin
    Result := '';
    RttiContext := TRttiContext.Create;
    try
      for RttiType in RttiContext.GetTypes do
      begin
        if (RttiType.TypeKind = tkInterface) and
           IsEqualGUID(GetTypeData(RttiType.Handle).Guid, AGUID) then
        begin
          Result := RttiType.Name;
          Exit;
        end;
      end;
    finally
      RttiContext.Free;
    end;
  end;

{$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
  procedure CalcIndexedProperty(Indexed: TRttiIndexedProperty;
    IndexedProp: TCnPropertyObject);
  var
    M: TRttiMethod;
    P: TArray<TRttiParameter>;
    I: Integer;
  begin
    IndexedProp.PropName := Indexed.Name + '[';
    M := Indexed.ReadMethod;
    if M <> nil then
    begin
      P := M.GetParameters;
      IndexedProp.IndexParamCount := Length(P);
      for I := 0 to Length(P) - 2 do
      begin
        IndexedProp.PropName := IndexedProp.PropName + P[I].ToString + ', ';
        IndexedProp.AddIndexName(P[I].ToString);
      end;
      IndexedProp.PropName := IndexedProp.PropName + P[Length(P) - 1].ToString;
      IndexedProp.AddIndexName(P[Length(P) - 1].ToString);
    end
    else
    begin
      M := Indexed.WriteMethod;
      if M = nil then
      begin
        IndexedProp.PropName := IndexedProp.PropName + ']';
        Exit;
      end;
      P := M.GetParameters;
      IndexedProp.IndexParamCount := Length(P) - 1;
      for I := 0 to Length(P) - 3 do
      begin
        IndexedProp.PropName := IndexedProp.PropName + P[I].ToString + ', ';
        IndexedProp.AddIndexName(P[I].ToString);
      end;
      IndexedProp.PropName := IndexedProp.PropName + P[Length(P) - 2].ToString;
      IndexedProp.AddIndexName(P[Length(P) - 2].ToString);
    end;
    IndexedProp.PropName := IndexedProp.PropName + ']';
  end;
{$ENDIF}
{$ENDIF}

  function GUIDToString(const GUID: TGUID): string;
  begin
    SetLength(Result, 38);
    StrLFmt(PChar(Result), 38,'{%.8x-%.4x-%.4x-%.2x%.2x-%.2x%.2x%.2x%.2x%.2x%.2x}',
      [GUID.D1, GUID.D2, GUID.D3, GUID.D4[0], GUID.D4[1], GUID.D4[2], GUID.D4[3],
      GUID.D4[4], GUID.D4[5], GUID.D4[6], GUID.D4[7]]);
  end;

begin
  if ObjectInstance <> nil then
  begin
    if not IsRefresh then
    begin
      Properties.Clear;
      Fields.Clear;
      Events.Clear;
      Methods.Clear;
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
    try
      ATmpClass := ObjectInstance.ClassType;
      repeat
        Hies.Add(ATmpClass.ClassName);

        IntfTable := ATmpClass.GetInterfaceTable;
        if IntfTable <> nil then
        begin
          for I := 0 to IntfTable.EntryCount - 1 do
          begin
            IntfEntry := @IntfTable.Entries[I];
{$IFDEF FPC}
            if IntfEntry^.IID <> nil then
              Hies[Hies.Count - 1] := Hies[Hies.Count - 1] + ', ' +  GUIDToString(IntfEntry^.IID^);
{$ELSE}
  {$IFDEF SUPPORT_ENHANCED_RTTI}
            S := GetInterfaceNameByGUID(IntfEntry^.IID);
  {$ELSE}
            S := '';
  {$ENDIF}
            if S = '' then
              S := GUIDToString(IntfEntry^.IID);
            Hies[Hies.Count - 1] := Hies[Hies.Count - 1] + ', ' + S;
{$ENDIF}
          end;
        end;

        ATmpClass := ATmpClass.ClassParent;
      until ATmpClass = nil;

      Hierarchy := Hies.Text;
    finally
      Hies.Free;
    end;

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

    // �Ծɷ�ʽ������
    APropCount := GetTypeData(PTypeInfo(FObjectInstance.ClassInfo))^.PropCount;
    if APropCount > 0 then
    begin
      GetMem(PropListPtr, APropCount * SizeOf(Pointer));
      GetPropList(PTypeInfo(FObjectInstance.ClassInfo), tkAny, PropListPtr);

      for I := 0 to APropCount - 1 do
      begin
        PropInfo := PropListPtr^[I];
{$IFDEF FPC}
        K := PropInfo^.PropType^.Kind;
{$ELSE}
        K := PropInfo^.PropType^^.Kind;
{$ENDIF}
        if K in tkProperties then
        begin
          try
            if not IsRefresh then
              AProp := TCnPropertyObject.Create
            else
              AProp := IndexOfProperty(Properties, PropInfoName(PropInfo));

            AProp.PropName := PropInfoName(PropInfo);
{$IFDEF FPC}
            AProp.PropType := PropInfo^.PropType^.Kind;
            AProp.PropTypeName := PropInfo^.PropType^.Name;
{$ELSE}
            AProp.PropType := PropInfo^.PropType^^.Kind;
            AProp.PropTypeName := PropInfo^.PropType^^.Name;
{$ENDIF}
            AProp.IsObjOrIntf := AProp.PropType in [tkClass, tkInterface];

            // ��д��Ȩ�ޣ�����ָ�����ͣ��ſ��޸ģ����������û����
{$IFDEF FPC}
            AProp.CanModify := (PropInfo^.SetProc <> nil) and (PropInfo^.PropType^.Kind
              in CnCanModifyPropTypes);
{$ELSE}
            AProp.CanModify := (PropInfo^.SetProc <> nil) and (PropInfo^.PropType^^.Kind
              in CnCanModifyPropTypes);
{$ENDIF}

            try
              AProp.PropValue := GetPropValue(FObjectInstance, PropInfoName(PropInfo));
            except
              ; // Interface ���Ϳ��ܻ��������֮
            end;

            AProp.ObjValue := nil;
            AProp.IntfValue := nil;
            if AProp.IsObjOrIntf then
            begin
              if AProp.PropType = tkClass then
                AProp.ObjValue := GetObjectProp(FObjectInstance, PropInfo)
              else
                AProp.IntfValue := IUnknown(GetOrdProp(FObjectInstance, PropInfo));
            end;

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
          except
            ;
          end;
        end;

        // ���¼�
        if K = tkMethod then
        begin
          try
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
          except
            ;
          end;
        end;
      end;
      FreeMem(PropListPtr);
    end;

{$IFDEF SUPPORT_ENHANCED_RTTI}
    // D2010 �����ϣ�ʹ���� RTTI ������ȡ��������
    RttiContext := TRttiContext.Create;
    try
      RttiType := RttiContext.GetType(FObjectInstance.ClassInfo);
      if RttiType <> nil then
      begin
        for RttiProperty in RttiType.GetProperties do
        begin
          if RttiProperty.PropertyType.TypeKind in tkProperties then
          begin
            // �� Properties���ڷ�ˢ��ʱҪ�ж��Ƿ��ظ�
            if not AlreadyHasProperty(RttiProperty.Name) or IsRefresh then
            begin
              if not IsRefresh then
              begin
                AProp := TCnPropertyObject.Create;
                AProp.IsNewRTTI := True;
              end
              else
                AProp := IndexOfProperty(Properties, RttiProperty.Name);

              AProp.PropName := RttiProperty.Name;
              AProp.PropType := RttiProperty.PropertyType.TypeKind;
              AProp.PropTypeName := RttiProperty.PropertyType.Name;
              AProp.IsObjOrIntf := AProp.PropType in [tkClass, tkInterface];

              // ��д��Ȩ�ޣ�����ָ�����ͣ��ſ��޸ģ����������û����
              AProp.CanModify := (RttiProperty.IsWritable) and (RttiProperty.PropertyType.TypeKind
                in CnCanModifyPropTypes);

              if RttiProperty.IsReadable then
              begin
                try
                  AProp.PropRttiValue := RttiProperty.GetValue(FObjectInstance)
                except
                  // Getting Some Property causes Exception. Catch it.
                  AProp.PropRttiValue := nil;
                end;

                AProp.ObjValue := nil;
                AProp.IntfValue := nil;
                try
                  if AProp.IsObjOrIntf and RttiProperty.GetValue(FObjectInstance).IsObject then
                    AProp.ObjValue := RttiProperty.GetValue(FObjectInstance).AsObject
                  else if AProp.IsObjOrIntf and (RttiProperty.GetValue(FObjectInstance).TypeInfo <> nil) and
                    (RttiProperty.GetValue(FObjectInstance).TypeInfo^.Kind = tkInterface) then
                    AProp.IntfValue := RttiProperty.GetValue(FObjectInstance).AsInterface;
                except
                  // Getting Some Property causes Exception. Catch it.;
                end;
              end
              else
                AProp.PropRttiValue := SCnCanNotReadValue;

              S := GetRttiPropValueStr(FObjectInstance, RttiProperty);
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
          end
          else if RttiProperty.PropertyType.TypeKind = tkMethod then
          begin
            // �� Event���ڷ�ˢ��ʱҪ�ж��Ƿ��ظ�
            if not AlreadyHasEvent(RttiProperty.Name) or IsRefresh then
            begin
              if not IsRefresh then
              begin
                AEvent := TCnEventObject.Create;
                AEvent.IsNewRTTI := True;
              end
              else
                AEvent := IndexOfEvent(FEvents, RttiProperty.Name);

              AEvent.EventName := RttiProperty.Name;
              AEvent.EventType := RttiProperty.PropertyType.Name;
              S := GetRttiPropValueStr(FObjectInstance, RttiProperty);
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
        end;

{$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
        // RTTIIndexedProperties
        for RttiIndexedProperty in RttiType.GetIndexedProperties do
        begin
          if not AlreadyHasProperty(RttiIndexedProperty.Name) or IsRefresh then
          begin
            if not IsRefresh then
            begin
              AProp := TCnPropertyObject.Create;
              AProp.IsNewRTTI := True;
            end
            else
              AProp := IndexOfProperty(Properties, RttiIndexedProperty.Name);

            CalcIndexedProperty(RttiIndexedProperty, AProp);
            AProp.PropType := RttiIndexedProperty.PropertyType.TypeKind;
            AProp.PropTypeName := RttiIndexedProperty.PropertyType.Name;
            AProp.IsObjOrIntf := AProp.PropType in [tkClass, tkInterface];

            AProp.DisplayValue := '<Indexed Property>';
            AProp.Changed := False;
            if not IsRefresh then
              Properties.Add(AProp);

            Include(FContentTypes, pctProps);
          end;
        end;
{$ENDIF}

        // ��ȡ Methods
        try
          for RttiMethod in RttiType.GetMethods do  // ��Щ�ڴ˳� Exception
          begin
            S := GetMethodFullName(RttiMethod, True); // ��Ҫ�ӵ�ַ��������
            if not IsRefresh then
            begin
              AMethod := TCnMethodObject.Create;
              AMethod.IsNewRTTI := True;
            end
            else
              AMethod := IndexOfMethod(FMethods, S);

            AMethod.MethodSimpleName := RttiMethod.Name;
            AMethod.FullNameWithAddress := S;
            I := Pos(': ', S);
            if I > 1 then
            begin
              AMethod.FullName := Copy(S, I + 2, MaxInt);
              AMethod.Address := Copy(S, 1, I - 1);
            end
            else
            begin
              AMethod.FullName := S;
              AMethod.Address := 'nil';
            end;

            if S <> AMethod.DisplayValue then
            begin
              AMethod.DisplayValue := S;
              AMethod.Changed := True;
            end
            else
              AMethod.Changed := False;

            if not IsRefresh then
              FMethods.Add(AMethod);

            Include(FContentTypes, pctMethods);
          end;
        except
          ;
        end;

        // ��ȡ Fields
        try
          for RttiField in RttiType.GetFields do // ��Щ�ڴ˳� Exception
          begin
            if not IsRefresh then
              AField := TCnFieldObject.Create
            else
              AField := IndexOfField(FFields, RttiField.Name);

            AField.FieldName := RttiField.Name; // ������ RttiField.ToString������ IndexOfFields �Ҳ���
            AField.Offset := RttiField.Offset;
            AField.FieldType := RttiField.FieldType;

            if RttiField.FieldType <> nil then // �п��� FieldType Ϊ nil
            begin
              AField.IsObjOrIntf := RttiField.FieldType.TypeKind in [tkClass, tkInterface];
              AField.FieldTypeName := RttiField.FieldType.Name;
            end
            else
            begin
              AField.IsObjOrIntf := False;
              AField.FieldTypeName := '<no type>';
            end;

            try
              AField.FieldValue := RttiField.GetValue(FObjectInstance);
            except
              // Getting Some Property causes Exception. Catch it.
              AField.FieldValue := nil;
            end;

            AField.ObjValue := nil;
            AField.IntfValue := nil;
            try
              if AField.IsObjOrIntf and RttiField.GetValue(FObjectInstance).IsObject then
                AField.ObjValue := RttiField.GetValue(FObjectInstance).AsObject
              else if AField.IsObjOrIntf and (RttiField.GetValue(FObjectInstance).TypeInfo <> nil) and
                (RttiField.GetValue(FObjectInstance).TypeInfo^.Kind = tkInterface) then
                AField.IntfValue := RttiField.GetValue(FObjectInstance).AsInterface;
            except
              // Getting Some Property causes Exception. Catch it.;
            end;

            S := GetRttiFieldValueStr(FObjectInstance, RttiField);
            if S <> AField.DisplayValue then
            begin
              AField.DisplayValue := S;
              AField.Changed := True;
            end
            else
              AField.Changed := False;

            if not IsRefresh then
              FFields.Add(AField);

            Include(FContentTypes, pctFields);
          end;
        except
          ;
        end;
      end;
    finally
      RttiContext.Free;
    end;

{$ENDIF}

    // ���������ʾ���� published ���һЩ��֪�Ĺ�������
    if FObjectInstance is TComponent then
    begin
      if not AlreadyHasProperty('Owner') or IsRefresh then
      begin
        // ��� Component �� Owner
        if not IsRefresh then
          AProp := TCnPropertyObject.Create
        else
          AProp := IndexOfProperty(Properties, 'Owner');

        AProp.PropName := 'Owner';
        AProp.PropType := tkClass;
        AProp.PropTypeName := 'TComponent';
        AProp.IsObjOrIntf := True;
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
      end;

      if not AlreadyHasProperty('ComponentIndex') or IsRefresh then
      begin
        // ��� Component �� ComponentIndex
        if not IsRefresh then
          AProp := TCnPropertyObject.Create
        else
          AProp := IndexOfProperty(Properties, 'ComponentIndex');

        AProp.PropName := 'ComponentIndex';
        AProp.PropType := tkInteger;
        AProp.PropTypeName := 'Integer';
        AProp.IsObjOrIntf := False;
        AProp.PropValue := (FObjectInstance as TComponent).ComponentIndex;
        AProp.ObjValue := nil;

        S := IntToStr(AProp.PropValue);
        AddNewProp(S, AProp);
      end;

      if not AlreadyHasProperty('ComponentState') or IsRefresh then
      begin
        // ��� Component �� ComponentState
        if not IsRefresh then
          AProp := TCnPropertyObject.Create
        else
          AProp := IndexOfProperty(Properties, 'ComponentState');

        AProp.PropName := 'ComponentState';
        AProp.PropType := tkSet;
        AProp.PropTypeName := 'TComponentState';
        AProp.IsObjOrIntf := False;

        IntSet := 0;
        Move((FObjectInstance as TComponent).ComponentState, IntSet,
          SizeOf((FObjectInstance as TComponent).ComponentState));
        AProp.PropValue := IntSet;
        AProp.ObjValue := nil;

        S := GetSetStr(TypeInfo(TCnComponentState), AProp.PropValue);
        AddNewProp(S, AProp);
      end;

      if not AlreadyHasProperty('ComponentStyle') or IsRefresh then
      begin
        // ��� Component �� ComponentStyle
        if not IsRefresh then
          AProp := TCnPropertyObject.Create
        else
          AProp := IndexOfProperty(Properties, 'ComponentStyle');

        AProp.PropName := 'ComponentStyle';
        AProp.PropType := tkSet;
        AProp.PropTypeName := 'TComponentStyle';
        AProp.IsObjOrIntf := False;

        IntSet := 0;
        Move((FObjectInstance as TComponent).ComponentStyle, IntSet,
          SizeOf((FObjectInstance as TComponent).ComponentStyle));
        AProp.PropValue := IntSet;
        AProp.ObjValue := nil;

        S := GetSetStr(TypeInfo(TCnComponentStyle), AProp.PropValue);
        AddNewProp(S, AProp);
      end;
    end;

    if FObjectInstance is TControl then
    begin
      if not AlreadyHasProperty('Parent') or IsRefresh then
      begin
        // ��� Control �� Parent
        if not IsRefresh then
          AProp := TCnPropertyObject.Create
        else
          AProp := IndexOfProperty(Properties, 'Parent');

        AProp.PropName := 'Parent';
        AProp.PropType := tkClass;
        AProp.PropTypeName := 'TControl';
        AProp.IsObjOrIntf := True;
        AProp.PropValue := Integer((FObjectInstance as TControl).Parent);
        AProp.ObjValue := (FObjectInstance as TControl).Parent;

        S := GetObjValueStr(AProp.ObjValue);
        AddNewProp(S, AProp);
      end;

      if not AlreadyHasProperty('Text') or IsRefresh then
      begin
        // ��� Control �� Text
        if not IsRefresh then
          AProp := TCnPropertyObject.Create
        else
          AProp := IndexOfProperty(Properties, 'Text');

        AProp.PropName := 'Text';
        AProp.PropType := tkString;
        AProp.PropTypeName := 'String';
        AProp.IsObjOrIntf := False;
        AProp.PropValue := TConrolAccess(FObjectInstance).Text;
        AProp.ObjValue := nil;

        S := TConrolAccess(FObjectInstance).Text;
        AddNewProp(S, AProp);
      end;

      if not AlreadyHasProperty('ControlState') or IsRefresh then
      begin
        // ��� Control �� ControlState
        if not IsRefresh then
          AProp := TCnPropertyObject.Create
        else
          AProp := IndexOfProperty(Properties, 'ControlState');

        AProp.PropName := 'ControlState';
        AProp.PropType := tkSet;
        AProp.PropTypeName := 'TControlState';
        AProp.IsObjOrIntf := False;

        IntSet := 0;
        Move((FObjectInstance as TControl).ControlState, IntSet,
          SizeOf((FObjectInstance as TControl).ControlState));
        AProp.PropValue := IntSet;
        AProp.ObjValue := nil;

        S := GetSetStr(TypeInfo(TCnControlState), AProp.PropValue);
        AddNewProp(S, AProp);
      end;

      if not AlreadyHasProperty('ControlStyle') or IsRefresh then
      begin
        // ��� Control �� ControlStyle
        if not IsRefresh then
          AProp := TCnPropertyObject.Create
        else
          AProp := IndexOfProperty(Properties, 'ControlStyle');

        AProp.PropName := 'ControlStyle';
        AProp.PropType := tkSet;
        AProp.PropTypeName := 'TControlStyle';
        AProp.IsObjOrIntf := False;

        IntSet := 0;
        Move((FObjectInstance as TControl).ControlStyle, IntSet,
          SizeOf((FObjectInstance as TControl).ControlStyle));
        AProp.PropValue := IntSet;
        AProp.ObjValue := nil;

        S := GetSetStr(TypeInfo(TCnControlStyle), AProp.PropValue);
        AddNewProp(S, AProp);
      end;
    end;

    DoAfterEvaluateProperties;

    // ���� CollectionItem��Components �� Controls��ȡ��ֱ�ӱȽ��Ƿ� Changed ���ɡ�
    if ObjectInstance is TCollection then
    begin
      // ����� Items
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
{$IFDEF WIN64}
        AItemObj.DisplayValue := Format('%s: $%16.16x', [AItemObj.ObjClassName, NativeInt(AItemObj.ObjValue)]);
{$ELSE}
        AItemObj.DisplayValue := Format('%s: $%8.8x', [AItemObj.ObjClassName, Integer(AItemObj.ObjValue)]);
{$ENDIF}

        if not IsExisting then
          CollectionItems.Add(AItemObj);

        Include(FContentTypes, pctCollectionItems);
      end;

      DoAfterEvaluateCollections;
    end
    else if ObjectInstance is TMenuItem then
    begin
      // ����� Items
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

{$IFDEF WIN64}
        AMenuObj.DisplayValue := Format('%s: $%16.16x', [AMenuObj.ObjClassName, NativeInt(AMenuObj.ObjValue)]);
{$ELSE}
        AMenuObj.DisplayValue := Format('%s: $%8.8x', [AMenuObj.ObjClassName, Integer(AMenuObj.ObjValue)]);
{$ENDIF}

        if not IsExisting then
          FMenuItems.Add(AMenuObj);

        Include(FContentTypes, pctMenuItems);
      end;

      DoAfterEvaluateMenuItems;
    end
    else if ObjectInstance is TComponent then
    begin
      // ����� Componets
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
{$IFDEF WIN64}
        ACompObj.DisplayValue := Format('%s: %s: $%16.16x', [ACompObj.CompName,
          ACompObj.ObjClassName, NativeInt(ACompObj.ObjValue)]);
{$ELSE}
        ACompObj.DisplayValue := Format('%s: %s: $%8.8x', [ACompObj.CompName,
          ACompObj.ObjClassName, Integer(ACompObj.ObjValue)]);
{$ENDIF}

        if not IsExisting then
          Components.Add(ACompObj);

        Include(FContentTypes, pctComponents);
      end;

      DoAfterEvaluateComponents;

      // ����� Controls
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
{$IFDEF WIN64}
          AControlObj.DisplayValue := Format('%s: %s: $%16.16x', [AControlObj.CtrlName,
            AControlObj.ObjClassName, NativeInt(AControlObj.ObjValue)]);
{$ELSE}
          AControlObj.DisplayValue := Format('%s: %s: $%8.8x', [AControlObj.CtrlName,
            AControlObj.ObjClassName, Integer(AControlObj.ObjValue)]);
{$ENDIF}

          if not IsExisting then
            Controls.Add(AControlObj);

          Include(FContentTypes, pctControls);
        end;

        DoAfterEvaluateControls;
      end;

{$IFDEF ENABLE_FMX}
      // �жϲ���� FMX �� Controls
      if CnFmxIsInheritedFromControl(ObjectInstance) then
      begin
        AFmxControl := ObjectInstance as TComponent;
        for I := 0 to CnFmxGetControlsCount(AFmxControl) - 1 do
        begin
          IsExisting := IsRefresh and (I < FControls.Count);
          if IsExisting then
            AControlObj := TCnControlObject(FControls.Items[I])
          else
            AControlObj := TCnControlObject.Create;

          AControlObj.ObjClassName := CnFmxGetControlByIndex(AFmxControl, I).ClassName;
          AControlObj.CtrlName := CnFmxGetControlByIndex(AFmxControl, I).Name;
          AControlObj.Index := I;
          if AControlObj.ObjValue <> CnFmxGetControlByIndex(AFmxControl, I) then
          begin
            AControlObj.ObjValue := CnFmxGetControlByIndex(AFmxControl, I);
            AControlObj.Changed := True;
          end
          else
            AControlObj.Changed := False;

          AControlObj.DisplayName := Format('%s.Controls[%d]', [AFmxControl.Name, I]);
{$IFDEF WIN64}
          AControlObj.DisplayValue := Format('%s: %s: $%16.16x', [AControlObj.CtrlName,
            AControlObj.ObjClassName, NativeInt(AControlObj.ObjValue)]);
{$ELSE}
          AControlObj.DisplayValue := Format('%s: %s: $%8.8x', [AControlObj.CtrlName,
            AControlObj.ObjClassName, Integer(AControlObj.ObjValue)]);
{$ENDIF}

          if not IsExisting then
            Controls.Add(AControlObj);

          Include(FContentTypes, pctControls);
        end;

        DoAfterEvaluateControls;
      end;
{$ENDIF}

      // ����� ImageList��������ͼƬ
      if ObjectInstance is TCustomImageList then
      begin
        FGraphics.Graphic := ObjectInstance;
        Include(FContentTypes, pctGraphics);
      end;
    end
    else if (ObjectInstance is TGraphic) or (ObjectInstance is TPicture) then
    begin // ����ͼ������
      FGraphics.Graphic := ObjectInstance;
      Include(FContentTypes, pctGraphics);
    end;
  end;

  if FContentTypes = [] then
    Include(FContentTypes, pctProps);
  FInspectComplete := True;
end;

{$IFDEF SUPPORT_ENHANCED_RTTI}

function TCnLocalObjectInspector.ChangeFieldValue(const FieldName, Value: string;
  FieldObj: TCnFieldObject): Boolean;
{$IFDEF SUPPORT_ENHANCED_RTTI}
var
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiField: TRttiField;
  ValueRec: TValue;
  ASet: Integer;
{$ENDIF}
begin
  Result := False;
  if ObjectInstance = nil then
    Exit;

  RttiContext := TRttiContext.Create;
  try
    RttiType := RttiContext.GetType(ObjectInstance.ClassInfo);
    if RttiType = nil then
      Exit;

    RttiField := RttiType.GetField(FieldName);
    if (RttiField = nil) or (RttiField.FieldType = nil) then
      Exit;

    case RttiField.FieldType.TypeKind of
      tkInteger:
        begin
          try
            ValueRec := StrToInt(Value);
            RttiField.SetValue(ObjectInstance, ValueRec);
          except
            // �ж��Ƿ��� TColor �� clRed ����
            if RttiField.FieldType.Name = 'TColor' then
            begin
              try
                ValueRec := StringToColor(Value);
                RttiField.SetValue(ObjectInstance, ValueRec);
              except
                Exit;
              end;
            end
            else
              Exit;
          end;
        end;
      tkInt64:
        begin
          try
            ValueRec := StrToInt64(Value);
            RttiField.SetValue(ObjectInstance, ValueRec);
          except
            Exit;
          end;
        end;
      tkFloat:
        begin
          try
            ValueRec := StrToFloat(Value);
            RttiField.SetValue(ObjectInstance, ValueRec);
          except
            Exit;
          end;
        end;
      tkChar,
      tkWChar:
        begin
          try
            ValueRec := TValue.FromOrdinal(RttiField.FieldType.Handle, StrToInt64(Value));
            RttiField.SetValue(ObjectInstance, ValueRec);
          except
            Exit;
          end;
        end;
      tkLString,
      tkWString,
{$IFDEF UNICODE}
      tkUString,
{$ENDIF}
      tkString:
        begin
          ValueRec := Value;
          RttiField.SetValue(ObjectInstance, ValueRec);
        end;
      tkEnumeration:
        begin
          // ö���� Boolean ���ᵽ�⣬�ȴ��ַ���ת�� Integer����ת�� Enum ֵ
          ValueRec := TValue.FromOrdinal(RttiField.FieldType.Handle,
            GetEnumValue(RttiField.FieldType.Handle, Value));
          RttiField.SetValue(ObjectInstance, ValueRec);
        end;
      tkSet:
        begin
          // �����ַ�����ת�� Integer����ͨ�� Make ת�� TValue
          ASet := StringToSet(RttiField.FieldType.Handle, Value);
          TValue.Make(@ASet, RttiField.FieldType.Handle, ValueRec);
          RttiField.SetValue(ObjectInstance, ValueRec);
        end;
    end;

    Result := True;
  finally
    RttiContext.Free;
  end;
end;

{$ENDIF}

function TCnLocalObjectInspector.ChangePropertyValue(const PropName, Value: string;
  PropObj: TCnPropertyObject): Boolean;
var
  PropInfo: PPropInfo;
  K: TTypeKind;
  VInt: Integer;
  VInt64: Int64;
  VFloat: Double;
  C: TColor;
{$IFDEF SUPPORT_ENHANCED_RTTI}
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiProperty: TRttiProperty;
  ValueRec: TValue;
  ASet: Integer;
{$ENDIF}
begin
  Result := False;
  if ObjectInstance = nil then
    Exit;

{$IFDEF SUPPORT_ENHANCED_RTTI}
  if PropObj.IsNewRTTI then
  begin
    RttiContext := TRttiContext.Create;
    try
      RttiType := RttiContext.GetType(ObjectInstance.ClassInfo);
      if RttiType = nil then
        Exit;

      RttiProperty := RttiType.GetProperty(PropName);
      if (RttiProperty = nil) or (RttiProperty.PropertyType = nil) then
        Exit;

      case RttiProperty.PropertyType.TypeKind of
        tkInteger:
          begin
            try
              ValueRec := StrToInt(Value);
              RttiProperty.SetValue(ObjectInstance, ValueRec);
            except
              // �ж��Ƿ��� TColor �� clRed ����
              if RttiProperty.PropertyType.Name = 'TColor' then
              begin
                try
                  ValueRec := StringToColor(Value);
                  RttiProperty.SetValue(ObjectInstance, ValueRec);
                except
                  Exit;
                end;
              end
              else
                Exit;
            end;
          end;
        tkInt64:
          begin
            try
              ValueRec := StrToInt64(Value);
              RttiProperty.SetValue(ObjectInstance, ValueRec);
            except
              Exit;
            end;
          end;
        tkFloat:
          begin
            try
              ValueRec := StrToFloat(Value);
              RttiProperty.SetValue(ObjectInstance, ValueRec);
            except
              Exit;
            end;
          end;
        tkChar,
        tkWChar:
          begin
            try
              ValueRec := TValue.FromOrdinal(RttiProperty.PropertyType.Handle, StrToInt64(Value));
              RttiProperty.SetValue(ObjectInstance, ValueRec);
            except
              Exit;
            end;
          end;
        tkLString,
        tkWString,
{$IFDEF UNICODE}
        tkUString,
{$ENDIF}
        tkString:
          begin
            ValueRec := Value;
            RttiProperty.SetValue(ObjectInstance, ValueRec);
          end;
        tkEnumeration:
          begin
            // ö���� Boolean ���ᵽ�⣬�ȴ��ַ���ת�� Integer����ת�� Enum ֵ
            ValueRec := TValue.FromOrdinal(RttiProperty.PropertyType.Handle,
              GetEnumValue(RttiProperty.PropertyType.Handle, Value));
            RttiProperty.SetValue(ObjectInstance, ValueRec);
          end;
        tkSet:
          begin
            // �����ַ�����ת�� Integer����ͨ�� Make ת�� TValue
            ASet := StringToSet(RttiProperty.PropertyType.Handle, Value);
            TValue.Make(@ASet, RttiProperty.PropertyType.Handle, ValueRec);
            RttiProperty.SetValue(ObjectInstance, ValueRec);
          end;
      end;

      Result := True;
    finally
      RttiContext.Free;
    end;
    Exit;
  end;
{$ENDIF}

  PropInfo := GetPropInfo(ObjectInstance, PropName);
  if (PropInfo = nil) or (PropInfo^.SetProc = nil) then
    Exit;

{$IFDEF FPC}
  K := PropInfo^.PropType^.Kind;
{$ELSE}
  K := PropInfo^.PropType^^.Kind;
{$ENDIF}
  case K of
    tkInteger:
      begin
        try
          VInt := StrToInt(Value);
          SetOrdProp(ObjectInstance, PropName, VInt);
        except
          // �ж��Ƿ��� TColor �� clRed ����
{$IFDEF FPC}
          if PropInfo^.PropType^.Name = 'TColor' then
          begin
            try
              C := StringToColor(Value);
              SetOrdProp(ObjectInstance, PropName, C);
            except
              Exit;
            end;
          end
          else
            Exit;
{$ELSE}
          if PropInfo^.PropType^^.Name = 'TColor' then
          begin
            try
              C := StringToColor(Value);
              SetOrdProp(ObjectInstance, PropName, C);
            except
              Exit;
            end;
          end
          else
            Exit;
{$ENDIF}
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

constructor TCnLocalObjectInspector.Create(Data: Pointer);
begin
  inherited;

end;

destructor TCnLocalObjectInspector.Destroy;
begin

  inherited;
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
  FListViewHeaderHeight := 8; // ��ͷ�߶�
  
  Left := CnFormLeft;
  Top := CnFormTop;
  Inc(CnFormLeft, 20);
  Inc(CnFormTop, 20);
  if CnFormLeft >= Screen.Width - Self.Width - 30 then CnFormLeft := 50;
  if CnFormTop >= Screen.Height - Self.Height - 30 then CnFormTop := 50;

  FImgBk := TBitmap.Create;
  FImgBk.Handle := LoadBitmap(HInstance, 'CNTRANSBK');
  FGraphicBmp := TBitmap.Create;

{$IFDEF FPC}
  // tsSwitch
  tsSwitch := TTabControl.Create(Self);
  tsSwitch.TabPosition := tpBottom;

  // tsTree
  tsTree := TTabControl.Create(Self);
  tsTree.TabPosition := tpBottom;
{$ELSE}
  // tsSwitch
  tsSwitch := TTabSet.Create(Self);

  // tsTree
  tsTree := TTabSet.Create(Self);
{$ENDIF}

  // tsSwitch
  tsSwitch.Name := 'tsSwitch';
  tsSwitch.Parent := pnlSwitchTab;
  tsSwitch.Left := 0;
  tsSwitch.Top := 0;
  tsSwitch.Width := 351;
  tsSwitch.Height := 21;
  tsSwitch.Align := alClient;
  tsSwitch.Font.Charset := ANSI_CHARSET;
  tsSwitch.Font.Color := clWindowText;
  tsSwitch.Font.Height := -11;
  tsSwitch.Font.Name := 'Tahoma';
  tsSwitch.Font.Style := [];

  tsSwitch.Tabs.Clear;
  tsSwitch.Tabs.Add('Properties');
  tsSwitch.Tabs.Add('Events');
  tsSwitch.Tabs.Add('Methods');
  tsSwitch.Tabs.Add('Strings');
  tsSwitch.Tabs.Add('CollectionItems');
  tsSwitch.Tabs.Add('Components');
  tsSwitch.Tabs.Add('Controls');
  tsSwitch.Tabs.Add('Hierarchy');

  tsSwitch.TabIndex := 0;
  tsSwitch.OnChange := tsSwitchChange;

  // tsTree
  tsTree.Name := 'tsTree';
  tsTree.Parent := pnlTreeTab;
  tsTree.Left := 0;
  tsTree.Top := 0;
  tsTree.Width := 100;
  tsTree.Height := 21;
  tsTree.Align := alClient;
  tsTree.Font.Charset := ANSI_CHARSET;
  tsTree.Font.Color := clWindowText;
  tsTree.Font.Height := -11;
  tsTree.Font.Name := 'Tahoma';
  tsTree.Font.Style := [];

  tsTree.Tabs.Clear;
  tsTree.Tabs.Add('Components');
  tsTree.Tabs.Add('Controls');
  tsTree.Tabs.Add('Screen Forms');

  tsTree.TabIndex := 0;
  tsTree.OnChange := tsTreeChange;

{$IFDEF COMPILER7_UP}
  pnlHierarchy.ParentBackground := False;
{$ENDIF}
end;

procedure TCnPropSheetForm.InspectObject(Data: Pointer);
const
  IMG_MARGIN = 15;
  IMG_INTERVAL = 30;
var
  I, ImgTop, ImgLeft: Integer;
  ImageList: TCustomImageList;
  CountInLine, PaintHeight: Integer;

  procedure InternalDrawGraphic(AGraphic: TGraphic);
  const
    EMPTY_STR = '<Empty>';
  var
    S: string;
    Bmp32Draw: Boolean;
//    Bf: TBlendFunction;
  begin
    if AGraphic = nil then
      Exit;

{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
    Bmp32Draw := False; // ֧�� AlphaFormat �ģ������Լ���
{$ELSE}
    Bmp32Draw := (AGraphic is TBitmap) and ((AGraphic as TBitmap).PixelFormat = pf32bit);
{$ENDIF}

    if Bmp32Draw then
      FGraphicBmp.PixelFormat := pf32bit
    else
      FGraphicBmp.PixelFormat := pf24bit;

    FGraphicBmp.Height := AGraphic.Height;
    FGraphicBmp.Width := AGraphic.Width;

//    if AGraphic.Transparent
//{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
//      or AGraphic.SupportsPartialTransparency
//{$ENDIF}
//      then // �����͸���Ȼ��͸���ȣ���

    // �Ȳ�����������������
    TileBkToImageBmp;

    // ��֧�� Alpha �������£�32 λ���ֱ�ӵ��� Draw ��ử���ڵ�
    // ����� AlphaBlend �������ж��������Ƿ� PreMultiply ����Ȩ��һ�»���ֱ�ӻ�

//    if Bmp32Draw and not AGraphic.Empty then
//    begin
//      // ��֧�� Alpha �������£�32 λλͼ�Լ�����͸�������������û�ɹ������Ȳ���
//      Bf.BlendOp := AC_SRC_OVER;
//      Bf.BlendFlags := 0;
//      Bf.SourceConstantAlpha := $FF;
//      Bf.AlphaFormat := AC_SRC_ALPHA;
//
//      Windows.AlphaBlend(FGraphicBmp.Canvas.Handle, 0, 0, FGraphicBmp.Width, FGraphicBmp.Height,
//        (AGraphic as TBitmap).Canvas.Handle, 0, 0, FGraphicBmp.Width, FGraphicBmp.Height, Bf);
//    end
//    else
      FGraphicBmp.Canvas.Draw(0, 0, AGraphic);
      // Draw ��װ��ȫ͸���ȵĻ����Լ���͸����ϻ���������ֻ�� XE2 �����ϰ汾������

    if AGraphic.Empty then
      S := EMPTY_STR
    else
    begin
      S := Format('W: %d, H: %d.', [AGraphic.Width, AGraphic.Height]);

      if AGraphic.Transparent then
        S := S + ' Transparent.'
{$IFDEF TGRAPHIC_SUPPORT_PARTIALTRANSPARENCY}
      else if AGraphic.SupportsPartialTransparency then // ���֧�� Alpha ͸����
        S := S + ' Alpha Transparent.'
{$ENDIF}
      else
        S := S + ' No Transparent.';
    end;

    lblGraphicInfo.Caption := S;
  end;

begin
  if FInspector = nil then
  begin
    if FInspectorClass = nil then // ���δָ�� InspectorClass ����Ĭ�ϵģ���Ҳ�������
      FInspectorClass := ObjectInspectorClass;
    if FInspectorClass = nil then
      Exit;

    FInspector := TCnObjectInspector(FInspectorClass.NewInstance);
    FInspector.Create(Data);
  end;

  // �����ڲ��¼�
  FInspector.OnAfterEvaluateProperties := AfterEvaluateProperties;
  FInspector.OnAfterEvaluateComponents := AfterEvaluateComponents;
  FInspector.OnAfterEvaluateControls := AfterEvaluateControls;
  FInspector.OnAfterEvaluateCollections := AfterEvaluateCollections;
  FInspector.OnAfterEvaluateHierarchy := AfterEvaluateHierarchy;

  if FObjectPointer <> nil then
    FInspector.ObjectAddr := FObjectPointer
  else if FObjectExpr <> '' then
  begin
    // �� FObjectExpr ���ַ����������� Inspector ʵ���� ObjectAddr��Inspector �ڲ��������ݴ���
    FInspector.ObjectAddr := PChar(FObjectExpr);
  end;

  FInspector.InspectObject;

  while not FInspector.InspectComplete do
    Application.ProcessMessages;

  lvProp.Items.Clear;
  lvEvent.Items.Clear;
  lvField.Items.Clear;
  lvMethod.Items.Clear;
  lvCollectionItem.Items.Clear;
  lvMenuItem.Items.Clear;
  lvComp.Items.Clear;
  lvControl.Items.Clear;

  if FInspector.ObjClassName <> '' then
    edtClassName.Text := FInspector.ObjClassName
  else
    edtClassName.Text := 'Unknown Object';

  if FObjectExpr = '' then
  begin
{$IFDEF WIN64}
    edtObj.Text := Format('%16.16x', [NativeInt(FInspector.ObjectAddr)]);
    edtClassName.Text := Format('%s: $%16.16x', [edtClassName.Text, NativeInt(FInspector.ObjectAddr)]);
{$ELSE}
    edtObj.Text := Format('%8.8x', [Integer(FInspector.ObjectAddr)]);
    edtClassName.Text := Format('%s: $%8.8x', [edtClassName.Text, Integer(FInspector.ObjectAddr)]);
{$ENDIF}
  end;

  for I := 0 to FInspector.PropCount - 1 do
  begin
    with lvProp.Items.Add do
    begin
      Data := FInspector.Properties.Items[I];
      Caption := TCnPropertyObject(FInspector.Properties.Items[I]).PropName;
      SubItems.Add(TCnPropertyObject(FInspector.Properties.Items[I]).PropTypeName);
      SubItems.Add(TCnPropertyObject(FInspector.Properties.Items[I]).DisplayValue);
    end;
  end;

{$IFDEF SUPPORT_ENHANCED_RTTI}
  for I := 0 to FInspector.FieldCount - 1 do
  begin
    with lvField.Items.Add do
    begin
      Data := FInspector.Fields.Items[I];
      Caption := TCnFieldObject(FInspector.Fields.Items[I]).FieldName;
      SubItems.Add(TCnFieldObject(FInspector.Fields.Items[I]).FieldTypeName);
      SubItems.Add(TCnFieldObject(FInspector.Fields.Items[I]).DisplayValue);
    end;
  end;
{$ENDIF}

  for I := 0 to FInspector.EventCount - 1 do
  begin
    with lvEvent.Items.Add do
    begin
      Data := FInspector.Events.Items[I];
      Caption := TCnEventObject(FInspector.Events.Items[I]).EventName;
      SubItems.Add(TCnEventObject(FInspector.Events.Items[I]).EventType);
      SubItems.Add(TCnEventObject(FInspector.Events.Items[I]).DisplayValue);
    end;
  end;

  for I := 0 to FInspector.MethodCount - 1 do
  begin
    with lvMethod.Items.Add do
    begin
      Data := FInspector.Methods.Items[I];
      Caption := TCnMethodObject(FInspector.Methods.Items[I]).MethodSimpleName;
      SubItems.Add(TCnMethodObject(FInspector.Methods.Items[I]).Address);
      SubItems.Add(TCnMethodObject(FInspector.Methods.Items[I]).FullName);
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

  pbGraphic.Canvas.FillRect(pbGraphic.Canvas.ClipRect);
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
    else if FGraphicObject is TCustomImageList then
    begin
      if (FGraphicObject as TCustomImageList).Count > 0 then
      begin
        // ���� ImageList �ߴ��Լ� PaintBox �ߴ����Ű����
        FGraphicBmp.Width := bxGraphic.Width;
        FGraphicBmp.Canvas.Brush.Color := clWhite;
        FGraphicBmp.Canvas.Brush.Style := bsSolid;
        FGraphicBmp.Canvas.FillRect(Rect(0, 0, FGraphicBmp.Width, FGraphicBmp.Height));

        ImageList := FGraphicObject as TCustomImageList;

        // һ�л� (FGraphicBmp.Width - IMG_MARGIN) div (ImageList.Width + IMG_INTERVAL) ��
        CountInLine := (FGraphicBmp.Width - IMG_MARGIN) div (ImageList.Width + IMG_INTERVAL);

        // �ܻ� ImageList.Count div CountInLine + 1 �У�������߶�
        PaintHeight := (ImageList.Count div CountInLine + 1) * (ImageList.Height + IMG_INTERVAL) + IMG_INTERVAL;
        if bxGraphic.Height > PaintHeight then
          FGraphicBmp.Height := bxGraphic.Height  // ��������߶�
        else
        begin
          FGraphicBmp.Height := PaintHeight;      // ���򳬳��߶�
          pbGraphic.Height := PaintHeight;
        end;

        ImgLeft := IMG_MARGIN;
        ImgTop := IMG_MARGIN;

        for I := 0 to ImageList.Count - 1 do
        begin
          ImageList.Draw(FGraphicBmp.Canvas, ImgLeft, ImgTop, I);
          Inc(ImgLeft, ImageList.Width + IMG_INTERVAL);
          if ImgLeft + IMG_MARGIN > FGraphicBmp.Width - ImageList.Width then
          begin
            Inc(ImgTop, ImageList.Height + IMG_INTERVAL);
            ImgLeft := IMG_MARGIN;
          end;
        end;
      end;
    end;
    pbGraphic.Invalidate;
  end;

  UpdateHierarchys;
  ContentTypes := FInspector.ContentTypes;

  if FObjectExpr = '' then
  begin
    btnLocate.Visible := (TObject(FInspector.ObjectAddr) is TGraphicControl) or
      (TObject(FInspector.ObjectAddr) is TWinControl)
      {$IFDEF ENABLE_FMX} or CnFmxIsInheritedFromControl(TObject(FInspector.ObjectAddr)) {$ENDIF};
  end
  else
    btnLocate.Visible := False;
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
  begin
    if AType in FContentTypes then
      tsSwitch.Tabs.Add(SCnPropContentType[AType]);
  end;

  tsSwitch.TabIndex := 0;
{$IFDEF FPC}
  tsSwitch.OnChange(tsSwitch);
{$ENDIF}
end;

procedure TCnPropSheetForm.UpdateUIStrings;
begin

end;

procedure TCnPropSheetForm.FormResize(Sender: TObject);
var
  FixWidth: Integer;
begin
  if Parent <> nil then
    FixWidth := 16
  else
    FixWidth := 24;

  lvProp.Columns[2].Width := Self.ClientWidth - lvProp.Columns[0].Width - lvProp.Columns[1].Width - FixWidth;
  lvEvent.Columns[2].Width := Self.ClientWidth - lvEvent.Columns[0].Width - lvEvent.Columns[1].Width - FixWidth;
  lvField.Columns[2].Width := Self.ClientWidth - lvField.Columns[0].Width - lvField.Columns[1].Width - FixWidth;
  lvMethod.Columns[2].Width := Self.ClientWidth - lvMethod.Columns[0].Width - lvMethod.Columns[1].Width - FixWidth;
  lvCollectionItem.Columns[1].Width := Self.ClientWidth - lvCollectionItem.Columns[0].Width - FixWidth;
  lvMenuItem.Columns[1].Width := Self.ClientWidth - lvMenuItem.Columns[0].Width - FixWidth;
  lvComp.Columns[1].Width := Self.ClientWidth - lvComp.Columns[0].Width - FixWidth;
  lvControl.Columns[1].Width := Self.ClientWidth - lvControl.Columns[0].Width - FixWidth;
  UpdatePanelPositions;
end;

procedure TCnPropSheetForm.FormDestroy(Sender: TObject);
begin
  FGraphicBmp.Free;
  FImgBk.Free;
  FHierarchys.Free;
  FHierLines.Free;
  FHierPanels.Free;
  FComponentTree.Free;
  FControlTree.Free;
  FScreenTree.Free;

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
  // ���� FHierarchys ���� Hierarchy ͼ
  FHierPanels.Clear;
  FHierLines.Clear;
  for I := 0 to FHierarchys.Count - 1 do
  begin
    APanel := TPanel.Create(nil);
    APanel.Caption := FHierarchys.Strings[I];
    APanel.BevelOuter := bvNone;
    APanel.BevelInner := bvRaised;
    APanel.Parent := pnlHierarchy;
    APanel.Color := clBtnFace;
{$IFDEF COMPILER7_UP}
    APanel.ParentBackground := False;
{$ENDIF}
    FHierPanels.Add(APanel);

    ABevel := TBevel.Create(nil);
    ABevel.Shape := bsLeftLine;
    ABevel.Parent := pnlHierarchy;
    FHierLines.Add(ABevel);
  end;
  UpdatePanelPositions;
end;

procedure TCnPropSheetForm.tsSwitchChange(Sender: TObject {$IFNDEF FPC}; NewTab: Integer;
  var AllowChange: Boolean {$ENDIF});
var
  Str: string;
  AControl: TWinControl;
  NeedChangePanel: Boolean;
begin
  AControl := nil;
  NeedChangePanel := False;
{$IFDEF FPC}
  Str := tsSwitch.Tabs.Strings[tsSwitch.TabIndex];
{$ELSE}
  Str := tsSwitch.Tabs.Strings[NewTab];
{$ENDIF}
  case IndexOfContentTypeStr(Str) of
    pctProps:             AControl := lvProp;
    pctFields:            AControl := lvField;
    pctEvents:            AControl := lvEvent;
    pctMethods:           AControl := lvMethod;
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
var
  Obj: TObject;
{$IFDEF SUPPORT_ENHANCED_RTTI}
  {$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
  I: Integer;
  S: string;
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiIndexedProp: TRttiIndexedProperty;
  IndexParams: array of TValue;
  P: TArray<TRttiParameter>;
  IndexedValue: TValue;
  {$ENDIF}
{$ENDIF}

  // ��ֲ�� A.Bouchez ��ʵ��
  function ObjectFromInterface(const AIntf: IUnknown): TObject;
  begin
    Result := nil;
    if AIntf = nil then
      Exit;

  {$IFDEF SUPPORT_INTERFACE_AS_OBJECT}
    Result := AIntf as TObject;
  {$ELSE}
    with PObjectFromInterfaceStub(PPointer(PPointer(AIntf)^)^)^ do
    case Stub of
      $04244483: Result := Pointer(Integer(AIntf) + ShortJmp);
      $04244481: Result := Pointer(Integer(AIntf) + LongJmp);
      else       Result := nil;
    end;
  {$ENDIF}
  end;

begin
  if FCurrObj <> nil then
  begin
    EvaluatePointer(FCurrObj, FInspectParam, nil, False, Self);
  end
  else if FCurrIntf <> nil then
  begin
    Obj := ObjectFromInterface(FCurrIntf);
    EvaluatePointer(Obj, FInspectParam, nil, False, Self);
  end;

{$IFDEF SUPPORT_ENHANCED_RTTI}
  {$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
  if (FCurrProp <> nil) and (FCurrProp.IndexParamCount > 0) then
  begin
    // TODO: �õ���ȡ��������Ҫ��һ������ Index ֵ������ Rtti ������ȡ��ʵֵ
    RttiContext := TRttiContext.Create;
    IndexParams := nil;
    try
      RttiType := RttiContext.GetType(TObject(FObjectPointer).ClassInfo);
      if RttiType <> nil then
      begin
        S := FCurrProp.PropName; // PropName ������ [I: Integer] ������Ҫȥ��
        I := Pos('[', S);
        if I > 1 then
          S := Copy(S, 1, I - 1);

        RttiIndexedProp := RttiType.GetIndexedProperty(S);
        // �ȴ� Obj �� PropName �õ� RttiIndexedProperty
        if (RttiIndexedProp <> nil) and (RttiIndexedProp.ReadMethod <> nil) then
        begin
          SetLength(IndexParams, FCurrProp.IndexParamCount);
          P := RttiIndexedProp.ReadMethod.GetParameters;

          for I := Low(IndexParams) to High(IndexParams) do
          begin
            S := '0';
            if InputQuery(SCnInputGetIndexedPropertyCaption,
              Format(SCnInputGetIndexedPropertyPrompt, [FCurrProp.IndexNames[I]]), S) then
              // ���� ReadMethod �� GetParam �б����û�������� Index ֵ
              case P[I].ParamType.TypeKind of
                tkInteger, tkInt64:
                  IndexParams[I] := StrToInt(S);
                tkFloat:
                  IndexParams[I] := StrToFloat(S);
                tkChar, tkWChar, tkString, tkWString{$IFDEF UNICODE}, tkUString {$ENDIF}:
                  IndexParams[I] := S;
              end
            else
              Exit;
          end;
          IndexedValue := RttiIndexedProp.GetValue(FObjectPointer, IndexParams);

          if RttiIndexedProp.PropertyType.TypeKind = tkClass then
          begin
            if IndexedValue.AsObject <> nil then
            begin
              EvaluatePointer(IndexedValue.AsObject);
              Exit;
            end;
          end;

          ShowMessage(IndexedValue.ToString);
        end;
      end;
    finally
      SetLength(IndexParams, 0);
      RttiContext.Free;
    end;
  end;
  {$ENDIF}
{$ENDIF}
end;

procedure TCnPropSheetForm.lvPropCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  ARect: TRect;
  ALv: TListView;
  DispObj: TCnDisplayObject;
begin
  DefaultDraw := True;
  if Sender is TListView then
  begin
    ALv := Sender as TListView;
    ListView_GetSubItemRect(ALv.Handle, Item.Index, 0, LVIR_BOUNDS, @ARect);
    ALv.Canvas.Brush.Color := $00FFBBBB;

    if Item <> nil then
    begin
      DispObj := TCnDisplayObject(Item.Data);
      if DispObj <> nil then
      begin
        if DispObj.IsNewRTTI then
          ALv.Canvas.Brush.Color := $00FFCCCC;
      end;
    end;

    if ALv.Focused then
    begin
      if (Item <> nil) and (Item.Data <> nil) and (ALv.Selected = Item) and
        ((TCnDisplayObject(Item.Data).ObjValue <> nil) or
        (TCnDisplayObject(Item.Data).IntfValue <> nil)
{$IFDEF SUPPORT_ENHANCED_RTTI}
  {$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
        or ((TObject(Item.Data) is TCnPropertyObject) and
        (TCnPropertyObject(Item.Data).IndexParamCount > 0)) // ��ʾ�� Index ���͵�����
  {$ENDIF}
{$ENDIF}
        ) then
      begin
        ARect := Item.DisplayRect(drSelectBounds);
        FCurrObj := TCnDisplayObject(Item.Data).ObjValue;
        FCurrIntf := TCnDisplayObject(Item.Data).IntfValue;

{$IFDEF SUPPORT_ENHANCED_RTTI}
  {$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
        if TCnDisplayObject(Item.Data) is TCnPropertyObject then
          FCurrProp := TCnPropertyObject(Item.Data)
        else
          FCurrProp := nil;
  {$ENDIF}
{$ENDIF}

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
    if (Item <> nil) and (Item.Data <> nil) and ((Sender as TListView).Selected = Item) and
       ((TCnDisplayObject(Item.Data).ObjValue <> nil)
{$IFDEF SUPPORT_ENHANCED_RTTI}
  {$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
        or (TCnPropertyObject(Item.Data).IndexParamCount > 0) // ��ʾ�� Index ���͵�����
  {$ENDIF}
{$ENDIF}
      ) then
      // pnlInspectBtn.Visible := True
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
      if SubItem = 1 then
      begin
        ALv.Canvas.Font.Color := clBlack;
        ALv.Canvas.Font.Style := [];
      end
      else
      begin
        ALv.Canvas.Font.Color := clRed;
        ALv.Canvas.Font.Style := [fsBold];
      end;
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
  if (KeyState[VK_SHIFT] and $80) <> 0 then // �� Shift ȫ��
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
    // �ر�ʱ������Դ�Ĵ������ǰ��
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
  EvaluatePointer(FObjectPointer, FInspectParam, Self, FSyncMode);
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
  begin
    EvaluatePointer(Pointer(P), FInspectParam, Self);
    ShowTree := False;
  end;
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
    APanel.Width := pnlHierarchy.ClientWidth - PanelMargin * 2;
    APanel.Top := PanelMargin + I * PanelStep;
    APanel.Height := PanelStep - PanelMargin;
    APanel.Color := clBtnFace;

    ABevel := TBevel(FHierLines.Items[I]);
    ABevel.Left := pnlHierarchy.ClientWidth div 2;
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
    if FInspector.ChangePropertyValue(Prop.PropName, S, Prop) then
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
{$IFDEF ENABLE_FMX}
  R: TRect;
  Ctrl: TComponent;
{$ENDIF}
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
{$IFDEF ENABLE_FMX}
  else if CnFmxIsInheritedFromControl(TObject(FInspector.ObjectAddr)) then
  begin
    Ctrl := TComponent(FInspector.ObjectAddr);
    R := CnFmxGetControlRect(Ctrl);
    CnFmxControlCanvasFillRect(Ctrl, R, clRed);
  end
{$ENDIF}
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

procedure TCnPropSheetForm.pbGraphicMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  S: string;
  Rec: TRect;
  Pt: TPoint;
  C: TColor;

  function GetPixelFormatName(Fmt: TPixelFormat): string;
  begin
    case Fmt of
      pfDevice: Result := 'Device';
      pf1bit: Result := '1 Bit';
      pf4bit: Result := '4 Bit';
      pf8bit: Result := '8 Bit';
      pf15bit: Result := '15 Bit';
      pf16bit: Result := '16 Bit';
      pf24bit: Result := '24 Bit';
      pf32bit: Result := '32 Bit';
      pfCustom: Result := 'Custom';
    end;
  end;

begin
  S := '';
  if not FGraphicBmp.Empty then
  begin
    Rec := Rect(0, 0, FGraphicBmp.Width, FGraphicBmp.Height);
    Pt := Point(X, Y);
    if PtInRect(Rec, Pt) then
    begin
      C := FGraphicBmp.Canvas.Pixels[X, Y];
      S := Format('%s: X: %d, Y: %d, Color $%8.8x', [GetPixelFormatName(FGraphicBmp.PixelFormat),
        X, Y, C]);
    end;
  end;
  lblPixel.Caption := S;
end;

procedure TCnPropSheetForm.SetShowTree(const Value: Boolean);
begin
  if FShowTree <> Value then
  begin
    FShowTree := Value;
    pnlTree.Visible := Value;

    if Value then // ��ʾ��������
    begin
      if pnlTree.Width < CnPnlTreeWidth then
        pnlTree.Width := CnPnlTreeWidth;
      Width := Width + CnPnlTreeWidth;
      Left := Left - CnPnlTreeWidth;
      if Left < 10 then
        Left := 10;
      btnTree.Caption := '>';
    end
    else // ������������
    begin
      Width := Width - CnPnlTreeWidth;
      Left := Left + CnPnlTreeWidth;
      btnTree.Caption := '<';
    end;
  end;
end;

procedure TCnPropSheetForm.btnTreeClick(Sender: TObject);
begin
  ShowTree := not ShowTree;
  if ShowTree then
  begin
    SearchTrees;
    UpdateToTree(tsTree.TabIndex);
  end;
end;

procedure TCnPropSheetForm.SearchTrees;
var
  Comp: TComponent;
  Ctrl, RootControl: TControl;
{$IFDEF ENABLE_FMX}
  FmxCtrl, RootFmxControl: TComponent;
{$ENDIF}
  RootComponent: TComponent;


  procedure AddComponentToTree(AComp: TComponent; ParentLeaf: TCnLeaf = nil);
  var
    I: Integer;
    Leaf: TCnLeaf;
  begin
    if ParentLeaf = nil then
      ParentLeaf := FComponentTree.Root;

    Leaf := FComponentTree.AddChild(ParentLeaf);
    Leaf.Obj := AComp;

{$IFDEF WIN64}
    Leaf.Text := Format('%s: %s: $%16.16x', [AComp.Name, AComp.ClassName, NativeInt(AComp)]);
{$ELSE}
    Leaf.Text := Format('%s: %s: $%8.8x', [AComp.Name, AComp.ClassName, Integer(AComp)]);
{$ENDIF}

    for I := 0 to AComp.ComponentCount - 1 do
      AddComponentToTree(AComp.Components[I], Leaf);
  end;

  procedure AddControltoTree(ACtrl: TControl; ParentLeaf: TCnLeaf = nil);
  var
    I: Integer;
    Leaf: TCnLeaf;
  begin
    if ParentLeaf = nil then
      ParentLeaf := FControlTree.Root;

    Leaf := FControlTree.AddChild(ParentLeaf);
    Leaf.Obj := ACtrl;

{$IFDEF WIN64}
    Leaf.Text := Format('%s: %s: $%16.16x', [ACtrl.Name, ACtrl.ClassName, NativeInt(ACtrl)]);
{$ELSE}
    Leaf.Text := Format('%s: %s: $%8.8x', [ACtrl.Name, ACtrl.ClassName, Integer(ACtrl)]);
{$ENDIF}

    if ACtrl is TWinControl then
    begin
      for I := 0 to (ACtrl as TWinControl).ControlCount - 1 do
        AddControltoTree((ACtrl as TWinControl).Controls[I], Leaf);
    end;
  end;

  procedure AddScreenFormsToTree;
  var
    I: Integer;
    Leaf: TCnLeaf;
    F: TComponent;
  begin
    for I := 0 to Screen.CustomFormCount - 1 do
    begin
      Leaf := FScreenTree.AddChild(FScreenTree.Root);
      F := Screen.CustomForms[I];
      Leaf.Obj := F;
{$IFDEF WIN64}
      Leaf.Text := Format('%s: %s: $%16.16x', [F.Name, F.ClassName, NativeInt(F)]);
{$ELSE}
      Leaf.Text := Format('%s: %s: $%8.8x', [F.Name, F.ClassName, Integer(F)]);
{$ENDIF}
    end;

{$IFDEF ENABLE_FMX}
    for I := 0 to CnFmxGetScreenFormCount - 1 do
    begin
      Leaf := FScreenTree.AddChild(FScreenTree.Root);
      F := CnFmxGetScreenForms(I);
      Leaf.Obj := F;
{$IFDEF WIN64}
      Leaf.Text := Format('%s: %s: $%16.16x', [F.Name, F.ClassName, NativeInt(F)]);
{$ELSE}
      Leaf.Text := Format('%s: %s: $%8.8x', [F.Name, F.ClassName, Integer(F)]);
{$ENDIF}
    end;
{$ENDIF}
  end;

{$IFDEF ENABLE_FMX}
  procedure AddFmxControltoTree(ACtrl: TComponent; ParentLeaf: TCnLeaf = nil);
  var
    I: Integer;
    Leaf: TCnLeaf;
    Ctrl: TComponent;
  begin
    if ParentLeaf = nil then
      ParentLeaf := FControlTree.Root;

    Leaf := FControlTree.AddChild(ParentLeaf);
    Leaf.Obj := ACtrl;

{$IFDEF WIN64}
    Leaf.Text := Format('%s: %s: $%16.16x', [ACtrl.Name, ACtrl.ClassName, NativeInt(ACtrl)]);
{$ELSE}
    Leaf.Text := Format('%s: %s: $%8.8x', [ACtrl.Name, ACtrl.ClassName, Integer(ACtrl)]);
{$ENDIF}

    if CnFmxGetControlsCount(ACtrl) > 0 then
    begin
      for I := 0 to CnFmxGetControlsCount(ACtrl) - 1 do
        AddFmxControltoTree(CnFmxGetControlByIndex(ACtrl, I), Leaf);
    end
    else  // ���� Form ���� Controls ���ԣ��ô� Children ����
    begin
      for I := 0 to CnFmxGetChildrenCount(ACtrl) - 1 do
      begin
        Ctrl := CnFmxGetChildByIndex(ACtrl, I);
        if CnFmxIsInheritedFromControl(Ctrl) then
          AddFmxControltoTree(Ctrl, Leaf);
      end;
    end;
  end;
{$ENDIF}

begin
  if FObjectPointer = nil then
    Exit;

  if FComponentTree = nil then
  begin
    FComponentTree := TCnTree.Create;
    FComponentTree.OnSaveANode := SaveATreeNode;
  end
  else
    FComponentTree.Clear;

  if FControlTree = nil then
  begin
    FControlTree := TCnTree.Create;
    FControlTree.OnSaveANode := SaveATreeNode;
  end
  else
    FControlTree.Clear;

  if FScreenTree = nil then
  begin
    FScreenTree := TCnTree.Create;
    FScreenTree.OnSaveANode := SaveATreeNode;
  end;

  try
    if TObject(FObjectPointer) is TComponent then
    begin
      Comp := TObject(FObjectPointer) as TComponent;
      RootComponent := Comp;
      while RootComponent.Owner <> nil do
        RootComponent := RootComponent.Owner;
      AddComponentToTree(RootComponent);
    end;

    if TObject(FObjectPointer) is TControl then
    begin
      Ctrl := TObject(FObjectPointer) as TControl;
      RootControl := Ctrl;
      while RootControl.Parent <> nil do
        RootControl := RootControl.Parent;
      AddControlToTree(RootControl);
    end;

    AddScreenFormsToTree;

{$IFDEF ENABLE_FMX}
    if CnFmxIsInheritedFromControl(TObject(FObjectPointer)) or
      CnFmxIsInheritedFromCommonCustomForm(TObject(FObjectPointer)) then
    begin
      FmxCtrl := TObject(FObjectPointer) as TComponent;
      RootFmxControl := FmxCtrl;
      while CnFmxGetControlParent(RootFmxControl) <> nil do
        RootFmxControl := CnFmxGetControlParent(RootFmxControl);
      AddFmxControlToTree(RootFmxControl);
    end;
{$ENDIF}
  except
    ; // ������� TObject�������쳣
  end;
end;

procedure TCnPropSheetForm.UpdateToTree(TreeType: Integer);
var
  I: Integer;
  Ptr: Pointer;
begin
  if TreeType = CN_TREE_TYPE_COMPONENT then
    FComponentTree.SaveToTreeView(TreeView)
  else if TreeType = CN_TREE_TYPE_CONTROL then
    FControlTree.SaveToTreeView(TreeView)
  else if TreeType = CN_TREE_TYPE_SCREENFORM then
    FScreenTree.SaveToTreeView(TreeView)
  else
    Exit;

  // չ��
  if TreeView.Items.Count > 0 then
    TreeView.Items[0].Expand(True);

  // ��λ
  for I := 0 to TreeView.Items.Count - 1 do
  begin
    Ptr := TreeView.Items[I].Data;
    if Ptr = FObjectPointer then
    begin
      TreeView.Items[I].Selected := True;
      TreeView.Items[I].MakeVisible;
      TreeView.SetFocus;
      Exit;
    end;
  end;
end;

procedure TCnPropSheetForm.tsTreeChange(Sender: TObject {$IFNDEF FPC}; NewTab: Integer;
  var AllowChange: Boolean {$ENDIF});
begin
{$IFDEF FPC}
  UpdateToTree(tsTree.TabIndex);
{$ELSE}
  UpdateToTree(NewTab);
{$ENDIF}
end;

procedure TCnPropSheetForm.SaveATreeNode(ALeaf: TCnLeaf;
  ATreeNode: TTreeNode; var Valid: Boolean);
begin
  ATreeNode.Text := ALeaf.Text;
  ATreeNode.Data := Pointer(ALeaf.Obj);
end;

procedure TCnPropSheetForm.TreeViewDblClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  Node := TreeView.Selected;
  if Node.Data <> nil then
    EvaluatePointer(Node.Data, FInspectParam, Self);
end;

procedure TCnPropSheetForm.TileBkToImageBmp;
var
  X, Y, DX, DY: Integer;
begin
  DX := FImgBk.Width;
  DY := FImgBk.Height;
  Y := 0;
  while Y < FGraphicBmp.Height do
  begin
    X := 0;
    while X < FGraphicBmp.Width do
    begin
      FGraphicBmp.Canvas.Draw(X, Y, FImgBk);
      Inc(X, DX);
    end;
    Inc(Y, DY);
  end;
end;

{$IFDEF SUPPORT_ENHANCED_RTTI}

{ TCnFieldObject }

constructor TCnFieldObject.Create;
begin
  FIsNewRTTI := True;
  FCanModify := True;
end;

{$ENDIF}

procedure TCnPropSheetForm.btnSearchClick(Sender: TObject);
var
  StartIdx, I: Integer;
  S: string;
begin
  // Search Tree���ӵ�ǰѡ�д������ң��ҵ�β��û���ٻ������ٴ�ͷ�ҵ��Լ���ͣ
  if edtSearch.Text = '' then
    Exit;

  if TreeView.Selected <> nil then
  begin
    StartIdx := TreeView.Selected.AbsoluteIndex + 1;
  end
  else
  begin
    StartIdx := 1;
  end;

  S := UpperCase(edtSearch.Text);

  // ������
  for I := StartIdx to TreeView.Items.Count - 1 do
  begin
    if Pos(S, UpperCase(TreeView.Items[I].Text)) > 0 then
    begin
      TreeView.Items[I].Selected := True;
      TreeView.Items[I].MakeVisible;
      TreeView.SetFocus;
      Exit;
    end;
  end;

  // �ص�ǰ���ҵ��Լ�
  for I := 0 to StartIdx - 1 do
  begin
    if Pos(S, UpperCase(TreeView.Items[I].Text)) > 0 then
    begin
      TreeView.Items[I].Selected := True;
      TreeView.Items[I].MakeVisible;
      TreeView.SetFocus;
      Exit;
    end;
  end;

  MessageBox(Handle, 'NOT Found.', 'Hint', MB_OK or MB_ICONERROR);
end;

procedure TCnPropSheetForm.edtSearchKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
  begin
    btnSearch.Click;
    edtSearch.SetFocus;
  end;
end;

procedure TCnPropSheetForm.Copy1Click(Sender: TObject);
var
  LV: TListView;
begin
  if pmSheet.PopupComponent is TListView then
  begin
    LV := pmSheet.PopupComponent as TListView;
    if LV.Selected <> nil then
    begin
      if LV.Columns.Count = 2 then
        Clipboard.AsText := LV.Selected.Caption + ' ' + LV.Selected.SubItems[0]
      else if LV.Columns.Count = 1 then
        Clipboard.AsText := LV.Selected.Caption
    end;
  end;
end;

procedure TCnPropSheetForm.CopyAll1Click(Sender: TObject);
var
  SL: TStringList;
  LV: TListView;
  I: Integer;
begin
  if pmSheet.PopupComponent is TListView then
  begin
    LV := pmSheet.PopupComponent as TListView;
    if LV.Items.Count = 0 then
      Exit;

    SL := TStringList.Create;
    try
      if LV.Columns.Count = 2 then
      begin
        for I := 0 to LV.Items.Count - 1 do
          SL.Add(LV.Items[I].Caption + ' ' + LV.Items[I].SubItems[0]);
      end
      else if LV.Columns.Count = 1 then
      begin
        for I := 0 to LV.Items.Count - 1 do
          SL.Add(LV.Items[I].Caption);
      end;
      Clipboard.AsText := SL.Text;
    finally
      SL.Free;
    end;
  end;
end;

procedure TCnPropSheetForm.pbGraphicPaint(Sender: TObject);
var
  R: TRect;
begin
  if not FGraphicBmp.Empty and (pbGraphic.Width <> FGraphicBmp.Width) then
    pbGraphic.Width := FGraphicBmp.Width;

  R := Rect(0, 0, pbGraphic.Width, pbGraphic.Height);
  pbGraphic.Canvas.Brush.Color := pbGraphic.Color;
  pbGraphic.Canvas.Brush.Style := bsSolid;
  pbGraphic.Canvas.FillRect(R);

  pbGraphic.Canvas.Draw(0, 0, FGraphicBmp);
end;

procedure TCnPropSheetForm.lvFieldDblClick(Sender: TObject);
{$IFDEF SUPPORT_ENHANCED_RTTI}
var
  Field: TCnFieldObject;
  S: string;
{$ENDIF}
begin
{$IFDEF SUPPORT_ENHANCED_RTTI}
  if lvField.Selected = nil then
    Exit;

  Field := TCnFieldObject(lvField.Selected.Data);
  if (Field = nil) or not Field.CanModify then
    Exit;

  S := Field.DisplayValue;
  if InputQuery(SCnInputNewValueCaption, Format(SCnInputNewValuePrompt, [Field.FieldName]), S) then
  begin
    if FInspector.ChangeFieldValue(Field.FieldName, S, Field) then
      btnRefresh.Click
    else
      ShowMessage(SCnErrorChangeValue);
  end;
{$ENDIF}
end;

{ TCnPropertyObject }

constructor TCnPropertyObject.Create;
begin
  inherited;
{$IFDEF SUPPORT_ENHANCED_RTTI}
{$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
  FIndexNames := TStringList.Create;
{$ENDIF}
{$ENDIF}
end;

destructor TCnPropertyObject.Destroy;
begin
{$IFDEF SUPPORT_ENHANCED_RTTI}
{$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}
  FIndexNames.Free;
{$ENDIF}
{$ENDIF}
  inherited;
end;

{$IFDEF SUPPORT_ENHANCED_RTTI}
{$IFDEF SUPPORT_ENHANCED_INDEXEDPROPERTY}

function TCnPropertyObject.GetIndexNames(Index: Integer): string;
begin
  Result := FIndexNames[Index];
end;

function TCnPropertyObject.AddIndexName(const AName: string): Integer;
begin
  Result := FIndexNames.Add(AName);
end;

{$ENDIF}
{$ENDIF}

initialization
  FSheetList := TComponentList.Create(True);
  ObjectInspectorClass := TCnLocalObjectInspector;

finalization
  // Free All Form Instances.
  FreeAndNil(FSheetList);

end.
