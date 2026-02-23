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

unit CnMenuHook;
{ |<PRE>
================================================================================
* 软件名称：CnWizards IDE 专家工具包
* 单元名称：菜单挂接服务单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：该单元用来实现对 IDE 内部 PopupMenu 的挂接操作，通过修改菜单的
*           OnPopup 事件，在弹出前先删除自定义的菜单，执行原来的 OnPopup 后再重
*           新增加定义的菜单，以实现自定义菜单的功能。
*           之所以采用该方法，是因为直接修改 PopupMenu 在 IDE 中可能会导致出错。
*           单元提供了以下类：
*             - TCnAbstractMenuItemDef
*               抽象的用户菜单项基类，如果需要特别定制的菜单处理服务，可以自己
*               从该类中派生。
*             - TCnMenuItemDef
*               普通的用户菜单项类，可以满足绝大部分需要，使用时直接创建该类实
*               例并注册到管理器中即可。
*             - TCnSepMenuItemDef
*               用来生成一个分隔菜单项。
*             - TCnMenuHook
*               菜单管理器，用于管理一组相同功能的菜单，如代码编辑器可能会有多
*               个实例，每个实例都有一个 PopupMenu，这样就可以用一个管理器来管
*               理。管理器提供了挂接 PopupMenu 方法、注册自定义菜单项以及其它
*               服务。
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串支持本地化处理方式
* 修改记录：2021.06.15
*               增加快捷键
*           2003.10.11
*               修改部分标识符，使之更容易理解，增加注释
*           2003.05.01
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Forms, ActnList, Menus, Contnrs,
  CnConsts, CnClasses, CnCompConsts;

type

//==============================================================================
// 抽象的用户菜单项基类
//==============================================================================

{ TCnAbstractMenuItemDef }

  TCnMenuItemInsertPos = (ipFirst, ipLast, ipAfter, ipBefore);
  TCnMenuItemStatus = set of (msVisible, msEnabled, msChecked);

  TCnAbstractMenuItemDef = class(TObject)
  private
    FActive: Boolean;
  protected
    function GetName: string; virtual; abstract;
    function GetInsertPos: TCnMenuItemInsertPos; virtual; abstract;
    function GetRelItemName: string; virtual; abstract;
    function GetCaption: string; virtual; abstract;
    function GetHint: string; virtual; abstract;
    function GetStatus : TCnMenuItemStatus; virtual; abstract;
    function GetAction: TCustomAction; virtual; abstract;
    function GetImageIndex: Integer; virtual; abstract;
    function GetShortCut: TShortCut; virtual; abstract;

    procedure MenuItemCreated(MenuItem: TMenuItem); virtual; abstract;
    {* 当用户菜单项被创建后调用该方法}
  public
    procedure Execute(Sender: TObject); virtual; abstract;
    {* 菜单项执行方法}

    property Active: Boolean read FActive write FActive;
    {* 菜单项定义是否有效，如果无效，则菜单不会自动创建}
    property Name: string read GetName;
    {* 菜单项的组件名}
    property InsertPos: TCnMenuItemInsertPos read GetInsertPos;
    {* 用户菜单项的插入位置}
    property RelItemName: string read GetRelItemName;
    {* 当 InsertPos 为 ipAfter, ipBefore 时，相对的原菜单名}
    property Caption: string read GetCaption;
    {* 菜单项的标题}
    property Hint: string read GetHint;
    {* 菜单项的提示信息}
    property Status: TCnMenuItemStatus read GetStatus;
    {* 菜单项的状态}
    property Action: TCustomAction read GetAction;
    {* 菜单项对应的 Action}
    property ImageIndex: Integer read GetImageIndex;
    {* 菜单项对应的 ImageIndex}
    property ShortCut: TShortCut read GetShortCut;
    {* 菜单项对应的快捷键}
  end;

//==============================================================================
// 普通的用户菜单项类
//==============================================================================

{ TCnMenuItemDef }

  TCnMenuItemCreatedEvent = procedure (Sender: TObject; MenuItem: TMenuItem) of object;

  TCnMenuItemDef = class(TCnAbstractMenuItemDef)
  private
    FName: string;
    FInsertPos: TCnMenuItemInsertPos;
    FRelItemName: string;
    FCaption: string;
    FHint: string;
    FAction: TCustomAction;
    FImageIndex: Integer;
    FShortCut: TShortCut;
    FStatus: TCnMenuItemStatus;
    FOnClick: TNotifyEvent;
    FOnCreated: TCnMenuItemCreatedEvent;
  protected
    function GetName: string; override;
    function GetInsertPos: TCnMenuItemInsertPos; override;
    function GetRelItemName: string; override;
    function GetCaption: string; override;
    function GetHint: string; override;
    function GetStatus: TCnMenuItemStatus; override;
    function GetAction: TCustomAction; override;
    function GetImageIndex: Integer; override;
    function GetShortCut: TShortCut; override;
    procedure MenuItemCreated(MenuItem: TMenuItem); override;
  public
    constructor Create(const AName, ACaption: string; AOnClick: TNotifyEvent;
      AInsertPos: TCnMenuItemInsertPos; const ARelItemName: string = '';
      const AHint: string = ''; AAction: TCustomAction = nil; ImgIndex: Integer = -1;
      AShortCut: TShortCut = 0);
    destructor Destroy; override;
    procedure Execute(Sender: TObject); override;

    procedure SetCaption(const Value: string);
    {* 设置菜单标题}
    procedure SetHint(const Value: string);
    {* 设置菜单提示信息}
    procedure SetImageIndex(Value: Integer);
    {* 设置菜单项的 ImageIndex}

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    {* 菜单点击事件}
    property OnCreated: TCnMenuItemCreatedEvent read FOnCreated write FOnCreated;
    {* 当菜单项被动态创建之后调用，用户可以在该事件中修改菜单属性}
  end;

//==============================================================================
// 分隔菜单项类
//==============================================================================

{ TCnSepMenuItemDef }

  TCnSepMenuItemDef = class(TCnMenuItemDef)
  public
    constructor Create(AInsertPos: TCnMenuItemInsertPos; const ARelItemName: string);
  end;

//==============================================================================
// 被挂接的 TPopupMenu 菜单对象数据类
//==============================================================================

{ TCnMenuObj }

  TCnMenuObj = class(TObject)
  private
    FOldOnPopup: TNotifyEvent;
    FMenu: TPopupMenu;
  public
    constructor Create(AMenu: TPopupMenu; NewOnPopup: TNotifyEvent);
    destructor Destroy; override;
    property Menu: TPopupMenu read FMenu;
    property OldOnPopup: TNotifyEvent read FOldOnPopup;
  end;

//==============================================================================
// 菜单挂接管理器
//==============================================================================

{ TCnMenuHook }

  TCnMenuPopupEvent = procedure (Sender: TObject; Menu: TPopupMenu) of object;

{$IFNDEF FPC}
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
{$ENDIF}
  TCnMenuHook = class(TCnComponent)
  private
    FText: string;
    FMenuList: TObjectList;
    FMenuItemDefList: TObjectList;
    FActive: Boolean;
    FOnAfterPopup: TCnMenuPopupEvent;
    FOnBeforePopup: TCnMenuPopupEvent;
    procedure SetActive(const Value: Boolean);
    function GetMenuItemDef(Index: Integer): TCnAbstractMenuItemDef;
    function GetMenuItemDefCount: Integer;
  protected
    function GetMenuObj(Menu: TPopupMenu): TCnMenuObj;
    procedure OnMenuPopup(Sender: TObject); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function FindMenuItem(AMenu: TPopupMenu; const AName: string): TMenuItem;
    procedure DoRemoveMenuItem(AMenu: TPopupMenu; const AName: string);
    procedure DoAddMenuItem(AMenu: TPopupMenu; Item: TCnAbstractMenuItemDef);

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure HookMenu(AMenu: TPopupMenu);
    {* 挂接一个 PopupMenu 菜单}
    procedure UnHookMenu(AMenu: TPopupMenu);
    {* 取消对 PopupMenu 菜单的挂接}
    function IsHooked(AMenu: TPopupMenu): Boolean;
    {* 判断 PopupMenu 菜单是否已经挂接}

    function AddMenuItemDef(Item: TCnAbstractMenuItemDef): Integer;
    {* 增加一个用户菜单项定义，返回列表索引号}
    procedure RemoveMenuItemDef(Item: TCnAbstractMenuItemDef);
    {* 移去一个用户菜单项定义}
    function IndexOfMenuItemDef(const AName: string): Integer;
    {* 查找指定菜单在列表中的索引号}

    property MenuItemDefCount: Integer read GetMenuItemDefCount;
    {* 用户菜单项定义计数}
    property MenuItemDefs[Index: Integer]: TCnAbstractMenuItemDef read GetMenuItemDef;
    {* 用户菜单项定义数组}

  published
    property Active: Boolean read FActive write SetActive;
    {* 菜单挂接活跃属性}
    property Text: string read FText write FText;
    {* 额外字符串数据}

    property OnBeforePopup: TCnMenuPopupEvent read FOnBeforePopup write FOnBeforePopup;
    {* 被挂接的菜单弹出前事件，此时用户菜单项已经释放，用户可在此进行特别的处理}
    property OnAfterPopup: TCnMenuPopupEvent read FOnAfterPopup write FOnAfterPopup;
    {* 被挂接的菜单弹出后事件，此时用户菜单项已经创建，用户可在此进行特别的处理}
  end;

implementation

const
  csMenuItemTag = $8080;

//==============================================================================
// 普通的用户菜单项类
//==============================================================================

{ TCnMenuItemDef }

constructor TCnMenuItemDef.Create(const AName, ACaption: string;
  AOnClick: TNotifyEvent; AInsertPos: TCnMenuItemInsertPos; const ARelItemName,
  AHint: string; AAction: TCustomAction; ImgIndex: Integer; AShortCut: TShortCut);
begin
  inherited Create;
  FActive := True;
  FStatus := [msVisible, msEnabled];
  FName := AName;
  FCaption := ACaption;
  FOnClick := AOnClick;
  FInsertPos := AInsertPos;
  FRelItemName := ARelItemName;
  FHint := AHint;
  FAction := AAction;
  FImageIndex := ImgIndex;
  FShortCut := AShortCut;
  FOnCreated := nil;
end;

destructor TCnMenuItemDef.Destroy;
begin

  inherited;
end;

procedure TCnMenuItemDef.Execute(Sender: TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Sender);
end;

function TCnMenuItemDef.GetAction: TCustomAction;
begin
  Result := FAction;
end;

function TCnMenuItemDef.GetCaption: string;
begin
  Result := FCaption;
end;

function TCnMenuItemDef.GetHint: string;
begin
  Result := FHint;
end;

function TCnMenuItemDef.GetInsertPos: TCnMenuItemInsertPos;
begin
  Result := FInsertPos;
end;

function TCnMenuItemDef.GetName: string;
begin
  Result := FName;
end;

function TCnMenuItemDef.GetRelItemName: string;
begin
  Result := FRelItemName;
end;

function TCnMenuItemDef.GetStatus: TCnMenuItemStatus;
begin
  Result := FStatus;
end;

procedure TCnMenuItemDef.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TCnMenuItemDef.SetHint(const Value: string);
begin
  FHint := Value;
end;

procedure TCnMenuItemDef.MenuItemCreated(MenuItem: TMenuItem);
begin
  if Assigned(FOnCreated) then
    FOnCreated(Self, MenuItem);
end;

function TCnMenuItemDef.GetImageIndex: Integer;
begin
  Result := FImageIndex;
end;

procedure TCnMenuItemDef.SetImageIndex(Value: Integer);
begin
  FImageIndex := Value;
end;

//==============================================================================
// 分隔菜单项类
//==============================================================================

function TCnMenuItemDef.GetShortCut: TShortCut;
begin
  Result := FShortCut;
end;

{ TCnSepMenuItemDef }

constructor TCnSepMenuItemDef.Create(AInsertPos: TCnMenuItemInsertPos;
  const ARelItemName: string);
begin
  inherited Create('', '-', nil, AInsertPos, ARelItemName, '', nil);
end;

//==============================================================================
// 被挂接的 TPopupMenu 菜单对象数据类
//==============================================================================

{ TMenuObj }

constructor TCnMenuObj.Create(AMenu: TPopupMenu; NewOnPopup: TNotifyEvent);
begin
  inherited Create;
  FMenu := AMenu;
  FOldOnPopup := FMenu.OnPopup;
  FMenu.OnPopup := NewOnPopup;
end;

destructor TCnMenuObj.Destroy;
begin
  FMenu.OnPopup := FOldOnPopup;
  inherited;
end;

//==============================================================================
// 菜单挂接管理器
//==============================================================================

{ TCnMenuHook }

constructor TCnMenuHook.Create(AOwner: TComponent);
begin
  inherited;
  FMenuList := TObjectList.Create;
  FMenuItemDefList := TObjectList.Create;
  FActive := True;
  FOnAfterPopup := nil;
  FOnBeforePopup := nil;
end;

destructor TCnMenuHook.Destroy;
begin
  FMenuItemDefList.Free;
  FMenuList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// 菜单项处理
//------------------------------------------------------------------------------

function TCnMenuHook.FindMenuItem(AMenu: TPopupMenu;
  const AName: string): TMenuItem;
var
  I: Integer;
begin
  Result := nil;
  if (AMenu = nil) or (AName = '') then Exit;

  for I := 0 to AMenu.Items.Count - 1 do
    if SameText(AMenu.Items[I].Name, AName) then
    begin
      Result := AMenu.Items[I];
      Exit;
    end;
end;

procedure TCnMenuHook.DoAddMenuItem(AMenu: TPopupMenu;
  Item: TCnAbstractMenuItemDef);
var
  MenuItem, RelItem: TMenuItem;
  Idx: Integer;
begin
  Assert(Assigned(AMenu));
  Assert(Assigned(Item));
  
  if FActive and Item.Active then
  begin
    MenuItem := FindMenuItem(AMenu, Item.Name);
    if not Assigned(MenuItem) then
    begin
      MenuItem := TMenuItem.Create(AMenu);
      MenuItem.Name := Item.Name;

      RelItem := FindMenuItem(AMenu, Item.RelItemName);
      Idx := 0;
      case Item.InsertPos of
        ipFirst: Idx := 0;
        ipLast: Idx := AMenu.Items.Count;
        ipAfter:
          if Assigned(RelItem) then
            Idx := RelItem.MenuIndex + 1
          else
            Idx := AMenu.Items.Count;
        ipBefore:
          if Assigned(RelItem) then
            Idx := RelItem.MenuIndex
          else
            Idx := 0;
      end;

      AMenu.Items.Insert(Idx, MenuItem);
    end;

    // 定义一个 Tag，以标志没有 Name 的自定义菜单
    MenuItem.Tag := csMenuItemTag;
    MenuItem.Caption := Item.Caption;
    MenuItem.Hint := Item.Hint;
    MenuItem.Enabled := msEnabled in Item.Status;
    MenuItem.Visible := msVisible in Item.Status;
    MenuItem.Checked := msChecked in Item.Status;
    MenuItem.ImageIndex := Item.ImageIndex;
    MenuItem.ShortCut := Item.ShortCut;
    MenuItem.OnClick := Item.Execute;
    MenuItem.Action := Item.Action;
    
    Item.MenuItemCreated(MenuItem);
  end
end;

procedure TCnMenuHook.DoRemoveMenuItem(AMenu: TPopupMenu;
  const AName: string);
var
  Item: TMenuItem;
begin
  Item := FindMenuItem(AMenu, AName);
  if Assigned(Item) then
    Item.Free;
end;

//------------------------------------------------------------------------------
// 菜单挂接处理
//------------------------------------------------------------------------------

function TCnMenuHook.GetMenuObj(Menu: TPopupMenu): TCnMenuObj;
var
  I: Integer;
begin
  for I := 0 to FMenuList.Count - 1 do
  begin
    if TCnMenuObj(FMenuList[I]).Menu = Menu then
    begin
      Result := TCnMenuObj(FMenuList[I]);
      Exit;
    end;
  end;
  Result := nil;
end;

procedure TCnMenuHook.HookMenu(AMenu: TPopupMenu);
begin
  if not IsHooked(AMenu) then
  begin
    FMenuList.Add(TCnMenuObj.Create(AMenu, OnMenuPopup));
    AMenu.FreeNotification(Self);
  end;
end;

procedure TCnMenuHook.UnHookMenu(AMenu: TPopupMenu);
var
  Obj: TCnMenuObj;
begin
  Obj := GetMenuObj(AMenu);
  if Assigned(Obj) then
  begin
    Obj.Menu.RemoveFreeNotification(Self);
    FMenuList.Remove(Obj);
  end;
end;

function TCnMenuHook.IsHooked(AMenu: TPopupMenu): Boolean;
begin
  Result := Assigned(GetMenuObj(AMenu)); 
end;

procedure TCnMenuHook.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TPopupMenu) then
    UnHookMenu(AComponent as TPopupMenu)
end;

//------------------------------------------------------------------------------
// 新增菜单挂接项处理
//------------------------------------------------------------------------------

function TCnMenuHook.AddMenuItemDef(
  Item: TCnAbstractMenuItemDef): Integer;
begin
  Result := FMenuItemDefList.IndexOf(Item);
  if Result < 0 then
    Result := FMenuItemDefList.Add(Item);
end;

procedure TCnMenuHook.RemoveMenuItemDef(Item: TCnAbstractMenuItemDef);
begin
  FMenuItemDefList.Remove(Item);
end;

function TCnMenuHook.IndexOfMenuItemDef(
  const AName: string): Integer;
var
  I: Integer;
begin
  for I := 0 to MenuItemDefCount - 1 do
  begin
    if SameText(MenuItemDefs[I].Name, AName) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TCnMenuHook.GetMenuItemDefCount: Integer;
begin
  Result := FMenuItemDefList.Count;
end;

function TCnMenuHook.GetMenuItemDef(
  Index: Integer): TCnAbstractMenuItemDef;
begin
  Result := TCnAbstractMenuItemDef(FMenuItemDefList[Index]);
end;

procedure TCnMenuHook.OnMenuPopup(Sender: TObject);
var
  Menu: TPopupMenu;
  MenuObj: TCnMenuObj;
  I: Integer;
begin
  if not (Sender is TPopupMenu) then
    Exit;
    
  Menu := Sender as TPopupMenu;

  // 必须先把以前注册的菜单清掉，否则会出错
  for I := 0 to MenuItemDefCount - 1 do
    DoRemoveMenuItem(Menu, MenuItemDefs[I].Name);
    
  // 根据 Tag 移去没有名字的菜单项
  for I := Menu.Items.Count - 1 downto 0 do
  begin
    if Menu.Items[I].Tag = csMenuItemTag then
      Menu.Items[I].Free;
  end;

  if Assigned(FOnBeforePopup) then
    FOnBeforePopup(Self, Menu);

  // 调用原来的事件
  MenuObj := GetMenuObj(Menu);
  if Assigned(MenuObj) then
  begin
    if Assigned(MenuObj.OldOnPopup) then
      MenuObj.OldOnPopup(Sender);
  end;

  // 如果菜单项本身没有内容，则说明不会弹出，此处也不添加内容，避免强行弹出
  if Menu.Items.Count = 0 then
    Exit;

  if Active then
  begin
    // 重新更新自定义菜单项
    for I := 0 to MenuItemDefCount - 1 do
    begin
      if MenuItemDefs[I].Active then
        DoAddMenuItem(Menu, MenuItemDefs[I]);
    end;

    if Assigned(FOnAfterPopup) then
      FOnAfterPopup(Self, Menu);
  end;
end;

procedure TCnMenuHook.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TCnMenuHook.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnMenuHookName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnMenuHookComment;
end;

end.
