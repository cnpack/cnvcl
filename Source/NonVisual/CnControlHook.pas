{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2009 CnPack 开发组                       }
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

unit CnControlHook;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：控件消息处理过程挂接组件单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：该单元定义了 TCnControlHook 组件，允许通过替换 TControl 子类的
*           WindowProc 属性来获得控件的消息通知。
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnControlHook.pas,v 1.7 2009/01/02 08:27:39 liuxiao Exp $
* 修改记录：2003.04.30 V1.2
*               修正控件在消息处理过程中释放导致挂接对象出错的问题
*           2002.10.19 V1.1
*               重新编写比较完善的组件
*           2002.10.15 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, CnClasses, CnConsts,
  CnCompConsts;

type

//==============================================================================
// 控件挂接子项
//==============================================================================

{ TCnControlHookItem }

  TCnControlHook = class;
  TCnControlHookCollection = class;

  THookMessageEvent = procedure (Sender: TObject; Control: TControl;
    var Msg: TMessage; var Handled: Boolean) of object;
  {* 挂接消息事件
   |<PRE>
     Sender: TObject      - 产生事件的组件
     Control: TControl    - 该消息要发送的控件对象，即被挂接的控件
     var Msg: TMessage    - 消息变量
     var Handled: Boolean - 事件处理过程是否捕获该消息，如果为真将不调用原控件消息过程
   |</PRE>}

  TCnControlHookItem = class(TCollectionItem)
  {* 控件挂接子项类，用于 TCnControlHook 组件中。
     当被挂接的控件释放时，相关联的 Item 对象也会被自动释放，
     用户可不用考虑重复挂接的问题，但也不要静态访问 Item 对象。}
  private
    FOwner: TCnControlHookCollection;
    FControl: TControl;
    FBeforeMessage: THookMessageEvent;
    FAfterMessage: THookMessageEvent;
    procedure SetControl(const Value: TControl);
    procedure Hook;
    procedure UnHook;
  protected
    function DoAfterMessage(Control: TControl; var Msg: TMessage): Boolean; dynamic;
    function DoBeforeMessage(Control: TControl; var Msg: TMessage): Boolean; dynamic;
    property Owner: TCnControlHookCollection read FOwner;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Control: TControl read FControl write SetControl;
    {* 要 Hook 的控件}
    property BeforeMessage: THookMessageEvent read FBeforeMessage write FBeforeMessage;
    {* 控件消息事件，在默认消息处理过程之前调用}
    property AfterMessage: THookMessageEvent read FAfterMessage write FAfterMessage;
    {* 控件消息事件，在默认消息处理过程之后调用}
  end;

//==============================================================================
// 控件挂接列表类
//==============================================================================
   
{ TCnControlHookCollection }

  TCnControlHookCollection = class(TOwnedCollection)
  {* 控件挂接列表类，用于 TCnControlHook 组件中}
  private
    FOwner: TCnControlHook;
    function GetItem(Index: Integer): TCnControlHookItem;
    procedure SetItem(Index: Integer; const Value: TCnControlHookItem);
  protected
    property ControlHook: TCnControlHook read FOwner;
  public
    constructor Create(AOwner: TCnControlHook);
    destructor Destroy; override;
    function Add(Control: TControl): TCnControlHookItem;
    {* 增加一个控件挂接项}
    procedure Remove(Control: TControl);
    {* 删除一个控件挂接项}
    function IndexOf(Control: TControl): Integer;
    {* 查找控件挂接项}
    property Items[Index: Integer]: TCnControlHookItem read GetItem write SetItem; default;
    {* 控件挂接项数组}
  end;

//==============================================================================
// 控件消息过程挂接组件
//==============================================================================
   
{ TCnControlHook }

  TCnControlHook = class(TCnComponent)
  {* 控件消息过程挂接组件，允许通过替换 TControl 子类的 WindowProc 属性来获得控件的消息通知}
  private
    FActive: Boolean;
    FItems: TCnControlHookCollection;
    FBeforeMessage: THookMessageEvent;
    FAfterMessage: THookMessageEvent;
    procedure SetActive(const Value: Boolean);
    procedure SetItems(const Value: TCnControlHookCollection);
  protected
    function DoAfterMessage(Control: TControl; var Msg: TMessage): Boolean; dynamic;
    function DoBeforeMessage(Control: TControl; var Msg: TMessage): Boolean; dynamic;
    procedure Loaded; override;
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    function IndexOf(Control: TControl): Integer;
    {* 返回指定被挂接控件的索引号，如果不存在，返回 -1}
    function Hook(Control: TControl): TCnControlHookItem;
    {* 挂接指定控件，返回挂接项，如果已挂接返回原挂接项}
    procedure UnHook(Control: TControl);
    {* 取消对指定控件的挂接}
    function IsHooked(Control: TControl): Boolean;
    {* 判断指定控件是否被挂接}
  published
    property Active: Boolean read FActive write SetActive default True;
    {* 是否允许使用}
    property Items: TCnControlHookCollection read FItems write SetItems;
    {* 挂接控件列表}
    property BeforeMessage: THookMessageEvent read FBeforeMessage write FBeforeMessage;
    {* 控件消息事件，在默认消息处理过程之前调用}
    property AfterMessage: THookMessageEvent read FAfterMessage write FAfterMessage;
    {* 控件消息事件，在默认消息处理过程之后调用}
  end;

implementation

const
  UM_DESTROYHOOK = WM_USER + 101;

type

//==============================================================================
// 控件消息处理过程挂接对象（私有类）
//==============================================================================
   
{ TCnControlHookObject }

  TCnControlHookMgr = class;

  TCnControlHookObject = class
  private
    FList: TList;
    FControlHookMgr: TCnControlHookMgr;
    FControl: TControl;
    FOldWndProc: TWndMethod;
    FUpdateCount: Integer;
    FAutoFree: Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): TCnControlHookItem;
  protected
    procedure WndProc(var Message: TMessage);
    property Control: TControl read FControl;
    property ControlHookMgr: TCnControlHookMgr read FControlHookMgr;
  public
    constructor Create(AControlHookMgr: TCnControlHookMgr; AControl: TControl);
    destructor Destroy; override;
    function Add(Item: TCnControlHookItem): Integer;
    procedure DoFree;
    function Updating: Boolean;
    
    procedure Delete(Item: TCnControlHookItem); overload;
    procedure Delete(Index: Integer); overload;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TCnControlHookItem read GetItem;
  end;

//==============================================================================
// 控件消息处理过程挂接组件（私有类）
//==============================================================================

{ TCnControlHookMgr }

  TCnControlHookMgr = class(TComponent)
  {* 控件消息挂接组件，通过替换 TControl 子类的 WindowProc 属性来工作}
  private
    FList: TList;
    function GetCount: Integer;
    function GetHookedControls(Index: Integer): TControl;
    function GetItem(Index: Integer): TCnControlHookObject;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure HookControl(Item: TCnControlHookItem);
    procedure UnhookControl(Item: TCnControlHookItem); overload;
    procedure UnhookControl(Control: TControl); overload;
    function IndexOf(Control: TControl): Integer;
    property Count: Integer read GetCount;
    property HookedControls[Index: Integer]: TControl read GetHookedControls;
    property Items[Index: Integer]: TCnControlHookObject read GetItem;
  end;

var
  ControlHookMgr: TCnControlHookMgr;

// 返回挂接管理器
function GetControlHookMgr: TCnControlHookMgr;
begin
  if not Assigned(ControlHookMgr) then
    ControlHookMgr := TCnControlHookMgr.Create(nil);
  Result := ControlHookMgr;
end;
  
//==============================================================================
// 控件消息处理过程挂接对象（私有类）
//==============================================================================
   
{ TCnControlHookObject }

// 构造器
constructor TCnControlHookObject.Create(AControlHookMgr: TCnControlHookMgr;
  AControl: TControl);
begin
  Assert(Assigned(AControlHookMgr) and Assigned(AControl));
  FUpdateCount := 0;
  FAutoFree := False;
  FList := TList.Create;
  FControlHookMgr := AControlHookMgr;
  FControl := AControl;
  FOldWndProc := FControl.WindowProc;
  FControl.WindowProc := WndProc;
  FControl.FreeNotification(FControlHookMgr);
end;

// 析构器
destructor TCnControlHookObject.Destroy;
var
  i: Integer;
begin
  try                                  // 异常保护
    if Assigned(FControl) then
    begin
      FControlHookMgr.FList.Remove(Self);
      FControl.RemoveFreeNotification(FControlHookMgr);
      FControl.WindowProc := FOldWndProc;
      FControl := nil;
    end;

    for i := 0 to Count - 1 do
      Items[i].Free;
    FList.Free;
  except
    Application.HandleException(Self);
  end;
  inherited;
end;

function TCnControlHookObject.Updating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TCnControlHookObject.DoFree;
begin
  if Updating then
  begin
    FAutoFree := True;
    try
      FControlHookMgr.FList.Remove(Self);
      FControl.RemoveFreeNotification(FControlHookMgr);
      FControl.WindowProc := FOldWndProc;
      FControl := nil;
    except
      Application.HandleException(Self);
    end;
  end
  else
    Free;
end;

// 新的消息处理过程
procedure TCnControlHookObject.WndProc(var Message: TMessage);
var
  i: Integer;
  Handled: Boolean;
begin
  try
    Inc(FUpdateCount);
    try
      Handled := False;
      // 调用挂接消息前处理过程
      for i := Count - 1 downto 0 do       // 后挂接的先处理
        if Assigned(Items[i].FOwner) and Assigned(Items[i].FOwner.FOwner) and
          Items[i].FOwner.FOwner.FActive and Items[i].DoBeforeMessage(FControl,
          Message) then
        begin
          Handled := True;
          Break;
        end;

      if Handled then Exit;

      // 调用原处理过程
      if Assigned(FOldWndProc) then
        FOldWndProc(Message);

      // 调用挂接消息后处理过程
      if not FAutoFree then
      begin
        for i := Count - 1 downto 0 do       // 后挂接的先处理
          if Assigned(Items[i].FOwner) and Assigned(Items[i].FOwner.FOwner) and
            Items[i].FOwner.FOwner.FActive and Items[i].DoAfterMessage(FControl,
            Message) then
            Break;
      end;
    finally
      Dec(FUpdateCount);
    end;

    // 此处进行释放
    if FAutoFree then
      Free;
  except
    Application.HandleException(Self);
  end;
end;

//------------------------------------------------------------------------------
// 列表操作方法
//------------------------------------------------------------------------------

// 增加一项
function TCnControlHookObject.Add(Item: TCnControlHookItem): Integer;
begin
  if FList.IndexOf(Item) < 0 then
  begin
    Item.FControl := FControl;
    Result := FList.Add(Item);
  end
  else
    Result := -1;
end;

// 根据索引号删除一项
procedure TCnControlHookObject.Delete(Index: Integer);
begin
  if (Index >= 0) and (Index < FList.Count) then
  begin
    FList.Delete(Index);
    if Count = 0 then                  // 无挂接项时自动释放
      DoFree;
  end;
end;

// 根据子项删除一项
procedure TCnControlHookObject.Delete(Item: TCnControlHookItem);
begin
  Delete(FList.IndexOf(Item));
end;

//------------------------------------------------------------------------------
// 属性读写方法
//------------------------------------------------------------------------------

// Count 属性读方法
function TCnControlHookObject.GetCount: Integer;
begin
  Result := FList.Count;
end;

// Items 数组属性读方法
function TCnControlHookObject.GetItem(Index: Integer): TCnControlHookItem;
begin
  Result := TCnControlHookItem(FList[Index]);
end;

//==============================================================================
// 控件消息处理过程挂接组件（私有类）
//==============================================================================

{ TCnControlHookMgr }

// 构造器
constructor TCnControlHookMgr.Create(AOwner: TComponent);
begin
  inherited;
  FList := TList.Create;
end;

// 析构器
destructor TCnControlHookMgr.Destroy;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
    Items[i].DoFree;

  FList.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// 挂接相关方法
//------------------------------------------------------------------------------

// 组件通知事件
procedure TCnControlHookMgr.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TControl) then
    UnhookControl(TControl(AComponent)); // 控件释放时反挂接
end;

// 返回控件索引号
function TCnControlHookMgr.IndexOf(Control: TControl): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if HookedControls[i] = Control then
    begin
      Result := i;
      Exit;
    end;
end;

// 挂接控件
procedure TCnControlHookMgr.HookControl(Item: TCnControlHookItem);
var
  Obj: TCnControlHookObject;
  Idx: Integer;
begin
  Assert(Assigned(Item) and Assigned(Item.FControl));
  Idx := IndexOf(Item.FControl);
  if Idx < 0 then
  begin
    Obj := TCnControlHookObject.Create(Self, Item.FControl);
    Obj.Add(Item);
    FList.Add(Obj);
  end
  else
    Items[Idx].Add(Item);
end;

// 反挂接控件
procedure TCnControlHookMgr.UnhookControl(Item: TCnControlHookItem);
var
  Idx: Integer;
begin
  Assert(Assigned(Item) and Assigned(Item.FControl));
  Idx := IndexOf(Item.FControl);
  if Idx >= 0 then
    Items[Idx].Delete(Item);
end;

// 反挂接控件
procedure TCnControlHookMgr.UnhookControl(Control: TControl);
var
  Idx: Integer;
begin
  Idx := IndexOf(Control);
  if Idx >= 0 then
    Items[Idx].DoFree;
end;

//------------------------------------------------------------------------------
// 属性读写方法
//------------------------------------------------------------------------------

// HookedControlCount 属性读方法
function TCnControlHookMgr.GetCount: Integer;
begin
  Result := FList.Count;
end;

// HookedControls 属性读方法
function TCnControlHookMgr.GetHookedControls(Index: Integer): TControl;
begin
  Result := TCnControlHookObject(FList[Index]).Control;
end;

// Items 数组属性读方法
function TCnControlHookMgr.GetItem(Index: Integer): TCnControlHookObject;
begin
  Result := TCnControlHookObject(FList[Index]);
end;

//==============================================================================
// 控件挂接子项
//==============================================================================
   
{ TCnControlHookItem }

// 类构造器
constructor TCnControlHookItem.Create(Collection: TCollection);
begin
  inherited;
  Assert(Assigned(Collection));
  FOwner := TCnControlHookCollection(Collection);
end;

// 类析构器
destructor TCnControlHookItem.Destroy;
begin
  if Assigned(FControl) then
    GetControlHookMgr.UnhookControl(Self);
  inherited;
end;

// 对象赋值
procedure TCnControlHookItem.Assign(Source: TPersistent);
begin
  if Source is TCnControlHookItem then
  begin
    TCnControlHookItem(Source).Control := FControl;
  end
  else
    inherited;
end;

// 产生 AfterMessage 事件
function TCnControlHookItem.DoAfterMessage(Control: TControl;
  var Msg: TMessage): Boolean;
begin
  Result := FOwner.FOwner.DoAfterMessage(Control, Msg);
  if not Result and FOwner.FOwner.FActive and Assigned(FAfterMessage) then
    FAfterMessage(Self, Control, Msg, Result);
end;

// 产生 BeforeMessage 事件
function TCnControlHookItem.DoBeforeMessage(Control: TControl;
  var Msg: TMessage): Boolean;
begin
  Result := FOwner.FOwner.DoBeforeMessage(Control, Msg);
  if not Result and FOwner.FOwner.FActive and Assigned(FBeforeMessage) then
    FBeforeMessage(Self, Control, Msg, Result);
end;

// 挂接
procedure TCnControlHookItem.Hook;
begin
  if ([csLoading, csDesigning] * FOwner.FOwner.ComponentState = []) and
    Assigned(FControl) then
    GetControlHookMgr.HookControl(Self);
end;

// 反挂接
procedure TCnControlHookItem.UnHook;
begin
  if ([csLoading, csDesigning] * FOwner.FOwner.ComponentState = []) and
    Assigned(FControl) then
    GetControlHookMgr.UnhookControl(Self);
end;

// Control 属性写方法
procedure TCnControlHookItem.SetControl(const Value: TControl);
begin
  if Value <> FControl then
  begin
    UnHook;
    FControl := Value;
    Hook;
  end;
end;

//==============================================================================
// 控件挂接列表类
//==============================================================================
   
{ TCnControlHookCollection }

// 构造器
constructor TCnControlHookCollection.Create(AOwner: TCnControlHook);
begin
  inherited Create(AOwner, TCnControlHookItem);
  FOwner := AOwner;
end;

// 析构器
destructor TCnControlHookCollection.Destroy;
begin
  inherited;
end;

// 增加一项
function TCnControlHookCollection.Add(Control: TControl): TCnControlHookItem;
var
  Idx: Integer;
begin
  Idx := IndexOf(Control);
  if Idx >= 0 then
    Result := Items[Idx]
  else
  begin
    Result := TCnControlHookItem(inherited Add);
    Result.Control := Control;
  end;
end;

// 删除一项
procedure TCnControlHookCollection.Remove(Control: TControl);
var
  Idx: Integer;
begin
  Idx := IndexOf(Control);
  if Idx >= 0 then
    Items[Idx].Free;
end;

// 查找子项
function TCnControlHookCollection.IndexOf(Control: TControl): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].FControl = Control then
    begin
      Result := i;
      Exit;
    end;
end;

// Items 数组属性读方法
function TCnControlHookCollection.GetItem(
  Index: Integer): TCnControlHookItem;
begin
  Result := TCnControlHookItem(inherited Items[Index]);
end;

// Items 数组属性写方法
procedure TCnControlHookCollection.SetItem(Index: Integer;
  const Value: TCnControlHookItem);
begin
  inherited SetItem(Index, Value);
end;

//==============================================================================
// 控件消息过程挂接组件
//==============================================================================
   
{ TCnControlHook }

// 构造器
constructor TCnControlHook.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TCnControlHookCollection.Create(Self);
  FActive := True;
end;

// 析构器
destructor TCnControlHook.Destroy;
begin
  FItems.Free;
  inherited;
end;

// 运行期属性已装载
procedure TCnControlHook.Loaded;
var
  i: Integer;
begin
  inherited;
  for i := 0 to Items.Count - 1 do
    Items.Items[i].Hook;
end;

// 挂接指定控件，返回挂接项索引号，如果已挂接返回原挂接项索引号
function TCnControlHook.Hook(Control: TControl): TCnControlHookItem;
begin
  Result := Items.Add(Control);
end;

// 返回指定被挂接控件的索引号，如果不存在，返回 -1
function TCnControlHook.IndexOf(Control: TControl): Integer;
begin
  Result := Items.IndexOf(Control);
end;

// 判断指定控件是否被挂接
function TCnControlHook.IsHooked(Control: TControl): Boolean;
begin
  Result := IndexOf(Control) >= 0;
end;

// 取消对指定控件的挂接
procedure TCnControlHook.UnHook(Control: TControl);
begin
  Items.Remove(Control);
end;

//------------------------------------------------------------------------------
// 产生事件方法
//------------------------------------------------------------------------------

// 产生AfterMessage事件
function TCnControlHook.DoAfterMessage(Control: TControl;
  var Msg: TMessage): Boolean;
begin
  Result := False;
  if Active and Assigned(FAfterMessage) then
    FAfterMessage(Self, Control, Msg, Result);
end;

// 产生BeforeMessage事件
function TCnControlHook.DoBeforeMessage(Control: TControl;
  var Msg: TMessage): Boolean;
begin
  Result := False;
  if Active and Assigned(FBeforeMessage) then
    FBeforeMessage(Self, Control, Msg, Result);
end;

// Active 属性写方法
procedure TCnControlHook.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

// Items 属性写方法
procedure TCnControlHook.SetItems(
  const Value: TCnControlHookCollection);
begin
  FItems.Assign(Value);
end;

// 取作者信息
procedure TCnControlHook.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnControlHookName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnControlHookComment;
end;

initialization

finalization
  if Assigned(ControlHookMgr) then
    FreeAndNil(ControlHookMgr);

end.
