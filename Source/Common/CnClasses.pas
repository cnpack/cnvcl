{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2018 CnPack 开发组                       }
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

unit CnClasses;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：基本类定义单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：该单元定义了组件包的基础类库
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2018.08.30 V1.4
*               新增 TCnUInt32List/TCnUInt64List 类
*           2003.03.02 V1.3
*               新增 TCnLockObject 类
*           2002.09.10 V1.2
*               修改 TCnComponent 部分方法
*           2002.07.09 V1.1
*               新增少量属性
*           2002.04.08 V1.0
*               新增 TCnComponent 组件基类
*           2002.01.11 V0.01Demo
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, TypInfo, Consts, CnNativeDecl;

type

//==============================================================================
// 用于线程同步的对象类
//==============================================================================

{ TCnLockObject }

  TCnLockObject = class (TObject)
  {* 用于线程同步的对象类}
  private
    FLock: TRTLCriticalSection;
    FLockCount: Integer;
    function GetLocking: Boolean;
  protected
    property LockCount: Integer read FLockCount;
    {* 当前Lock计数，只读属性}
  public
    constructor Create;
    {* 构造器，用于产生一个该类的实例}
    destructor Destroy; override;
    procedure Lock;
    {* 进入临界区，为保证多线程同步而加锁，必须与Unlock成对使用}
    function TryLock: Boolean;
    {* 如果当前Lock计数为零，则加锁返回真，否则返回假。
       如果返回真，必须在操作完成后调用UnLock释放锁}
    procedure Unlock;
    {* 退出临界区，释放同步锁，必须与Lock成对使用}
    property Locking: Boolean read GetLocking;
    {* 取当前加锁状态}
  end;

//==============================================================================
// 使用 RTTI 实现了 Assign 方法的 TPersistent 类
//==============================================================================

{ TCnAssignablePersistent }

  TCnAssignablePersistent = class(TPersistent)
  public
    procedure Assign(Source: TPersistent); override;
  end;

//==============================================================================
// 使用 RTTI 实现了 Assign 方法的 TCollectionItem 类
//==============================================================================

{ TCnAssignableCollectionItem }

  TCnAssignableCollectionItem = class(TCollectionItem)
  public
    procedure Assign(Source: TPersistent); override;
  end;

//==============================================================================
// 使用 RTTI 实现了 Assign 方法的 TCollection 类
//==============================================================================

{ TCnAssignableCollection }

  TCnAssignableCollection = class(TCollection)
  public
    procedure Assign(Source: TPersistent); override;
  end;

//==============================================================================
// 带更新通知、线程安全的持久性类
//==============================================================================

{ TCnPersistent }

  TCnPersistent = class(TPersistent)
  {* 带更新通知，线程安全的持久性类}
  private
    FUpdateCount: Integer;
    FOnChanging: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOwner: TPersistent;
    FLockObject: TCnLockObject;
    function GetLocking: Boolean;
    function GetLockObject: TCnLockObject;
  protected
    function GetOwner: TPersistent; override;
    procedure Changing; virtual;
    {* 对象内容开始更新，如果更新计数为0，产生OnChanging事件，可重载}
    procedure Changed; virtual;
    {* 对象内容已变更，如果更新计数为0，产生OnChange事件，可重载}

    procedure SetUpdating(Updating: Boolean); virtual;
    {* 更新状态变更过程，可重载。
       默认为开始更新时调用Changing，结束时调用Changed}
    function IsUpdating: Boolean;
    {* 当前更新计数是否大于0（正在更新）}

    procedure OnChildChanging(Sender: TObject); virtual;
    {* 子属性开始更新事件处理过程，可做为参数传递给TCnPersistent.Create过程
       默认为产生OnChanging事件，可重载}
    procedure OnChildChange(Sender: TObject); virtual;
    {* 子属性已变更事件处理过程，可做为参数传递给TCnPersistent.Create过程
       默认为产生OnChange事件，可重载}

    property Owner: TPersistent read FOwner write FOwner;
    {* 对象的所有者 }
    property LockObject: TCnLockObject read GetLockObject;
    {* 线程同步对象 }
  public
    constructor Create; overload; virtual;
    {* 构造器，用于产生一个该类的实例，可重载}
    constructor Create(AOwner: TPersistent); overload;
    {* 构造器，参数为实例的所有者，当类直接或间接包含TCollection，并需要作为
       published 属性时使用}
    constructor Create(ChangeProc: TNotifyEvent); overload;
    {* 构造器，参数用于给OnChange事件指定一个初始值}
    constructor Create(ChangingProc, ChangeProc: TNotifyEvent); overload;
    {* 构造器，参数用于给OnChanging和OnChange事件指定一个初始值}
    destructor Destroy; override;

    procedure BeginUpdate; virtual;
    {* 开始更新，如果当前更新计数为0，自动调用Changing方法，可重载。
       在对成批属性进行修改时请调用该方法，注意必须与EndUpdate成对使用}
    procedure EndUpdate; virtual;
    {* 结束更新，如果当前更新计数为0，自动调用Change方法，可重载。
       在对成批属性修改后请调用该方法，注意必须与BeginUpdate成对使用}

    procedure Lock;
    {* 进入临界区，为保证多线程同步而加锁，必须与Unlock成对使用}
    function TryLock: Boolean;
    {* 如果当前Lock计数为零，则加锁返回真，否则返回假。
       如果返回真，必须在操作完成后调用UnLock释放锁}
    procedure Unlock;
    {* 退出临界区，释放同步锁，必须与Lock成对使用}

    property Locking: Boolean read GetLocking;
    {* 取当前加锁状态}
  published
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    {* 对象开始更新事件}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {* 对象属性已变更事件}
  end;

//==============================================================================
// 带Enabled的更新通知持久性类
//==============================================================================

{ TCnEnabledPersistent }

  TCnEnabledPersistent = class(TCnPersistent)
  {* 带Enabled的更新通知持久性类}
  private
    FEnabled: Boolean;
  protected
    procedure SetEnabled(const Value: Boolean); virtual;
    procedure SetUpdating(Updating: Boolean); override;
  public
    constructor Create; override;
    {* 构造器，用于产生一个该类的实例}
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    {* Enabled属性，如果为假，对Changing、Changed方法的调用将不产生更新事件}
  end;

//==============================================================================
// 带更新通知的持久性类
//==============================================================================

{ TCnNotifyClass }

  TCnNotifyClass = class(TPersistent)
  {* 带更新通知的持久性类，控件包中大部分持久类的基类，一般不需要直接使用}
  private
    FOnChanged: TNotifyEvent;
  protected
    FOwner: TPersistent;
    procedure Changed; virtual;
    procedure OnChildChanged(Sender: TObject); virtual;
    function GetOwner: TPersistent; override;
  public
    constructor Create(ChangedProc: TNotifyEvent); virtual;
    {* 类构造器，参数为通知事件}
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法}
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    {* 属性已变更事件}
  end;

//==============================================================================
// 不可视组件基础类
//==============================================================================

{ TCnComponent }

  TCnCopyright = type string;

  TCnComponent = class(TComponent)
  {* CnPack不可视组件基类}
  private
    FAbout: TCnCopyright;
    procedure SetAbout(const Value: TCnCopyright);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); virtual;
      abstract;
    {* 取组件信息，用于提供组件的说明和版权信息。抽象方法，子类必须实现。
     |<PRE>
       var AName: string      - 组件名称，可以是支持本地化的字符串
       var Author: string     - 组件作者，如果有多个作者，用分号分隔
       var Email: string      - 组件作者邮箱，如果有多个作者，用分号分隔
       var Comment:           - 组件说明，可以是支持本地化带换行符的字符串
     |</PRE>}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property About: TCnCopyright read FAbout write SetAbout stored False;
    {* 组件版本属性，仅在设计期使用}
  end;

//==============================================================================
// 单实例接口对象基础类
//==============================================================================

{ TSingletonInterfacedObject }

  TSingletonInterfacedObject = class(TInterfacedObject)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

//==============================================================================
// UInt32 列表类
//==============================================================================

const
  CN_MAX_UINT32_SIZE = MaxInt div 16;

type
  PCnUInt32Array = ^TCnUInt32Array;
  TCnUInt32Array = array[0..CN_MAX_UINT32_SIZE - 1] of Cardinal;

  TCnUInt32List = class(TObject)
  {* 容纳 UInt32 的 List}
  private
    FList: PCnUInt32Array;
    FCount: Integer;
    FCapacity: Integer;
    FIgnoreDuplicated: Boolean;
  protected
    function Get(Index: Integer): Cardinal;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Cardinal);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Cardinal): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Exchange(Index1, Index2: Integer);
    function Expand: TCnUInt32List;
    function Extract(Item: Cardinal): Cardinal;
    function First: Cardinal;
    function IndexOf(Item: Cardinal): Integer;
    procedure Insert(Index: Integer; Item: Cardinal);
    function Last: Cardinal;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Cardinal): Integer;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Cardinal read Get write Put; default;
    property List: PCnUInt32Array read FList;
    property IgnoreDuplicated: Boolean read FIgnoreDuplicated write FIgnoreDuplicated;
  end;

//==============================================================================
// UInt64 列表类
//==============================================================================

const
  CN_MAX_UINT64_SIZE = MaxInt div 16;
  CN_NOT_FOUND_INDEX = TUInt64(-1);

type
  PCnUInt64Array = ^TCnUInt64Array;
  TCnUInt64Array = array[0..CN_MAX_UINT64_SIZE - 1] of TUInt64;

  TCnUInt64List = class(TObject)
  {* 容纳 UInt64 的 List，不支持 UInt64 的平台下用 Int64 代替}
  private
    FList: PCnUInt64Array;
    FCount: TUInt64;
    FCapacity: TUInt64;
    FIgnoreDuplicated: Boolean;
  protected
    function Get(Index: TUInt64): TUInt64;
    procedure Grow; virtual;
    procedure Put(Index: TUInt64; Item: TUInt64);
    procedure SetCapacity(NewCapacity: TUInt64);
    procedure SetCount(NewCount: TUInt64);
  public
    destructor Destroy; override;
    function Add(Item: TUInt64): TUInt64;
    procedure Clear; virtual;
    procedure Delete(Index: TUInt64);
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Exchange(Index1, Index2: TUInt64);
    function Expand: TCnUInt64List;
    function Extract(Item: TUInt64): TUInt64;
    function First: TUInt64;
    function IndexOf(Item: TUInt64): TUInt64;
    // 由于下标用 TUInt64，之前返回 -1 在 UInt64 场合下不能用大于 0 来处理，
    // 得判断是否等于 CN_NOT_FOUND_INDEX
    procedure Insert(Index: TUInt64; Item: TUInt64);
    function Last: TUInt64;
    procedure Move(CurIndex, NewIndex: TUInt64);
    function Remove(Item: TUInt64): TUInt64;
    property Capacity: TUInt64 read FCapacity write SetCapacity;
    property Count: TUInt64 read FCount write SetCount;
    property Items[Index: TUInt64]: TUInt64 read Get write Put; default;
    // 内部下标、尺寸均由 TUInt64 表示，不过由于编译器限制实际上达不到 TUInt64
    property List: PCnUInt64Array read FList;
    property IgnoreDuplicated: Boolean read FIgnoreDuplicated write FIgnoreDuplicated;
  end;

procedure AssignPersistent(Source, Dest: TPersistent; UseDefineProperties:
  Boolean = True);

implementation

uses
  CnConsts;

type
  TPersistentHack = class(TPersistent);

procedure AssignPersistent(Source, Dest: TPersistent; UseDefineProperties: 
  Boolean = True);
var
  Stream: TMemoryStream;
  Reader: TReader;
  Writer: TWriter;
  Count: Integer;
  PropIdx: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
begin
  if Source is Dest.ClassType then
  begin
    // 使用 RTTI 来保证赋值所有 published 属性（流不能传递值为 Default 的属性）
    Count := GetPropList(Dest.ClassInfo, tkProperties - [tkArray, tkRecord,
      tkInterface], nil);
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      GetPropList(Source.ClassInfo, tkProperties - [tkArray, tkRecord,
        tkInterface], @PropList^[0]);
      for PropIdx := 0 to Count - 1 do
      begin
        PropInfo := PropList^[PropIdx];
        case PropInfo^.PropType^^.Kind of
          tkInteger, tkChar, tkWChar, tkClass, tkEnumeration, tkSet:
            SetOrdProp(Dest, PropInfo, GetOrdProp(Source, PropInfo));
          tkFloat:
            SetFloatProp(Dest, PropInfo, GetFloatProp(Source, PropInfo));
          tkString, tkLString, tkWString{$IFDEF UNICODE_STRING}, tkUString{$ENDIF}:
            SetStrProp(Dest, PropInfo, GetStrProp(Source, PropInfo));
          tkVariant:
            SetVariantProp(Dest, PropInfo, GetVariantProp(Source, PropInfo));
          tkInt64:
            SetInt64Prop(Dest, PropInfo, GetInt64Prop(Source, PropInfo));
          tkMethod:
            SetMethodProp(Dest, PropInfo, GetMethodProp(Source, PropInfo));
        end;
      end;
    finally
      FreeMem(PropList);
    end;

    // 使用流来传递自定义的属性
    if UseDefineProperties then
    begin
      Stream := nil;
      Reader := nil;
      Writer := nil;
      try
        Stream := TMemoryStream.Create;
        Writer := TWriter.Create(Stream, 4096);
        TPersistentHack(Source).DefineProperties(Writer);
        Writer.FlushBuffer;
        Stream.Position := 0;
        Reader := TReader.Create(Stream, 4096);
        TPersistentHack(Dest).DefineProperties(Reader);
      finally
        FreeAndNil(Reader);
        FreeAndNil(Writer);
        FreeAndNil(Stream);
      end;
    end;
  end;
end;

//==============================================================================
// 支持线程安全的基础类
//==============================================================================

var
  CounterLock: TRTLCriticalSection;

{ TCnLockObject }

// 初始化
constructor TCnLockObject.Create;
begin
  inherited;
  InitializeCriticalSection(FLock); // 初始化临界区
end;

// 释放
destructor TCnLockObject.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

// 尝试进入临界区（如果已加锁返回 False）
function TCnLockObject.TryLock: Boolean;
begin
  EnterCriticalSection(CounterLock);
  try
    Result := FLockCount = 0;
    if Result then Lock;
  finally
    LeaveCriticalSection(CounterLock);
  end;
end;

// 加锁
procedure TCnLockObject.Lock;
begin
  EnterCriticalSection(CounterLock);
  Inc(FLockCount);
  LeaveCriticalSection(CounterLock);
  EnterCriticalSection(FLock);
end;

// 释放锁
procedure TCnLockObject.Unlock;
begin
  LeaveCriticalSection(FLock);
  EnterCriticalSection(CounterLock);
  Dec(FLockCount);
  LeaveCriticalSection(CounterLock);
end;

function TCnLockObject.GetLocking: Boolean;
begin
  Result := FLockCount > 0;
end;

//==============================================================================
// 使用 RTTI 实现了 Assign 方法的 TPersistent 类
//==============================================================================

{ TCnAssignablePersistent }

procedure TCnAssignablePersistent.Assign(Source: TPersistent);
begin
  if Source is ClassType then 
  begin
    AssignPersistent(Source, Self);
  end
  else
    inherited Assign(Source);
end;

//==============================================================================
// 使用 RTTI 实现了 Assign 方法的 TCollectionItem 类
//==============================================================================

{ TCnAssignableCollectionItem }

procedure TCnAssignableCollectionItem.Assign(Source: TPersistent);
begin
  if Source is ClassType then
  begin
    AssignPersistent(Source, Self);
  end
  else
    inherited Assign(Source);
end;

//==============================================================================
// 使用 RTTI 实现了 Assign 方法的 TCollection 类
//==============================================================================

{ TCnAssignableCollection }

procedure TCnAssignableCollection.Assign(Source: TPersistent);
begin
  if Source is ClassType then
  begin
    AssignPersistent(Source, Self);
  end;
  inherited Assign(Source);
end;

//==============================================================================
// 带更新通知、线程安全的持久性类
//==============================================================================

{ TCnPersistent }

// 初始化（供重载）
constructor TCnPersistent.Create;
begin
  inherited;
  FUpdateCount := 0;
end;

// 初始化，参数为实例的所有者
constructor TCnPersistent.Create(AOwner: TPersistent);
begin
  Create;
  FOwner := AOwner;
end;

// 初始化，参数为更新通知事件
constructor TCnPersistent.Create(ChangeProc: TNotifyEvent);
begin
  Create;
  FOnChange := ChangeProc;
end;

// 初始化，参数为更新通知事件
constructor TCnPersistent.Create(ChangingProc, ChangeProc: TNotifyEvent);
begin
  Create;
  FOnChanging := ChangingProc;
  FOnChange := ChangeProc;
end;

destructor TCnPersistent.Destroy;
begin
  if Assigned(FLockObject) then
    FLockObject.Free;
  inherited;
end;

//------------------------------------------------------------------------------
// 更新通知部分
//------------------------------------------------------------------------------

// 开始更新
procedure TCnPersistent.BeginUpdate;
begin
  if not IsUpdating then SetUpdating(True); // 开始更新
  Inc(FUpdateCount);
end;

// 结束更新
procedure TCnPersistent.EndUpdate;
begin                         // Assert不需要本地化
  Assert(FUpdateCount > 0, 'Unpaired TCnPersistent.EndUpdate');
  Dec(FUpdateCount);
  if not IsUpdating then SetUpdating(False);
end;

// 正在变更
procedure TCnPersistent.Changing;
begin
  if not IsUpdating and Assigned(FOnChanging) then FOnChanging(Self);
end;

// 变更结束
procedure TCnPersistent.Changed;
begin
  if not IsUpdating and Assigned(FOnChange) then FOnChange(Self);
end;

// 取所有者
function TCnPersistent.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// 正在更新
function TCnPersistent.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

// 更新状态变更过程
procedure TCnPersistent.SetUpdating(Updating: Boolean);
begin
  if Updating then
    Changing
  else
    Changed;
end;

// 子单位变更
procedure TCnPersistent.OnChildChanging(Sender: TObject);
begin
  if not IsUpdating and Assigned(FOnChanging) then FOnChanging(Sender);
end;

// 子单位已变更
procedure TCnPersistent.OnChildChange(Sender: TObject);
begin
  if not IsUpdating and Assigned(FOnChange) then FOnChange(Sender);
end;

//------------------------------------------------------------------------------
// 线程安全处理部分
//------------------------------------------------------------------------------

// 进入临界区，为保证多线程同步而加锁，必须与Unlock成对使用
procedure TCnPersistent.Lock;
begin
  LockObject.Lock;
end;

// 如果当前Lock计数为零，则加锁返回真，否则返回假
function TCnPersistent.TryLock: Boolean;
begin
  Result := LockObject.TryLock;
end;

// 退出临界区，释放同步锁，必须与Lock成对使用
procedure TCnPersistent.Unlock;
begin
  LockObject.Unlock;
end;

// Locking 属性读方法
function TCnPersistent.GetLocking: Boolean;
begin
  Result := LockObject.GetLocking;
end;

// LockObject 属性读方法，仅在需要时创建内部对象
function TCnPersistent.GetLockObject: TCnLockObject;
begin
  if not Assigned(FLockObject) then
    FLockObject := TCnLockObject.Create;
  Result := FLockObject;
end;

//==============================================================================
// 带Enabled的更新通知持久性类
//==============================================================================

{ TCnEnabledPersistent }

// 赋值
procedure TCnEnabledPersistent.Assign(Source: TPersistent);
begin
  if Source is TCnEnabledPersistent then
    FEnabled := TCnEnabledPersistent(Source).FEnabled
  else
    inherited Assign(Source);
end;

// 更新通知
procedure TCnEnabledPersistent.SetUpdating(Updating: Boolean);
begin
  if FEnabled then            // 如果能用则通知
    inherited SetUpdating(Updating); 
end;

// 创建
constructor TCnEnabledPersistent.Create;
begin
  inherited Create;
  FEnabled := False;
end;

// 设置参数
procedure TCnEnabledPersistent.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := True;         // 允许通知
    Changed;
    FEnabled := Value;
  end;
end;

{ TCnNotifyClass }

//--------------------------------------------------------//
//带更新通知的持久性类                                    //
//--------------------------------------------------------//

//赋值
procedure TCnNotifyClass.Assign(Source: TPersistent);
begin
  if not (Source is TCnNotifyClass) then
    inherited Assign(Source);
end;

//更新通知
procedure TCnNotifyClass.Changed;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

//创建
constructor TCnNotifyClass.Create(ChangedProc: TNotifyEvent);
begin
  inherited Create;
  FOnChanged := ChangedProc;
end;

//取所有者
function TCnNotifyClass.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//子单位更新通知
procedure TCnNotifyClass.OnChildChanged(Sender: TObject);
begin
  Changed;
end;

//==============================================================================
// 不可视组件基础类
//==============================================================================

{ TCnComponent }

// 初始化
constructor TCnComponent.Create(AOwner: TComponent);
begin
  inherited;
  FAbout := SCnPackAbout;
end;

// 设置关于属性
procedure TCnComponent.SetAbout(const Value: TCnCopyright);
begin
  // 不处理
end;

//==============================================================================
// 单实例接口对象基础类
//==============================================================================

{ TSingletonInterfacedObject }

function TSingletonInterfacedObject._AddRef: Integer;
begin
  Result := 1;
end;

function TSingletonInterfacedObject._Release: Integer;
begin
  Result := 1;
end;

{ TCnUInt32List }

function TCnUInt32List.Add(Item: Cardinal): Integer;
begin
  if FIgnoreDuplicated and (IndexOf(Item) >= 0) then
  begin
    Result := -1;
    Exit;
  end;

  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TCnUInt32List.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCnUInt32List.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);

  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Cardinal));
end;

destructor TCnUInt32List.Destroy;
begin
  Clear;
  inherited;
end;

class procedure TCnUInt32List.Error(Msg: PResStringRec; Data: Integer);
begin
  TCnUInt32List.Error(LoadResString(Msg), Data);
end;

class procedure TCnUInt32List.Error(const Msg: string; Data: Integer);
begin
  raise EListError.CreateFmt(Msg, [Data])
end;

procedure TCnUInt32List.Exchange(Index1, Index2: Integer);
var
  Item: Cardinal;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SListIndexError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TCnUInt32List.Expand: TCnUInt32List;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TCnUInt32List.Extract(Item: Cardinal): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  I := IndexOf(Item);
  if I >= 0 then
  begin
    Result := Item;
    FList^[I] := 0;
    Delete(I);
  end;
end;

function TCnUInt32List.First: Cardinal;
begin
  Result := Get(0);
end;

function TCnUInt32List.Get(Index: Integer): Cardinal;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Result := FList^[Index];
end;

procedure TCnUInt32List.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TCnUInt32List.IndexOf(Item: Cardinal): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TCnUInt32List.Insert(Index: Integer; Item: Cardinal);
begin
  if (Index < 0) or (Index > FCount) then
    Error(@SListIndexError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Cardinal));
  FList^[Index] := Item;
  Inc(FCount);
end;

function TCnUInt32List.Last: Cardinal;
begin
  Result := Get(FCount - 1);
end;

procedure TCnUInt32List.Move(CurIndex, NewIndex: Integer);
var
  Item: Cardinal;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(@SListIndexError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := 0;
    Delete(CurIndex);
    Insert(NewIndex, 0);
    FList^[NewIndex] := Item;
  end;
end;

procedure TCnUInt32List.Put(Index: Integer; Item: Cardinal);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  if FIgnoreDuplicated and (IndexOf(Item) >= 0) then
    Exit;

  FList^[Index] := Item;
end;

function TCnUInt32List.Remove(Item: Cardinal): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnUInt32List.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Cardinal));
    FCapacity := NewCapacity;
  end;
end;

procedure TCnUInt32List.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(@SListCountError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Cardinal), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

{ TCnUInt64List }

function TCnUInt64List.Add(Item: TUInt64): TUInt64;
begin
  if FIgnoreDuplicated and (IndexOf(Item) <> CN_NOT_FOUND_INDEX) then
  begin
    Result := -1;
    Exit;
  end;

  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TCnUInt64List.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCnUInt64List.Delete(Index: TUInt64);
begin
  if (UInt64Compare(Index, 0) < 0) or (UInt64Compare(Index, FCount) >= 0) then
    Error(@SListIndexError, Index);

  Dec(FCount);
  if UInt64Compare(Index, FCount) < 0 then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TUInt64));
end;

destructor TCnUInt64List.Destroy;
begin
  Clear;
  inherited;
end;

class procedure TCnUInt64List.Error(Msg: PResStringRec; Data: Integer);
begin
  TCnUInt64List.Error(LoadResString(Msg), Data);
end;

class procedure TCnUInt64List.Error(const Msg: string; Data: Integer);
begin
  raise EListError.CreateFmt(Msg, [Data])
end;

procedure TCnUInt64List.Exchange(Index1, Index2: TUInt64);
var
  Item: TUInt64;
begin

  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SListIndexError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TCnUInt64List.Expand: TCnUInt64List;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TCnUInt64List.Extract(Item: TUInt64): TUInt64;
var
  I: Integer;
begin
  Result := 0;
  I := IndexOf(Item);
  if I <> CN_NOT_FOUND_INDEX then
  begin
    Result := Item;
    FList^[I] := 0;
    Delete(I);
  end;
end;

function TCnUInt64List.First: TUInt64;
begin
  Result := Get(0);
end;

function TCnUInt64List.Get(Index: TUInt64): TUInt64;
begin
  if (UInt64Compare(Index, 0) < 0) or (UInt64Compare(Index, FCount) >= 0) then
    Error(@SListIndexError, Index);
  Result := FList^[Index];
end;

procedure TCnUInt64List.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TCnUInt64List.IndexOf(Item: TUInt64): TUInt64;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := CN_NOT_FOUND_INDEX;
end;

procedure TCnUInt64List.Insert(Index: TUInt64; Item: TUInt64);
begin
  if (UInt64Compare(Index, 0) < 0) or (UInt64Compare(Index, FCount) >= 0) then
    Error(@SListIndexError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TUInt64));
  FList^[Index] := Item;
  Inc(FCount);
end;

function TCnUInt64List.Last: TUInt64;
begin
  Result := Get(FCount - 1);
end;

procedure TCnUInt64List.Move(CurIndex, NewIndex: TUInt64);
var
  Item: TUInt64;
begin
  if CurIndex <> NewIndex then
  begin
    if (UInt64Compare(NewIndex, 0) < 0) or (UInt64Compare(NewIndex, FCount) >= 0) then
      Error(@SListIndexError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := 0;
    Delete(CurIndex);
    Insert(NewIndex, 0);
    FList^[NewIndex] := Item;
  end;
end;

procedure TCnUInt64List.Put(Index: TUInt64; Item: TUInt64);
begin
  if (UInt64Compare(Index, 0) < 0) or (UInt64Compare(Index, FCount) >= 0) then
    Error(@SListIndexError, Index);
  if FIgnoreDuplicated and (IndexOf(Item) <> CN_NOT_FOUND_INDEX) then
    Exit;

  FList^[Index] := Item;
end;

function TCnUInt64List.Remove(Item: TUInt64): TUInt64;
begin
  Result := IndexOf(Item);
  if Result <> CN_NOT_FOUND_INDEX then
    Delete(Result);
end;

procedure TCnUInt64List.SetCapacity(NewCapacity: TUInt64);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(TUInt64));
    FCapacity := NewCapacity;
  end;
end;

procedure TCnUInt64List.SetCount(NewCount: TUInt64);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(@SListCountError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(TUInt64), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

initialization
  InitializeCriticalSection(CounterLock);

finalization
  DeleteCriticalSection(CounterLock);

end.

