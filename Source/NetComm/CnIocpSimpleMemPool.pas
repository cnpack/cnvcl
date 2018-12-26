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

unit CnIocpSimpleMemPool;
{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：Windows完成端口(IOCP)组件使用的简单内存池实现单元
* 单元作者：cnwinds
*           菩提(cxmld@126.com) 移植修改
* 备    注：
*   1.TCnMemPoolMgr类是内存池管理的实现。
*     CnMemPoolMgr是TCnMemPoolMgr类的全局对象，可以通过该对象使用内存池。
*     他将大小(MemorySize)相同的内存块(TCnMemoryBlockItem)放在一个(TCnMemoryTypeItem)中进行管理。
*     TCnMemPoolMgr类中管理多个内存类型块(TCnMemoryTypeItem)。
*     一个内存类型块(TCnMemoryTypeItem)中包含了多个内存块(TCnMemoryBlockItem)。
*     阈值(Threshold)控制在一个TMemoryTypeItem中内存块的个数。
*       在系统频繁申请内存块的时候，总个数会大于阈值。
*       当系统对内存块的并发使用数低于阈值的时候释放内存块，让总个数等于阈值。
*       这个策略可以避免繁忙的时候频繁申请、释放内存，或者空闲的时候浪费内存。
*   2.TCnMemoryPool是一个控件。为了能可视化开发而产生的类。
*     可能出现多个控件的内存块大小相同，这样将对应到同一个内存类型块(TCnMemoryTypeItem)
*     出现这种情况多个控件将共用内存类型块(TCnMemoryTypeItem)中的内存块。阈值将取他们设置的最大值。
*
TODO >>>
*   1.TCnIocpMemPool类增加了一个方法:GetFreeMemoryType, 获取一个空闲的内存类型
*   2.TCnIocpMemPool分配的内存大小是固定,最大值由每一次分配决定
*   3.增加 TCnIocpSimpleMemPool来包装 TCnIocpMemPool的功能, 即租用内存和归还内存
*   4.使用"租用"和"归还"是为了区别正常的"分配内存"和"释放内存"
*   5.TCnIocpSimpleMemPool对应一个内存类型, 每个内存类型的类型值,自动获取
*   6.二个自定义名词(可能名字起得不够好):"内存块"和"内存类型块"
*     每个 TCnIocpSimpleMemPool 对应一个内存类型块, 它由多个"内存块"组成.每次用户
*     租用就是得到一个整的"内存块", 大小由第一次租用时确定.
*     TCnIocpMemPool包含了多个  "内存类型块", 即每注册一次时,就分配一个"内存类型块"
TODO >>>
*
* 开发平台：PWin2000Pro + Delphi 7.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.09.16 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, SyncObjs, Windows, Controls;

const
  SCnErrorNotRegister = '没有注册该内存类型(%d)!';
  SCnErrorBlockNotRent = '内存块没有被借出！';
  SCnErrorBlockUnknow = '没有该内存块！';

type
  TCreateMemoryEvent = procedure(Sender: TObject; var MemoryPtr: Pointer) of object;
  TFreeMemoryEvent = procedure(Sender: TObject; MemoryPtr: Pointer) of object;

  TCnMemoryBlockItem = record
  {* 内存块头}
    MemoryBlockPtr: Pointer;              //内存指针
    RentTime: Cardinal;                   //租用时间
    IsRent: Boolean;                      //是否租用
    RentCount: Cardinal;                  //租用次数
    Size: Cardinal;
  end;
  PCnMemoryBlockItem = ^TCnMemoryBlockItem;

  TCnMemoryTypeItem = record
  {* 内存类型块头}
    RefCount: Cardinal;                   //该类型块的引用次数
    MemorySize: Cardinal;                 //内存块的大小
    CreateMemoryProc: TCreateMemoryEvent; //创建内存方法指针
    FreeMemoryProc: TFreeMemoryEvent;     //释放内存方法指针
    Threshold: Cardinal;                  //内存块个数的阈值
                                          //如果缓存的块数多于该值则要启动清理程序。
    IdelCount: Cardinal;                  //空闲内存块的个数
    Lock: TCriticalSection;               //互锁相关
    MemoryBlockList: TList;               //内存块列表
  end;
  PCnMemoryTypeItem = ^TCnMemoryTypeItem;

  TCnSimpleMemPoolMgr = class
  private
    FLock: TCriticalSection;
    FMemoryTypeList: TList;

    function RegisterMemoryTypeItem(MemorySize: Cardinal;
                                    CreateMemoryProc: TCreateMemoryEvent;
                                    FreeMemoryProc: TFreeMemoryEvent): PCnMemoryTypeItem;
    {* 注册内存类型块(带线程锁)}

    procedure UnregisterMemoryTypeItem(MemoryTypeItem: PCnMemoryTypeItem);
    {* 注销内存类型块}

    function CreateMemoryBlockItem(MemoryTypeItem: PCnMemoryTypeItem): PCnMemoryBlockItem;
    procedure FreeMemoryBlockItem(MemoryTypeItem: PCnMemoryTypeItem;
      MemoryBlockItem: PCnMemoryBlockItem);

    function FindMemoryTypeItem(MemorySize: Cardinal;
                                CreateMemoryProc: TCreateMemoryEvent;
                                FreeMemoryProc: TFreeMemoryEvent): PCnMemoryTypeItem;

    procedure Clear;

  public
    constructor Create;
    destructor Destroy; override;

    function RegisterMemoryType(MemorySize: Cardinal;
                                CreateMemoryProc: TCreateMemoryEvent;
                                FreeMemoryProc: TFreeMemoryEvent): PCnMemoryTypeItem;
    {* 注册内存类型块 参数:内存类型, 创建和释放方法指针
       两方法指针是事件通知，同时可以自定义分配内存与释放内存的方法}

    procedure UnregisterMemoryType(MemoryTypeItem: PCnMemoryTypeItem);
    {* 注销内存类型块}
    
    procedure SetThreshold(MemoryTypeItem: PCnMemoryTypeItem; Threshold: Cardinal);
    {* 设置租用内存块的阈值。
      阈值和上限的区别：
        阈值表示当系统空闲的时候建议不要超过的值。上限表示任何时候都不能超过该值。
    }

    procedure RentMemory(MemoryTypeItem: PCnMemoryTypeItem; var MemoryPtr: Pointer);
    {* 租用一块内存}
    procedure ReturnMemory(MemoryTypeItem: PCnMemoryTypeItem; MemoryPtr: Pointer);
    {* 返还一块内存}
  end;

  TCnCustomSimpleMemPool = class (TComponent)
  private
    FMemorySize: Cardinal;
    FThreshold : Cardinal;
    FOnCreateMemory : TCreateMemoryEvent;
    FOnFreeMemory   : TFreeMemoryEvent;
    FMemTypeItem : PCnMemoryTypeItem;
    FIsReg: Boolean;    //是否已经注册到内存池管理器了

    procedure EnsureRegister;
    procedure DoRegister;
    procedure DoUnregister;

    procedure SetThreshold(const Value: Cardinal);
    procedure SetMemorySize(const Value: Cardinal);
    procedure SetOnCreateMemory(const Value: TCreateMemoryEvent);
    procedure SetOnFreeMemory(const Value: TFreeMemoryEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RentMemory(var MemoryPtr: Pointer);
    {* 租用内存}
    procedure ReturnMemory(MemoryPtr: Pointer);
    {* 归还内存}
  public
    property MemorySize: Cardinal read FMemorySize write SetMemorySize;
    {* 内存块的大小} 
    property Threshold : Cardinal read FThreshold write SetThreshold;
    {* 内存块的数量阈值(不是最大值,即可分配更多的内存块)}
    property OnCreateMemory : TCreateMemoryEvent read FOnCreateMemory write SetOnCreateMemory;
    {* 自定义在系统中分配内存的方法,默认实现采用 GetMemory}
    property OnFreeMemory: TFreeMemoryEvent read FOnFreeMemory write SetOnFreeMemory;
    {* 自定义在系统中释放内存的方法,默认实现采用 FreeMemory}
  end;

  TCnIocpSimpleMemPool = class(TCnCustomSimpleMemPool)
  published
    property MemorySize;
    {* 内存块的大小}
    property Threshold;
    {* 内存块的数量阈值(不是最大值,即可分配更多的内存块)}
    property OnCreateMemory;
    {* 自定义在系统中分配内存的方法,默认实现采用 GetMemory}
    property OnFreeMemory;
    {* 自定义在系统中释放内存的方法,默认实现采用 FreeMemory}
  end;

var
  CnSimpleMemPoolMgr: TCnSimpleMemPoolMgr;

implementation

{ TCnSimpleMemPoolMgr }

constructor TCnSimpleMemPoolMgr.Create;
begin
  FMemoryTypeList := TList.Create;
  FLock := TCriticalSection.Create;
end;

destructor TCnSimpleMemPoolMgr.Destroy;
begin
  Clear;
  FreeAndNil(FMemoryTypeList);
  FreeAndNil(FLock);
  inherited;
end;

procedure TCnSimpleMemPoolMgr.Clear;
var
  TypeItem: PCnMemoryTypeItem;
  I: Integer;
begin
  // 清除所有内存块
  FLock.Enter;
  try
    for I := 0 to FMemoryTypeList.Count - 1 do
    begin
      TypeItem := PCnMemoryTypeItem(FMemoryTypeList[I]);
      UnregisterMemoryTypeItem(TypeItem);
    end;
  finally
    FLock.Release;
  end;
end;

function TCnSimpleMemPoolMgr.RegisterMemoryTypeItem(MemorySize: Cardinal;
  CreateMemoryProc: TCreateMemoryEvent; FreeMemoryProc: TFreeMemoryEvent): PCnMemoryTypeItem;
begin
  Result := New(PCnMemoryTypeItem);
  Result^.Lock := TCriticalSection.Create;
  Result^.RefCount := 1;
  Result^.MemorySize := MemorySize;
  Result^.MemoryBlockList := TList.Create;
  Result^.CreateMemoryProc := CreateMemoryProc;
  Result^.FreeMemoryProc := FreeMemoryProc;
  Result^.Threshold := 20;
  Result^.IdelCount := 0;
end;

procedure TCnSimpleMemPoolMgr.UnregisterMemoryTypeItem(MemoryTypeItem: PCnMemoryTypeItem);
var
  I: Integer;
begin
  for I := 0 to MemoryTypeItem^.MemoryBlockList.Count - 1 do
    FreeMemoryBlockItem(MemoryTypeItem, MemoryTypeItem^.MemoryBlockList[I]);
  FreeAndNil(MemoryTypeItem^.Lock);
  FreeAndNil(MemoryTypeItem^.MemoryBlockList);
  Dispose(MemoryTypeItem);
end;

function TCnSimpleMemPoolMgr.CreateMemoryBlockItem(
  MemoryTypeItem: PCnMemoryTypeItem): PCnMemoryBlockItem;
var
  Size: Integer;
begin
  Size := MemoryTypeItem^.MemorySize;
  //创建内存块
  Result := New(PCnMemoryBlockItem);
  //申请内存。如果没有设置回调函数则使用GetMemory申请内存。
  if (Assigned(MemoryTypeItem.CreateMemoryProc)) then
    MemoryTypeItem^.CreateMemoryProc(Self, Result^.MemoryBlockPtr)
  else
    Result^.MemoryBlockPtr := GetMemory(Size);
  Result^.RentTime := 0;
  Result^.IsRent := False;
  Result^.RentCount := 0;
  Result^.Size := Size;
end;

procedure TCnSimpleMemPoolMgr.FreeMemoryBlockItem(MemoryTypeItem: PCnMemoryTypeItem;
  MemoryBlockItem: PCnMemoryBlockItem);
begin
  //释放内存
  if (Assigned(MemoryTypeItem.FreeMemoryProc)) then
    MemoryTypeItem.FreeMemoryProc(Self, MemoryBlockItem^.MemoryBlockPtr)
  else
    FreeMemory(MemoryBlockItem^.MemoryBlockPtr);
  //释放内存块
  Dispose(MemoryBlockItem);
end;

function TCnSimpleMemPoolMgr.FindMemoryTypeItem(MemorySize: Cardinal;
  CreateMemoryProc: TCreateMemoryEvent; FreeMemoryProc: TFreeMemoryEvent): PCnMemoryTypeItem;
var
  I: Integer;
begin
  FLock.Enter;
  try
    for I := 0 to FMemoryTypeList.Count - 1 do
    begin       
      Result := PCnMemoryTypeItem(FMemoryTypeList[I]);
      if (Result^.MemorySize = MemorySize) and
        (@Result^.CreateMemoryProc = @CreateMemoryProc) and
        (@Result^.FreeMemoryProc = @FreeMemoryProc) then Exit;
    end;
    Result := nil;
  finally
    FLock.Release;
  end;
end;

function TCnSimpleMemPoolMgr.RegisterMemoryType(MemorySize: Cardinal;
  CreateMemoryProc: TCreateMemoryEvent; FreeMemoryProc: TFreeMemoryEvent): PCnMemoryTypeItem;
begin
  Result := FindMemoryTypeItem(MemorySize, CreateMemoryProc, FreeMemoryProc);
  if Result = nil then                      //不存在,就创建
  begin
    Result := RegisterMemoryTypeItem(MemorySize, CreateMemoryProc, FreeMemoryProc);
    FLock.Enter;
    try
      FMemoryTypeList.Add(Result);            //并加入List中
    finally
      FLock.Release;  
    end;
  end else
  begin
    Inc(Result^.RefCount);                  //存在则增加引用计数
  end;
end;

procedure TCnSimpleMemPoolMgr.UnregisterMemoryType(MemoryTypeItem: PCnMemoryTypeItem);
begin
  //减少引用计数
  Dec(MemoryTypeItem^.RefCount);
  if MemoryTypeItem^.RefCount <> 0 then Exit;

  FLock.Enter;
  try
    FMemoryTypeList.Remove(MemoryTypeItem);
  finally
    FLock.Release;
  end;
  UnregisterMemoryTypeItem(MemoryTypeItem);
end;

procedure TCnSimpleMemPoolMgr.SetThreshold(MemoryTypeItem: PCnMemoryTypeItem; Threshold: Cardinal);
begin
  //如果一个MemoryTypeItem有多个引用，则使用最大的阈值
  if MemoryTypeItem <> nil then
  begin
    if MemoryTypeItem^.RefCount = 1 then
      MemoryTypeItem^.Threshold := Threshold
    else
      if MemoryTypeItem^.Threshold < Threshold then
        MemoryTypeItem^.Threshold := Threshold;
  end;
end;

procedure TCnSimpleMemPoolMgr.RentMemory(MemoryTypeItem: PCnMemoryTypeItem; var MemoryPtr: Pointer);
var
  BlockItem: PCnMemoryBlockItem;
begin
  //不需要循环查找，只要找到第一个内存块，如果被租用则表示所有内存块都已经被租用了。
  MemoryTypeItem^.Lock.Enter;
  try
    if MemoryTypeItem^.MemoryBlockList.Count > 0 then
    begin
      BlockItem := PCnMemoryBlockItem(MemoryTypeItem^.MemoryBlockList[0]);
      if not BlockItem^.IsRent then      //第0个内存块是否已租用
      begin
        MemoryTypeItem^.MemoryBlockList.Remove(BlockItem);
        MemoryTypeItem^.MemoryBlockList.Add(BlockItem);  //将内存块重新放入到LIST的最后
        MemoryPtr := BlockItem.MemoryBlockPtr;     //取得内存块的指针
        Inc(BlockItem^.RentCount);                 //租用总数+1
        BlockItem^.RentTime := GetTickCount;       //租用时间
        BlockItem^.IsRent := True;                 //置租用标志

        //空闲内存块个数减一
        Dec(MemoryTypeItem^.IdelCount);

        Exit;
      end;
    end;

    // 新创建一个内存块
    BlockItem := CreateMemoryBlockItem(MemoryTypeItem);
    MemoryPtr := BlockItem^.MemoryBlockPtr;
    Inc(BlockItem^.RentCount);
    BlockItem^.RentTime := GetTickCount;
    BlockItem^.IsRent := True;
    BlockItem^.Size := MemoryTypeItem^.MemorySize;
    MemoryTypeItem^.MemoryBlockList.Add(BlockItem);
  finally
    MemoryTypeItem^.Lock.Release;
  end;
end;

procedure TCnSimpleMemPoolMgr.ReturnMemory(MemoryTypeItem: PCnMemoryTypeItem; MemoryPtr: Pointer);
var
  I: Integer;
  BlockItem: PCnMemoryBlockItem;
  ReleaseCount: Cardinal;
  UsedCount: Cardinal;
  TotalCount: Cardinal;
begin
  //对内存块的调整不是强制性的，原则是在方便的时候调整一下内块的个数
  MemoryTypeItem^.Lock.Enter;
  try
    ReleaseCount := 0;
    //判断是否要删除内存块
    TotalCount := MemoryTypeItem^.MemoryBlockList.Count;
    if TotalCount > MemoryTypeItem^.Threshold then
    begin
      UsedCount := TotalCount - MemoryTypeItem^.IdelCount;
      if UsedCount < MemoryTypeItem^.Threshold then
      begin
        //计算要删除内存块的个数
        //不表示一定要删除这么多个内存块，理想情况下会删除这么多内存块
        ReleaseCount := TotalCount - MemoryTypeItem^.Threshold;
      end;
    end;
    
    for I := MemoryTypeItem^.MemoryBlockList.Count - 1 downto 0 do
    begin
      BlockItem := PCnMemoryBlockItem(MemoryTypeItem^.MemoryBlockList[I]);
      if MemoryPtr = BlockItem^.MemoryBlockPtr then         //查询内存块(比较地址相同)
      begin
        if BlockItem^.IsRent then
        begin
          //归还内存块
          BlockItem^.RentTime := 0;
          BlockItem^.IsRent := False;
          MemoryTypeItem^.MemoryBlockList.Remove(BlockItem);
          MemoryTypeItem^.MemoryBlockList.Insert(0, BlockItem);  //插入到第0个
          //空闲内存块个数加一
          Inc(MemoryTypeItem^.IdelCount);
          Exit;
        end 
        else
          raise Exception.Create(SCnErrorBlockNotRent); //没有被租用异常 
      end;
      //释放内存块
      if (ReleaseCount <> 0) and (not BlockItem^.IsRent) then
      begin
        FreeMemoryBlockItem(MemoryTypeItem, BlockItem);
        MemoryTypeItem^.MemoryBlockList.Remove(BlockItem);
        Dec(ReleaseCount);
      end;
    end;
    raise Exception.Create(SCnErrorBlockUnknow);        //没有找到内存块抛出异常
  finally
    MemoryTypeItem^.Lock.Release;
  end;
end;

{ TCnIocpSimpleMemPool }

constructor TCnCustomSimpleMemPool.Create(AOwner: TComponent);
begin
  inherited;
  FThreshold := 20;
  FMemorySize := 1024;
  FIsReg := False;
  //使用延迟注册方式，避免初始化参数造成反复注册
  //DoRegister;
end;

destructor TCnCustomSimpleMemPool.Destroy;
begin
  DoUnregister;
  inherited;
end;

procedure TCnCustomSimpleMemPool.EnsureRegister;
begin
  if not FIsReg then
    DoRegister;
end;

procedure TCnCustomSimpleMemPool.DoRegister;
begin
  if (not (csDesigning in ComponentState)) and (not FIsReg) then
  begin
    FMemTypeItem := CnSimpleMemPoolMgr.RegisterMemoryType(
      FMemorySize, FOnCreateMemory, FOnFreeMemory);
    CnSimpleMemPoolMgr.SetThreshold(FMemTypeItem, Threshold);
    FIsReg := True;
  end;
end;

procedure TCnCustomSimpleMemPool.DoUnregister;
begin
  if FIsReg then
  begin
    CnSimpleMemPoolMgr.UnregisterMemoryType(FMemTypeItem);
    FIsReg := False;
  end;
end;

procedure TCnCustomSimpleMemPool.RentMemory(var MemoryPtr: Pointer);
begin
  EnsureRegister;
  
  CnSimpleMemPoolMgr.RentMemory(FMemTypeItem, MemoryPtr);
end;

procedure TCnCustomSimpleMemPool.ReturnMemory(MemoryPtr: Pointer);
begin
  EnsureRegister;
  
  CnSimpleMemPoolMgr.ReturnMemory(FMemTypeItem, MemoryPtr);
end;

procedure TCnCustomSimpleMemPool.SetMemorySize(const Value: Cardinal);
begin
  if FMemorySize <> Value then
  begin
    if FIsReg then DoUnregister;

    FMemorySize := Value;
  end;
end;

procedure TCnCustomSimpleMemPool.SetThreshold(const Value: Cardinal);
begin
  if FThreshold <> Value then
  begin
    FThreshold := Value;
    if FIsReg then
      CnSimpleMemPoolMgr.SetThreshold(FMemTypeItem, FThreshold);
  end;
end;

procedure TCnCustomSimpleMemPool.SetOnCreateMemory(const Value: TCreateMemoryEvent);
begin
  if @FOnCreateMemory <> @Value then
  begin
    if FIsReg then DoUnregister;

    FOnCreateMemory := Value;
  end;
end;

procedure TCnCustomSimpleMemPool.SetOnFreeMemory(const Value: TFreeMemoryEvent);
begin
  if @FOnFreeMemory <> @Value then
  begin
    if FIsReg then DoUnregister;

    FOnFreeMemory := Value;
  end;
end;

initialization
  CnSimpleMemPoolMgr := TCnSimpleMemPoolMgr.Create;

finalization
  CnSimpleMemPoolMgr.Free;

end.
