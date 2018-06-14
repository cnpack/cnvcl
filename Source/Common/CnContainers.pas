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

unit CnContainers;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：链表队列实现
* 单元作者：小峰
* 备    注：简单的链表队列类，从尾Push，从头Pop，参数可以是对象（被转换成指针）。
*           操作时内部有互斥机制，无需在外部通过临界区互斥。操作例子：
*           声明：
*           var
*             Q: TCnQueue;
*
*           创建：
*             Q := TCnQueue.Create;
*            
*           使用：
*
*           var
*             TmpObj: TObject;
*           begin
*             TmpObj := TObject.Create;
*             Q.Push(Data); // 放入队列尾
*           end;
*            
*           var
*             TmpObj: TObject;
*           begin
*             TmpObj := TObject(Q.Pop); // 从队列头中取出
*             TmpObj.Free;
*           end;
*
*           释放：
*             Q.Free;
* 开发平台：PWinXP + Delphi 7
* 兼容测试：PWin2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2017.01.17 V1.2
*               加入 TCnObjectRingBuffer 循环缓冲区实现
*           2016.12.02 V1.1
*               加入 TCnObjectStack 实现，允许 Clear 等方法
*           2008.04.30 V1.0
*               小峰从原始代码移植而来。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes;

type
  TCnQueue = class
  private
    FMultiThread: Boolean;
    FHead: TObject;
    FTail: TObject;
    FSize: Integer;
    FLock: TRTLCriticalSection;
    procedure FreeNode(Value: TObject);
    function GetSize: Integer;
  public
    constructor Create(MultiThread: Boolean = False);
    destructor Destroy; override;
    procedure Push(Data: Pointer);
    function Pop: Pointer;
    property Size: Integer read GetSize;
  end;

  TCnObjectStack = class(TObject)
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count: Integer;
    function IsEmpty: Boolean;
    procedure Clear;

    procedure Push(AObject: TObject);
    function Pop: TObject;
    function Peek: TObject;
  end;

  ECnRingBufferFullException = class(Exception);

  ECnRingBufferEmptyException = class(Exception);

  TCnObjectRingBuffer = class(TObject)
  {* 循环队列缓冲区}
  private
    FFullOverwrite: Boolean;
    FMultiThread: Boolean;
    FSize: Integer;
    FList: TList;
    FLock: TRTLCriticalSection;
    // Idx 可以理解为始终指向相邻位置中间的缝，编号从第 0 到第 Size - 1 ( 第 Size 也即等于第 0 )
    // 有元素的情况下，FrontIdx 高后始终是元素，前可能是空，或绕回来的尾巴
    //                 BackIdx 的低前始终是元素，后可能是空，或绕回来的头
    // 无元素的情况下，FrontIdx 和 BackIdx 相等
    FFrontIdx: Integer;
    FBackIdx: Integer;
    FCount: Integer;
    function GetCount: Integer;
  public
    constructor Create(ASize: Integer; AFullOverwrite: Boolean = False;
      AMultiThread: Boolean = False);
    {* 构造函数，ASize 是缓冲区容量；AFullOverwrite 是否允许缓冲区满后再塞东西时
      覆盖以前的数据，AMultiThread 是否需要多线程互斥}
    destructor Destroy; override;
    {* 析构函数}

    procedure PushToFront(AObject: TObject);
    {* 从循环队列缓冲区前方推入一个 Object，前方是指内部存储索引低的一端，如满且不允许覆盖则抛异常}
    function PopFromBack: TObject;
    {* 从循环队列缓冲区后方弹出一个 Object，后方是指内部存储索引高的一端，无可弹则抛异常}

    procedure PushToBack(AObject: TObject);
    {* 从循环队列缓冲区后方推入一个 Object，后方是指内部存储索引高的一端，如满且不允许覆盖则抛异常}
    function PopFromFront: TObject;
    {* 从循环队列缓冲区前方弹出一个 Object，前方是指内部存储索引低的一端，无可弹则抛异常}

    procedure Dump(List: TList; out FrontIdx: Integer; out BackIdx: Integer);
    {* 把全部内容导出至一 TList，以及指针位置}

    property FullOverwrite: Boolean read FFullOverwrite;
    {* 该循环队列缓冲区满时是否允许覆盖旧数据}
    property MultiThread: Boolean read FMultiThread;
    {* 该循环队列缓冲区是否需要支持多线程并发访问，为 True 时内部有临界区处理}
    property Size: Integer read FSize;
    {* 从循环队列缓冲区的尺寸}
    property Count: Integer read GetCount;
    {* 从循环队列缓冲区内的有效元素数量}
  end;

implementation

type
  TCnNode = class
  private
    FNext: TCnNode;
    FData: Pointer;
  public
    property Next: TCnNode read FNext write FNext;
    property Data: Pointer read FData write FData;
  end;

{ TCnQueue }

procedure TCnQueue.FreeNode(Value: TObject);
var
  Tmp: TCnNode;
begin
  Tmp := TCnNode(Value).Next;
  TCnNode(Value).Free;
  if Tmp = nil then
    Exit;
  FreeNode(Tmp);
end;

constructor TCnQueue.Create(MultiThread: Boolean);
begin
  FMultiThread := MultiThread;
  FHead := nil;
  FTail := nil;
  FSize := 0;
  if FMultiThread then
    InitializeCriticalSection(FLock);
end;

destructor TCnQueue.Destroy;
begin
  if FHead <> nil then
    FreeNode(FHead);
  if FMultiThread then
    DeleteCriticalSection(FLock);
  inherited;
end;

function TCnQueue.Pop: Pointer;
var
  Tmp: TCnNode;
begin
  if FMultiThread then
    EnterCriticalSection(FLock);
  try
    Result := nil;
    if FHead = nil then
      Exit;

    Result := TCnNode(FHead).Data;
    Tmp := TCnNode(FHead).Next;
    TCnNode(FHead).Free;
    FHead := Tmp;
    
    if Tmp = nil then
      FTail := nil;
    FSize := FSize - 1;
  finally
    if FMultiThread then
      LeaveCriticalSection(FLock);
  end;
end;

procedure TCnQueue.Push(Data: Pointer);
var
  Tmp: TCnNode;
begin
  if FMultiThread then
    EnterCriticalSection(FLock);
  try
    if Data = nil then Exit;
    Tmp := TCnNode.Create;
    Tmp.Data := Data;
    Tmp.Next := nil;
    
    if FTail = nil then
    begin
      FTail := Tmp;
      FHead := Tmp;
    end
    else
    begin
      TCnNode(FTail).Next := Tmp;
      FTail := Tmp
    end;
    
    FSize := FSize + 1;
  finally
    if FMultiThread then
      LeaveCriticalSection(FLock);
  end;
end;

function TCnQueue.GetSize: Integer;
begin
  Result := FSize;
end;

{ TCnObjectStack }

procedure TCnObjectStack.Clear;
begin
  FList.Clear;
end;

function TCnObjectStack.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCnObjectStack.Create;
begin
  FList := TList.Create;
end;

destructor TCnObjectStack.Destroy;
begin
  FList.Free;
  inherited;
end;

function TCnObjectStack.IsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

function TCnObjectStack.Peek: TObject;
begin
  Result := TObject(FList[FList.Count - 1]);
end;

function TCnObjectStack.Pop: TObject;
begin
  Result := TObject(FList[FList.Count - 1]);
  FList.Delete(FList.Count - 1);
end;

procedure TCnObjectStack.Push(AObject: TObject);
begin
  FList.Add(AObject);
end;

{ TCnRingBuffer }

constructor TCnObjectRingBuffer.Create(ASize: Integer; AFullOverwrite,
  AMultiThread: Boolean);
begin
  Assert(ASize > 0);

  FSize := ASize;
  FFullOverwrite := AFullOverwrite;
  FMultiThread := AMultiThread;

  FList := TList.Create;
  FList.Count := FSize;

  if FMultiThread then
    InitializeCriticalSection(FLock);
end;

destructor TCnObjectRingBuffer.Destroy;
begin
  if FMultiThread then
    DeleteCriticalSection(FLock);
  FList.Free;
  inherited;
end;

procedure TCnObjectRingBuffer.Dump(List: TList; out FrontIdx: Integer; out BackIdx: Integer);
var
  I: Integer;
begin
  FrontIdx := FFrontIdx;
  BackIdx := FBackIdx;
  if List <> nil then
  begin
    List.Clear;
    for I := 0 to FList.Count - 1 do
      List.Add(FList[I]);
  end;
end;

function TCnObjectRingBuffer.GetCount: Integer;
begin
  Result := FCount;
end;

function TCnObjectRingBuffer.PopFromBack: TObject;
begin
  Result := nil;
  if FMultiThread then
    EnterCriticalSection(FLock);

  try
    if FCount <= 0 then
      raise ECnRingBufferEmptyException.Create('Ring Buffer Empty. Can NOT Pop From Back.');

    Dec(FBackIdx);
    if FBackIdx < 0 then
      FBackIdx := FSize - 1;
    Result := FList[FBackIdx];
    FList[FBackIdx] := nil;
    Dec(FCount);
  finally
    if FMultiThread then
      LeaveCriticalSection(FLock);
  end;
end;

function TCnObjectRingBuffer.PopFromFront: TObject;
begin
  Result := nil;
  if FMultiThread then
    EnterCriticalSection(FLock);

  try
    if FCount <= 0 then
      raise ECnRingBufferEmptyException.Create('Ring Buffer Empty. Can NOT Pop From Front.');

    Result := FList[FFrontIdx];
    FList[FFrontIdx] := nil;

    Inc(FFrontIdx);
    if FFrontIdx >= FSize then
      FFrontIdx := 0;
    Dec(FCount);
  finally
    if FMultiThread then
      LeaveCriticalSection(FLock);
  end;
end;

procedure TCnObjectRingBuffer.PushToBack(AObject: TObject);
begin
  if FMultiThread then
    EnterCriticalSection(FLock);

  try
    if not FFullOverwrite and (FCount >= FSize) then
      raise ECnRingBufferFullException.Create('Ring Buffer Full. Can NOT Push To Back.');

    FList[FBackIdx] := AObject;
    Inc(FBackIdx);
    if FBackIdx >= FSize then
      FBackIdx := 0;

    if FCount < FSize then
      Inc(FCount);
  finally
    if FMultiThread then
      LeaveCriticalSection(FLock);
  end;
end;

procedure TCnObjectRingBuffer.PushToFront(AObject: TObject);
begin
  if FMultiThread then
    EnterCriticalSection(FLock);

  try
    if not FFullOverwrite and (FCount >= FSize) then
      raise ECnRingBufferFullException.Create('Ring Buffer Full. Can NOT Push To Front.');

    Dec(FFrontIdx);
    if FFrontIdx < 0 then
      FFrontIdx := FSize - 1;
    FList[FFrontIdx] := AObject;

    if FCount < FSize then
      Inc(FCount);
  finally
    if FMultiThread then
      LeaveCriticalSection(FLock);
  end;
end;

end.
