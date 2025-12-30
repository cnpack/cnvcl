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

unit CnContainers;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：链表队列实现
* 单元作者：小峰
* 备    注：简单的链表队列类，从尾 Push，从头 Pop，参数可以是对象（被转换成指针）。
*           操作时内部可在创建时设置互斥机制，无需在外部通过临界区互斥。操作例子：
*           声明：
*           var
*             Q: TCnLinkedQueue;
*
*           创建：
*             Q := TCnLinkedQueue.Create;
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
*
* 开发平台：PWinXP + Delphi 7
* 兼容测试：PWin2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.04.06 V1.5
*               几个列表类加入排序功能
*           2024.05.10 V1.4
*               将 CnClasses 中的 UInt32/UInt64 列表移动至此处，没泛型只能这样
*           2024.04.28 V1.4
*               增加对象队列，更改链表队列类名
*           2023.08.21 V1.4
*               增加扩展精度浮点数列表
*           2020.11.05 V1.3
*               将大数池基类抽取至此处
*           2017.01.17 V1.2
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
  SysUtils, Classes, Contnrs, SyncObjs, CnNative
  {$IFDEF FPC} , RTLConsts {$ELSE}
  {$IFDEF COMPILER6_UP}, RTLConsts {$ELSE}, Consts {$ENDIF} {$ENDIF}
  {$IFDEF POSIX}, System.Generics.Collections {$ENDIF};

{$DEFINE MULTI_THREAD} // 数学对象池支持多线程，性能略有下降，如不需要，注释此行即可

type
  TCnLinkedQueue = class(TObject)
  {* 指针队列实现类，内部采用链表实现。可运行期创建时指定是否支持多线程互斥}
  private
    FMultiThread: Boolean;
    FHead: TObject;
    FTail: TObject;
    FSize: Integer;
    FLock: TCriticalSection;
    procedure FreeNode(Value: TObject);
    function GetSize: Integer;
  public
    constructor Create(MultiThread: Boolean = False); virtual;
    {* 构造函数。

       参数：
         MultiThread: Boolean             - 是否需要多线程互斥

       返回值：                           - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    procedure Push(Data: Pointer);
    {* 队列尾挂上一指针。

       参数：
         Data: Pointer                    - 待推入的指针

       返回值：                           - 无
    }

    function Pop: Pointer;
    {* 队列头弹出一指针，如队列空则返回 nil。

       参数：
         （无）

       返回值：                           - 弹出的指针
    }

    property Size: Integer read GetSize;
    {* 内部指针数}
  end;

  TCnObjectQueue = class(TObject)
  {* 对象队列实现类，可运行期创建时指定是否支持多线程互斥。
    内部采用列表实现，仅对象引用，不持有对象}
  private
    FMultiThread: Boolean;
    FLock: TCriticalSection;
    FList: TList;
  public
    constructor Create(MultiThread: Boolean = False); virtual;
    {* 构造函数。

       参数：
         MultiThread: Boolean             - 是否需要多线程互斥

       返回值：                           - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    function Count: Integer;
    {* 队列内元素数量。

       参数：
         （无）

       返回值：                           - 返回队列内元素数量
    }

    function IsEmpty: Boolean;
    {* 队列是否为空。

       参数：
         （无）

       返回值：                           - 返回队列是否为空
    }

    procedure Clear;
    {* 清除队列内所有元素}

    procedure Push(AObject: TObject);
    {* 将一对象入队列。

       参数：
         AObject: TObject                 - 待推入队列的对象

       返回值：                           - 无
    }

    function Pop: TObject;
    {* 队列内出一对象，如队列空则抛异常。

       参数：
         （无）

       返回值：                           - 弹出的对象
    }
  end;

  TCnObjectStack = class(TObject)
  {* 对象栈实现类，可运行期创建时指定是否支持多线程互斥。
     内部采用列表实现，仅对象引用，不持有对象}
  private
    FMultiThread: Boolean;
    FLock: TCriticalSection;
    FList: TList;
  public
    constructor Create(MultiThread: Boolean = False); virtual;
    {* 构造函数。

       参数：
         MultiThread: Boolean             - 是否需要多线程互斥

       返回值：                           - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    function Count: Integer;
    {* 栈内元素数量}
    function IsEmpty: Boolean;
    {* 栈是否为空}
    procedure Clear;
    {* 清除栈内所有元素}

    procedure Push(AObject: TObject);
    {* 将一对象入栈}
    function Pop: TObject;
    {* 出栈至一对象，如堆栈空则抛异常}
    function Peek: TObject;
    {* 取栈顶对象，如堆栈空则抛异常}
  end;

  ECnRingBufferFullException = class(Exception);
  {* 循环队列缓冲区满时触发的异常}

  ECnRingBufferEmptyException = class(Exception);
  {* 循环队列缓冲区空时触发的异常}

  TCnObjectRingBuffer = class(TObject)
  {* 对象的循环队列缓冲区}
  private
    FFullOverwrite: Boolean;
    FMultiThread: Boolean;
    FSize: Integer;
    FList: TList;
    FLock: TCriticalSection;
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
    {* 构造函数。

       参数：
         ASize: Integer                   - 循环队列缓冲区的元素容量
         AFullOverwrite: Boolean          - 是否允许缓冲区满后再写入内容时覆盖以前的数据
         AMultiThread: Boolean            - 是否需要多线程互斥

       返回值：                           - 返回创建的对象实例
    }

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

  TCnMathObjectPool = class(TObjectList)
  {* 数学对象池实现类，允许使用到数学对象池的地方自行继承并创建池}
  private
{$IFDEF MULTI_THREAD}
    FCriticalSection: TCriticalSection;
{$ENDIF}
    procedure Enter; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
    procedure Leave; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
  protected
    function CreateObject: TObject; virtual; abstract;
    {* 子类必须重载的创建具体对象的方法}
  public
    constructor Create; reintroduce;
    {* 构造函数，不通过 TObjectList 持有对象}

    destructor Destroy; override;
    {* 析构函数，显式释放内部对象}

    function Obtain: TObject;
    {* 从对象池获取一个对象，不用时需调用 Recycle 归还。

       参数：
         （无）

       返回值：TObject                    - 返回的对象
    }

    procedure Recycle(Num: TObject);
    {* 将一个对象归还至对象池。

       参数：
         Num: TObject                     - 待归还的对象

       返回值：（无）
    }
  end;

//==============================================================================
// Int32 列表类
//==============================================================================

  TCnInt32CompareProc = function(I1, I2: Integer): Integer;

  TCnIntegerList = class(TList)
  {* 整数列表，利用 32 位 Pointer 或 64 位 Pointer 的低 32 位存 Integer}
  private
    function Get(Index: Integer): Integer;
    procedure Put(Index: Integer; const Value: Integer);
  public
    function Add(Item: Integer): Integer; reintroduce;
    procedure AddList(List: TCnIntegerList);
    procedure Insert(Index: Integer; Item: Integer); reintroduce;
    procedure IntSort(CompareProc: TCnInt32CompareProc = nil);
    {* 排序，默认从小到大}
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property Items[Index: Integer]: Integer read Get write Put; default;
  end;

//==============================================================================
// Int64 列表类
//==============================================================================

  PInt64List = ^TInt64List;
  TInt64List = array[0..MaxListSize - 1] of Int64;

  TCnInt64CompareProc = function(I1, I2: Int64): Integer;

  TCnInt64List = class(TObject)
  {* 64 位有符号整数列表}
  private
    FList: PInt64List;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Int64;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Int64);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Int64): Integer;
    procedure AddList(List: TCnInt64List);
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure DeleteLow(ACount: Integer);
    {* 新增方法，删除 ACount 个最低端元素，如果 Count 不够则删除 Count 个}
    class procedure Error(const Msg: string; Data: Integer); virtual;
    procedure Exchange(Index1: Integer; Index2: Integer);
    function Expand: TCnInt64List;
    function First: Int64;
    function IndexOf(Item: Int64): Integer;
    procedure Insert(Index: Integer; Item: Int64);
    procedure InsertBatch(Index: Integer; ACount: Integer);
    {* 新增方法，在某位置批量插入全 0 值 ACount 个}
    function Last: Int64;
    procedure Move(CurIndex: Integer; NewIndex: Integer);
    function Remove(Item: Int64): Integer;
    procedure IntSort(CompareProc: TCnInt64CompareProc = nil);
    {* 排序，默认从小到大}
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Int64 read Get write Put; default;
    property List: PInt64List read FList;
  end;

//==============================================================================
// UInt32 列表类
//==============================================================================

const
  CN_MAX_UINT32_SIZE = MaxInt div 16;

type
  PCnUInt32Array = ^TCnUInt32Array;
  TCnUInt32Array = array[0..CN_MAX_UINT32_SIZE - 1] of Cardinal;

  TCnUInt32CompareProc = function(U1, U2: Cardinal): Integer;

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
    procedure AddList(List: TCnUInt32List);
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Exchange(Index1: Integer; Index2: Integer);
    function Expand: TCnUInt32List;
    function Extract(Item: Cardinal): Cardinal;
    function First: Cardinal;
    function IndexOf(Item: Cardinal): Integer;
    procedure Insert(Index: Integer; Item: Cardinal);
    function Last: Cardinal;
    procedure Move(CurIndex: Integer; NewIndex: Integer);
    function Remove(Item: Cardinal): Integer;
    procedure IntSort(CompareProc: TCnUInt32CompareProc = nil);
    {* 排序，默认从小到大}
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

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
  CN_NOT_FOUND_INDEX: TUInt64 = TUInt64(-1);

type
  PCnUInt64Array = ^TCnUInt64Array;
  TCnUInt64Array = array[0..CN_MAX_UINT64_SIZE - 1] of TUInt64;

  TCnUInt64CompareProc = function(U1, U2: TUInt64): Integer;

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
    procedure AddList(List: TCnUInt64List);
    procedure Clear; virtual;
    procedure Delete(Index: TUInt64);
    class procedure Error(const Msg: string; Data: Integer); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: Integer); overload;
    procedure Exchange(Index1: TUInt64; Index2: TUInt64);
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
    procedure IntSort(CompareProc: TCnUInt64CompareProc = nil);
    {* 排序，默认从小到大}
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property Capacity: TUInt64 read FCapacity write SetCapacity;
    property Count: TUInt64 read FCount write SetCount;
    property Items[Index: TUInt64]: TUInt64 read Get write Put; default;
    // 内部下标、尺寸均由 TUInt64 表示，不过由于编译器限制实际上达不到 TUInt64
    property List: PCnUInt64Array read FList;
    property IgnoreDuplicated: Boolean read FIgnoreDuplicated write FIgnoreDuplicated;
  end;

  PExtendedList = ^TExtendedList;
  TExtendedList = array[0..MaxListSize - 1] of Extended;

  TCnExtendedCompareProc = function(E1, E2: Extended): Integer;

  TCnExtendedList = class(TObject)
  {* 扩展精度浮点数列表，注意不同平台下元素长度可能不一样}
  private
    FList: PExtendedList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Extended;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Extended);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function Add(Item: Extended): Integer;
    procedure AddList(List: TCnExtendedList);
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure DeleteLow(ACount: Integer);
    {* 新增方法，删除 ACount 个最低端元素，如果 Count 不够则删除 Count 个}
    class procedure Error(const Msg: string; Data: Integer); virtual;
    procedure Exchange(Index1: Integer; Index2: Integer);
    function Expand: TCnExtendedList;
    function First: Extended;
    function IndexOf(Item: Extended): Integer;
    procedure Insert(Index: Integer; Item: Extended);
    procedure InsertBatch(Index: Integer; ACount: Integer);
    {* 新增方法，在某位置批量插入全 0 值 ACount 个}
    function Last: Extended;
    procedure Move(CurIndex: Integer; NewIndex: Integer);
    function Remove(Item: Extended): Integer;
    procedure FloatSort(CompareProc: TCnExtendedCompareProc = nil);
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Extended read Get write Put; default;
    property List: PExtendedList read FList;
  end;

  PRefObjectList = ^TRefObjectList;
  TRefObjectList = array[0..MaxListSize - 1] of TObject;

  TCnRefObjectList = class(TObject)
  {* 对象引用列表，类似于 TObjectList 但不 Own 对象}
  private
    FList: PRefObjectList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): TObject;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: TObject);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    {* 析构函数}
    function Add(Item: TObject): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure DeleteLow(ACount: Integer);
    {* 新增方法，删除 ACount 个最低端元素，如果 Count 不够则删除 Count 个}
    class procedure Error(const Msg: string; Data: Integer); virtual;
    procedure Exchange(Index1: Integer; Index2: Integer);
    function Expand: TCnRefObjectList;
    function First: TObject;
    function IndexOf(Item: TObject): Integer;
    procedure Insert(Index: Integer; Item: TObject);
    procedure InsertBatch(Index: Integer; ACount: Integer);
    {* 新增方法，在某位置批量插入全 0 值 ACount 个}
    function Last: TObject;
    procedure Move(CurIndex: Integer; NewIndex: Integer);
    function Remove(Item: TObject): Integer;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: TObject read Get write Put; default;
    property List: PRefObjectList read FList;
  end;

{$IFDEF POSIX}

  TCnInternalList<T> = class(TList<T>)
  {* MACOS/LINUX 等平台下的 TList 没有 IgnoreDuplicated 功能，需要新写一个类并手工去重}
  public
    procedure RemoveDuplictedElements;
    {* 去除重复的元素}
  end;

{$ENDIF}

  TCnBytesObject = class
  {* 封装了字节数组的对象，内部复制内容进行管理}
  private
    FData: TBytes;
  public
    constructor Create(AMem: Pointer = nil; MemByteSize: Integer = 0); virtual;
    {* 构造函数。

       参数：
         AMem: Pointer                    - 数据块地址，作为内容复制到内部的字节数组中
         MemByteSize: Integer             - 数据块字节长度

       返回值：                           - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    property Data: TBytes read FData write FData;
    {* 内容字节数组}
  end;

  TCnBytesPair = class
  {* 封装了俩字节数组的对象，内部复制内容进行管理}
  private
    FKey: TBytes;
    FValue: TBytes;
  public
    constructor Create(AKeyMem: Pointer = nil; KeyMemByteSize: Integer = 0;
      AValueMem: Pointer = nil; ValueMemByteSize: Integer = 0); virtual;
    {* 构造函数。

       参数：
         AKeyMem: Pointer                 - Key 的数据块地址，作为内容复制到内部的字节数组中
         KeyMemByteSize: Integer          - Key 的数据块字节长度
         AValueMem: Pointer               - Value 的数据块地址，作为内容复制到内部的字节数组中
         ValueMemByteSize: Integer        - Value 的数据块字节长度

       返回值：                           - 返回创建的对象实例
    }

    destructor Destroy; override;
    {* 析构函数}

    property Key: TBytes read FKey write FKey;
    {* Key 字节数组}
    property Value: TBytes read FValue write FValue;
    {* Value 字节数组}
  end;

procedure CnIntegerListCopy(Dst: TCnIntegerList; Src: TCnIntegerList);
{* 复制 TCnIntegerList。

   参数：
     Dst: TCnIntegerList              - 目标列表
     Src: TCnIntegerList              - 源列表

   返回值：                           - 无
}

procedure CnInt64ListCopy(Dst: TCnInt64List; Src: TCnInt64List);
{* 复制 TCnInt64List。

   参数：
     Dst: TCnInt64List                - 目标列表
     Src: TCnInt64List                - 源列表

   返回值：                           - 无
}

procedure CnRefObjectListCopy(Dst: TCnRefObjectList; Src: TCnRefObjectList);
{* 复制 TCnRefObjectList。

   参数：
     Dst: TCnRefObjectList            - 目标列表
     Src: TCnRefObjectList            - 源列表

   返回值：                           - 无
}

implementation

resourcestring
  SCnInt64ListError = 'Int64 List Error. %d';
  SCnExtendedListError = 'Float List Error. %d';
  SCnRefObjectListError = 'Reference Object List Error. %d';
  SCnEmptyPopFromBackError = 'Ring Buffer Empty. Can NOT Pop From Back.';
  SCnEmptyPopFromFrontError = 'Ring Buffer Empty. Can NOT Pop From Front.';
  SCnFullPushToBackError = 'Ring Buffer Full. Can NOT Push To Back.';
  SCnFullPushToFrontError = 'Ring Buffer Full. Can NOT Push To Front.';

type
  TCnQueueNode = class
  private
    FNext: TCnQueueNode;
    FData: Pointer;
  public
    property Next: TCnQueueNode read FNext write FNext;
    property Data: Pointer read FData write FData;
  end;

threadvar
  FCompareProcExtended: TCnExtendedCompareProc;
  FCompareProcInt32: TCnInt32CompareProc;
  FCompareProcUInt32: TCnUInt32CompareProc;
  FCompareProcInt64: TCnInt64CompareProc;
  FCompareProcUInt64: TCnUInt64CompareProc;

function DefExtendedCompareProc(E1, E2: Extended): Integer;
begin
  if Abs(E1 - E2) < 0.000001 then
    Result := 0
  else if E1 > E2 then
    Result := 1
  else
    Result := -1;
end;

function DefInt32CompareProc(I1, I2: Integer): Integer;
begin
  if I1 = I2 then
    Result := 0
  else if I1 > I2 then
    Result := 1
  else
    Result := -1;
end;

function DefInt64CompareProc(I1, I2: Int64): Integer;
begin
  if I1 = I2 then
    Result := 0
  else if I1 > I2 then
    Result := 1
  else
    Result := -1;
end;

function DefUInt32CompareProc(U1, U2: Cardinal): Integer;
begin
  if U1 = U2 then
    Result := 0
  else if U1 > U2 then
    Result := 1
  else
    Result := -1;
end;

function DefUInt64CompareProc(U1, U2: TUInt64): Integer;
begin
  Result := UInt64Compare(U1, U2);
end;

function MyExtendedSortCompare(P1, P2: Pointer; ElementByteSize: Integer): Integer;
begin
  if Assigned(FCompareProcExtended) then
    Result := FCompareProcExtended(PExtended(P1)^, PExtended(P2)^)
  else
    Result := DefExtendedCompareProc(PExtended(P1)^, PExtended(P2)^);
end;

function MyInt32SortCompare(Item1, Item2: Pointer): Integer;
begin
  if Assigned(FCompareProcInt32) then
    Result := FCompareProcInt32(Integer(Item1), Integer(Item2))
  else
    Result := DefInt32CompareProc(Integer(Item1), Integer(Item2));
end;

function MyUInt32SortCompare(P1, P2: Pointer; ElementByteSize: Integer): Integer;
begin
  if Assigned(FCompareProcUInt32) then
    Result := FCompareProcUInt32(PCardinal(P1)^, PCardinal(P2)^)
  else
    Result := DefUInt32CompareProc(PCardinal(P1)^, PCardinal(P2)^);
end;

function MyInt64SortCompare(P1, P2: Pointer; ElementByteSize: Integer): Integer;
begin
  if Assigned(FCompareProcInt64) then
    Result := FCompareProcInt64(PInt64(P1)^, PInt64(P2)^)
  else
    Result := DefInt64CompareProc(PInt64(P1)^, PInt64(P2)^);
end;

function MyUInt64SortCompare(P1, P2: Pointer; ElementByteSize: Integer): Integer;
begin
  if Assigned(FCompareProcUInt64) then
    Result := FCompareProcUInt64(PUInt64(P1)^, PUInt64(P2)^)
  else
    Result := DefUInt64CompareProc(PUInt64(P1)^, PUInt64(P2)^);
end;

{ TCnQueue }

procedure TCnLinkedQueue.FreeNode(Value: TObject);
var
  Tmp: TCnQueueNode;
begin
  Tmp := TCnQueueNode(Value).Next;
  TCnQueueNode(Value).Free;
  if Tmp = nil then
    Exit;
  FreeNode(Tmp);
end;

constructor TCnLinkedQueue.Create(MultiThread: Boolean);
begin
  inherited Create;
  FMultiThread := MultiThread;
  FHead := nil;
  FTail := nil;
  FSize := 0;
  if FMultiThread then
    FLock := TCriticalSection.Create;
end;

destructor TCnLinkedQueue.Destroy;
begin
  if FHead <> nil then
    FreeNode(FHead);
  if FMultiThread then
    FLock.Free;
  inherited;
end;

function TCnLinkedQueue.Pop: Pointer;
var
  Tmp: TCnQueueNode;
begin
  if FMultiThread then
    FLock.Enter;

  try
    Result := nil;
    if FHead = nil then
      Exit;

    Result := TCnQueueNode(FHead).Data;
    Tmp := TCnQueueNode(FHead).Next;
    TCnQueueNode(FHead).Free;
    FHead := Tmp;
    
    if Tmp = nil then
      FTail := nil;
    FSize := FSize - 1;
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

procedure TCnLinkedQueue.Push(Data: Pointer);
var
  Tmp: TCnQueueNode;
begin
  if FMultiThread then
    FLock.Enter;

  try
    if Data = nil then Exit;
    Tmp := TCnQueueNode.Create;
    Tmp.Data := Data;
    Tmp.Next := nil;
    
    if FTail = nil then
    begin
      FTail := Tmp;
      FHead := Tmp;
    end
    else
    begin
      TCnQueueNode(FTail).Next := Tmp;
      FTail := Tmp;
    end;
    
    FSize := FSize + 1;
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

function TCnLinkedQueue.GetSize: Integer;
begin
  Result := FSize;
end;

{ TCnObjectQueue }

procedure TCnObjectQueue.Clear;
begin
  if FMultiThread then
    FLock.Enter;

  try
    FList.Clear;
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

function TCnObjectQueue.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCnObjectQueue.Create(MultiThread: Boolean);
begin
  inherited Create;
  FList := TList.Create;
  FMultiThread := MultiThread;
  if FMultiThread then
    FLock := TCriticalSection.Create;
end;

destructor TCnObjectQueue.Destroy;
begin
  if FMultiThread then
    FLock.Free;
  FList.Free;
  inherited;
end;

function TCnObjectQueue.IsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

function TCnObjectQueue.Pop: TObject;
begin
  if FMultiThread then
    FLock.Enter;

  try
    Result := TObject(FList[0]);
    FList.Delete(0);
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

procedure TCnObjectQueue.Push(AObject: TObject);
begin
  if FMultiThread then
    FLock.Enter;

  try
    FList.Add(AObject);
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

{ TCnObjectStack }

procedure TCnObjectStack.Clear;
begin
  if FMultiThread then
    FLock.Enter;

  try
    FList.Clear;
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

function TCnObjectStack.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCnObjectStack.Create(MultiThread: Boolean);
begin
  inherited Create;
  FList := TList.Create;
  FMultiThread := MultiThread;
  if FMultiThread then
    FLock := TCriticalSection.Create;
end;

destructor TCnObjectStack.Destroy;
begin
  if FMultiThread then
    FLock.Free;
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
  if FMultiThread then
    FLock.Enter;

  try
    Result := TObject(FList[FList.Count - 1]);
    FList.Delete(FList.Count - 1);
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

procedure TCnObjectStack.Push(AObject: TObject);
begin
  if FMultiThread then
    FLock.Enter;

  try
    FList.Add(AObject);
  finally
    if FMultiThread then
      FLock.Leave;
  end;
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
    FLock := TCriticalSection.Create;
end;

destructor TCnObjectRingBuffer.Destroy;
begin
  if FMultiThread then
    FLock.Free;
  FList.Free;
  inherited;
end;

procedure TCnObjectRingBuffer.Dump(List: TList; out FrontIdx: Integer;
  out BackIdx: Integer);
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

{$HINTS OFF}

function TCnObjectRingBuffer.PopFromBack: TObject;
begin
  Result := nil;  // 不加则低版本 Delphi 有警告，加则高版本 Delphi 有警告
  if FMultiThread then
    FLock.Enter;

  try
    if FCount <= 0 then
      raise ECnRingBufferEmptyException.Create(SCnEmptyPopFromBackError);

    Dec(FBackIdx);
    if FBackIdx < 0 then
      FBackIdx := FSize - 1;
    Result := TObject(FList[FBackIdx]);
    FList[FBackIdx] := nil;
    Dec(FCount);
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

function TCnObjectRingBuffer.PopFromFront: TObject;
begin
  Result := nil; // 不加则低版本 Delphi 有警告，加则高版本 Delphi 有警告
  if FMultiThread then
    FLock.Enter;

  try
    if FCount <= 0 then
      raise ECnRingBufferEmptyException.Create(SCnEmptyPopFromFrontError);

    Result := TObject(FList[FFrontIdx]);
    FList[FFrontIdx] := nil;

    Inc(FFrontIdx);
    if FFrontIdx >= FSize then
      FFrontIdx := 0;
    Dec(FCount);
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

{$HINTS ON}

procedure TCnObjectRingBuffer.PushToBack(AObject: TObject);
begin
  if FMultiThread then
    FLock.Enter;

  try
    if not FFullOverwrite and (FCount >= FSize) then
      raise ECnRingBufferFullException.Create(SCnFullPushToBackError);

    FList[FBackIdx] := AObject;
    Inc(FBackIdx);
    if FBackIdx >= FSize then
      FBackIdx := 0;

    if FCount < FSize then
      Inc(FCount);
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

procedure TCnObjectRingBuffer.PushToFront(AObject: TObject);
begin
  if FMultiThread then
    FLock.Enter;

  try
    if not FFullOverwrite and (FCount >= FSize) then
      raise ECnRingBufferFullException.Create(SCnFullPushToFrontError);

    Dec(FFrontIdx);
    if FFrontIdx < 0 then
      FFrontIdx := FSize - 1;
    FList[FFrontIdx] := AObject;

    if FCount < FSize then
      Inc(FCount);
  finally
    if FMultiThread then
      FLock.Leave;
  end;
end;

{ TCnMathObjectPool }

constructor TCnMathObjectPool.Create;
begin
  inherited Create(False);
{$IFDEF MULTI_THREAD}
  FCriticalSection := TCriticalSection.Create;
{$ENDIF}
end;

destructor TCnMathObjectPool.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TObject(Items[I]).Free;

{$IFDEF MULTI_THREAD}
  FCriticalSection.Free;
{$ENDIF}
  inherited;
end;

procedure TCnMathObjectPool.Enter;
begin
{$IFDEF MULTI_THREAD}
  FCriticalSection.Enter;
{$ENDIF}
end;

procedure TCnMathObjectPool.Leave;
begin
{$IFDEF MULTI_THREAD}
  FCriticalSection.Leave;
{$ENDIF}
end;

function TCnMathObjectPool.Obtain: TObject;
begin
  Enter;
  try
    if Count = 0 then
      Result := CreateObject
    else
    begin
      Result := TObject(Items[Count - 1]);
      Delete(Count - 1);
    end;
  finally
    Leave;
  end;
end;

procedure TCnMathObjectPool.Recycle(Num: TObject);
begin
  if Num <> nil then
  begin
    Enter;
    try
      Add(Num);
    finally
      Leave;
    end;
  end;
end;

{ TCnIntegerList }

function TCnIntegerList.Add(Item: Integer): Integer;
begin
  Result := inherited Add(IntegerToPointer(Item));
end;

procedure TCnIntegerList.AddList(List: TCnIntegerList);
var
  I: Integer;
begin
  if (List <> nil) and (List.Count > 0) then
  begin
    for I := 0 to List.Count - 1 do
      Add(List[I]);
  end;
end;

function TCnIntegerList.Get(Index: Integer): Integer;
begin
  Result := PointerToInteger(inherited Get(Index));
end;

procedure TCnIntegerList.Insert(Index, Item: Integer);
begin
  inherited Insert(Index, IntegerToPointer(Item));
end;

procedure TCnIntegerList.IntSort(CompareProc: TCnInt32CompareProc);
begin
  FCompareProcInt32 := CompareProc;
  Sort(MyInt32SortCompare);
end;

function TCnIntegerList.ToString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if I = 0 then
      Result := IntToStr(Items[I])
    else
      Result := Result + ',' + IntToStr(Items[I]);
  end;
end;

procedure TCnIntegerList.Put(Index: Integer; const Value: Integer);
begin
  inherited Put(Index, IntegerToPointer(Value));
end;

{ TCnInt64List }

destructor TCnInt64List.Destroy;
begin
  Clear;
end;

function TCnInt64List.Add(Item: Int64): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TCnInt64List.AddList(List: TCnInt64List);
var
  I: Integer;
begin
  if (List <> nil) and (List.Count > 0) then
  begin
    for I := 0 to List.Count - 1 do
      Add(List[I]);
  end;
end;

procedure TCnInt64List.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCnInt64List.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnInt64ListError, Index);

  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Int64));
end;

procedure TCnInt64List.DeleteLow(ACount: Integer);
begin
  if ACount > 0 then
  begin
    if ACount >= FCount then
      Clear
    else
    begin
      Dec(FCount, ACount);

      // 从 0 删除到 ACount - 1，也就是把 ACount 到 Count - 1 处的 Move 到 0
      System.Move(FList^[ACount], FList^[0],
        FCount * SizeOf(Int64));
    end;
  end;
end;

class procedure TCnInt64List.Error(const Msg: string; Data: Integer);
begin
  raise EListError.CreateFmt(Msg, [Data]);
end;

procedure TCnInt64List.Exchange(Index1: Integer; Index2: Integer);
var
  Item: Int64;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(SCnInt64ListError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(SCnInt64ListError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TCnInt64List.Expand: TCnInt64List;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TCnInt64List.First: Int64;
begin
  Result := Get(0);
end;

function TCnInt64List.Get(Index: Integer): Int64;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnInt64ListError, Index);
  Result := FList^[Index];
end;

procedure TCnInt64List.Grow;
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

function TCnInt64List.IndexOf(Item: Int64): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TCnInt64List.Insert(Index: Integer; Item: Int64);
begin
  if (Index < 0) or (Index > FCount) then
    Error(SCnInt64ListError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Int64));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TCnInt64List.InsertBatch(Index, ACount: Integer);
begin
  if ACount <= 0 then
    Exit;

  if (Index < 0) or (Index > FCount) then
    Error(SCnInt64ListError, Index);
  SetCapacity(FCount + ACount); // 容量扩充至至少 FCount + ACount，FCount 没变

  System.Move(FList^[Index], FList^[Index + ACount],
    (FCount - Index) * SizeOf(Int64));
  System.FillChar(FList^[Index], ACount * SizeOf(Int64), 0);
  FCount := FCount + ACount;
end;

function TCnInt64List.Last: Int64;
begin
  Result := Get(FCount - 1);
end;

procedure TCnInt64List.Move(CurIndex, NewIndex: Integer);
var
  Item: Int64;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(SCnInt64ListError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := 0;
    Delete(CurIndex);
    Insert(NewIndex, 0);
    FList^[NewIndex] := Item;
  end;
end;

procedure TCnInt64List.Put(Index: Integer; Item: Int64);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnInt64ListError, Index);

  FList^[Index] := Item;
end;

function TCnInt64List.Remove(Item: Int64): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnInt64List.IntSort(CompareProc: TCnInt64CompareProc);
begin
  FCompareProcInt64 := CompareProc;
  if FCount >= 1 then
    MemoryQuickSort(FList, SizeOf(Int64), FCount, MyInt64SortCompare);
end;

function TCnInt64List.ToString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if I = 0 then
      Result := IntToStr(Items[I])
    else
      Result := Result + ',' + IntToStr(Items[I]);
  end;
end;

procedure TCnInt64List.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(SCnInt64ListError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Int64));
    FCapacity := NewCapacity;
  end;
end;

procedure TCnInt64List.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(SCnInt64ListError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Int64), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
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

procedure TCnUInt32List.AddList(List: TCnUInt32List);
var
  I: Integer;
begin
  if (List <> nil) and (List.Count > 0) then
  begin
    for I := 0 to List.Count - 1 do
      Add(List[I]);
  end;
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

procedure TCnUInt32List.Exchange(Index1: Integer; Index2: Integer);
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

procedure TCnUInt32List.IntSort(CompareProc: TCnUInt32CompareProc);
begin
  FCompareProcUInt32 := CompareProc;
  if FCount >= 1 then
    MemoryQuickSort(FList, SizeOf(Cardinal), FCount, MyUInt32SortCompare);
end;

function TCnUInt32List.ToString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if I = 0 then
      Result := UInt32ToStr(Items[I])
    else
      Result := Result + ',' + UInt32ToStr(Items[I]);
  end;
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
    Result := CN_NOT_FOUND_INDEX;
    Exit;
  end;

  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TCnUInt64List.AddList(List: TCnUInt64List);
var
  I: Integer;
begin
  if (List <> nil) and (List.Count > 0) then
  begin
    for I := 0 to List.Count - 1 do
      Add(List[I]);
  end;
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

procedure TCnUInt64List.Exchange(Index1: TUInt64; Index2: TUInt64);
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
  SetCapacity(FCapacity + TUInt64(Delta));
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

procedure TCnUInt64List.IntSort(CompareProc: TCnUInt64CompareProc);
begin
  FCompareProcUInt64 := CompareProc;
  if FCount >= 1 then
    MemoryQuickSort(FList, SizeOf(TUInt64), FCount, MyUInt64SortCompare);
end;

function TCnUInt64List.ToString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if I = 0 then
      Result := UInt64ToStr(Items[I])
    else
      Result := Result + ',' + UInt64ToStr(Items[I]);
  end;
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
  begin
    for I := FCount - 1 downto NewCount do
      Delete(I);
  end;
  FCount := NewCount;
end;

{ TCnExtendedList }

destructor TCnExtendedList.Destroy;
begin
  Clear;
end;

function TCnExtendedList.Add(Item: Extended): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TCnExtendedList.AddList(List: TCnExtendedList);
var
  I: Integer;
begin
  if (List <> nil) and (List.Count > 0) then
  begin
    for I := 0 to List.Count - 1 do
      Add(List[I]);
  end;
end;

procedure TCnExtendedList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCnExtendedList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnExtendedListError, Index);

  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Extended));
end;

procedure TCnExtendedList.DeleteLow(ACount: Integer);
begin
  if ACount > 0 then
  begin
    if ACount >= FCount then
      Clear
    else
    begin
      Dec(FCount, ACount);

      // 从 0 删除到 ACount - 1，也就是把 ACount 到 Count - 1 处的 Move 到 0
      System.Move(FList^[ACount], FList^[0],
        FCount * SizeOf(Extended));
    end;
  end;
end;

class procedure TCnExtendedList.Error(const Msg: string; Data: Integer);
begin
  raise EListError.CreateFmt(Msg, [Data]);
end;

procedure TCnExtendedList.Exchange(Index1: Integer; Index2: Integer);
var
  Item: Extended;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(SCnExtendedListError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(SCnExtendedListError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TCnExtendedList.Expand: TCnExtendedList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TCnExtendedList.First: Extended;
begin
  Result := Get(0);
end;

function TCnExtendedList.Get(Index: Integer): Extended;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnExtendedListError, Index);
  Result := FList^[Index];
end;

procedure TCnExtendedList.Grow;
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

function TCnExtendedList.IndexOf(Item: Extended): Integer;
begin
  Result := 0;
  while (Result < FCount) and (Abs(FList^[Result] - Item) < 0.00001) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TCnExtendedList.Insert(Index: Integer; Item: Extended);
begin
  if (Index < 0) or (Index > FCount) then
    Error(SCnExtendedListError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Extended));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TCnExtendedList.InsertBatch(Index, ACount: Integer);
begin
  if ACount <= 0 then
    Exit;

  if (Index < 0) or (Index > FCount) then
    Error(SCnExtendedListError, Index);
  SetCapacity(FCount + ACount); // 容量扩充至至少 FCount + ACount，FCount 没变

  System.Move(FList^[Index], FList^[Index + ACount],
    (FCount - Index) * SizeOf(Extended));
  System.FillChar(FList^[Index], ACount * SizeOf(Extended), 0);
  FCount := FCount + ACount;
end;

function TCnExtendedList.Last: Extended;
begin
  Result := Get(FCount - 1);
end;

procedure TCnExtendedList.Move(CurIndex: Integer; NewIndex: Integer);
var
  Item: Extended;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(SCnExtendedListError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := 0;
    Delete(CurIndex);
    Insert(NewIndex, 0);
    FList^[NewIndex] := Item;
  end;
end;

procedure TCnExtendedList.Put(Index: Integer; Item: Extended);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnExtendedListError, Index);

  FList^[Index] := Item;
end;

function TCnExtendedList.Remove(Item: Extended): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnExtendedList.FloatSort(CompareProc: TCnExtendedCompareProc);
begin
  FCompareProcExtended := CompareProc;
  if FCount >= 1 then
    MemoryQuickSort(FList, SizeOf(Extended), FCount, MyExtendedSortCompare);
end;

function TCnExtendedList.ToString: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if I = 0 then
      Result := FloatToStr(Items[I])
    else
      Result := Result + ',' + FloatToStr(Items[I]);
  end;
end;

procedure TCnExtendedList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(SCnExtendedListError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Extended));
    FCapacity := NewCapacity;
  end;
end;

procedure TCnExtendedList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(SCnExtendedListError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Extended), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

{ TCnRefObjectList }

destructor TCnRefObjectList.Destroy;
begin
  Clear;
end;

function TCnRefObjectList.Add(Item: TObject): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
end;

procedure TCnRefObjectList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCnRefObjectList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnRefObjectListError, Index);

  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(TObject));
end;

procedure TCnRefObjectList.DeleteLow(ACount: Integer);
begin
  if ACount > 0 then
  begin
    if ACount >= FCount then
      Clear
    else
    begin
      Dec(FCount, ACount);

      // 从 0 删除到 ACount - 1，也就是把 ACount 到 Count - 1 处的 Move 到 0
      System.Move(FList^[ACount], FList^[0],
        FCount * SizeOf(TObject));
    end;
  end;
end;

class procedure TCnRefObjectList.Error(const Msg: string; Data: Integer);
begin
  raise EListError.CreateFmt(Msg, [Data]);
end;

procedure TCnRefObjectList.Exchange(Index1: Integer; Index2: Integer);
var
  Item: TObject;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(SCnRefObjectListError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(SCnRefObjectListError, Index2);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TCnRefObjectList.Expand: TCnRefObjectList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TCnRefObjectList.First: TObject;
begin
  Result := Get(0);
end;

function TCnRefObjectList.Get(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnRefObjectListError, Index);
  Result := FList^[Index];
end;

procedure TCnRefObjectList.Grow;
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

function TCnRefObjectList.IndexOf(Item: TObject): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TCnRefObjectList.Insert(Index: Integer; Item: TObject);
begin
  if (Index < 0) or (Index > FCount) then
    Error(SCnRefObjectListError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TObject));
  FList^[Index] := Item;
  Inc(FCount);
end;

procedure TCnRefObjectList.InsertBatch(Index, ACount: Integer);
begin
  if ACount <= 0 then
    Exit;

  if (Index < 0) or (Index > FCount) then
    Error(SCnRefObjectListError, Index);
  SetCapacity(FCount + ACount); // 容量扩充至至少 FCount + ACount，FCount 没变

  System.Move(FList^[Index], FList^[Index + ACount],
    (FCount - Index) * SizeOf(TObject));
  System.FillChar(FList^[Index], ACount * SizeOf(TObject), 0);
  FCount := FCount + ACount;
end;

function TCnRefObjectList.Last: TObject;
begin
  Result := Get(FCount - 1);
end;

procedure TCnRefObjectList.Move(CurIndex, NewIndex: Integer);
var
  Item: TObject;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(SCnRefObjectListError, NewIndex);
    Item := Get(CurIndex);
    FList^[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList^[NewIndex] := Item;
  end;
end;

procedure TCnRefObjectList.Put(Index: Integer; Item: TObject);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(SCnRefObjectListError, Index);

  FList^[Index] := Item;
end;

function TCnRefObjectList.Remove(Item: TObject): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCnRefObjectList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(SCnRefObjectListError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(TObject));
    FCapacity := NewCapacity;
  end;
end;

procedure TCnRefObjectList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(SCnRefObjectListError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(TObject), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure CnIntegerListCopy(Dst: TCnIntegerList; Src: TCnIntegerList);
begin
  if (Src <> nil) and (Dst <> nil) and (Src <> Dst) then
  begin
    Dst.Count := Src.Count;
    if Src.Count > 0 then
    begin
{$IFDEF LIST_NEW_POINTER}
      Move(Src.List[0], Dst.List[0], Src.Count * SizeOf(Integer));
{$ELSE}
      Move(Src.List^, Dst.List^, Src.Count * SizeOf(Integer));
{$ENDIF}
    end;
  end;
end;

procedure CnInt64ListCopy(Dst: TCnInt64List; Src: TCnInt64List);
begin
  if (Src <> nil) and (Dst <> nil) and (Src <> Dst) then
  begin
    Dst.Count := Src.Count;
    if Src.Count > 0 then
      Move(Src.List^, Dst.List^, Src.Count * SizeOf(Int64));
  end;
end;

procedure CnRefObjectListCopy(Dst: TCnRefObjectList; Src: TCnRefObjectList);
begin
  if (Src <> nil) and (Dst <> nil) and (Src <> Dst) then
  begin
    Dst.Count := Src.Count;
    if Src.Count > 0 then
      Move(Src.List^, Dst.List^, Src.Count * SizeOf(TObject));
  end;
end;

{$IFDEF POSIX}

{ TCnInternalList<T> }

procedure TCnInternalList<T>.RemoveDuplictedElements;
var
  I, J: Integer;
  V: NativeInt;
  Dup: Boolean;
begin
  for I := Count - 1 downto 0 do
  begin
    V := ItemValue(Items[I]);
    Dup := False;
    for J := 0 to I - 1 do
    begin
      if V = ItemValue(Items[J]) then
      begin
        Dup := True;
        Break;
      end;
    end;

    if Dup then
      Delete(I);
  end;
end;

{$ENDIF}

{ TCnBytesObject }

constructor TCnBytesObject.Create(AMem: Pointer; MemByteSize: Integer);
begin
  inherited Create;
  if (AMem <> nil) and (MemByteSize > 0) then
  begin
    SetLength(FData, MemByteSize);
    Move(AMem^, FData[0], MemByteSize);
  end;
end;

destructor TCnBytesObject.Destroy;
begin
  SetLength(FData, 0);
  inherited;
end;

{ TCnBytesPair }

constructor TCnBytesPair.Create(AKeyMem: Pointer; KeyMemByteSize: Integer;
  AValueMem: Pointer; ValueMemByteSize: Integer);
begin
  inherited Create;

  if (AKeyMem <> nil) and (KeyMemByteSize > 0) then
  begin
    SetLength(FKey, KeyMemByteSize);
    Move(AKeyMem^, FKey[0], KeyMemByteSize);
  end;

  if (AValueMem <> nil) and (ValueMemByteSize > 0) then
  begin
    SetLength(FValue, ValueMemByteSize);
    Move(AValueMem^, FValue[0], ValueMemByteSize);
  end;
end;

destructor TCnBytesPair.Destroy;
begin
  SetLength(FKey, 0);
  SetLength(FValue, 0);
  inherited;
end;

end.
