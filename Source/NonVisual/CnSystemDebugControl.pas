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

unit CnSystemDebugControl;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包不可视组件
* 单元名称：封装了 SystemDebugControl 系统调用的组件单元
* 单元作者：刘啸（liuxiao@cnpack.org)
* 开发平台：PWinXPPro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7/2005 + C++Build 5/6
* 备　　注：参考了网上于旸的 SystemDebugControl 分析文章以及其它网络资源
* 修改记录：2019.05.19 V1.1
*               修正误用 GetModuleHandle 导致可能释放出错的问题
*           2008.09.18 V1.0
*               LiuXiao 实现单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Classes, CnCommon;

type
  _DEBUG_CONTROL_CODE = (
    SysDbgDummyZero,  // 0,
    //以下5个在Windows NT各个版本上都有
    SysDbgGetTraceInformation,   // 1,
    SysDbgSetInternalBreakpoint, // 2,
    SysDbgSetSpecialCall,        // 3,
    SysDbgClearSpecialCalls,     // 4,
    SysDbgQuerySpecialCalls,     // 5,

    // 以下是NT 5.1 新增的
    SysDbgDbgBreakPointWithStatus,  // 6,

    //获取KdVersionBlock
    SysDbgSysGetVersion,  // 7,

    //从内核空间拷贝到用户空间，或者从用户空间拷贝到用户空间
    //但是不能从用户空间拷贝到内核空间
    SysDbgCopyMemoryChunks_0,  // 8,

    //从用户空间拷贝到内核空间，或者从用户空间拷贝到用户空间
    //但是不能从内核空间拷贝到用户空间
    SysDbgCopyMemoryChunks_1,  // 9,
  
    //从物理地址拷贝到用户空间，不能写到内核空间
    SysDbgCopyMemoryChunks_2,  // 10,
  
    //从用户空间拷贝到物理地址，不能读取内核空间
    SysDbgCopyMemoryChunks_3,  // 11,
  
    //读写处理器相关控制块
    SysDbgSysReadControlSpace,   // 12,
    SysDbgSysWriteControlSpace,  // 13,

    //读写端口
    SysDbgSysReadIoSpace,   // 14,
    SysDbgSysWriteIoSpace,  // 15,

    //分别调用RDMSR和WRMSR
    SysDbgSysReadMsr,   // 16,
    SysDbgSysWriteMsr,  // 17,

    //读写总线数据
    SysDbgSysReadBusData,     // 18,
    SysDbgSysWriteBusData,    // 19,

    SysDbgSysCheckLowMemory,  // 20,

    // 以下是NT 5.2 新增的
    //分别调用_KdEnableDebugger和_KdDisableDebugger
    SysDbgEnableDebugger,   // 21,
    SysDbgDisableDebugger,  // 22,
    
    //获取和设置一些调试相关的变量
    SysDbgGetAutoEnableOnEvent,  // 23,
    SysDbgSetAutoEnableOnEvent,  // 24,
    SysDbgGetPitchDebugger,      // 25,
    SysDbgSetDbgPrintBufferSize, // 26,
    SysDbgGetIgnoreUmExceptions, // 27,
    SysDbgSetIgnoreUmExceptions  // 28
  );
  DEBUG_CONTROL_CODE = _DEBUG_CONTROL_CODE;

  TIOStruct = record
    IoAddr: DWORD;
    Reserved1: DWORD;
    pBuffer: Pointer;
    NumBYTEs: DWORD;
    Reserved4: DWORD;
    Reserved5: DWORD;
    Reserved6: DWORD;
    Reserved7: DWORD;
  end;

  _MEMORY_CHUNKS = record
    Address: ULONG;
    Data: Pointer;
    Length: ULONG;
  end;
  MEMORY_CHUNKS = _MEMORY_CHUNKS;
  PMEMORY_CHUNKS = ^_MEMORY_CHUNKS;

  _DBGKD_GET_VERSION64 = packed record
    MajorVersion:    Word;
    MinorVersion:    Word;
    ProtocolVersion: Word;
    Flags:           Word;
    MachineType:     Word;
    MaxPacketType:   Byte;
    MaxStateChange:  Byte;
    MaxManipulate:   Byte;
    Simulation:      Byte;
    Unused:          Word;
    KernBase:           Int64;
    PsLoadedModuleList: Int64;
    DebuggerDataList:   Int64;
  end;
  DBGKD_GET_VERSION64 = _DBGKD_GET_VERSION64;
  PDBGKD_GET_VERSION64 = ^_DBGKD_GET_VERSION64;

  TCnSystemDebugControl = class(TComponent)
  private
    FKernelBase: Int64;

    procedure KbcWait4IBE;
    {* 等待键盘缓冲区为空}
    procedure InternalCopyMemory(Code: DEBUG_CONTROL_CODE; Address: Cardinal;
      Memory: Pointer; Length: Cardinal);
    {* 封装的内存复制操作}
    procedure SysGetVersion;
    {* 封装获取版本号的操作}
    function GetKernelBase: Cardinal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function InPortB(Port: DWORD): Byte;
    {* 读物理端口}
    procedure OutPortB(Port: DWORD; Value: Byte);
    {* 写物理端口}

    // 以下是读写物理端口的具体应用封装
    procedure SimuKeyDown(VKey: Cardinal);
    {* 模拟按下一个键}
    procedure SimuKeyUp(VKey: Cardinal);
    {* 模拟抬起一个键}
    procedure SimuKey(VKey: Cardinal);
    {* 模拟按下抬起一个键}
    procedure BeepOn(Freq: Integer);
    {* 以指定频率开始让扬声器发声}
    procedure BeepOff;
    {* 让扬声器停止发声}
    function ReadCMOS(Index: Byte): Byte;
    {* 读 CMOS 内容}
    function ReadFirstHardDiskSerialNumber: string;
    {* 读取硬盘序列号}

    procedure ReadKernelMemory(Address: Cardinal; Memory: Pointer;
      Length: Cardinal);
    {* 读取内核空间指定地址的指定长度的内容，内容将复制到 Memory 指的空间中}
    procedure WriteKernelMemory(Address: Cardinal; Memory: Pointer;
      Length: Cardinal);
    {* 将指定 Memory 地址的指定长度的内容写入内核空间指定地址}
    procedure ReadPhysicalMemory(Address: Cardinal; Memory: Pointer;
      Length: Cardinal);
    {* 读取物理指定地址的指定长度的内容，内容将复制到 Memory 指的空间中}
    procedure WritePhysicalMemory(Address: Cardinal; Memory: Pointer;
      Length: Cardinal);
    {* 将指定 Memory 地址的指定长度的内容写入指定物理地址}

    property KernelBase: Cardinal read GetKernelBase;
    {* 内核映像基址，有 MZ 俩字符}
  end;

implementation

const
  KBC_KEY_CMD  = $64; // 键盘数据端口号
  KBC_KEY_DATA = $60; // 键盘操作端口号

type
  TZwSystemDebugControl = function (
    ControlCode: _DEBUG_CONTROL_CODE;
    InputBuffer: Pointer;
    InputBufferLength: ULONG;
    OutputBuffer: Pointer;
    OutputBufferLength: ULONG;
    ReturnLength: PULONG): LongInt; stdcall;

var
  NtDllHandle: THandle = 0;
  ZwSystemDebugControl: TZwSystemDebugControl = nil;

{ TCnSystemDebugControl }

procedure TCnSystemDebugControl.BeepOff;
var
  B: Byte;
begin
  B := InPortB($61) and $FC;
  OutPortB($61, B);
end;

procedure TCnSystemDebugControl.BeepOn(Freq: Integer);
var
  B: Byte;
begin
  if (Freq >= 20) and (Freq <= 20000) then
  begin
    Freq := Trunc(1193181 / Freq);
    B := InPortB($61);
    if (B and 3) = 0 then
    begin
      OutPortB($61, (B or 3));
      OutPortB($43, $B6);
    end;
    OutPortB($42, Byte(Freq and $FF));
    OutPortB($42, Byte(Freq shr 8));
  end;
end;

constructor TCnSystemDebugControl.Create(AOwner: TComponent);
begin
  inherited;
  if NtDllHandle = 0 then
    raise Exception.Create('Only Windows XP/2003 or Later can be Supported.');
end;

destructor TCnSystemDebugControl.Destroy;
begin

  inherited;
end;

function TCnSystemDebugControl.InPortB(Port: DWORD): Byte;
var
  Value: BYTE;
  Io: TIOStruct;
begin
  Value := 0;
  Io.IoAddr := Port;
  Io.Reserved1 := 0;
  Io.pBuffer := Pointer(@Value);
  Io.NumBYTEs := SizeOf(Byte);
  Io.Reserved4 := 1;
  Io.Reserved5 := 0;
  Io.Reserved6 := 1;
  Io.Reserved7 := 0;
  ZwSystemDebugControl(SysDbgSysReadIoSpace, @Io, SizeOf(Io), nil, 0, nil);
  Result := Value;
end;

procedure TCnSystemDebugControl.KbcWait4IBE;
var
  RegVal: DWORD;
begin
  repeat
    RegVal := InPortB(KBC_KEY_CMD);
  until (RegVal and $00000002) = 0;
end;

procedure TCnSystemDebugControl.OutPortB(Port: DWORD; Value: Byte);
var
  Io: TIOStruct;
begin
  Io.IoAddr := Port;
  Io.Reserved1 := 0;
  Io.pBuffer := Pointer(@Value);
  Io.NumBYTEs := SizeOf(Byte);
  Io.Reserved4 := 1;
  Io.Reserved5 := 0;
  Io.Reserved6 := 1;
  Io.Reserved7 := 0;
  ZwSystemDebugControl(SysDbgSysWriteIoSpace, @Io, sizeof(Io), nil, 0, nil);
end;

procedure TCnSystemDebugControl.ReadKernelMemory(Address: Cardinal;
  Memory: Pointer; Length: Cardinal);
begin
  InternalCopyMemory(SysDbgCopyMemoryChunks_0, Address, Memory, Length);
end;

procedure TCnSystemDebugControl.ReadPhysicalMemory(Address: Cardinal;
  Memory: Pointer; Length: Cardinal);
begin
  InternalCopyMemory(SysDbgCopyMemoryChunks_2, Address, Memory, Length);
end;

procedure TCnSystemDebugControl.WriteKernelMemory(Address: Cardinal;
  Memory: Pointer; Length: Cardinal);
begin
  InternalCopyMemory(SysDbgCopyMemoryChunks_1, Address, Memory, Length);
end;

procedure TCnSystemDebugControl.WritePhysicalMemory(Address: Cardinal;
  Memory: Pointer; Length: Cardinal);
begin
  InternalCopyMemory(SysDbgCopyMemoryChunks_3, Address, Memory, Length);
end;

procedure TCnSystemDebugControl.SimuKey(VKey: Cardinal);
begin
  SimuKeyDown(VKey);
  SimuKeyUp(VKey);
end;

procedure TCnSystemDebugControl.SimuKeyDown(VKey: Cardinal);
var
  ScanCode: Cardinal;
begin
  ScanCode := MapVirtualKey(VKey, 0);
  KBCWait4IBE;                               // 发送数据前应该先等待键盘缓冲区为空
  OutPortB(KBC_KEY_CMD, $D2);                // 发送键盘写入命令, 0xD2:写键盘缓冲区,0xD3:写鼠标缓冲区,
  KBCWait4IBE;
  OutPortB(KBC_KEY_DATA, ScanCode);          // 写入按键信息,按下键
end;

procedure TCnSystemDebugControl.SimuKeyUp(VKey: Cardinal);
var
  ScanCode: Cardinal;
begin
  ScanCode := MapVirtualKey(VKey, 0);
  KBCWait4IBE;                               // 等待键盘缓冲区为空
  OutPortB(KBC_KEY_CMD, $D2);                // 发送键盘写入命令
  KBCWait4IBE;
  OutPortB(KBC_KEY_DATA, (ScanCode or $80)); // 写入按键信息，释放键
end;

function GetNtNativeAPIs: Boolean;
begin
  if (Win32Platform = VER_PLATFORM_WIN32_NT)
    and (Win32MajorVersion >= 5) and (Win32MinorVersion >= 1) then
  begin
    // 不能用 GetModuleHandle，因为下文无法判定这个 Handle 咋来的，可能导致错误的 FreeLibrary
    if NtDllHandle = 0 then
      NtDllHandle := LoadLibrary('NTDLL.DLL');

    if NtDllHandle <> 0 then
    begin
      @ZwSystemDebugControl := GetProcAddress(NtDllHandle, 'ZwSystemDebugControl');
    end;
  end;

  Result := NtDllHandle <> 0;
end;

procedure FreeNtNativeAPIs;
begin
  if NtDllHandle <> 0 then
  begin
    FreeLibrary(NtDllHandle);
    NtDllHandle := 0;
  end;
end;

procedure TCnSystemDebugControl.InternalCopyMemory(
  Code: DEBUG_CONTROL_CODE; Address: Cardinal; Memory: Pointer;
  Length: Cardinal);
var
  M: MEMORY_CHUNKS;
  Len: Integer;
begin
  if Code in [SysDbgCopyMemoryChunks_0..SysDbgCopyMemoryChunks_3] then
  begin
    M.Address := Address;
    M.Data := Memory;
    M.Length := Length;
    ZwSystemDebugControl(Code, @M, SizeOf(MEMORY_CHUNKS), nil, 0, @Len);
  end;
end;

function TCnSystemDebugControl.GetKernelBase: Cardinal;
begin
  if FKernelBase = 0 then
    SysGetVersion;
  Result := Cardinal(FKernelBase);
end;

procedure TCnSystemDebugControl.SysGetVersion;
var
  Block: DBGKD_GET_VERSION64;
begin
  ZwSystemDebugControl(SysDbgSysGetVersion, nil, 0, @Block,
    SizeOf(DBGKD_GET_VERSION64), nil);

  FKernelBase := Block.KernBase;
end;

function TCnSystemDebugControl.ReadCMOS(Index: Byte): Byte;
begin
  OutPortB($70, Index);     // 写要读的索引值
  // Sleep(0);
  Result := InPortB($71);   // 读值
end;

function TCnSystemDebugControl.ReadFirstHardDiskSerialNumber: string;
var
  I: Integer;

  function WaitUntilIdle: Byte;
  begin
    Result := InPortB($1F7);
    while Result >= $80 do
      Result := InPortB($1F7);
  end;

begin
  WaitUntilIdle;
  OutPortB($1F6, $A0);

  if (WaitUntilIdle and $50) <> $50 then
    Exit;

  OutPortB($1F6, $A0);
  OutPortB($1F7, $EC);

  if (WaitUntilIdle and $58) <> $58 then
    Exit;

  SetLength(Result, 512);
  for I := 0 to 511 do
    Result[I + 1] := Chr(InPortB($1F0));
end;

initialization
  GetNtNativeAPIs;
  AdjustDebugPrivilege(True);

finalization
  AdjustDebugPrivilege(False);
  FreeNtNativeAPIs;

end.
