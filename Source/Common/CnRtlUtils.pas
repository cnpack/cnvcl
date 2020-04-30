{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
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

unit CnRtlUtils;
{* |<PRE>
================================================================================
* 软件名称：CnDebugger
* 单元名称：CnDebug 相关的运行期工具单元
* 单元作者：刘啸（liuxiao@cnpack.org）
* 备    注：该单元实现了部分 CnDebugger 所需的 Module/Stack 相关内容
*           部分内容引用了 JCL
* 开发平台：PWin7 + Delphi 5
* 兼容测试：Win32/Win64
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2020.04.26
*               创建单元,实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Contnrs, TLHelp32, Psapi;

type
  PCnStackFrame = ^TCnStackFrame;
  TCnStackFrame = record
    CallersEBP: Pointer;
    CallerAdr: Pointer;
  end;

  TCnModuleInfo = class(TObject)
  {* 描述一个模块信息，exe 或 dll 或 bpl 等，支持 32/64 位}
  private
    FSize: Cardinal;
    FStartAddr: Pointer;
    FEndAddr: Pointer;
    FBaseName: string;
    FFullName: string;
    FIsDelphi: Boolean;
    FHModule: HMODULE;
  public
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property BaseName: string read FBaseName write FBaseName;
    {* 模块文件名}
    property FullName: string read FFullName write FFullName;
    {* 模块完整路径名}
    property Size: Cardinal read FSize write FSize;
    {* 模块大小}
    property HModule: HMODULE read FHModule write FHModule;
    {* 模块 Handle，也就是 AllocationBase，一般等于 StartAddr}
    property StartAddr: Pointer read FStartAddr write FStartAddr;
    {* 模块在本进程地址空间的起始地址，也即 lpBaseOfDll}
    property EndAddr: Pointer read FEndAddr write FEndAddr;
    {* 模块在本进程地址空间的结束地址}
    property IsDelphi: Boolean read FIsDelphi write FIsDelphi;
    {* 是否是 Delphi 模块，指通过 System.pas 中的 LibModuleList 注册过的}
  end;

  TCnModuleInfoList = class(TObjectList)
  {* 描述本进程内的所有模块信息，exe 或 dll 或 bpl 等，支持 32/64 位}
  private
    FDelphiOnly: Boolean;
    function GetItems(Index: Integer): TCnModuleInfo;
    function GetModuleFromAddress(Addr: Pointer): TCnModuleInfo;
    function AddModule(PH: THandle; MH: HMODULE): TCnModuleInfo;
    procedure CheckDelphiModule(Info: TCnModuleInfo);
  protected
    procedure BuildModulesList;
    function CreateItemForAddress(Addr: Pointer; AIsDelphi: Boolean): TCnModuleInfo;
  public
    constructor Create(ADelphiOnly: Boolean = False); virtual;
    destructor Destroy; override;

    procedure DumpToStrings(List: TStrings);

    function IsValidModuleAddress(Addr: Pointer): Boolean;
    {* 判断某地址是否属于本进程内一个模块内的地址}
    function IsDelphiModuleAddress(Addr: Pointer): Boolean;
    {* 判断某地址是否属于本进程内一个 Delphi 模块内的地址}
    property Items[Index: Integer]: TCnModuleInfo read GetItems;
  end;

  TCnStackInfo = class
  private
    FCallerAddr: Pointer;
  public
    property CallerAddr: Pointer read FCallerAddr write FCallerAddr;
  end;

  TCnStackInfoList = class(TObjectList)
  private
    FModuleList: TCnModuleInfoList;
    FStackBase: Pointer;
    FStackTop: Pointer;
    function GetItems(Index: Integer): TCnStackInfo;
    procedure TraceStackFrames;
    function IsValidStackAddr(StackAddr: Pointer): Boolean;
{$IFDEF WIN64}
    function ValidCallSite64(CodeAddr: Pointer; var CallInstructionSize: Cardinal): Boolean;
{$ELSE}
    function ValidCallSite32(CodeAddr: DWORD; var CallInstructionSize: Cardinal): Boolean;
{$ENDIF}
    function NextStackFrame(var StackFrame: PCnStackFrame; var ACallersEBP: Pointer;
      out CallerAddr: Pointer): Boolean;
  public
    constructor Create(AStackBase: Pointer; OnlyDelphi: Boolean = False);
    destructor Destroy; override;

    procedure DumpToStrings(List: TStrings);

    property Items[Index: Integer]: TCnStackInfo read GetItems; default;
  end;

implementation

const
{$IFDEF WIN64}
  HEX_FMT = '$%16.16x';
{$ELSE}
  HEX_FMT = '$%8.8x';
{$ENDIF}

  MODULE_INFO_FMT = 'HModule: ' + HEX_FMT + ' Base: ' + HEX_FMT + ' End: ' +
    HEX_FMT + ' Size: ' + HEX_FMT + ' IsDelphiModule %d. Name: %s - %s';
  STACK_INFO_FMT = 'Caller: ' + HEX_FMT;

  MAX_STACK_COUNT = 2048;

type
{$IFDEF WIN64}
  TCnNativeUInt = NativeUInt;
{$ELSE}
  TCnNativeUInt = Cardinal;
{$ENDIF}

  NT_TIB32 = packed record
    ExceptionList: DWORD;
    StackBase: DWORD;           // 以下暂不需要了
  end;

  NT_TIB64 = packed record
    ExceptionList: TCnNativeUInt;
    StackBase: TCnNativeUInt;   // 以下暂不需要了
  end;

// 查询某虚拟地址所属的分配模块 Handle，也就是 AllocationBase
function ModuleFromAddr(const Addr: Pointer): HMODULE;
var
  MI: TMemoryBasicInformation;
begin
  VirtualQuery(Addr, MI, SizeOf(MI));
  if MI.State <> MEM_COMMIT then
    Result := 0
  else
    Result := HMODULE(MI.AllocationBase);
end;

{$IFDEF WIN64}

function GetEBP64: Pointer;
asm
        MOV     RAX, RBP
end;

function GetESP64: Pointer;
asm
        MOV     RAX, RSP
end;

function GetStackTop64: TCnNativeUInt;
asm
        MOV     RAX, FS:[0].NT_TIB64.StackBase
end;

{$ELSE}

function GetEBP32: Pointer;
asm
        MOV     EAX, EBP
end;

function GetESP32: Pointer;
asm
        MOV     EAX, ESP
end;

function GetStackTop32: TCnNativeUInt;
asm
        MOV     EAX, FS:[0].NT_TIB32.StackBase
end;

{$ENDIF}

{ TCnStackInfoList }

constructor TCnStackInfoList.Create(AStackBase: Pointer; OnlyDelphi: Boolean);
begin
  inherited Create(True);
  FStackBase := AStackBase;
{$IFDEF WIN64}
  FStackTop := Pointer(GetStackTop64);
{$ELSE}
  FStackTop := Pointer(GetStackTop32);
{$ENDIF}
  FModuleList := TCnModuleInfoList.Create(OnlyDelphi);

  TraceStackFrames;
end;

destructor TCnStackInfoList.Destroy;
begin
  FModuleList.Free;
  inherited;
end;

function TCnStackInfoList.GetItems(Index: Integer): TCnStackInfo;
begin
  Result := TCnStackInfo(inherited Items[Index]);
end;

{$IFDEF WIN64}

function TCnStackInfoList.ValidCallSite64(CodeAddr: Pointer; var CallInstructionSize: Cardinal): Boolean;
begin
  Result := False;
end;

{$ELSE}

// Copied from JCL
function TCnStackInfoList.ValidCallSite32(CodeAddr: DWORD; var CallInstructionSize: Cardinal): Boolean;
var
  CodeDWORD4: DWORD;
  CodeDWORD8: DWORD;
  C4P, C8P: PDWORD;
  RM1, RM2, RM5: Byte;
begin
  // First check that the address is within range of our code segment!
  C8P := PDWORD(CodeAddr - 8);
  C4P := PDWORD(CodeAddr - 4);
  Result := (CodeAddr > 8) and FModuleList.IsValidModuleAddress(Pointer(C8P)) and not IsBadReadPtr(C8P, 8);

  // Now check to see if the instruction preceding the return address
  // could be a valid CALL instruction
  if Result then
  begin
    try
      CodeDWORD8 := PDWORD(C8P)^;
      CodeDWORD4 := PDWORD(C4P)^;
      // CodeDWORD8 = (ReturnAddr-5):(ReturnAddr-6):(ReturnAddr-7):(ReturnAddr-8)
      // CodeDWORD4 = (ReturnAddr-1):(ReturnAddr-2):(ReturnAddr-3):(ReturnAddr-4)

      // ModR/M bytes contain the following bits:
      // Mod        = (76)
      // Reg/Opcode = (543)
      // R/M        = (210)
      RM1 := (CodeDWORD4 shr 24) and $7;
      RM2 := (CodeDWORD4 shr 16) and $7;
      //RM3 := (CodeDWORD4 shr 8)  and $7;
      //RM4 :=  CodeDWORD4         and $7;
      RM5 := (CodeDWORD8 shr 24) and $7;
      //RM6 := (CodeDWORD8 shr 16) and $7;
      //RM7 := (CodeDWORD8 shr 8)  and $7;

      // Check the instruction prior to the potential call site.
      // We consider it a valid call site if we find a CALL instruction there
      // Check the most common CALL variants first
      if ((CodeDWORD8 and $FF000000) = $E8000000) then
        // 5 bytes, "CALL NEAR REL32" (E8 cd)
        CallInstructionSize := 5
      else
      if ((CodeDWORD4 and $F8FF0000) = $10FF0000) and not (RM1 in [4, 5]) then
        // 2 bytes, "CALL NEAR [EAX]" (FF /2) where Reg = 010, Mod = 00, R/M <> 100 (1 extra byte)
        // and R/M <> 101 (4 extra bytes)
        CallInstructionSize := 2
      else
      if ((CodeDWORD4 and $F8FF0000) = $D0FF0000) then
        // 2 bytes, "CALL NEAR EAX" (FF /2) where Reg = 010 and Mod = 11
        CallInstructionSize := 2
      else
      if ((CodeDWORD4 and $00FFFF00) = $0014FF00) then
        // 3 bytes, "CALL NEAR [EAX+EAX*i]" (FF /2) where Reg = 010, Mod = 00 and RM = 100
        // SIB byte not validated
        CallInstructionSize := 3
      else
      if ((CodeDWORD4 and $00F8FF00) = $0050FF00) and (RM2 <> 4) then
        // 3 bytes, "CALL NEAR [EAX+$12]" (FF /2) where Reg = 010, Mod = 01 and RM <> 100 (1 extra byte)
        CallInstructionSize := 3
      else
      if ((CodeDWORD4 and $0000FFFF) = $000054FF) then
        // 4 bytes, "CALL NEAR [EAX+EAX+$12]" (FF /2) where Reg = 010, Mod = 01 and RM = 100
        // SIB byte not validated
        CallInstructionSize := 4
      else
      if ((CodeDWORD8 and $FFFF0000) = $15FF0000) then
        // 6 bytes, "CALL NEAR [$12345678]" (FF /2) where Reg = 010, Mod = 00 and RM = 101
        CallInstructionSize := 6
      else
      if ((CodeDWORD8 and $F8FF0000) = $90FF0000) and (RM5 <> 4) then
        // 6 bytes, "CALL NEAR [EAX+$12345678]" (FF /2) where Reg = 010, Mod = 10 and RM <> 100 (1 extra byte)
        CallInstructionSize := 6
      else
      if ((CodeDWORD8 and $00FFFF00) = $0094FF00) then
        // 7 bytes, "CALL NEAR [EAX+EAX+$1234567]" (FF /2) where Reg = 010, Mod = 10 and RM = 100
        CallInstructionSize := 7
      else
      if ((CodeDWORD8 and $0000FF00) = $00009A00) then
        // 7 bytes, "CALL FAR $1234:12345678" (9A ptr16:32)
        CallInstructionSize := 7
      else
        Result := False;
      // Because we're not doing a complete disassembly, we will potentially report
      // false positives. If there is odd code that uses the CALL 16:32 format, we
      // can also get false negatives.
    except
      Result := False;
    end;
  end;
end;

{$ENDIF}

procedure TCnStackInfoList.TraceStackFrames;
var
  Info: TCnStackInfo;
  PSF: PCnStackFrame;
  ACallerAddr, ACallersEBP: Pointer;
begin
  Capacity := 32;
  if FStackBase = nil then
  begin
{$IFDEF WIN64}
    FStackBase := GetEBP64;
{$ELSE}
    FStackBase := GetEBP32;
{$ENDIF}
  end;
  PSF := PCnStackFrame(FStackBase);

  FStackBase := Pointer(TCnNativeUInt(PSF) - SizeOf(TCnNativeUInt));
  ACallersEBP := nil;
  ACallerAddr := nil;
  while NextStackFrame(PSF, ACallersEBP, ACallerAddr) and (Count < MAX_STACK_COUNT) do
  begin
    Info := TCnStackInfo.Create;
    Info.CallerAddr := ACallerAddr;
    Add(Info);
  end;
end;

function TCnStackInfoList.NextStackFrame(var StackFrame: PCnStackFrame; var ACallersEBP: Pointer; out CallerAddr: Pointer): Boolean;
var
  CallInstructionSize: Cardinal;
  StackFrameCallersEBP, NewEBP: Pointer;
  StackFrameCallerAdr: Pointer;
begin
  StackFrameCallersEBP := ACallersEBP;
  while IsValidStackAddr(StackFrame) do
  begin
    NewEBP := StackFrame^.CallersEBP;
    if TCnNativeUInt(NewEBP) <= TCnNativeUInt(StackFrameCallersEBP) then
      Break;
    StackFrameCallersEBP := NewEBP;

    StackFrameCallerAdr := StackFrame^.CallerAdr;
    if FModuleList.IsValidModuleAddress(Pointer(StackFrameCallerAdr))
      and IsValidStackAddr(StackFrameCallersEBP) then
    begin
      if TCnNativeUInt(StackFrameCallersEBP) > TCnNativeUInt(ACallersEBP) then
        ACallersEBP := StackFrameCallersEBP
      else
        Break;

{$IFDEF WIN64}
      if ValidCallSite64(StackFrameCallerAdr, CallInstructionSize) then
        CallerAddr := Pointer(TCnNativeUInt(StackFrameCallerAdr) - CallInstructionSize)
      else
        CallerAddr := Pointer(StackFrameCallerAdr);
{$ELSE}
      if ValidCallSite32(DWORD(StackFrameCallerAdr), CallInstructionSize) then
        CallerAddr := Pointer(TCnNativeUInt(StackFrameCallerAdr) - CallInstructionSize)
      else
        CallerAddr := Pointer(StackFrameCallerAdr);
{$ENDIF}
      if PCnStackFrame(StackFrame^.CallersEBP) = StackFrame then
        Break;

      StackFrame := PCnStackFrame(StackFrameCallersEBP);
      Result := True;
      Exit;
    end;

    StackFrame := PCnStackFrame(StackFrameCallersEBP);
  end;

  Result := False;
end;

function TCnStackInfoList.IsValidStackAddr(StackAddr: Pointer): Boolean;
begin
  Result := (TCnNativeUInt(FStackBase) < TCnNativeUInt(StackAddr)) and
    (TCnNativeUInt(StackAddr) < TCnNativeUInt(FStackTop));
end;

procedure TCnStackInfoList.DumpToStrings(List: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    List.Add(Format(STACK_INFO_FMT, [TCnNativeUInt(Items[I].CallerAddr)]));
end;

{ TCnModuleInfoList }

function TCnModuleInfoList.AddModule(PH: THandle; MH: HMODULE): TCnModuleInfo;
var
  ModuleInfo: TModuleInfo;
  Info: TCnModuleInfo;
  Res: DWORD;
  AName: array[0..MAX_PATH - 1] of Char;
begin
  Result := nil;

  // 根据每个 Module Handle 拿 Module 基地址等信息
  if GetModuleInformation(PH, MH, @ModuleInfo, SizeOf(TModuleInfo)) then
  begin
    Info := TCnModuleInfo.Create;
    Info.HModule := MH;
    Info.StartAddr := ModuleInfo.lpBaseOfDll;
    Info.Size := ModuleInfo.SizeOfImage;
    Info.EndAddr := Pointer(TCnNativeUInt(ModuleInfo.lpBaseOfDll) + ModuleInfo.SizeOfImage);

    Res := GetModuleBaseName(PH, MH, @AName[0], SizeOf(AName));
    if Res > 0 then
    begin
      SetLength(Info.FBaseName, Res);
      System.Move(AName[0], Info.FBaseName[1], Res * SizeOf(Char));
    end;
    Res := GetModuleFileName(MH, @AName[0], SizeOf(AName));
    if Res > 0 then
    begin
      SetLength(Info.FFullName, Res);
      System.Move(AName[0], Info.FFullName[1], Res * SizeOf(Char));
    end;
    Add(Info);
    Result := Info;
  end;
end;

procedure TCnModuleInfoList.BuildModulesList;
var
  ProcessHandle: THandle;
  Needed: DWORD;
  Modules: array of THandle;
  I, Cnt: Integer;
  Res: Boolean;
  MemInfo: TMemoryBasicInformation;
  Base: PByte;
  LastAllocBase: Pointer;
  QueryRes: DWORD;
  CurModule: PLibModule;
begin
  if FDelphiOnly then
  begin
    CurModule := LibModuleList;
    while CurModule <> nil do
    begin
      CreateItemForAddress(Pointer(CurModule.Instance), True);
      CurModule := CurModule.Next;
    end;
  end
  else
  begin
    ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, GetCurrentProcessId);
    if ProcessHandle <> 0 then
    begin
      try
        Res := EnumProcessModules(ProcessHandle, nil, 0, Needed);
        if Res then
        begin
          Cnt := Needed div SizeOf(HMODULE);
          SetLength(Modules, Cnt);
          if EnumProcessModules(ProcessHandle, @Modules[0], Needed, Needed) then
          begin
            for I := 0 to Cnt - 1 do
              CheckDelphiModule(AddModule(ProcessHandle, Modules[I]));
          end;
        end
        else
        begin
          Base := nil;
          LastAllocBase := nil;
          FillChar(MemInfo, SizeOf(TMemoryBasicInformation), #0);

          QueryRes := VirtualQueryEx(ProcessHandle, Base, MemInfo, SizeOf(TMemoryBasicInformation));
          while QueryRes = SizeOf(TMemoryBasicInformation) do
          begin
            if MemInfo.AllocationBase <> LastAllocBase then
            begin
              if MemInfo.Type_9 = MEM_IMAGE then
                CheckDelphiModule(AddModule(ProcessHandle, HMODULE(MemInfo.AllocationBase)));
              LastAllocBase := MemInfo.AllocationBase;
            end;
            Inc(Base, MemInfo.RegionSize);
            QueryRes := VirtualQueryEx(ProcessHandle, Base, MemInfo, SizeOf(TMemoryBasicInformation));
          end;
        end;
      finally
        CloseHandle(ProcessHandle);
      end;
    end;
  end;
end;

// 在 System 的系统模块链里查询 Delphi 模块
procedure TCnModuleInfoList.CheckDelphiModule(Info: TCnModuleInfo);
var
  CurModule: PLibModule;
begin
  if (Info <> nil) and (Info.HModule <> 0) then
  begin
    CurModule := LibModuleList;
    while CurModule <> nil do
    begin
      if CurModule.Instance = Info.HModule then
      begin
        Info.IsDelphi := True;
        Exit;
      end;
      CurModule := CurModule.Next;
    end;
  end;
end;

constructor TCnModuleInfoList.Create;
begin
  inherited Create(True);
  FDelphiOnly := ADelphiOnly;
  BuildModulesList;
end;

function TCnModuleInfoList.CreateItemForAddress(Addr: Pointer;
  AIsDelphi: Boolean): TCnModuleInfo;
var
  Module: HMODULE;
  ProcessHandle: THandle;
begin
  Result := nil;
  Module := ModuleFromAddr(Addr);
  if Module > 0 then
  begin
    ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, GetCurrentProcessId);
    if ProcessHandle <> 0 then
    begin
      try
        Result := AddModule(ProcessHandle, Module);
        if Result <> nil then
          Result.IsDelphi := AIsDelphi;
      finally
        CloseHandle(ProcessHandle);
      end;
    end;
  end;
end;

destructor TCnModuleInfoList.Destroy;
begin

  inherited;
end;

procedure TCnModuleInfoList.DumpToStrings(List: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    List.Add(Items[I].ToString);
end;

function TCnModuleInfoList.GetItems(Index: Integer): TCnModuleInfo;
begin
  Result := TCnModuleInfo(inherited Items[Index]);
end;

function TCnModuleInfoList.GetModuleFromAddress(Addr: Pointer): TCnModuleInfo;
var
  I: Integer;
  Item: TCnModuleInfo;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Item := Items[I];
    if (TCnNativeUInt(Item.StartAddr) <= TCnNativeUInt(Addr)) and
      (TCnNativeUInt(Item.EndAddr) > TCnNativeUInt(Addr)) then
    begin
      Result := Item;
      Exit;
    end;
  end;
end;

function TCnModuleInfoList.IsDelphiModuleAddress(Addr: Pointer): Boolean;
var
  Info: TCnModuleInfo;
begin
  Info := GetModuleFromAddress(Addr);
  Result := (Info <> nil) and Info.IsDelphi;
end;

function TCnModuleInfoList.IsValidModuleAddress(Addr: Pointer): Boolean;
begin
  Result := GetModuleFromAddress(Addr) <> nil;
end;

{ TCnModuleInfo }

function TCnModuleInfo.ToString: string;
begin
  Result := Format(MODULE_INFO_FMT, [FHModule, TCnNativeUInt(FStartAddr),
    TCnNativeUInt(FEndAddr), FSize, Integer(FIsDelphi), FBaseName, FFullName]);
end;

end.
