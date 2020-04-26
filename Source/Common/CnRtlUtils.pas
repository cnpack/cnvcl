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
  TCnStackInfo = class
  private
    FCallerAddr: Pointer;
  public
    property CallerAddr: Pointer read FCallerAddr;
  end;

  TCnStackInfoList = class(TObjectList)
  private
    FStackBase: Pointer;
    FStackTop: Pointer;
    function GetItems(Index: Integer): TCnStackInfo;
    procedure TraceStackFrames;
  public
    constructor Create(AStackBase: Pointer);
    destructor Destroy; override;

    property Items[Index: Integer]: TCnStackInfo read GetItems; default;
  end;

  TCnModuleInfo = class(TObject)
  private
    FSize: Cardinal;
    FStartAddr: Pointer;
    FEndAddr: Pointer;
    FBaseName: string;
    FFullName: string;
  public
    function ToString: string; {$IFDEF OBJECT_HAS_TOSTRING} override; {$ENDIF}

    property BaseName: string read FBaseName write FBaseName;
    property FullName: string read FFullName write FFullName;
    property Size: Cardinal read FSize write FSize;
    property StartAddr: Pointer read FStartAddr write FStartAddr;
    property EndAddr: Pointer read FEndAddr write FEndAddr;
  end;

  TCnModuleInfoList = class(TObjectList)
  private
    function GetItems(Index: Integer): TCnModuleInfo;
    function GetModuleFromAddress(Addr: Pointer): TCnModuleInfo;
    function AddModule(P: THandle; MH: HMODULE): TCnModuleInfo;
  protected
    procedure BuildModulesList;
    function CreateItemForAddress(Addr: Pointer; SystemModule: Boolean): TCnModuleInfo;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure DumpToStrings(List: TStrings);

    function IsValidModuleAddress(Addr: Pointer): Boolean;
    property Items[Index: Integer]: TCnModuleInfo read GetItems;
  end;

implementation

const
{$IFDEF WIN64}
  HEX_FMT = '$%16.16x';
{$ELSE}
  HEX_FMT = '$%8.8x';
{$ENDIF}

  MODULE_INFO_FMT = 'Base: ' + HEX_FMT + ' End: ' + HEX_FMT + ' Size: ' + HEX_FMT
    + ' Name: %s - %s';

type
{$IFDEF WIN64}
  TCnNativeUInt = NativeUInt;
{$ELSE}
  TCnNativeUInt = Cardinal;
{$ENDIF}

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

// 通过遍历 System 中的系统模块链判断指定模块 Handle 是否是系统模块
function IsSystemModule(const Module: HMODULE): Boolean;
var
  CurModule: PLibModule;
begin
  Result := False;
  if Module <> 0 then
  begin
    CurModule := LibModuleList;
    while CurModule <> nil do
    begin
      if CurModule.Instance = Module then
      begin
        Result := True;
        Break;
      end;
      CurModule := CurModule.Next;
    end;
  end;
end;

{ TCnStackInfoList }

constructor TCnStackInfoList.Create(AStackBase: Pointer);
begin

end;

destructor TCnStackInfoList.Destroy;
begin
  inherited;

end;

function TCnStackInfoList.GetItems(Index: Integer): TCnStackInfo;
begin
  Result := TCnStackInfo(inherited Items[Index]);
end;

procedure TCnStackInfoList.TraceStackFrames;
begin

end;

{ TCnModuleInfoList }

function TCnModuleInfoList.AddModule(P: THandle; MH: HMODULE): TCnModuleInfo;
var
  ModuleInfo: TModuleInfo;
  Info: TCnModuleInfo;
  Res: DWORD;
  AName: array[0..MAX_PATH - 1] of Char;
begin
  Result := nil;

  // 根据每个 Module Handle 拿 Module 基地址等信息
  if GetModuleInformation(P, MH, @ModuleInfo, SizeOf(TModuleInfo)) then
  begin
    Info := TCnModuleInfo.Create;
    Info.StartAddr := ModuleInfo.lpBaseOfDll;
    Info.Size := ModuleInfo.SizeOfImage;
    Info.EndAddr := Pointer(TCnNativeUInt(ModuleInfo.lpBaseOfDll) + ModuleInfo.SizeOfImage);

    Res := GetModuleBaseName(P, MH, @AName[0], SizeOf(AName));
    if Res > 0 then
    begin
      SetLength(Info.FBaseName, Res);
      System.Move(AName[0], Info.FBaseName[1], Res);
    end;
    Res := GetModuleFileName(MH, @AName[0], SizeOf(AName));
    if Res > 0 then
    begin
      SetLength(Info.FFullName, Res);
      System.Move(AName[0], Info.FFullName[1], Res);
    end;
    Add(Info);
    Result := Info;
  end;
end;

procedure TCnModuleInfoList.BuildModulesList;
var
  CurModule: PLibModule;
  ProcessHandle: THandle;
  Needed: DWORD;
  Modules: array of THandle;
  I, Cnt: Integer;
  Res: Boolean;
  MemInfo: TMemoryBasicInformation;
  Base: PByte;
  LastAllocBase: Pointer;
  QueryRes: DWORD;
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
            AddModule(ProcessHandle, Modules[I]);
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
              AddModule(ProcessHandle, HMODULE(MemInfo.AllocationBase));
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

constructor TCnModuleInfoList.Create;
begin
  inherited Create(True);
  BuildModulesList;
end;

function TCnModuleInfoList.CreateItemForAddress(Addr: Pointer;
  SystemModule: Boolean): TCnModuleInfo;
var
  Module: HMODULE;
begin
  Result := nil;
  Module := ModuleFromAddr(Addr);
  if Module > 0 then
    Result := AddModule(GetCurrentProcessId, Module);
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

function TCnModuleInfoList.IsValidModuleAddress(Addr: Pointer): Boolean;
begin
  Result := GetModuleFromAddress(Addr) <> nil;
end;

{ TCnModuleInfo }

function TCnModuleInfo.ToString: string;
begin
  Result := Format(MODULE_INFO_FMT, [TCnNativeUInt(FStartAddr),
    TCnNativeUInt(FEndAddr), FSize, FBaseName, FFullName]);
end;

end.
