{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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

unit CnInProcessAPIHook;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：进程内实现单个 API Hook 的单元
* 单元作者：CodeGame
* 备    注：D2009 或以上不支持，Win64 不支持。
* 开发平台：PWinXP + Delphi 2007
* 兼容测试：暂无
* 修改记录：2013.08.08 v1.0
*               移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF WIN32}

uses
  Windows, SysUtils, Classes;

type
  TCnHookType = (HT_None, HT_LONG_JMP, HT_LONG_CALL, HT_SHORT_JMP, HT_SHORT_CALL);

  TCnHookTableLong = packed record
    Long: Word;
    LongAddr: Cardinal;
  end;
  PCnHookTableLong = ^TCnHookTableLong;

  TCnHookTableShort = packed record
    Short: Byte;
    ShortAddr: Cardinal;
  end;
  PCnHookTableShort = ^TCnHookTableShort;

  TOnAPIHookProc = function(const Params: array of Pointer): DWORD of object;

  { TCnHookCore }

  TCnHookCore = class(TObject)
  private
    FAddr: Pointer;
    FEvent: Pointer;
    FStyle: TCnHookType;
    FAddrSize: Byte;
    FHooked: Boolean;
    procedure SetStyle(const Value: TCnHookType);
  protected
    FDWSize: Cardinal;
    FCode: Word;
    FOldProtect: Cardinal;
    FBakCode: array[0..255] of Char;
  public
    constructor Create;
    procedure Hook;
    procedure UnHook;
    property Addr: Pointer read FAddr write FAddr;
    property AddrSize: Byte read FAddrSize write FAddrSize;
    property Event: Pointer read FEvent write FEvent;
    property Style: TCnHookType read FStyle write SetStyle;
    property Hooked: Boolean read FHooked;
  end;

  PDynamicCode = ^DynamicCode;

  DynamicCode = packed record
    Mov: Word;
    EspConst: DWORD;
    Push: Byte;
    Self: DWORD;
    Call: Word;
    CallAddr: Pointer;
    RetCode: Byte;
    RetXX: WORD;
    EventAddr: Pointer;
    ExtraData: Pointer;
    ParamEsp: DWORD;
  end;

  TOnHookProc = function(Data: PDynamicCode): DWORD of object;

  { TCnHookAddress }

  TCnHookAddress = class(TObject)
  private
    FHooker: TCnHookCore;
    FInit: Boolean;
    FHook: Boolean;
    FRetCount: Byte;
    FMutex: Boolean;
    FOnHookProc: TOnHookProc;
    FExtraData: Pointer;
    procedure SetInit(const Value: Boolean);
    procedure SetHook(const Value: Boolean);
    function GetInstructionAddr: Pointer;
    procedure SetInstructionAddr(const Value: Pointer);
    function GetInstructionSize: Byte;
    procedure SetInstructionSize(const Value: Byte);
    procedure InitHook;
    procedure UnInitHook;
  protected
    FHookMark: array[0..7] of Char;
  public
    constructor Create;
    destructor Destroy; override;
    property Init: Boolean read FInit write SetInit;
    property Hook: Boolean read FHook write SetHook;
    property RetCount: Byte read FRetCount write FRetCount;
    property InstructionAddr: Pointer read GetInstructionAddr write SetInstructionAddr;
    property InstructionSize: Byte read GetInstructionSize write SetInstructionSize;
    property Mutex: Boolean read FMutex write FMutex;
    property ExtraData: Pointer read FExtraData write FExtraData;
    property OnHookProc: TOnHookProc read FOnHookProc write FOnHookProc;
  end;

 { TCnInProcessAPIHook }

  TCnInProcessAPIHook = class(TComponent)
  private
    FHooker: TCnHookAddress;
    FDllFunction: string;
    FDllName: string;
    FActive: Boolean;
    FRestoreWhenOnHook: Boolean;
    FOnAPIHookProc: TOnAPIHookProc;
    function GetMutex: Boolean;
    procedure SetMutex(const Value: Boolean);
    function GetParamCount: Byte;
    procedure SetParamCount(const Value: Byte);
    procedure SetActive(const Value: Boolean);
  protected
    FHookMark: string;
    function OnHookProc(Data: PDynamicCode): DWORD;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DllFunction: string read FDllFunction write FDllFunction;
    property DllName: string read FDllName write FDllName;
    property ParamCount: Byte read GetParamCount write SetParamCount;
    property Mutex: Boolean read GetMutex write SetMutex;
    property RestoreWhenOnHook: Boolean read FRestoreWhenOnHook write FRestoreWhenOnHook;
    property Active: Boolean read FActive write SetActive;
    property OnAPIHookProc: TOnAPIHookProc read FOnAPIHookProc write FOnAPIHookProc;
  end;

{$ENDIF}

implementation

{$IFDEF WIN32}

{ TCnHookCore }

constructor TCnHookCore.Create;
begin
  SetStyle(HT_LONG_JMP);
  FAddrSize := SizeOf(TCnHookTableLong);
end;

procedure TCnHookCore.SetStyle(const Value: TCnHookType);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    case FStyle of
      HT_LONG_JMP:
        begin
          FCode := $25FF;
          FDWSize := SizeOf(TCnHookTableLong);
        end;
      HT_LONG_CALL:
        begin
          FCode := $15FF;
          FDWSize := SizeOf(TCnHookTableLong);
        end;
      HT_SHORT_JMP:
        begin
          FCode := $E9;
          FDWSize := SizeOf(TCnHookTableShort);
        end;
      HT_SHORT_CALL:
        begin
          FCode := $E8;
          FDWSize := SizeOf(TCnHookTableShort);
        end;
    end;
    if FAddrSize < FDWSize then
      FAddrSize := FDWSize;
  end;
end;

procedure TCnHookCore.Hook;
var
  Code1: PCnHookTableLong;
  Code2: PCnHookTableShort;
  Addr: Cardinal;
begin
  if not FHooked then
  begin
    if VirtualProtect(FAddr, FAddrSize, PAGE_EXECUTE_WRITECOPY, FOldProtect) then
    begin
      FillMemory(@FBakCode[0], 256, $90);

        //备份代码
      CopyMemory(@FBakCode[0], FAddr, FAddrSize);
      case FStyle of
        HT_LONG_JMP, HT_LONG_CALL:
          begin
            Code1 := FAddr;
            Code1^.Long := FCode;
            Code1^.LongAddr := Cardinal(@FEvent);
          end;
        HT_SHORT_JMP, HT_SHORT_CALL:
          begin
              //计算相对地址
            Addr := Cardinal(FEvent) - Cardinal(FAddr) - FDWSize;
            Code2 := FAddr;
            Code2^.Short := FCode;
            Code2^.ShortAddr := Addr;
          end;
      end;
      FHooked := True;
    end;
  end;
end;

procedure TCnHookCore.UnHook;
begin
  if FHooked then
  begin
    CopyMemory(FAddr, @FBakCode[0], FAddrSize);
    VirtualProtect(FAddr, FAddrSize, FOldProtect, FOldProtect);
    FHooked := False;
  end;
end;

{ TCnHookAddress }

function DoOnHookProc(Self: TCnHookAddress): DWORD; stdcall;
var
  H: THandle;
begin
  Result := 0;
  if Assigned(Self.OnHookProc) then
  begin
    if Self.FMutex then
    begin
      H := OpenMutex(MUTEX_ALL_ACCESS, True, Self.FHookMark);

        //如果已经有就等待
      if H <> 0 then
        WaitForSingleObject(H, INFINITE)
      else
        H := CreateMutex(nil, True, Self.FHookMark);  //否则创建一个

        //执行事件
      Result := Self.OnHookProc(Self.FHooker.Event);

        //销毁事件
      ReleaseMutex(H);
      CloseHandle(H);
    end
    else
      //执行事件
      Result := Self.OnHookProc(Self.FHooker.Event);
  end;
end;

constructor TCnHookAddress.Create;
begin
  FHooker := TCnHookCore.Create;
end;

destructor TCnHookAddress.Destroy;
begin
  UnInitHook;
  FHooker.UnHook;
  FHooker.Free;
  inherited;
end;

function TCnHookAddress.GetInstructionAddr: Pointer;
begin
  Result := FHooker.Addr;
end;

procedure TCnHookAddress.SetInstructionAddr(const Value: Pointer);
begin
  FHooker.Addr := Value;
end;

function TCnHookAddress.GetInstructionSize: Byte;
begin
  Result := FHooker.AddrSize;
end;

procedure TCnHookAddress.SetInstructionSize(const Value: Byte);
begin
  FHooker.AddrSize := Value;
end;

procedure TCnHookAddress.SetInit(const Value: Boolean);
begin
  if FInit = Value then
    Exit;  //跟上次一样则什么都不做

  if Value then
  begin  //初始化
    if FInit = False then
    begin
      InitHook;
      FInit := True;
    end;
  end
  else  //反初始化
  begin
    if FInit then
    begin
      UnInitHook;
      FInit := False;
    end;
  end;
end;

procedure TCnHookAddress.SetHook(const Value: Boolean);
begin
  if FHook = Value then
    Exit;  //跟上次一样则什么都不做

  if Value then
  begin
    if not FHook then
    begin
      FHooker.Hook;
      FHook := True;
    end;
  end
  else
  begin
    if FHook then
    begin
      FHooker.UnHook;
      FHook := False;
    end;
  end;
end;

procedure TCnHookAddress.InitHook;
type
  PStr = ^Str;
  Str = array[0..3] of AnsiChar;
var
  FDynamicCode: PDynamicCode;
  Mark: AnsiString;
  Value1, Value2: DWORD;
begin
  //制定类型
  FHooker.Style := HT_SHORT_JMP;

  //分配内存
  FDynamicCode := VirtualAlloc(nil, SizeOf(DynamicCode), MEM_COMMIT, PAGE_EXECUTE_READWRITE);

  //保存地址
  FDynamicCode^.EventAddr := @DoOnHookProc;

  //写入相应语句
  FDynamicCode.mov := $2589; // Mov
  FDynamicCode.EspConst := DWORD(@FDynamicCode.ParamEsp);
  FDynamicCode^.Push := $68;  //PUSH
  FDynamicCode^.Self := DWORD(Self);  //写上 Self
  FDynamicCode^.Call := $15FF;  //CALL
  FDynamicCode^.CallAddr := @FDynamicCode^.EventAddr;  //事件发生
  FDynamicCode^.RetCode := $C2;  //RET
  FDynamicCode^.RetXX := FRetCount * 4;  //RET XX
  FDynamicCode^.ExtraData := FExtraData;  //额外数据

  //写入事件
  FHooker.Event := FDynamicCode;

  //制作 Mark
  Value1 := GetCurrentProcess;
  Value2 := DWORD(Self.InstructionAddr);
  Mark := PStr(@Value1)^ + PStr(@Value2)^;
  CopyMemory(@FHookMark[0], @Mark[1], 8);
end;

procedure TCnHookAddress.UnInitHook;
var
  FDynamicCode: PDynamicCode;
begin
  //释放 Hook
  Hook := False;
  FDynamicCode := FHooker.Event;
  FHooker.Event := nil;

  //释放内存
  VirtualFree(FDynamicCode, SizeOf(DynamicCode), MEM_DECOMMIT);
end;

{ TCnInProcessAPIHook }

constructor TCnInProcessAPIHook.Create(AOwner: TComponent);
begin
  inherited;
  FRestoreWhenOnHook := True;
  FHooker := TCnHookAddress.Create;
end;

destructor TCnInProcessAPIHook.Destroy;
begin
  FHooker.Free;
  inherited;
end;

function TCnInProcessAPIHook.GetMutex: Boolean;
begin
  Result := FHooker.Mutex;
end;

procedure TCnInProcessAPIHook.SetMutex(const Value: Boolean);
begin
  FHooker.Mutex := Value;
end;

function TCnInProcessAPIHook.GetParamCount: Byte;
begin
  Result := FHooker.RetCount;
end;

procedure TCnInProcessAPIHook.SetParamCount(const Value: Byte);
begin
  FHooker.RetCount := Value;
end;

procedure TCnInProcessAPIHook.SetActive(const Value: Boolean);
type
  PStr = ^Str;
  Str = array[0..3] of Char;
var
  Lib, Values: DWORD;
  tmp: string;
begin
  if Value = FActive then
    Exit;

  FActive := Value;
  if not (csDesigning in ComponentState) then
  begin
    if Value then
    begin
      //取得地址
      Lib := LoadLibrary(PChar(FDllName));
      FHooker.InstructionAddr := GetProcAddress(Lib, PChar(FDllFunction));
      FreeLibrary(Lib);

      //固定长度
      FHooker.InstructionSize := 5;
      //事件
      FHooker.OnHookProc := OnHookProc;
      //Self
      FHooker.ExtraData := Self;

      //设置标记
      Values := GetCurrentProcess;
      tmp := PStr(@Values)^;
      FHookMark := CharUpper(PChar(FDllName + FDllFunction + tmp));

      //打开 Hook
      FHooker.Init := True;
      FHooker.Hook := True;
    end
    else
    begin
      //关闭 Hook
      FHooker.Hook := False;
      FHooker.Init := False;
    end;
  end;
end;

function TCnInProcessAPIHook.OnHookProc(Data: PDynamicCode): DWORD;
var
  OBJ: TCnInProcessAPIHook;
  Params: array of Pointer;
  Param: Pointer;
  _Handle: THandle;
begin
  //取得 Self
  _Handle := 0;
  OBJ := Data^.ExtraData;
  if Assigned(OBJ.FOnAPIHookProc) then
  begin
    Param := Pointer(Data.ParamEsp + 4);
    SetLength(Params, OBJ.ParamCount);  // 设置参数个数
    CopyMemory(@Params[0], Param, Obj.ParamCount * 4);

    if OBJ.Mutex then
    begin
      _Handle := OpenMutex(MUTEX_ALL_ACCESS, True, PChar(OBJ.FHookMark));
      //如果已经有就等待
      if _Handle <> 0 then
        WaitForSingleObject(_Handle, INFINITE)
      else
        _Handle := CreateMutex(nil, True, PChar(OBJ.FHookMark));  //否则创建一个
    end;

    //执行事件
    if FRestoreWhenOnHook then
      OBJ.FHooker.Hook := False;
    Result := OBJ.FOnAPIHookProc(Params);
    CopyMemory(Param, @Params[0], Obj.ParamCount * 4);  //参数写回
    if FRestoreWhenOnHook then
      OBJ.FHooker.Hook := True;
    if OBJ.Mutex then
    begin
      //销毁事件
      ReleaseMutex(_Handle);
      CloseHandle(_Handle);
    end;
  end
  else
    Result := 0;
end;

{$ENDIF}
end.

