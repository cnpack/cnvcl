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

unit CnHardwareBreakpoint;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：硬件断点类，代码硬件HOOK单元
* 单元作者：CodeGame
* 备    注：提供类表：TCGL_VectoredException, TCGL_HardwareBreakpoints
* 开发平台：PWinXP + Delphi 2007
* 兼容测试：暂无
* 修改记录：2013.08.08 v1.0
*               移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes;

const
  THREAD_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $3FF);

type
  TCallbackInstance = array[1..12] of Byte; //对象成员回调

  PExceptionPointers = ^TExceptionPointers;
  TVEHCallbackProc = function(pException: PExceptionPointers): Integer of object; stdcall;
  THardwareBreakError = procedure(ErrorId: Integer; Error: Exception; pException: PExceptionPointers) of object;

  TCnVectoredException = class(TComponent) //VEH 基类
  private
    { Private declarations }
    FHandler: HWND; //VEH句柄
    FOnCallback: TVEHCallbackProc;
    FOnCallback_Instance: TCallbackInstance;
    procedure MakeCallbackInstance(var Instance: TCallbackInstance; ObjectAddr, FunctionAddr: Pointer);
    procedure SetBreakCallbackProc(FunctionAddr: Pointer); //设置回调函数地址
  protected
   { protected declarations }
    function DoVEHCallback(pException: PExceptionPointers): Integer; virtual; stdcall;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InstallVEH: boolean; virtual; //安装VEH
    procedure RemoveVEH; virtual; //卸载VEH

    property OnCallback: TVEHCallbackProc read FOnCallback write FOnCallback;
  published
    { published declarations }

  end;
  PBreakpointsProc = ^TBreakpointProc;
  TBreakpointProc = procedure(pException: PExceptionPointers) of object;

  TCnHardwareBreakpoint = class(TCnVectoredException) //硬断点类
  private
    { Private declarations }
    FDr1: DWORD;
    FDr2: DWORD;
    FDr3: DWORD;
    FDr4: DWORD;
    FOnBreakpoint1: TBreakpointProc;
    FOnBreakpoint2: TBreakpointProc;
    FOnBreakpoint3: TBreakpointProc;
    FOnBreakpoint4: TBreakpointProc;
    FOnHardwareBreakError: THardwareBreakError;
  protected
   { protected declarations }
    function DoVEHCallback(pException: PExceptionPointers): Integer; override; stdcall;
    procedure DoBreakpoint1(pException: PExceptionPointers); virtual;
    procedure DoBreakpoint2(pException: PExceptionPointers); virtual;
    procedure DoBreakpoint3(pException: PExceptionPointers); virtual;
    procedure DoBreakpoint4(pException: PExceptionPointers); virtual;
    procedure DoHardwareBreakError(ErrorId: Integer; Error: Exception; pException: PExceptionPointers); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearBreakpoints;
    procedure SetBreakpoints; //设置硬件断点使其生效

  published
    { published declarations }
    property BreakpointsAdderss1: DWORD read FDr1 write FDr1 default 0;
    property BreakpointsAdderss2: DWORD read FDr2 write FDr2 default 0;
    property BreakpointsAdderss3: DWORD read FDr3 write FDr3 default 0;
    property BreakpointsAdderss4: DWORD read FDr4 write FDr4 default 0;
    property OnBreakpoint1: TBreakpointProc read FOnBreakpoint1 write FOnBreakpoint1;
    property OnBreakpoint2: TBreakpointProc read FOnBreakpoint2 write FOnBreakpoint2;
    property OnBreakpoint3: TBreakpointProc read FOnBreakpoint3 write FOnBreakpoint3;
    property OnBreakpoint4: TBreakpointProc read FOnBreakpoint4 write FOnBreakpoint4;
    property OnHardwareBreakError: THardwareBreakError read FOnHardwareBreakError write FOnHardwareBreakError;

  end;

implementation

{ TCnVectoredException }

procedure TCnVectoredException.MakeCallbackInstance(var Instance: TCallbackInstance; ObjectAddr, FunctionAddr: Pointer);
  {----------------------------}
  { CallbackCode DASM          }
  {----------------------------}
  {    POP EAX;                }
  {    PUSH ObjectAddr;        }
  {    PUSH EAX;               }
  {    JMP FunctionAddr;       }
  {----------------------------}
const CallbackCode: TCallbackInstance =
//($8B,$04,$24,$50,$B8,$00,$00,$00,$00,$89,$44,$24,$04,$E9,$00,$00,$00,$00);
  ($58, $68, $00, $00, $00, $00, $50, $E9, $00, $00, $00, $00);
begin
  Move(CallbackCode, Instance, SizeOf(TCallbackInstance));
  PDWORD(@Instance[3])^ := DWORD(ObjectAddr);
  PDWORD(@Instance[9])^ := DWORD(DWORD(FunctionAddr) - DWORD(@Instance) - 12);
end;

procedure TCnVectoredException.SetBreakCallbackProc(FunctionAddr: Pointer);
begin
  MakeCallbackInstance(FOnCallback_Instance, Self, FunctionAddr);
end;

constructor TCnVectoredException.Create(AOwner: TComponent);
begin
  inherited;

  FHandler := 0;
  SetBreakCallbackProc(@TCnVectoredException.DoVEHCallback);
  InstallVEH;
end;

destructor TCnVectoredException.Destroy;
begin
  RemoveVEH;
  inherited;
end;

function TCnVectoredException.InstallVEH: boolean;
type
  TAddVectored = function(FirstHandler: Integer; VectoredHandler: Pointer): HWND; stdcall;
var
  _pAddVectored: TAddVectored;
begin
  Result := False;
  if FHandler <> 0 then Exit;
  _pAddVectored := GetProcAddress(LoadLibrary('Kernel32.dll'), 'AddVectoredExceptionHandler');
  if not Assigned(_pAddVectored) then Exit;
  FHandler := _pAddVectored(1, @Self.FOnCallback_Instance); //安装VEH
  Result := True;
end;

procedure TCnVectoredException.RemoveVEH;
type
  TRemoveVectored = function(VectoredHandler: HWND): Integer; stdcall;
var
  _pRemoveVectored: TRemoveVectored;
begin
  if FHandler = 0 then Exit;
  _pRemoveVectored := GetProcAddress(LoadLibrary('Kernel32.dll'), 'RemoveVectoredExceptionHandler');
  if Assigned(_pRemoveVectored) then _pRemoveVectored(FHandler); //卸载VEH
  FHandler := 0;
end;

function TCnVectoredException.DoVEHCallback(pException: PExceptionPointers): Integer;
begin
  Result := 0;
  if Assigned(Self.FOnCallback) then
  try
    Result := Self.FOnCallback(pException);
  except
  end;
end;

{ TCnHardwareBreakpoint }

procedure TCnHardwareBreakpoint.ClearBreakpoints;
var
  _Regs: CONTEXT;
begin
  {设置断点}
  FDr1 := 0;
  FDr2 := 0;
  FDr3 := 0;
  FDr4 := 0;
  _Regs.ContextFlags := CONTEXT_DEBUG_REGISTERS;
  GetThreadContext(GetCurrentThread, _Regs);
  _Regs.Dr0 := FDr1;
  _Regs.Dr1 := FDr2;
  _Regs.Dr2 := FDr3;
  _Regs.Dr3 := FDr4;
  _Regs.Dr7 := $7FF;
  SetThreadContext(GetCurrentThread, _Regs);
end;

constructor TCnHardwareBreakpoint.Create(AOwner: TComponent);
begin
  inherited;
  FDr1 := 0;
  FDr2 := 0;
  FDr3 := 0;
  FDr4 := 0;
  SetBreakCallbackProc(@TCnHardwareBreakpoint.DoVEHCallback);
end;

destructor TCnHardwareBreakpoint.Destroy;
begin
  ClearBreakpoints;
  inherited;
end;

procedure TCnHardwareBreakpoint.DoBreakpoint1(pException: PExceptionPointers);
begin
  if Assigned(Self.FOnBreakpoint1) then
  try
    Self.FOnBreakpoint1(pException);
  except
    on Error: Exception do DoHardwareBreakError(1, Error, pException);
  end;
end;

procedure TCnHardwareBreakpoint.DoBreakpoint2(pException: PExceptionPointers);
begin
  if Assigned(Self.FOnBreakpoint2) then
  try
    Self.FOnBreakpoint2(pException);
  except
    on Error: Exception do DoHardwareBreakError(2, Error, pException);
  end;
end;

procedure TCnHardwareBreakpoint.DoBreakpoint3(pException: PExceptionPointers);
begin
  if Assigned(Self.FOnBreakpoint3) then
  try
    Self.FOnBreakpoint3(pException);
  except
    on Error: Exception do DoHardwareBreakError(3, Error, pException);
  end;
end;

procedure TCnHardwareBreakpoint.DoBreakpoint4(pException: PExceptionPointers);
begin
  if Assigned(Self.FOnBreakpoint4) then
  try
    Self.FOnBreakpoint4(pException);
  except
    on Error: Exception do DoHardwareBreakError(4, Error, pException);
  end;
end;


procedure TCnHardwareBreakpoint.DoHardwareBreakError(ErrorId: Integer; Error: Exception;
  pException: PExceptionPointers); //异常处理
begin
  if Assigned(Self.FOnHardwareBreakError) then
    Self.FOnHardwareBreakError(ErrorId, Error, pException);
end;

function TCnHardwareBreakpoint.DoVEHCallback(pException: PExceptionPointers): Integer;
begin
  Result := 0;
  case PException^.ExceptionRecord^.ExceptionCode of
    EXCEPTION_SINGLE_STEP:
      begin
        if PException^.ContextRecord^.Eip = FDr1 then Self.DoBreakpoint1(pException) else
          if PException^.ContextRecord^.Eip = FDr2 then Self.DoBreakpoint2(pException) else
            if PException^.ContextRecord^.Eip = FDr3 then Self.DoBreakpoint3(pException) else
              if PException^.ContextRecord^.Eip = FDr4 then Self.DoBreakpoint4(pException);
        Result := -1;
      end;
  end;
end;

procedure TCnHardwareBreakpoint.SetBreakpoints;
var
  _Regs: CONTEXT;
begin
  {设置断点}
  _Regs.ContextFlags := CONTEXT_DEBUG_REGISTERS;
  GetThreadContext(GetCurrentThread, _Regs);
  _Regs.Dr0 := FDr1;
  _Regs.Dr1 := FDr2;
  _Regs.Dr2 := FDr3;
  _Regs.Dr3 := FDr4;
  _Regs.Dr7 := $7FF;
  SetThreadContext(GetCurrentThread, _Regs);
end;

end.
