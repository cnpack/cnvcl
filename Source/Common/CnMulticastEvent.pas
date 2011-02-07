{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2011 CnPack 开发组                       }
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

unit CnMulticastEvent;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：多播对象实现单元
* 单元作者：Chinbo（Shenloqi）
* 备    注：该单元定义了多播对象。由于内嵌汇编的限制，无法在 Delphi 5 下实现
* 开发平台：PWin2K SP3 + Delphi 7
* 兼容测试：PWin9X/2000/XP + Delphi 6/7 C++Builder 6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.12.30
*                创建单元，移植功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{.$DEFINE TEST}

{$DEFINE SINGLETON}

uses
  Windows, SysUtils, Classes, TypInfo {$IFDEF SINGLETON}, CnSingleton{$ENDIF};

type
  PMethod = ^TMethod;

  TCnMethodList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function Exists(ACode, AData: Pointer): Boolean;
    function IndexOf(ACode, AData: Pointer): Integer; reintroduce;
    function Remove(ACode, AData: Pointer): Integer; reintroduce;
  end;

  TCnMulticastEvent = class
  private
    FHandlers: TCnMethodList;
    FMethod: TMethod;
    FSavedMethod: TMethod;
    FSavedMethodPointer: PMethod;
    FContainedInManager: Boolean;

    procedure EventsFirer;
    procedure EventsFirer3;
    procedure EventsFirer4;
    procedure EventsFirer5;
    procedure EventsFirer6;
    procedure EventsFirer7;
    procedure EventsFirer8;
    procedure EventsFirer9;
    procedure EventsFirer10;
    procedure EventsFirer11;
    procedure EventsFirer12;
  public
    constructor Create(AMethod: PMethod; ATypeInfo: PTypeInfo);
    destructor Destroy; override;

    function Exists(ACode, AData: Pointer): Boolean; overload;
    function Exists(AMethod: TMethod): Boolean; overload;

    procedure Add(AMethod: PMethod);
    procedure Clear;
    procedure Insert(Index: Integer; AMethod: PMethod);
    procedure Remove(ACode, AData: Pointer); overload;
    procedure Remove(AMethod: TMethod); overload;

    property ContainedInManager: Boolean read FContainedInManager;
    property Handlers: TCnMethodList read FHandlers;
    property Method: TMethod read FMethod;
    property SavedMethod: TMethod read FSavedMethod;
    property SavedMethodPointer: PMethod read FSavedMethodPointer;
  end;

  TCnMulticastEventManager = class;

  TCnMulticastEventList = class(TStringList)
  private
    FManager: TCnMulticastEventManager;
  public
    constructor Create;
    destructor Destroy; override;

    function AddObject(const S: string; AObject: TObject): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;

    property Manager: TCnMulticastEventManager read FManager;
  end;

  TCnMulticastEventManager = class{$IFDEF SINGLETON}(TCnSingleton){$ENDIF}
  private
    FEvents: TCnMulticastEventList;
    function GetItems(Index: Pointer): TCnMulticastEvent;
  public
    constructor Create;
    destructor Destroy; override;

    function Add(AMethod: PMethod; ATypeInfo: PTypeInfo): TCnMulticastEvent; overload;
    function Add(AEvent: TCnMulticastEvent): TCnMulticastEvent; overload;
    function Exists(AEvent: TCnMulticastEvent): Boolean; overload;
    function Exists(AMethod: Pointer): Boolean; overload;
    function Remove(AEvent: TCnMulticastEvent): Integer; overload;
    function Remove(AMethod: Pointer; FreeEventObject: Boolean = False): Integer; overload;
    function PointerToString(APointer: Pointer): string;

    property Events: TCnMulticastEventList read FEvents;
    property Items[Index: Pointer]: TCnMulticastEvent read GetItems; default;
  end;

function CnMulticastEventManager: TCnMulticastEventManager;

function CnMakeMethod(ACode, AData: Pointer): TMethod;

function CnMakePMethod(ACode, AData: Pointer): PMethod;

implementation

function CnMakeMethod(ACode, AData: Pointer): TMethod;
begin
  Result.Code := ACode;
  Result.Data := AData;
end;

function CnMakePMethod(ACode, AData: Pointer): PMethod;
begin
  New(Result);
  Result.Code := ACode;
  Result.Data := AData;
end;

var
  FMulticastEventManager: TCnMulticastEventManager;

function CnMulticastEventManager: TCnMulticastEventManager;
begin
  if FMulticastEventManager = nil then
    FMulticastEventManager := TCnMulticastEventManager.Create;
  Result := FMulticastEventManager;
end;

{ TCnMethodList }

function TCnMethodList.Exists(ACode, AData: Pointer): Boolean;
begin
  Result := IndexOf(ACode, AData) >= 0;
end;

function TCnMethodList.IndexOf(ACode, AData: Pointer): Integer;
begin
  Result := 0;
  while Result < Count do
  begin
    with PMethod(List^[Result])^ do
    begin
      if (Code <> ACode) or (Data <> AData) then
        Inc(Result)
      else
        Break;
    end;
  end;
  if Result = Count then
    Result := -1;
end;

procedure TCnMethodList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    Dispose(Ptr);
  inherited;
end;

function TCnMethodList.Remove(ACode, AData: Pointer): Integer;
begin
  Result := IndexOf(ACode, AData);
  if Result >= 0 then
    Delete(Result);
end;

{ TCnMulticastEvent }

procedure TCnMulticastEvent.Add(AMethod: PMethod);
begin
  FHandlers.Add(AMethod);
end;

procedure TCnMulticastEvent.Clear;
begin
  FHandlers.Clear;
end;

constructor TCnMulticastEvent.Create;
var
  MethodTypeData: PTypeData;
  ArgsCount: Integer;
begin
  FContainedInManager := False;
  FHandlers := TCnMethodList.Create;
  MethodTypeData := nil;
  if ATypeInfo <> nil then MethodTypeData := GetTypeData(ATypeInfo);
  if MethodTypeData <> nil then
    ArgsCount := MethodTypeData.ParamCount 
  else
    ArgsCount := 0;
  if ArgsCount > 12 then
    raise Exception.Create('Only support 12 parameters.');
  case ArgsCount of
    3: FMethod.Code := @TCnMulticastEvent.EventsFirer3;
    4: FMethod.Code := @TCnMulticastEvent.EventsFirer4;
    5: FMethod.Code := @TCnMulticastEvent.EventsFirer5;
    6: FMethod.Code := @TCnMulticastEvent.EventsFirer6;
    7: FMethod.Code := @TCnMulticastEvent.EventsFirer7;
    8: FMethod.Code := @TCnMulticastEvent.EventsFirer8;
    9: FMethod.Code := @TCnMulticastEvent.EventsFirer9;
    10: FMethod.Code := @TCnMulticastEvent.EventsFirer10;
    11: FMethod.Code := @TCnMulticastEvent.EventsFirer11;
    12: FMethod.Code := @TCnMulticastEvent.EventsFirer12;
  else
    FMethod.Code := @TCnMulticastEvent.EventsFirer;
  end;
  FMethod.Data := Self;
  FSavedMethod := AMethod^;
  FSavedMethodPointer := AMethod;
  if AMethod <> nil then
  begin
    if AMethod^.Code <> nil then FHandlers.Add(CnMakePMethod(AMethod^.Code, AMethod^.Data));
    AMethod^ := FMethod;
  end;

  with CnMulticastEventManager do
  begin
    if not Exists(AMethod) then
    begin
      Add(Self);
    end;
  end;
end;

destructor TCnMulticastEvent.Destroy;
begin
  if FContainedInManager then
    CnMulticastEventManager.Remove(Self);
//  with TMethod(FSavedMethodPointer^) do
//    if (Code <> @TCnMulticastEvent.EventsFirer) or (Data <> Self) then
//      MessageBox(0, '', '', 0);
  FSavedMethodPointer^ := FSavedMethod;
  FHandlers.Free;
  inherited;
end;

function TListGet(Sender: TList; Index: Integer): PMethod;
begin
  Result := PMethod(Sender.Items[index]);
end;

// Only support two parameters
procedure TCnMulticastEvent.EventsFirer;
asm
  push ebx
  push esi
  push edi
  push ebp
  mov edi, eax //[esp + $10] // get self
  push ecx
  push edx
  push eax

  // for i := 0 to FHandlers.Count - 1 do
  mov eax, [edi+$04] //FHandlers
  mov ebx, [eax+$08] //FHandlers.Count
  dec ebx
  test ebx, ebx
  jl @endloop
  inc ebx
  xor esi, esi
  // call FHandlers[i];
@call:
  mov eax, [edi+$04]
  mov edx, esi
  call TListGet
  mov ebp, eax

  push ecx
  push edx
  push eax
  mov eax, TMethod[ebp].Data
  mov edx, [esp + $10]
  mov ecx, [esp + $14]

  call TMethod[ebp].Code

  pop eax
  pop edx
  pop ecx

  inc esi
  // for i := 0 to FHandlers.Count - 1 do
  dec ebx
  jnz @call
  // end;
@endloop:
  pop eax
  pop edx
  pop ecx

  pop ebp
  pop edi
  pop esi
  pop ebx
  ret
end;

procedure TCnMulticastEvent.EventsFirer3;
asm
  push ebx
  push esi
  push edi
  push ebp
  mov edi, eax
  push ecx
  push edx
  push eax
  mov eax, [edi+$04]
  mov ebx, [eax+$08]
  dec ebx
  test ebx, ebx
  jl @endloop
  inc ebx
  xor esi, esi
  jmp @call
@call0:
  mov esp, esp - $04
@call:
  mov eax, [edi+$04]
  mov edx, esi
  call TListGet
  mov ebp, eax
  push ecx
  push edx
  push eax
  mov eax, TMethod[ebp].Data
  mov edx, [esp + $10]
  mov ecx, [esp + $14]
  push [esp + $2C]
  call TMethod[ebp].Code
  pop eax
  pop edx
  pop ecx
  inc esi
  dec ebx
  jnz @call0
@endloop:
  pop eax
  pop edx
  pop ecx
  pop ebp
  pop edi
  pop esi
  pop ebx
  ret $0004
end;

procedure TCnMulticastEvent.EventsFirer4;
asm
  push ebx
  push esi
  push edi
  push ebp
  mov edi, eax
  push ecx
  push edx
  push eax
  mov eax, [edi+$04]
  mov ebx, [eax+$08]
  dec ebx
  test ebx, ebx
  jl @endloop
  inc ebx
  xor esi, esi
  jmp @call
@call0:
  mov esp, esp - $08
@call:
  mov eax, [edi+$04]
  mov edx, esi
  call TListGet
  mov ebp, eax
  push ecx
  push edx
  push eax
  mov eax, TMethod[ebp].Data
  mov edx, [esp + $10]
  mov ecx, [esp + $14]
  push [esp + $30]
  push [esp + $30]
  call TMethod[ebp].Code
  pop eax
  pop edx
  pop ecx
  inc esi
  dec ebx
  jnz @call0
@endloop:
  pop eax
  pop edx
  pop ecx
  pop ebp
  pop edi
  pop esi
  pop ebx
  ret $0008
end;

procedure TCnMulticastEvent.EventsFirer5;
asm
  push ebx
  push esi
  push edi
  push ebp
  mov edi, eax
  push ecx
  push edx
  push eax
  mov eax, [edi+$04]
  mov ebx, [eax+$08]
  dec ebx
  test ebx, ebx
  jl @endloop
  inc ebx
  xor esi, esi
  jmp @call
@call0:
  mov esp, esp - $0C
@call:
  mov eax, [edi+$04]
  mov edx, esi
  call TListGet
  mov ebp, eax
  push ecx
  push edx
  push eax
  mov eax, TMethod[ebp].Data
  mov edx, [esp + $10]
  mov ecx, [esp + $14]
  push [esp + $34]
  push [esp + $34]
  push [esp + $34]
  call TMethod[ebp].Code
  pop eax
  pop edx
  pop ecx
  inc esi
  dec ebx
  jnz @call0
@endloop:
  pop eax
  pop edx
  pop ecx
  pop ebp
  pop edi
  pop esi
  pop ebx
  ret $000C
end;

procedure TCnMulticastEvent.EventsFirer6;
asm
  push ebx
  push esi
  push edi
  push ebp
  mov edi, eax
  push ecx
  push edx
  push eax
  mov eax, [edi+$04]
  mov ebx, [eax+$08]
  dec ebx
  test ebx, ebx
  jl @endloop
  inc ebx
  xor esi, esi
  jmp @call
@call0:
  mov esp, esp - $10
@call:
  mov eax, [edi+$04]
  mov edx, esi
  call TListGet
  mov ebp, eax
  push ecx
  push edx
  push eax
  mov eax, TMethod[ebp].Data
  mov edx, [esp + $10]
  mov ecx, [esp + $14]
  push [esp + $38]
  push [esp + $38]
  push [esp + $38]
  push [esp + $38]
  call TMethod[ebp].Code
  pop eax
  pop edx
  pop ecx
  inc esi
  dec ebx
  jnz @call0
@endloop:
  pop eax
  pop edx
  pop ecx
  pop ebp
  pop edi
  pop esi
  pop ebx
  ret $0010
end;

procedure TCnMulticastEvent.EventsFirer7;
asm
  push ebx
  push esi
  push edi
  push ebp
  mov edi, eax
  push ecx
  push edx
  push eax
  mov eax, [edi+$04]
  mov ebx, [eax+$08]
  dec ebx
  test ebx, ebx
  jl @endloop
  inc ebx
  xor esi, esi
  jmp @call
@call0:
  mov esp, esp - $14
@call:
  mov eax, [edi+$04]
  mov edx, esi
  call TListGet
  mov ebp, eax
  push ecx
  push edx
  push eax
  mov eax, TMethod[ebp].Data
  mov edx, [esp + $10]
  mov ecx, [esp + $14]
  push [esp + $3C]
  push [esp + $3C]
  push [esp + $3C]
  push [esp + $3C]
  push [esp + $3C]
  call TMethod[ebp].Code
  pop eax
  pop edx
  pop ecx
  inc esi
  dec ebx
  jnz @call0
@endloop:
  pop eax
  pop edx
  pop ecx
  pop ebp
  pop edi
  pop esi
  pop ebx
  ret $0014
end;

procedure TCnMulticastEvent.EventsFirer8;
asm
  push ebx
  push esi
  push edi
  push ebp
  mov edi, eax
  push ecx
  push edx
  push eax
  mov eax, [edi+$04]
  mov ebx, [eax+$08]
  dec ebx
  test ebx, ebx
  jl @endloop
  inc ebx
  xor esi, esi
  jmp @call
@call0:
  mov esp, esp - $18
@call:
  mov eax, [edi+$04]
  mov edx, esi
  call TListGet
  mov ebp, eax
  push ecx
  push edx
  push eax
  mov eax, TMethod[ebp].Data
  mov edx, [esp + $10]
  mov ecx, [esp + $14]
  push [esp + $40]
  push [esp + $40]
  push [esp + $40]
  push [esp + $40]
  push [esp + $40]
  push [esp + $40]
  call TMethod[ebp].Code
  pop eax
  pop edx
  pop ecx
  inc esi
  dec ebx
  jnz @call0
@endloop:
  pop eax
  pop edx
  pop ecx
  pop ebp
  pop edi
  pop esi
  pop ebx
  ret $0018
end;

procedure TCnMulticastEvent.EventsFirer9;
asm
  push ebx
  push esi
  push edi
  push ebp
  mov edi, eax
  push ecx
  push edx
  push eax
  mov eax, [edi+$04]
  mov ebx, [eax+$08]
  dec ebx
  test ebx, ebx
  jl @endloop
  inc ebx
  xor esi, esi
  jmp @call
@call0:
  mov esp, esp - $1C
@call:
  mov eax, [edi+$04]
  mov edx, esi
  call TListGet
  mov ebp, eax
  push ecx
  push edx
  push eax
  mov eax, TMethod[ebp].Data
  mov edx, [esp + $10]
  mov ecx, [esp + $14]
  push [esp + $44]
  push [esp + $44]
  push [esp + $44]
  push [esp + $44]
  push [esp + $44]
  push [esp + $44]
  push [esp + $44]
  call TMethod[ebp].Code
  pop eax
  pop edx
  pop ecx
  inc esi
  dec ebx
  jnz @call0
@endloop:
  pop eax
  pop edx
  pop ecx
  pop ebp
  pop edi
  pop esi
  pop ebx
  ret $001C
end;

function TCnMulticastEvent.Exists(AMethod: TMethod): Boolean;
begin
  Result := FHandlers.Exists(AMethod.Code, AMethod.Data);
end;

function TCnMulticastEvent.Exists(ACode, AData: Pointer): Boolean;
begin
  Result := FHandlers.Exists(ACode, AData);
end;

procedure TCnMulticastEvent.EventsFirer10;
asm
  push ebx
  push esi
  push edi
  push ebp
  mov edi, eax
  push ecx
  push edx
  push eax
  mov eax, [edi+$04]
  mov ebx, [eax+$08]
  dec ebx
  test ebx, ebx
  jl @endloop
  inc ebx
  xor esi, esi
  jmp @call
@call0:
  mov esp, esp - $20
@call:
  mov eax, [edi+$04]
  mov edx, esi
  call TListGet
  mov ebp, eax
  push ecx
  push edx
  push eax
  mov eax, TMethod[ebp].Data
  mov edx, [esp + $10]
  mov ecx, [esp + $14]
  push [esp + $48]
  push [esp + $48]
  push [esp + $48]
  push [esp + $48]
  push [esp + $48]
  push [esp + $48]
  push [esp + $48]
  push [esp + $48]
  call TMethod[ebp].Code
  pop eax
  pop edx
  pop ecx
  inc esi
  dec ebx
  jnz @call0
@endloop:
  pop eax
  pop edx
  pop ecx
  pop ebp
  pop edi
  pop esi
  pop ebx
  ret $0020
end;

procedure TCnMulticastEvent.EventsFirer11;
asm
  push ebx
  push esi
  push edi
  push ebp
  mov edi, eax
  push ecx
  push edx
  push eax
  mov eax, [edi+$04]
  mov ebx, [eax+$08]
  dec ebx
  test ebx, ebx
  jl @endloop
  inc ebx
  xor esi, esi
  jmp @call
@call0:
  mov esp, esp - $24
@call:
  mov eax, [edi+$04]
  mov edx, esi
  call TListGet
  mov ebp, eax
  push ecx
  push edx
  push eax
  mov eax, TMethod[ebp].Data
  mov edx, [esp + $10]
  mov ecx, [esp + $14]
  push [esp + $4C]
  push [esp + $4C]
  push [esp + $4C]
  push [esp + $4C]
  push [esp + $4C]
  push [esp + $4C]
  push [esp + $4C]
  push [esp + $4C]
  push [esp + $4C]
  call TMethod[ebp].Code
  pop eax
  pop edx
  pop ecx
  inc esi
  dec ebx
  jnz @call0
@endloop:
  pop eax
  pop edx
  pop ecx
  pop ebp
  pop edi
  pop esi
  pop ebx
  ret $0024
end;

procedure TCnMulticastEvent.EventsFirer12;
asm
  push ebx
  push esi
  push edi
  push ebp
  mov edi, eax
  push ecx
  push edx
  push eax
  mov eax, [edi+$04]
  mov ebx, [eax+$08]
  dec ebx
  test ebx, ebx
  jl @endloop
  inc ebx
  xor esi, esi
  jmp @call
@call0:
  mov esp, esp - $28
@call:
  mov eax, [edi+$04]
  mov edx, esi
  call TListGet
  mov ebp, eax
  push ecx
  push edx
  push eax
  mov eax, TMethod[ebp].Data
  mov edx, [esp + $10]
  mov ecx, [esp + $14]
  push [esp + $50]
  push [esp + $50]
  push [esp + $50]
  push [esp + $50]
  push [esp + $50]
  push [esp + $50]
  push [esp + $50]
  push [esp + $50]
  push [esp + $50]
  push [esp + $50]
  call TMethod[ebp].Code
  pop eax
  pop edx
  pop ecx
  inc esi
  dec ebx
  jnz @call0
@endloop:
  pop eax
  pop edx
  pop ecx
  pop ebp
  pop edi
  pop esi
  pop ebx
  ret $0028
end;

procedure TCnMulticastEvent.Insert(Index: Integer; AMethod: PMethod);
begin
  FHandlers.Insert(Index, AMethod);
end;

procedure TCnMulticastEvent.Remove(AMethod: TMethod);
begin
  FHandlers.Remove(AMethod.Code, AMethod.Data);
end;

procedure TCnMulticastEvent.Remove(ACode, AData: Pointer);
begin
  FHandlers.Remove(ACode, AData);
end;

{ TCnMulticastEventList }

function TCnMulticastEventList.AddObject(const S: string;
  AObject: TObject): Integer;
begin
  Result := inherited AddObject(S, AObject);
end;

procedure TCnMulticastEventList.Clear;
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if Assigned(Objects[i]) then Objects[i].Free;
  end;

  inherited;
end;

constructor TCnMulticastEventList.Create;
begin
  Sorted := True;
end;

procedure TCnMulticastEventList.Delete(Index: Integer);
var
  Obj: TObject;
begin
  Obj := Objects[index];
  inherited;
  if Assigned(Obj) then Obj.Free;
end;

destructor TCnMulticastEventList.Destroy;
begin
  Clear;
  inherited;
end;

{ TCnMulticastEventManager }

function TCnMulticastEventManager.Add(AMethod: PMethod; ATypeInfo: PTypeInfo): TCnMulticastEvent;
begin
  if Exists(AMethod) then
    Result := Items[AMethod]
  else
    Result := TCnMulticastEvent.Create(AMethod, ATypeInfo);
end;

function TCnMulticastEventManager.Add(AEvent: TCnMulticastEvent): TCnMulticastEvent;
begin
  Result := AEvent;
  if (AEvent <> nil) and (not Exists(AEvent)) then
  begin
    FEvents.AddObject(PointerToString(AEvent.FSavedMethodPointer), AEvent);
    AEvent.FContainedInManager := True;
  end;
end;

constructor TCnMulticastEventManager.Create;
begin
  FEvents := TCnMulticastEventList.Create;
  FEvents.FManager := Self;
end;

destructor TCnMulticastEventManager.Destroy;
begin
  FEvents.Free;
  inherited;
end;

function TCnMulticastEventManager.Exists(AMethod: Pointer): Boolean;
begin
  Result := FEvents.IndexOf(PointerToString(AMethod)) >= 0;
end;

function TCnMulticastEventManager.Exists(AEvent: TCnMulticastEvent): Boolean;
begin
  Result := FEvents.IndexOfObject(AEvent) >= 0;
end;

function TCnMulticastEventManager.GetItems(Index: Pointer): TCnMulticastEvent;
var
  i: Integer;
begin
  Result := nil;
  i := FEvents.IndexOf(PointerToString(Index));
  if i >= 0 then
    Result := TCnMulticastEvent(FEvents.Objects[i]);
end;

function TCnMulticastEventManager.PointerToString(APointer: Pointer): string;
begin
  Result := IntToStr(Integer(APointer));
end;

function TCnMulticastEventManager.Remove(AMethod: Pointer; FreeEventObject: Boolean = False): Integer;
begin
  Result := FEvents.IndexOf(PointerToString(AMethod));
  if Result >= 0 then
  begin
    if not FreeEventObject then FEvents.Objects[Result] := nil;
    FEvents.Delete(Result);
  end;
end;

function TCnMulticastEventManager.Remove(AEvent: TCnMulticastEvent): Integer;
begin
  Result := FEvents.IndexOfObject(AEvent);
  if Result >= 0 then
  begin
    FEvents.Objects[Result] := nil;
    FEvents.Delete(Result);
  end;
end;

{$IFDEF TEST}
type
  TTestProc = procedure(a, b, c, d, e, f, g, h, i, j: Integer) of object;
  TTestHelper = class
    procedure OnChange(Sender: TObject);
    procedure OnTest(a, b, c, d, e, f, g, h, i, j: Integer);
  end;
  TTest = class
    FOnTest: TTestProc;
    procedure DoTest;
    property OnTest: TTestProc read FOnTest write FOnTest;
  end;

procedure TTest.DoTest;
begin
  if Assigned(OnTest) then OnTest(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
end;

procedure TTestHelper.OnChange(Sender: TObject);
begin
  Assert(Sender is TStringList);
  with TStringList(Sender) do
  begin
    MessageBox(0, PChar(Text), PChar(String(Sender.ClassName)), 0);
  end;
end;

procedure TTestHelper.OnTest(a, b, c, d, e, f, g, h, i, j: Integer);
begin
  MessageBox(0, PChar(Format('%s:%d,%d,%d,%d,%d,%d,%d,%d,%d,%d', [ClassName, a, b, c, d, e, f, g, h, i, j])), '', 0)
end;

procedure TestStringList(H: TTestHelper);
var
  L: TStringList;
  ME: TCnMulticastEvent;
begin
  L := TStringList.Create;
  L.OnChange := H.OnChange;
  ME := MulticastEventManager.Add(@@L.OnChange, TypeInfo(TNotifyEvent));
  ME.Add(CnMakePMethod(@TTestHelper.OnChange, L));
  L.Add('123');
  ME.Free;
  L.Add('456');
  L.Free;
end;

procedure TestTenParameters(H: TTestHelper);
var
  T: TTest;
  ME: TCnMulticastEvent;
begin
  T := TTest.Create;
  ME := MulticastEventManager.Add(@@T.OnTest, TypeInfo(TTestProc));
  ME.Add(MakePMethod(@TTestHelper.OnTest, H));
  T.DoTest;
  ME.Remove(@TTestHelper.OnTest, H);
  T.DoTest;
  ME.Free;
  T.DoTest;
  T.Free;
end;

procedure Test;
var
  H: TTestHelper;
begin
  H := TTestHelper.Create;
  TestStringList(H);
  TestTenParameters(H);
  H.Free;
end;
{$ENDIF}

initialization
  {$IFNDEF SINGLETON}FMulticastEventManager := TCnMulticastEventManager.Create;{$ENDIF}
  {$IFDEF TEST}Test;{$ENDIF}

finalization
  {$IFNDEF SINGLETON}FMulticastEventManager.Free;{$ENDIF}

end.
