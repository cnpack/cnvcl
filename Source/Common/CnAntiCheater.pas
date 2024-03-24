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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnAntiCheater;
{* |<PRE>
================================================================================
* 软件名称：开发包工具类库
* 单元名称：给子类 published 的 Integer Get/Set 属性提供内容保护的基础类实现单元
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：TCnAntiCheater 类在处理自身及其子类 published 的带 Get 和 Set 方法
            的 Integer 属性的时候会挂接这两个方法，在读写这些属性的过程中插入一
            自定义变换过程，从而达到内存中属性值和对外显示不一致的效果以躲开游戏
            修改器的追踪。

            前提条件：
            Get 与 Set 方法必须保持一定复杂度以保证函数体代码长于 5 字节才能挂接
            因此推荐在编译选项中“关闭优化”以生成函数堆栈保护代码来保证代码长度，
            或者在打开优化的情况下，Get 与 Set 方法中按类似于下面的这样写：

            function TCnAntiCheater.GetData: Integer;
            begin
              if FData <> 0 then
                Result := FData
              else
                Result := 0;
            end;

            procedure TCnAntiCheater.SetData(const Value: Integer);
            begin
              if FData <> Value then
                FData := Value;
            end;

* 开发平台：PWinXP + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2006.11.22 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Windows, SysUtils, TypInfo, CnMethodHook;

type
  ECnAntiCheaterHookException = class(Exception);

  TCnAntiCheater = class(TPersistent)
  {* 给子类提供 published 的 Integer 属性内容保护的基础类}
  private
    FData: Integer;
    function GetData: Integer;
    procedure SetData(const Value: Integer);
  protected
    function GetConvertCardinal(const Value: Cardinal): Cardinal; virtual;
    function SetConvertCardinal(const Value: Cardinal): Cardinal; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    // 挂接后实际执行的是这俩方法的复制品，其他旧方法的地址已被填入
    function MyGetCardinalData: Integer;
    procedure MySetCardinalData(const Value: Integer);

    class procedure HookSelfClass;
    class procedure UnHookSelfClass;

  published
    property Data: Integer read GetData write SetData;
  end;

  TCnClassHookItem = class(TObject)
  {* 每一实例描述了某类的挂接信息}
  private
    FHookEnabled: Boolean;
    FHookers: TList;
    FHookProcs: TList;
    FHookClassName: string;
    function GetHookItems(Index: Integer): TCnMethodHook;
    procedure SetHookEnabled(const Value: Boolean);
    function GetHookCount: Integer;
    function GetHookProcs(Index: Integer): Pointer;

  public
    constructor Create;
    destructor Destroy; override;
    function AddHooker(AMethodHook: TCnMethodHook; AProc: Pointer): Integer;
    
    property HookEnabled: Boolean read FHookEnabled write SetHookEnabled;
    property HookClassName: string read FHookClassName write FHookClassName;
    property HookItems[Index: Integer]: TCnMethodHook read GetHookItems;
    property HookProcs[Index: Integer]: Pointer read GetHookProcs;
    property HookCount: Integer read GetHookCount;
  end;

implementation

resourcestring
  SCnErrorCallbackPoolInit = 'Callback Pool Init Error!';
  SCnErrorHookPoolOverflow = 'Hook Pool Overflow!';

const
  THUNK_SIZE = 4096; // x86 页大小，目前只弄一个页面

  CALL_SIZE = 57;    // GetCall 和 SetCall 结构尺寸中的较大的

type
  TCnGetCall = packed record
    Code1: array[1..10] of Byte;
    HookInst1: Pointer;            // CnMethodHook 的实例地址
    Code2: array[1..9] of Byte;
    AddrGet: Pointer;              // 原有的 Get 方法的相对地址
    Code3: array[1..9] of Byte;
    OffSetConvert: Byte;           // Get 后调用的 Convert 方法在 VMT 中的地址偏移
    Code4: array[1..4] of Byte;
    HookInst2: Pointer;            // CnMethodHook 的实例地址
    Code5: array[1..4] of Byte;
  end;
  PCnGetCall = ^TCnGetCall;

const
  SCnGetCall: array[1..57] of Byte = ($55, $8B, $EC, $83, $C4, $F8, $89, $45,
    $FC, $B8, $00, $00, $00, $00, $8B, $10, $FF, $52, $04, $8B, $45, $FC, $E8,
    $59, $FD, $FF, $FF, $8B, $D0, $8B, $45, $FC, $8B, $08, $FF, $51, $0C, $89,
    $45, $F8, $B8, $00, $00, $00, $00, $8B, $10, $FF, $52, $00, $8B, $45, $F8,
    $59, $59, $5D, $C3);

{
function TCnAntiCheater.MyGetCardinalData: Integer;
begin
  TCnMethodHook($33333333).UnHookMethod;
  Result := GetConvertCardinal(GetData);
  TCnMethodHook($33333333).HookMethod;
end;

CnAntiCheater.pas.377: begin
000C 55               push ebp
000D 8BEC             mov ebp,esp
000F 83C4F8           add esp,-$08
0012 8945FC           mov [ebp-$04],eax
CnAntiCheater.pas.378: TCnMethodHook($33333333).UnHookMethod;
0015 B833333333       mov eax,$33333333                          !
001A 8B10             mov edx,[eax]
001C FF5204           call dword ptr [edx+$04]
CnAntiCheater.pas.379: Result := GetConvertCardinal(GetData);
001F 8B45FC           mov eax,[ebp-$04]
0022 E859FDFFFF       call TCnAntiCheater.GetData                !
0027 8BD0             mov edx,eax
0029 8B45FC           mov eax,[ebp-$04]
002C 8B08             mov ecx,[eax]
002E FF510C           call dword ptr [ecx+$0c]                   !
0031 8945F8           mov [ebp-$08],eax
CnAntiCheater.pas.380: TCnMethodHook($33333333).HookMethod;
0034 B833333333       mov eax,$33333333                          !
0039 8B10             mov edx,[eax]
003B FF5200           call dword ptr [edx]
003E 8B45F8           mov eax,[ebp-$08]
CnAntiCheater.pas.381: end;
0041 59               pop ecx
0042 59               pop ecx
0043 5D               pop ebp
0044 C3               ret
}

type
  TCnSetCall = packed record
    Code1: array[1..13] of Byte;
    HookInst1: Pointer;            // CnMethodHook 的实例地址
    Code2: array[1..15] of Byte;
    OffSetConvert: Byte;           // Set 前调用的 Convert 方法的 VMT 偏移
    Code3: array[1..6] of Byte;
    AddrSet: Pointer;              // 原有的 Set 方法的地址
    Code4: array[1..1] of Byte;
    HookInst2: Pointer;            // CnMethodHook 的实例地址
    Code5: array[1..9] of Byte;
  end;
  PCnSetCall = ^TCnSetCall;

const
  SCnSetCall: array[1..57] of Byte = ($55, $8B, $EC, $83, $C4, $F8, $89, $55,
    $F8, $89, $45, $FC, $B8, $00, $00, $00, $00, $8B, $10, $FF, $52, $04, $8B,
    $55, $F8, $8B, $45, $FC, $8B, $08, $FF, $51, $00, $8B, $D0, $8B, $45, $FC,
    $E8, $00, $00, $00, $00, $B8, $00, $00, $00, $00, $8B, $10, $FF, $52, $00,
    $59, $59, $5D, $C3);

{
procedure TCnAntiCheater.MySetCardinalData(const Value: Integer);
begin
  TCnWizMethodHook($33333333).UnHookMethod;
  SetData(SetConvertCardinal(Value));
  TCnMethodHook($33333333).HookMethod;
end;

CnAntiCheater.pas.384: begin
0048 55               push ebp
0049 8BEC             mov ebp,esp
004B 83C4F8           add esp,-$08
004E 8955F8           mov [ebp-$08],edx
0051 8945FC           mov [ebp-$04],eax
CnAntiCheater.pas.385: TCnMethodHook($33333333).UnHookMethod;
0054 B833333333       mov eax,$33333333                          !
0059 8B10             mov edx,[eax]
005B FF5204           call dword ptr [edx+$04]
CnAntiCheater.pas.386: SetData(SetConvertCardinal(Value));
005E 8B55F8           mov edx,[ebp-$08]
0061 8B45FC           mov eax,[ebp-$04]
0064 8B08             mov ecx,[eax]
0066 FF5110           call dword ptr [ecx+$10]                   !
0069 8BD0             mov edx,eax
006B 8B45FC           mov eax,[ebp-$04]
006E E829FDFFFF       call TCnAntiCheater.SetData                !
CnAntiCheater.pas.387: TCnMethodHook($33333333).HookMethod;
0073 B833333333       mov eax,$33333333                          !
0078 8B10             mov edx,[eax]
007A FF5200           call dword ptr [edx]
CnAntiCheater.pas.388: end;
007D 59               pop ecx
007E 59               pop ecx
007F 5D               pop ebp
0080 C3               ret
}

var
  ClassList: TStringList = nil;
  {* 存储需要 Hook 的 Class，其 Objects 存储一 TCnClassHookItem}

  FHookPool: Pointer = nil;

  FEmptyPtr: Integer = 0;

  FEnableProtect: Boolean = False;

procedure InitHookPool;
begin
  FHookPool := VirtualAlloc(nil, THUNK_SIZE, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if FHookPool = nil then
    raise ECnAntiCheaterHookException.Create(SCnErrorCallbackPoolInit);
  FEmptyPtr := 0;
end;

// 返回是否在 Hook 列表中
function SelfClassHooked(AClass: TClass): Boolean;
begin
  Result := (ClassList <> nil) and (ClassList.IndexOf(AClass.ClassName) >= 0);
end;

// 返回是否已经 Hooked
function SelfClassHookEnabled(AClass: TClass): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ClassList <> nil then
  begin
    I := ClassList.IndexOf(AClass.ClassName);
    if I >= 0 then
      Result := TCnClassHookItem(ClassList.Objects[I]).HookEnabled;
  end;
end;

// 为某类添加一 HookItem 到列表中
function AddHookItem(AClass: TClass): TCnClassHookItem;
var
  I: Integer;
  Item: TCnClassHookItem;
begin
  Result := nil;
  if ClassList = nil then
  begin
    ClassList := TStringList.Create;
    ClassList.Duplicates := dupIgnore;
  end;

  if ClassList.IndexOf(AClass.ClassName) >= 0 then
    Exit;

  I := ClassList.Add(AClass.ClassName);

  Item := TCnClassHookItem.Create;
  Item.HookClassName := AClass.ClassName;
  ClassList.Objects[I] := Item;
  Result := Item;
end;

// 返回某类的 HookItem
function GetHookItemFromClassList(AClass: TClass): TCnClassHookItem;
var
  I: Integer;
begin
  Result := nil;
  if ClassList = nil then
  begin
    ClassList := TStringList.Create;
    ClassList.Duplicates := dupIgnore;
  end;

  I := ClassList.IndexOf(AClass.ClassName);
  if I >= 0 then
    Result := TCnClassHookItem(ClassList.Objects[I]);
end;

// 设置某子类是否 Hook 完毕的标志
procedure SetSelfClassHooked(AClass: TClass; Hooked: Boolean);
var
  I: Integer;
  Item: TCnClassHookItem;
begin
  if ClassList = nil then
  begin
    ClassList := TStringList.Create;
    ClassList.Duplicates := dupIgnore;
  end;

  I := ClassList.IndexOf(AClass.ClassName);
  if I >= 0 then
  begin
    Item := TCnClassHookItem(ClassList.Objects[I]);
  end
  else
  begin
    Item := AddHookItem(AClass);
  end;

  Item.HookEnabled := Hooked;
end;

// 判断一过程是否已被 Hook
function AProcHooked(AProc: Pointer): Boolean;
var
  I, J: Integer;
  HookItem: TCnClassHookItem;
begin
  Result := False;
  if ClassList <> nil then
  begin
    for I := 0 to ClassList.Count - 1 do
    begin
      HookItem := TCnClassHookItem(ClassList.Objects[I]);
      if HookItem <> nil then
      begin
        for J := 0 to HookItem.HookCount - 1 do
        begin
          if HookItem.HookProcs[J] = AProc then
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

{ TCnAntiCheater }

constructor TCnAntiCheater.Create;
begin
  if not SelfClassHooked(Self.ClassType) then
    HookSelfClass;
end;

destructor TCnAntiCheater.Destroy;
begin
  inherited;

end;

function TCnAntiCheater.GetConvertCardinal(const Value: Cardinal): Cardinal;
begin
  // Get 的时候进行的转换，子类可重载
  Result := not Value;
end;

function TCnAntiCheater.SetConvertCardinal(const Value: Cardinal): Cardinal;
begin
  // Set 的时候进行的转换，子类可重载
  Result := not Value;
end;

function TCnAntiCheater.GetData: Integer;
begin
  if FData <> 0 then
    Result := FData
  else
    Result := 0;
end;

procedure TCnAntiCheater.SetData(const Value: Integer);
begin
  if FData <> Value then
    FData := Value;
end;

class procedure TCnAntiCheater.HookSelfClass;
var
  HookItem: TCnClassHookItem;
  AGet: PCnGetCall;
  ASet: PCnSetCall;
  I, APropCount: Integer;
  PropInfo: PPropInfo;
  PropListPtr: PPropList;
  AHooker: TCnMethodHook;
begin
  if not FEnableProtect or SelfClassHookEnabled(Self) then
    Exit;
  HookItem := AddHookItem(Self);

  if FHookPool = nil then
    InitHookPool;

  APropCount := GetTypeData(PTypeInfo(Self.ClassInfo))^.PropCount;
  GetMem(PropListPtr, APropCount * SizeOf(Pointer));
  GetPropList(PTypeInfo(Self.ClassInfo), tkAny, PropListPtr);

  try
    for I := 0 to APropCount - 1 do
    begin
      PropInfo := PropListPtr^[I];
      if PropInfo^.PropType^^.Kind in [tkInteger] then // 暂时只挂接 Integer 的属性
      begin
        // 注意预防对不同子类的同一个方法的重复挂接
        if PropInfo^.GetProc <> nil then // 挂接处理 Get 过程
        begin
          if not AProcHooked(PropInfo^.GetProc) then
          begin
            if FEmptyPtr = (THUNK_SIZE div CALL_SIZE) then
              raise ECnAntiCheaterHookException.Create(SCnErrorHookPoolOverflow);
    
            AGet := PCnGetCall(Integer(FHookPool) + FEmptyPtr * CALL_SIZE);
            Inc(FEmptyPtr);
    
            Move(SCnGetCall, AGet^.Code1, SizeOf(SCnGetCall));
            AGet^.AddrGet := Pointer(Integer(PropInfo^.GetProc) - Integer(AGet) - 27);
            // 27 为相对跳转指令相对于AGet头部的偏移，包括跳转指令本身
            AGet^.OffSetConvert := $0C; // GetConvert 方法在本类 VMT 中的偏移 $0c
    
            AHooker := TCnMethodHook.Create(PropInfo^.GetProc, AGet);
            AGet^.HookInst1 := AHooker;
            AGet^.HookInst2 := AHooker;
            HookItem.AddHooker(AHooker, PropInfo^.GetProc);
          end;
        end;
    
        if PropInfo^.SetProc <> nil then // 挂接处理 Set 过程
        begin
          if not AProcHooked(PropInfo^.SetProc) then
          begin
            if FEmptyPtr = (THUNK_SIZE div CALL_SIZE) then
              raise ECnAntiCheaterHookException.Create(SCnErrorHookPoolOverflow);
    
            ASet := PCnSetCall(Integer(FHookPool) + FEmptyPtr * CALL_SIZE);
            Inc(FEmptyPtr);
    
            Move(SCnSetCall, ASet^.Code1, SizeOf(SCnSetCall));
            ASet^.AddrSet := Pointer(Integer(PropInfo^.SetProc) - Integer(ASet) - 43);
            // 43 为相对跳转指令相对于ASet头部的偏移，包括跳转指令本身
            ASet^.OffSetConvert := $10; // SetConvert 方法在本类 VMT 中的偏移 $10
    
            AHooker := TCnMethodHook.Create(PropInfo^.SetProc, ASet);
            ASet^.HookInst1 := AHooker;
            ASet^.HookInst2 := AHooker;
            HookItem.AddHooker(AHooker, PropInfo^.SetProc);
          end;
        end;
      end;
    end;
  finally
    FreeMem(PropListPtr);
  end;

  SetSelfClassHooked(Self, True);
end;

class procedure TCnAntiCheater.UnHookSelfClass;
var
  HookItem: TCnClassHookItem;
  I: Integer;
begin
  if not SelfClassHookEnabled(Self) then
    Exit;

  HookItem := GetHookItemFromClassList(Self);
  for I := 0 to HookItem.HookCount - 1 do
    HookItem.HookItems[I].UnhookMethod;

  SetSelfClassHooked(Self, False);
end;

// 此函数用来获取编译后的汇编代码模板
function TCnAntiCheater.MyGetCardinalData: Integer;
begin
  TCnMethodHook($33333333).UnHookMethod;
  Result := GetConvertCardinal(GetData);
  TCnMethodHook($33333333).HookMethod;
end;

// 此函数用来获取编译后的汇编代码模板
procedure TCnAntiCheater.MySetCardinalData(const Value: Integer);
begin
  TCnMethodHook($33333333).UnHookMethod;
  SetData(SetConvertCardinal(Value));
  TCnMethodHook($33333333).HookMethod;
end;

{ TCnClassHookItem }

function TCnClassHookItem.AddHooker(AMethodHook: TCnMethodHook; AProc: Pointer): Integer;
begin
  Result := FHookers.Add(AMethodHook);
  FHookProcs.Add(AProc);
end;

constructor TCnClassHookItem.Create;
begin
  FHookers := TList.Create;
  FHookProcs := TList.Create;
end;

destructor TCnClassHookItem.Destroy;
var
  I: Integer;
begin
  for I := 0 to FHookers.Count - 1 do
    TCnMethodHook(FHookers.Items[I]).Free;
  FHookers.Free;
  FHookProcs.Free;
  inherited;
end;

function TCnClassHookItem.GetHookCount: Integer;
begin
  Result := FHookers.Count;
end;

function TCnClassHookItem.GetHookItems(Index: Integer): TCnMethodHook;
begin
  Result := TCnMethodHook(FHookers.Items[Index]);
end;

function TCnClassHookItem.GetHookProcs(Index: Integer): Pointer;
begin
  Result := FHookProcs.Items[Index];
end;

procedure TCnClassHookItem.SetHookEnabled(const Value: Boolean);
var
  I: Integer;
begin
  if FHookEnabled <> Value then
  begin
    FHookEnabled := Value;
    if not FHookEnabled then
      for I := 0 to FHookers.Count - 1 do
        TCnMethodHook(FHookers.Items[I]).UnhookMethod;
  end;
end;

procedure ClearClassList;
var
  I: Integer;
begin
  if ClassList <> nil then
  begin
    for I := 0 to ClassList.Count - 1 do
      TCnClassHookItem(ClassList.Objects[I]).Free;
    FreeAndNil(ClassList);
  end;
end;

initialization
  FEnableProtect := True;

finalization
  ClearClassList;
  if FHookPool <> nil then
    VirtualFree(FHookPool, 0, MEM_RELEASE);

end.
