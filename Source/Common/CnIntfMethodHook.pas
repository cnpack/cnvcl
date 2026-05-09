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

unit CnIntfMethodHook;
{ |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：接口方法挂钩单元
* 单元作者：CnPack 开发组
* 备    注：本单元用于挂钩（Hook）接口实例的指定方法。
*
*           InlineHook 支持两种方式定位目标方法：
*           1. 直接指定 MethodIndex（vtable 槽索引，从 0 开始，
*              0/1/2 分别对应 QueryInterface/_AddRef/_Release）。
*           2. 通过接口的 TypeInfo 加方法名字符串自动查找 MethodIndex，
*              需要 Delphi 2010 及以上版本，且接口须有 RTTI 信息
*              （在 $M+ 范围内声明，或继承自 IInvokable）。
*
*           挂钩原理：
*           Delphi 接口的 vtable 中每个槽存放一个 stub 函数地址，
*           该 stub 负责修正 Self 指针偏移后跳转到真实实现。
*           本单元通过 CnMethodHook 中已有的 GetInterfaceMethodAddress
*           解析 stub，取得真实实现地址，再用 TCnMethodHook 完成挂钩。
*
*           注意：
*           - 同名 overload 方法只能匹配第一个，需用 MethodIndex 重载区分。
*           - 接口无 RTTI 时（老 Delphi 或未加 $M+），只能用 MethodIndex。
*           - 新方法（ANewMethod）的调用约定和参数须与原方法完全一致，
*             且第一个隐含参数为实现对象的 Self（非接口指针）。
*
*           同时 CreateAtVirtualTable 方法支持更改指针表方式的 Hook，不破坏函数体
*
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：Delphi 5 或以上版本
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2026.05.09
*               加入更改指针表方式的 Hook
*           2026.03.31
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, TypInfo, CnMethodHook;

type
  ECnIntfHookException = class(Exception);
  {* 接口方法挂钩相关异常类 }

  TCnIntfMethodHook = class(TObject)
  {* 接口方法挂钩类。
     可修改接口方法表的入口地址实现挂钩，
     也可通过修改接口 vtable 对应 stub 函数入口的跳转目标，实现对接口方法的挂钩。
     后者内部委托 TCnMethodHook 完成实际的代码补丁工作。}
  private
    FVirtualTable: Pointer;
    FVirtualTableIndex: Integer;
    FVirtualTableMode: Boolean;
    FMethodHook: TCnMethodHook;
    FHooked: Boolean;
    FRealMethodAddr: Pointer;  // 真实实现地址（stub 解析后）
    FNewMethod: Pointer;       // 新方法地址
  public
    constructor Create(const AIntf: IUnknown; AMethodIndex: Integer;
      ANewMethod: Pointer; DefaultHook: Boolean = True);
    {* 通过 vtable 槽索引创建挂钩。

       AIntf        - 接口实例
       AMethodIndex - vtable 槽索引（从 0 开始，IUnknown 占 0/1/2）
       ANewMethod   - 替换的新方法地址
       DefaultHook  - 是否立即挂钩，默认为 True }

    constructor CreateAtVirtualTable(const AIntf; AMethodIndex: Integer;
      ANewMethod: Pointer; DefaultHook: Boolean = True);
    {* 通过 vtable 槽索引覆盖指针的方式实现挂钩，而并非代码体内部 Hook。

       AIntf        - 接口实例
       AMethodIndex - vtable 槽索引（从 0 开始，IUnknown 占 0/1/2）
       ANewMethod   - 替换的新方法地址
       DefaultHook  - 是否立即挂钩，默认为 True }

{$IFDEF SUPPORT_ENHANCED_RTTI}

    constructor CreateByName(const AIntf: IUnknown; AIntfTypeInfo: PTypeInfo;
      const AMethodName: string; ANewMethod: Pointer; DefaultHook: Boolean = True);
    {* 通过接口 TypeInfo 和方法名字符串创建挂钩（需 Delphi 2010+，且接口须有 RTTI）。

       AIntf         - 接口实例
       AIntfTypeInfo - 接口的 TypeInfo，如 TypeInfo(IMyInterface)
       AMethodName   - 方法名称字符串（不区分大小写）
       ANewMethod    - 替换的新方法地址
       DefaultHook   - 是否立即挂钩，默认为 True }

    constructor CreateAtVirtualTableByName(const AIntf: IUnknown; AIntfTypeInfo: PTypeInfo;
      const AMethodName: string; ANewMethod: Pointer; DefaultHook: Boolean = True);
    {* 通过接口 TypeInfo 和方法名字符串创建覆盖指针方式的挂钩（需 Delphi 2010+，且接口须有 RTTI）。

       AIntf         - 接口实例
       AIntfTypeInfo - 接口的 TypeInfo，如 TypeInfo(IMyInterface)
       AMethodName   - 方法名称字符串（不区分大小写）
       ANewMethod    - 替换的新方法地址
       DefaultHook   - 是否立即挂钩，默认为 True }

{$ENDIF}

    destructor Destroy; override;

    procedure HookMethod;
    {* 执行挂钩 }
    procedure UnhookMethod;
    {* 取消挂钩 }

    property Hooked: Boolean read FHooked;
    {* 是否已挂钩 }
    property RealMethodAddr: Pointer read FRealMethodAddr;
    {* 被挂钩方法的真实实现地址（可用于在新方法中调用原始实现） }

    property VirtualTableMode: Boolean read FVirtualTableMode;
    {* 使用接口表指针更新，而并非代码体内部 Hook}
  end;

{$IFDEF SUPPORT_ENHANCED_RTTI}

function CnGetIntfMethodIndexByName(AIntfTypeInfo: PTypeInfo;
  const AMethodName: string): Integer;
{* 通过接口 TypeInfo 和方法名查找 vtable 槽索引。
   返回 -1 表示未找到或接口无 RTTI。
   注意：索引从 0 开始，IUnknown 的 3 个方法占 0/1/2，
   接口自身声明的方法从 3 开始（若直接继承 IInterface/IUnknown）。}

{$ENDIF}

implementation

{$IFDEF SUPPORT_ENHANCED_RTTI}
uses
  Rtti;
{$ENDIF}

resourcestring
  SCnIntfHookNilIntf        = 'Interface instance cannot be nil.';
  SCnIntfHookInvalidIndex   = 'Invalid MethodIndex: %d.';
  SCnIntfHookNoRealAddr     = 'Cannot resolve real method address for MethodIndex %d.';
  SCnIntfHookMethodNotFound = 'Method "%s" not found in interface RTTI. ' +
    'Ensure the interface is declared with {$M+} or inherits from IInvokable.';

type
  TCnVTable = array[0..999] of Pointer;
  PCnVTable = ^TCnVTable;
  PPCnVTable = ^PCnVTable;

//==============================================================================
// 内部辅助函数
//==============================================================================

{ 从接口实例和 vtable 槽索引解析真实实现地址 }
function InternalGetRealAddr(const AIntf: IUnknown;
  AMethodIndex: Integer): Pointer;
begin
  Result := GetInterfaceMethodAddress(AIntf, AMethodIndex);
end;

//==============================================================================
// RTTI 路径：通过方法名查找 vtable 槽索引（仅 Delphi 2010 或以上有效）
//==============================================================================

{$IFDEF SUPPORT_ENHANCED_RTTI}

function CnGetIntfMethodIndexByName(AIntfTypeInfo: PTypeInfo;
  const AMethodName: string): Integer;
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Methods: TArray<TRttiMethod>;
  I: Integer;
begin
  Result := -1;
  if AIntfTypeInfo = nil then
    Exit;
  if AIntfTypeInfo^.Kind <> tkInterface then
    Exit;

  // TRttiContext 是带方法的 record，Create/Free 管理内部引用计数
  Ctx := TRttiContext.Create;
  RttiType := Ctx.GetType(AIntfTypeInfo);
  if RttiType = nil then
  begin
    Ctx.Free;
    Exit;
  end;

  // GetMethods 返回包含继承链的所有方法，顺序与 vtable 一致（父接口方法在前）
  // IInterface/IUnknown 的 3 个方法（QueryInterface/_AddRef/_Release）不在
  // RTTI 方法列表中，vtable 中它们占 slot 0/1/2，所以 RTTI 方法列表的第 I 项
  // 对应 vtable slot (3 + I)
  Methods := RttiType.GetMethods;
  for I := 0 to High(Methods) do
  begin
    if SameText(Methods[I].Name, AMethodName) then
    begin
      Result := 3 + I;
      Ctx.Free;
      Exit;
    end;
  end;
  Ctx.Free;
end;

{$ENDIF}

//==============================================================================
// TCnIntfMethodHook
//==============================================================================

constructor TCnIntfMethodHook.Create(const AIntf: IUnknown; AMethodIndex: Integer;
  ANewMethod: Pointer; DefaultHook: Boolean);
begin
  inherited Create;
  FHooked := False;
  FNewMethod := ANewMethod;

  if AIntf = nil then
    raise ECnIntfHookException.Create(SCnIntfHookNilIntf);
  if AMethodIndex < 0 then
    raise ECnIntfHookException.CreateFmt(SCnIntfHookInvalidIndex, [AMethodIndex]);

  FRealMethodAddr := InternalGetRealAddr(AIntf, AMethodIndex);
  if FRealMethodAddr = nil then
    raise ECnIntfHookException.CreateFmt(SCnIntfHookNoRealAddr, [AMethodIndex]);

  FMethodHook := TCnMethodHook.Create(FRealMethodAddr, FNewMethod, False);

  if DefaultHook then
    HookMethod;
end;

constructor TCnIntfMethodHook.CreateAtVirtualTable(const AIntf; AMethodIndex: Integer;
  ANewMethod: Pointer; DefaultHook: Boolean);
begin
  inherited Create;
  FHooked := False;
  FNewMethod := ANewMethod;

  if Pointer(AIntf) = nil then
    raise ECnIntfHookException.Create(SCnIntfHookNilIntf);
  if AMethodIndex < 0 then
    raise ECnIntfHookException.CreateFmt(SCnIntfHookInvalidIndex, [AMethodIndex]);

  FVirtualTableIndex := AMethodIndex;
  FVirtualTable := Pointer(AIntf);
  FVirtualTableMode := True;
  if DefaultHook then
    HookMethod;
end;

{$IFDEF SUPPORT_ENHANCED_RTTI}

constructor TCnIntfMethodHook.CreateByName(const AIntf: IUnknown;
  AIntfTypeInfo: PTypeInfo; const AMethodName: string; ANewMethod: Pointer;
  DefaultHook: Boolean);
var
  Idx: Integer;
begin
  inherited Create;
  FHooked := False;
  FNewMethod := ANewMethod;

  if AIntf = nil then
    raise ECnIntfHookException.Create(SCnIntfHookNilIntf);

  Idx := CnGetIntfMethodIndexByName(AIntfTypeInfo, AMethodName);
  if Idx < 0 then
    raise ECnIntfHookException.CreateFmt(SCnIntfHookMethodNotFound, [AMethodName]);

  FRealMethodAddr := InternalGetRealAddr(AIntf, Idx);
  if FRealMethodAddr = nil then
    raise ECnIntfHookException.CreateFmt(SCnIntfHookNoRealAddr, [Idx]);

  FMethodHook := TCnMethodHook.Create(FRealMethodAddr, FNewMethod, False);

  if DefaultHook then
    HookMethod;
end;

constructor TCnIntfMethodHook.CreateAtVirtualTableByName(const AIntf: IUnknown;
  AIntfTypeInfo: PTypeInfo; const AMethodName: string; ANewMethod: Pointer;
  DefaultHook: Boolean);
var
  Idx: Integer;
begin
  inherited Create;
  FHooked := False;
  FNewMethod := ANewMethod;

  if AIntf = nil then
    raise ECnIntfHookException.Create(SCnIntfHookNilIntf);

  Idx := CnGetIntfMethodIndexByName(AIntfTypeInfo, AMethodName);
  if Idx < 0 then
    raise ECnIntfHookException.CreateFmt(SCnIntfHookMethodNotFound, [AMethodName]);

  // TODO: 用 RTTI 根据方法名 AMethodName 获取 AIntf 的其 AMethodIndex
  // 并赋值给 FVirtualTableIndex，如有 overload 方法则只返回第一个

  FVirtualTable := Pointer(AIntf);
  FVirtualTableMode := True;

  if DefaultHook then
    HookMethod;
end;

{$ENDIF}

destructor TCnIntfMethodHook.Destroy;
begin
  if FHooked then
    UnhookMethod;
  FMethodHook.Free;
  inherited;
end;

procedure TCnIntfMethodHook.HookMethod;
var
  OP: DWORD;
begin
  if FHooked then Exit;

  if FVirtualTableMode then
  begin
    FRealMethodAddr := PPCnVTable(FVirtualTable)^^[FVirtualTableIndex];
    VirtualProtect(@PPCnVTable(FVirtualTable)^^[FVirtualTableIndex], SizeOf(Pointer),
      PAGE_EXECUTE_READWRITE, OP);

    PPCnVTable(FVirtualTable)^^[FVirtualTableIndex] := FNewMethod;
    VirtualProtect(@PPCnVTable(FVirtualTable)^^[FVirtualTableIndex], SizeOf(Pointer),
      OP, OP);
  end
  else
    FMethodHook.HookMethod;

  FHooked := True;
end;

procedure TCnIntfMethodHook.UnhookMethod;
var
  OP: DWORD;
begin
  if not FHooked then Exit;

  if FVirtualTableMode then
  begin
    VirtualProtect(@PPCnVTable(FVirtualTable)^^[FVirtualTableIndex], SizeOf(Pointer),
      PAGE_EXECUTE_READWRITE, OP);

    PPCnVTable(FVirtualTable)^^[FVirtualTableIndex] := FRealMethodAddr;
    VirtualProtect(@PPCnVTable(FVirtualTable)^^[FVirtualTableIndex], SizeOf(Pointer),
      OP, OP);
  end
  else
    FMethodHook.UnhookMethod;

  FHooked := False;
end;

end.
