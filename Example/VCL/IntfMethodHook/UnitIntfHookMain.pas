unit UnitIntfHookMain;
{
  CnIntfMethodHook 使用示例
  =====================
  本示例演示 TCnIntfMethodHook 的以下用法：

  场景一：通过 MethodIndex 挂钩 stdcall 方法（全版本兼容）
    - 挂钩 ICalc.Add，新方法中先记录日志再调用原始实现

  场景二：通过 MethodIndex 挂钩默认调用约定方法（全版本兼容）
    - 挂钩 ICalc.GetName，新方法直接返回修改后的字符串

  场景三：挂钩后调用原始实现（回调原始方法）
    - 演示 RealMethodAddr 属性的用法

  场景四：多次 Hook/Unhook 切换
    - 演示 HookMethod/UnhookMethod 的反复调用

  场景五（仅 Delphi 2010+）：通过方法名字符串挂钩
    - 使用 CreateByName，接口须有 RTTI（$M+ 或继承 IInvokable）

  注意：
    新方法的第一个参数是实现对象的 Self（TObject），不是接口指针。
    如果方法是 stdcall 方法，须在新方法声明上也加 stdcall，其他调用方式也要对应。
}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnMethodHook, CnIntfMethodHook, ComCtrls;

type
  TIntfHookForm = class(TForm)
    PageControl1: TPageControl;
    ts1: TTabSheet;
    mmoLog: TMemo;
    btnHookByIndex: TButton;
    btnUnhookByIndex: TButton;
    btnHookUnhookToggle: TButton;
    btnHookByName: TButton;
    btnUnhookByName: TButton;
    btnClearLog: TButton;
    btnCallGetName: TButton;
    btnCallAdd: TButton;
    btnHookByVTable: TButton;
    ts2: TTabSheet;
    btnHook: TButton;
    btnUnhook: TButton;
    btnCall1: TButton;
    btnCall2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnHookByIndexClick(Sender: TObject);
    procedure btnUnhookByIndexClick(Sender: TObject);
    procedure btnCallAddClick(Sender: TObject);
    procedure btnCallGetNameClick(Sender: TObject);
    procedure btnHookUnhookToggleClick(Sender: TObject);
    procedure btnHookByNameClick(Sender: TObject);
    procedure btnUnhookByNameClick(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure btnHookClick(Sender: TObject);
    procedure btnUnhookClick(Sender: TObject);
    procedure btnCall1Click(Sender: TObject);
    procedure btnCall2Click(Sender: TObject);
    procedure btnHookByVTableClick(Sender: TObject);
  private

  public

  end;

var
  IntfHookForm: TIntfHookForm;

implementation

{$R *.DFM}

uses
  CnDebug;

type
  ITest = interface
    procedure Test1(A: Integer); stdcall;
    procedure Test2(const S: string);
  end;

{$M+}

  TTestObj = class(TInterfacedObject, ITest)
  private
    FInt: Integer;
  public
    procedure Test1(A: Integer); stdcall;
    procedure Test2(const S: string);
  published
    property Int: Integer read FInt;
  end;

{$M-}

  TITestMethod3Proc = procedure(Self: TObject; A: Integer); stdcall;

  TITestMethod4Proc = procedure(Self: TObject; const S: string);

var
  TestIntf: ITest = nil;
  Test1Proc: Pointer = nil;
  Test2Proc: Pointer = nil;

  HookTest1: TCnMethodHook;
  HookTest2: TCnMethodHook;

procedure MyTest1(Self: TObject; A: Integer); stdcall;
begin
  CnDebugger.LogMsg('MyTest1 Called.');
  CnDebugger.LogObject(Self);

  HookTest1.UnhookMethod;
  try
    TITestMethod3Proc(Test1Proc)(Self, A);
    // 不能再用原来的接口方法调用来调用原方法了
  finally
    HookTest1.HookMethod;
  end;
end;

procedure MyTest2(Self: TObject; const S: string);
begin
  CnDebugger.LogMsg('MyTest2 Called.');
  CnDebugger.LogObject(Self);

  HookTest2.UnhookMethod;
  try
    TITestMethod4Proc(Test2Proc)(Self, S);
    // 不能再用原来的接口方法调用来调用原方法了
  finally
    HookTest2.HookMethod;
  end;
end;

{ TTestObj }

procedure TTestObj.Test1(A: Integer);
begin
  CnDebugger.LogMsg('TTestObj.Test1: ' + IntToStr(A));
  FInt := A;
end;

procedure TTestObj.Test2(const S: string);
begin
  CnDebugger.LogMsg('TTestObj.Test2: ' + S);
  FInt := StrToIntDef(S, 0);
end;

//==============================================================================
// 辅助函数
//==============================================================================

// 将指针格式化为十六进制字符串，兼容 32/64 位
function PtrToHex(P: Pointer): string;
begin
{$IFDEF CPU64BITS}
  Result := IntToHex(Int64(P), 16);
{$ELSE}
  Result := IntToHex(Integer(P), 8);
{$ENDIF}
end;

//==============================================================================
// 接口定义
// 注意：要通过方法名字符串挂钩，接口须在 {$M+} 范围内声明（或继承 IInvokable）
//==============================================================================

{$M+}
type
  // ICalc：演示用计算器接口
  // vtable 布局：slot 0=QueryInterface, 1=_AddRef, 2=_Release,
  //              3=Add(stdcall), 4=GetName, 5=SetValue
  ICalc = interface(IUnknown)
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function Add(A, B: Integer): Integer; stdcall;
    function GetName: string;
    procedure SetValue(const AValue: Integer);
  end;
{$M-}

//==============================================================================
// 接口实现类
//==============================================================================

type
  TCalcImpl = class(TInterfacedObject, ICalc)
  private
    FValue: Integer;
  public
    function Add(A, B: Integer): Integer; stdcall;
    function GetName: string;
    procedure SetValue(const AValue: Integer);
  end;

function TCalcImpl.Add(A, B: Integer): Integer;
begin
  Result := A + B;
  IntfHookForm.mmoLog.Lines.Add(
    Format('[原始 TCalcImpl.Add] %d + %d = %d', [A, B, Result]));
end;

function TCalcImpl.GetName: string;
begin
  Result := 'TCalcImpl';
  IntfHookForm.mmoLog.Lines.Add('[原始 TCalcImpl.GetName] 返回: ' + Result);
end;

procedure TCalcImpl.SetValue(const AValue: Integer);
begin
  FValue := AValue;
  IntfHookForm.mmoLog.Lines.Add(
    Format('[原始 TCalcImpl.SetValue] FValue := %d', [AValue]));
end;

//==============================================================================
// 新方法（Hook 替换函数）
// 重要：第一个参数是实现对象的 Self（TObject），不是接口指针
//==============================================================================

// 场景一：替换 ICalc.Add（stdcall，MethodIndex=3）
// 新方法中先记录日志，再通过 RealMethodAddr 调用原始实现
type
  TCalcAddFunc = function(Self: TObject; A, B: Integer): Integer; stdcall;

var
  HookAdd: TCnIntfMethodHook = nil;

function MyAdd(Self: TObject; A, B: Integer): Integer; stdcall;
var
  OrigResult: Integer;
begin
  IntfHookForm.mmoLog.Lines.Add(
    Format('[MyAdd Hook] 拦截到 Add(%d, %d)', [A, B]));

  // 通过 RealMethodAddr 回调原始实现（需先 Unhook 再调用，避免递归）
  HookAdd.UnhookMethod;
  try
    OrigResult := TCalcAddFunc(HookAdd.RealMethodAddr)(Self, A, B);
  finally
    HookAdd.HookMethod;
  end;

  // 在原始结果基础上加 1000，以便验证 Hook 生效
  Result := OrigResult + 1000;
  IntfHookForm.mmoLog.Lines.Add(
    Format('[MyAdd Hook] 修改结果: %d -> %d', [OrigResult, Result]));
end;

// 场景二：替换 ICalc.GetName（默认调用约定，MethodIndex=4）
// 新方法直接返回修改后的字符串，不调用原始实现
type
  TCalcGetNameFunc = function(Self: TObject): string;

var
  HookGetName: TCnIntfMethodHook = nil;

function MyGetName(Self: TObject): string;
begin
  Result := 'Hooked_' + TCalcImpl(Self).ClassName;
  IntfHookForm.mmoLog.Lines.Add('[MyGetName Hook] 返回: ' + Result);
end;

// 场景三：替换 ICalc.SetValue（默认调用约定，MethodIndex=5）
// 演示 Hook/Unhook 切换
type
  TCalcSetValueProc = procedure(Self: TObject; const AValue: Integer);

var
  HookSetValue: TCnIntfMethodHook = nil;

procedure MySetValue(Self: TObject; const AValue: Integer);
begin
  IntfHookForm.mmoLog.Lines.Add(
    Format('[MySetValue Hook] 拦截到 SetValue(%d)，放大 10 倍后传入', [AValue]));
  HookSetValue.UnhookMethod;
  try
    TCalcSetValueProc(HookSetValue.RealMethodAddr)(Self, AValue * 10);
  finally
    HookSetValue.HookMethod;
  end;
end;

type
  TCalcAddVTableFunc = function(Self: TObject; A, B: Integer): Integer; stdcall;

var
  HookAddByVTable: TCnIntfMethodHook = nil;

function MyAddByVTable(Self: TObject; A, B: Integer): Integer; stdcall;
var
  OrigResult: Integer;
begin
  IntfHookForm.mmoLog.Lines.Add(
    Format('[MyAddByVTable Hook] 进入 Add(%d, %d)', [A, B]));

  HookAddByVTable.UnhookMethod;
  try
    OrigResult := TCalcAddVTableFunc(HookAddByVTable.RealMethodAddr)(Self, A, B);
  finally
    HookAddByVTable.HookMethod;
  end;

  Result := OrigResult + 2000;
  IntfHookForm.mmoLog.Lines.Add(
    Format('[MyAddByVTable Hook] 返回值: %d -> %d', [OrigResult, Result]));
end;

{$IFDEF SUPPORT_ENHANCED_RTTI}
// 场景五：通过方法名字符串挂钩 ICalc.Add（仅 Delphi 2010+）
var
  HookAddByName: TCnIntfMethodHook = nil;

function MyAddByName(Self: TObject; A, B: Integer): Integer; stdcall;
begin
  Result := (A + B) * 2;  // 结果乘以 2，与场景一区分
  IntfHookForm.mmoLog.Lines.Add(
    Format('[MyAddByName Hook] Add(%d,%d) 结果乘 2 = %d', [A, B, Result]));
end;
{$ENDIF}

//==============================================================================
// 全局接口实例
//==============================================================================

var
  GCalc: ICalc = nil;

//==============================================================================
// 窗体事件
//==============================================================================

procedure TIntfHookForm.FormCreate(Sender: TObject);
begin
  GCalc := TCalcImpl.Create;
  mmoLog.Lines.Add('接口实例已创建：TCalcImpl');
  mmoLog.Lines.Add('  ICalc.Add      vtable slot = 3，地址 = $' +
    PtrToHex(GetInterfaceMethodAddress(GCalc, 3)));
  mmoLog.Lines.Add('  ICalc.GetName  vtable slot = 4，地址 = $' +
    PtrToHex(GetInterfaceMethodAddress(GCalc, 4)));
  mmoLog.Lines.Add('  ICalc.SetValue vtable slot = 5，地址 = $' +
    PtrToHex(GetInterfaceMethodAddress(GCalc, 5)));
  mmoLog.Lines.Add('');

  // Manually Test
  TestIntf := TTestObj.Create;

  Test1Proc := GetInterfaceMethodAddress(TestIntf, 3);
  CnDebugger.LogPointer(Test1Proc, 'Intf Test1');
  CnDebugger.LogPointer(@TTestObj.Test1, 'TTestObj.Test1');

  Test2Proc := GetInterfaceMethodAddress(TestIntf, 4);
  CnDebugger.LogPointer(Test2Proc, 'Intf Test2');
  CnDebugger.LogPointer(@TTestObj.Test2, 'TTestObj.Test2');

  HookTest1 := TCnMethodHook.Create(Test1Proc, @MyTest1, False);
  HookTest2 := TCnMethodHook.Create(Test2Proc, @MyTest2, False);
end;

procedure TIntfHookForm.FormDestroy(Sender: TObject);
begin
{$IFDEF SUPPORT_ENHANCED_RTTI}
  HookAddByName.Free;
  HookAddByName := nil;
{$ENDIF}

  HookTest2.Free;
  HookTest1.Free;
  TestIntf := nil;

  HookSetValue.Free;
  HookSetValue := nil;
  HookGetName.Free;
  HookGetName := nil;
  HookAddByVTable.Free;
  HookAddByVTable := nil;
  HookAdd.Free;
  HookAdd := nil;
  GCalc := nil;
end;

// 场景一+二+三：通过 MethodIndex 挂钩三个方法
procedure TIntfHookForm.btnHookByIndexClick(Sender: TObject);
begin
  if HookAdd <> nil then
  begin
    ShowMessage('已经挂钩，请先取消挂钩。');
    Exit;
  end;

  // 挂钩 Add（slot 3，stdcall）
  HookAdd := TCnIntfMethodHook.Create(GCalc, 3, @MyAdd);
  // 挂钩 GetName（slot 4，默认调用约定）
  HookGetName := TCnIntfMethodHook.Create(GCalc, 4, @MyGetName);
  // 挂钩 SetValue（slot 5，默认调用约定）
  HookSetValue := TCnIntfMethodHook.Create(GCalc, 5, @MySetValue);

  mmoLog.Lines.Add('=== 通过 MethodIndex 挂钩成功 ===');
  mmoLog.Lines.Add('  HookAdd.RealMethodAddr     = $' +
    PtrToHex(HookAdd.RealMethodAddr));
  mmoLog.Lines.Add('  HookGetName.RealMethodAddr = $' +
    PtrToHex(HookGetName.RealMethodAddr));
  mmoLog.Lines.Add('');
end;

procedure TIntfHookForm.btnUnhookByIndexClick(Sender: TObject);
begin
  if HookAdd = nil then
  begin
    ShowMessage('尚未挂钩。');
    Exit;
  end;
  HookSetValue.Free; HookSetValue := nil;
  HookGetName.Free;  HookGetName := nil;
  HookAdd.Free;      HookAdd := nil;
  mmoLog.Lines.Add('=== 通过 MethodIndex 取消挂钩 ===');
  mmoLog.Lines.Add('');
end;

// 调用 ICalc.Add，验证 Hook 效果
procedure TIntfHookForm.btnCallAddClick(Sender: TObject);
var
  R: Integer;
begin
  R := GCalc.Add(10, 20);
  mmoLog.Lines.Add(Format('>>> GCalc.Add(10, 20) 返回: %d', [R]));
  mmoLog.Lines.Add('');
end;

// 调用 ICalc.GetName 和 SetValue，验证 Hook 效果
procedure TIntfHookForm.btnCallGetNameClick(Sender: TObject);
begin
  mmoLog.Lines.Add('>>> GCalc.GetName = ' + GCalc.GetName);
  GCalc.SetValue(5);
  mmoLog.Lines.Add('');
end;

// 场景四：反复切换 HookAdd 的挂钩状态
procedure TIntfHookForm.btnHookUnhookToggleClick(Sender: TObject);
begin
  if HookAdd = nil then
  begin
    ShowMessage('请先点击"通过 Index 挂钩"创建 Hook 对象。');
    Exit;
  end;

  if HookAdd.Hooked then
  begin
    HookAdd.UnhookMethod;
    mmoLog.Lines.Add('--- HookAdd 已临时取消挂钩 ---');
  end
  else
  begin
    HookAdd.HookMethod;
    mmoLog.Lines.Add('--- HookAdd 已重新挂钩 ---');
  end;
  mmoLog.Lines.Add('');
end;

// 场景五：通过方法名字符串挂钩（Delphi 2010+）
procedure TIntfHookForm.btnHookByNameClick(Sender: TObject);
begin
{$IFDEF SUPPORT_ENHANCED_RTTI}
  if HookAddByName <> nil then
  begin
    ShowMessage('已经通过名称挂钩，请先取消。');
    Exit;
  end;
  // 接口须有 RTTI（已在 {$M+} 范围内声明）
  HookAddByName := TCnIntfMethodHook.CreateByName(
    GCalc, TypeInfo(ICalc), 'Add', @MyAddByName);
  mmoLog.Lines.Add('=== 通过方法名 "Add" 挂钩成功（RTTI 路径）===');
  mmoLog.Lines.Add('  HookAddByName.RealMethodAddr = $' +
    PtrToHex(HookAddByName.RealMethodAddr));
  mmoLog.Lines.Add('');
{$ELSE}
  ShowMessage('当前 Delphi 版本不支持 Extended RTTI，请使用 MethodIndex 方式。');
{$ENDIF}
end;

procedure TIntfHookForm.btnUnhookByNameClick(Sender: TObject);
begin
{$IFDEF SUPPORT_ENHANCED_RTTI}
  if HookAddByName = nil then
  begin
    ShowMessage('尚未通过名称挂钩。');
    Exit;
  end;
  HookAddByName.Free;
  HookAddByName := nil;
  mmoLog.Lines.Add('=== 通过方法名取消挂钩 ===');
  mmoLog.Lines.Add('');
{$ELSE}
  ShowMessage('当前 Delphi 版本不支持 Extended RTTI。');
{$ENDIF}
end;

procedure TIntfHookForm.btnClearLogClick(Sender: TObject);
begin
  mmoLog.Clear;
end;

procedure TIntfHookForm.btnHookClick(Sender: TObject);
begin
  if HookTest1.Hooked then
  begin
    ShowMessage('Already Hookd, Do Nothing.');
    Exit;
  end;

  HookTest1.HookMethod;
  HookTest2.HookMethod;
  ShowMessage('Hooked Success');
end;

procedure TIntfHookForm.btnUnhookClick(Sender: TObject);
begin
  if not HookTest1.Hooked then
  begin
    ShowMessage('Not Hookd, Do Nothing.');
    Exit;
  end;

  HookTest1.UnhookMethod;
  HookTest2.UnhookMethod;
  ShowMessage('Unhooked Success');
end;

procedure TIntfHookForm.btnCall1Click(Sender: TObject);
begin
  TestIntf.Test1(333);
end;

procedure TIntfHookForm.btnCall2Click(Sender: TObject);
begin
  TestIntf.Test2('444');
end;

procedure TIntfHookForm.btnHookByVTableClick(Sender: TObject);
begin
  if HookAddByVTable = nil then
  begin
    if (HookAdd <> nil) or (HookGetName <> nil) or (HookSetValue <> nil) then
    begin
      ShowMessage('已有 Hook by Index 了，VTable Hook 没法实施');
      Exit;
    end;
{$IFDEF SUPPORT_ENHANCED_RTTI}
    if HookAddByName <> nil then
    begin
      ShowMessage('已有 Hook by Name 了，VTable Hook 没法实施');
      Exit;
    end;
{$ENDIF}

    HookAddByVTable := TCnIntfMethodHook.CreateAtVirtualTable(GCalc, 3, @MyAddByVTable);
    mmoLog.Lines.Add('=== 开始 CreateAtVirtualTable ===');
    mmoLog.Lines.Add('  HookAddByVTable.RealMethodAddr = $' +
      PtrToHex(HookAddByVTable.RealMethodAddr));
    mmoLog.Lines.Add('  调用 "Call Add(10, 20)" 检查是否增加 2000');
    mmoLog.Lines.Add('');
  end
  else
  begin
    HookAddByVTable.Free;
    HookAddByVTable := nil;
    mmoLog.Lines.Add('=== 取消 CreateAtVirtualTable 了 ===');
    mmoLog.Lines.Add('');
  end;
end;

end.
