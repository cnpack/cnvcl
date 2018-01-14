unit UnitIntfHook;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TIntfHookForm = class(TForm)
    btnHook: TButton;
    btnUnhook: TButton;
    btnCall1: TButton;
    btnCall2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnHookClick(Sender: TObject);
    procedure btnUnhookClick(Sender: TObject);
    procedure btnCall1Click(Sender: TObject);
    procedure btnCall2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  IntfHookForm: TIntfHookForm;

implementation

{$R *.DFM}

uses
  CnDebug, CnMethodHook;

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

procedure TIntfHookForm.FormCreate(Sender: TObject);
begin
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

procedure TIntfHookForm.FormDestroy(Sender: TObject);
begin
  HookTest2.Free;
  HookTest1.Free;
  TestIntf := nil;
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

end.
