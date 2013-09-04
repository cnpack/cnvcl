unit uHardwareBreakDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CnHardwareBreakpoint;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CnHardwareBreakpoint1Breakpoint1(
      pException: PExceptionPointers);
    procedure Button6Click(Sender: TObject);
    procedure CnHardwareBreakpoint1Breakpoint2(
      pException: PExceptionPointers);
    procedure CnHardwareBreakpoint1Breakpoint3(
      pException: PExceptionPointers);
    procedure CnHardwareBreakpoint1Breakpoint4(
      pException: PExceptionPointers);
    procedure CnHardwareBreakpoint1HardwareBreakError(ErrorId: Integer;
      Error: Exception; pException: PExceptionPointers);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    CnHardwareBreakpoint1: TCnHardwareBreakpoint;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('这是按钮1');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  ShowMessage('这是按钮2');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  ShowMessage('这是按钮3');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  ShowMessage('这是按钮4');
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  _ButtonClick: TNotifyEvent;
begin
  _ButtonClick := Self.Button1Click;
  CnHardwareBreakpoint1.BreakpointsAdderss1 := dword(@_ButtonClick);

  _ButtonClick := Self.Button2Click;
  CnHardwareBreakpoint1.BreakpointsAdderss2 := dword(@_ButtonClick);

  _ButtonClick := Self.Button3Click;
  CnHardwareBreakpoint1.BreakpointsAdderss3 := dword(@_ButtonClick);

  _ButtonClick := Self.Button4Click;
  CnHardwareBreakpoint1.BreakpointsAdderss4 := dword(@_ButtonClick);
  CnHardwareBreakpoint1.SetBreakpoints;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  CnHardwareBreakpoint1.ClearBreakpoints;
end;

procedure TForm1.CnHardwareBreakpoint1Breakpoint1(
  pException: PExceptionPointers);
begin
  ShowMessage('This is the button for 1');
  Inc(pException.ContextRecord.Esp, 4); //模拟Push ebp
  PDWORD(pException.ContextRecord.Esp)^ := pException.ContextRecord.Ebp; //模拟Push ebp
  Inc(pException.ContextRecord.Eip, 1); //下条指令
end;

procedure TForm1.CnHardwareBreakpoint1Breakpoint2(
  pException: PExceptionPointers);
begin
  ShowMessage('This is the button for 2');
  Inc(pException.ContextRecord.Esp, 4); //模拟Push ebp
  PDWORD(pException.ContextRecord.Esp)^ := pException.ContextRecord.Ebp; //模拟Push ebp
  Inc(pException.ContextRecord.Eip, 1); //下条指令
end;

procedure TForm1.CnHardwareBreakpoint1Breakpoint3(
  pException: PExceptionPointers);
begin
  ShowMessage('This is the button for 3');
  Inc(pException.ContextRecord.Esp, 4); //模拟Push ebp
  PDWORD(pException.ContextRecord.Esp)^ := pException.ContextRecord.Ebp; //模拟Push ebp
  Inc(pException.ContextRecord.Eip, 1); //下条指令
end;

procedure TForm1.CnHardwareBreakpoint1Breakpoint4(
  pException: PExceptionPointers);
var
  _Err: Dword;
begin
  Inc(pException.ContextRecord.Esp, 4); //模拟Push ebp
  PDWORD(pException.ContextRecord.Esp)^ := pException.ContextRecord.Ebp; //模拟Push ebp
  Inc(pException.ContextRecord.Eip, 1); //下条指令
  _Err := pdword($1000)^; //演示读取错误地址
end;

procedure TForm1.CnHardwareBreakpoint1HardwareBreakError(ErrorId: Integer;
  Error: Exception; pException: PExceptionPointers);
begin
  ShowMessage(Format('BreakpointsId:%d 出错！错误信息：%s', [ErrorId, Error.Message]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CnHardwareBreakpoint1 := TCnHardwareBreakpoint.Create(Self);
  CnHardwareBreakpoint1.OnBreakpoint1 := CnHardwareBreakpoint1Breakpoint1;
  CnHardwareBreakpoint1.OnBreakpoint2 := CnHardwareBreakpoint1Breakpoint2;
  CnHardwareBreakpoint1.OnBreakpoint3 := CnHardwareBreakpoint1Breakpoint3;
  CnHardwareBreakpoint1.OnBreakpoint4 := CnHardwareBreakpoint1Breakpoint4;
  CnHardwareBreakpoint1.OnHardwareBreakError := CnHardwareBreakpoint1HardwareBreakError;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CnHardwareBreakpoint1.Free;
end;

end.

