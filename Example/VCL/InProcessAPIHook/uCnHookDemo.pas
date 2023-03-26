unit uCnHookDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CnInProcessAPIHook;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    function CnHookAPI1APIHookProc(const Params: array of Pointer): Cardinal;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FCnAPIHook: TCnInProcessAPIHook;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
 MessageBox(Handle, PChar('MessageBox API'), PChar('HOOK Caption'), MB_OK + MB_ICONINFORMATION);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 FCnAPIHook.Active := True;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
 FCnAPIHook.Active := False;
end;

function TForm1.CnHookAPI1APIHookProc(const Params: array of Pointer): Cardinal;
begin
  Memo1.Lines.Add(IntToStr(Length(Params)));
  Memo1.Lines.Add(Format('Hook: Handle:%x,Text:%s,Caption:%s,Type:%d',
    [Dword(Params[0]),PChar(Params[1]),PChar(Params[2]),dword(Params[3])]))
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCnAPIHook := TCnInProcessAPIHook.Create(Self);
  FCnAPIHook.DllFunction := 'MessageBoxA';
  FCnAPIHook.DllName := 'User32.dll';
  FCnAPIHook.ParamCount := 4;
  FCnAPIHook.Mutex := True;
  FCnAPIHook.RestoreWhenOnHook := True;
  FCnAPIHook.OnAPIHookProc := CnHookAPI1APIHookProc;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FCnAPIHook.Free;
end;

end.
