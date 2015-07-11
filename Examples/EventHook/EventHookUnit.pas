unit EventHookUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CnEventHook, StdCtrls;

type
  TFormEventHook = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    Hook, HookMouse: TCnEventHook;
    FControl: TControl;
    procedure ButtonHookClick(Sender: TObject);
    procedure HookMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  public
    { Public declarations }
  end;

var
  FormEventHook: TFormEventHook;

implementation

{$R *.dfm}

type
  TOnClickProc = procedure (Self: TObject; Sender: TObject);

procedure TFormEventHook.Button1Click(Sender: TObject);
begin
  Caption := 'Test';
end;

procedure TFormEventHook.ButtonHookClick(Sender: TObject);
begin
  ShowMessage('Hook Click');

  // To call origin event handler, using:
  if Hook.Trampoline <> nil then
    TOnClickProc(Hook.Trampoline)(Hook.TrampolineData, Sender);
end;

procedure TFormEventHook.FormDestroy(Sender: TObject);
begin
  Hook.Free;
  HookMouse.Free;
end;

procedure TFormEventHook.FormCreate(Sender: TObject);
begin
  Hook := TCnEventHook.Create(Button1, 'OnClick', Self, @TFormEventHook.ButtonHookClick);
  FControl := TControl.Create(Self);
  HookMouse := TCnEventHook.Create(FControl, 'OnMouseMove', Self,
    @TFormEventHook.HookMouseMove);
end;

procedure TFormEventHook.HookMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin

end;

procedure TFormEventHook.Button2Click(Sender: TObject);
begin
  if HookMouse.Hooked then
    ShowMessage('Mouse Event Hooked to a Control.');
end;

end.
