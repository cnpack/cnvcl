unit EventHookUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CnEventHook, StdCtrls;

type
  TFormEventHook = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    Hook: TCnEventHook;
    procedure ButtonHookClick(Sender: TObject);
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
end;

procedure TFormEventHook.FormCreate(Sender: TObject);
begin
  Hook := TCnEventHook.Create(Button1, 'OnClick', Self, @TFormEventHook.ButtonHookClick);
end;

end.
