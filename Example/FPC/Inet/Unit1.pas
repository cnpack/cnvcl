unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls,
  Windows, Forms, Dialogs, StdCtrls, ComCtrls, CnInetUtils;

type
  TForm1 = class(TForm)
    lbl1: TLabel;
    mmo1: TMemo;
    cbb1: TComboBox;
    btn1: TButton;
    btn2: TButton;
    pb1: TProgressBar;
    btn3: TButton;
    dlgSave1: TSaveDialog;
    btn4: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
  private
    CnInet: TCnInet;
    Aborted: Boolean;
    procedure OnProgress(Sender: TObject; TotalSize, CurrSize: Integer;
      var Abort: Boolean);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  CnInet := TCnInet.Create;
  CnInet.OnProgress := OnProgress;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CnInet.Free;
end;

procedure TForm1.OnProgress(Sender: TObject; TotalSize, CurrSize: Integer;
  var Abort: Boolean);
begin
  if TotalSize > 0 then
  begin
    pb1.Max := TotalSize;
    pb1.Position := CurrSize;
  end;
  Abort := Aborted;
end;

procedure TForm1.btn1Click(Sender: TObject);
var
  E: Cardinal;
begin
  if cbb1.Text <> '' then
  begin
    Aborted := False;
    mmo1.Lines.Text := CnInet.GetString(cbb1.Text);

    if mmo1.Lines.Text = '' then
    begin
      E := GetLastError;
      if E <> 0 then
        ShowMessage('Error Code: ' + IntToStr(E));
    end;
  end;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  if (cbb1.Text <> '') and dlgSave1.Execute then
  begin
    Aborted := False;
    if CnInet.GetFile(cbb1.Text, dlgSave1.FileName) then
      ShowMessage('Get file success!')
    else
      ShowMessage('Get file fail!');
  end;
end;

procedure TForm1.btn4Click(Sender: TObject);
begin
  Aborted := True;
end;

end.
