unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, CnHint;

type
  TForm1 = class(TForm)
    lbl1: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    RadioGroup1: TRadioGroup;
    bvl1: TBevel;
    RadioGroup2: TRadioGroup;
    lbl2: TLabel;
    Edit2: TEdit;
    lbl3: TLabel;
    edt1: TEdit;
    btn1: TSpeedButton;
    dlgOpen1: TOpenDialog;
    pnl1: TPanel;
    C: TCnHint;
    Cw: TCnHintWindow;
    lbl4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RadioGroup2Click(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure edt1KeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  C.HintStyle := hsBalloonHint;
  C.HintPosition := hpDownLeft;

  cw.HintPosition := hpUpRight;

  RadioGroup1.ItemIndex := Integer(C.HintPosition);
  RadioGroup2.ItemIndex := Integer(C.HintStyle);
  Edit2.Text := C.Title;
end;

procedure TForm1.FormClick(Sender: TObject);
begin
  cw.ActivateHintFromPos(Mouse.CursorPos, '点击而出的 Hint 内容', '点击出 Hint 的 Title');
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  C.HintPosition := THintPosition(RadioGroup1.ItemIndex);
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
  C.HintStyle := THintStyle(RadioGroup2.ItemIndex);
end;

procedure TForm1.Edit2Change(Sender: TObject);
begin
  C.Title := Edit2.Text;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  if dlgOpen1.Execute then
  begin
    edt1.Text := dlgOpen1.FileName;
    C.Glyph.LoadFromFile(dlgOpen1.FileName);
  end;
end;

procedure TForm1.edt1KeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    C.Glyph.LoadFromFile(edt1.Text);
end;

end.
