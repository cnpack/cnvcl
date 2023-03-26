unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CnValidateImage, ExtCtrls, CnSpin;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Edit2: TEdit;
    CnValidateImage1: TCnValidateImage;
    Label1: TLabel;
    CnSpinEdit1: TCnSpinEdit;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    CnSpinEdit2: TCnSpinEdit;
    Label4: TLabel;
    CnSpinEdit3: TCnSpinEdit;
    Label5: TLabel;
    Label6: TLabel;
    CnSpinEdit4: TCnSpinEdit;
    chkFixStyle: TCheckBox;
    chkFixColor: TCheckBox;
    chkFixPos: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CnSpinEdit1Change(Sender: TObject);
    procedure CnSpinEdit2Change(Sender: TObject);
    procedure CnSpinEdit4Change(Sender: TObject);
    procedure chkFixStyleClick(Sender: TObject);
    procedure chkFixColorClick(Sender: TObject);
    procedure chkFixPosClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Edit2.Text:=CnValidateImage1.Value;
  if not CnValidateImage1.ValidateInput(Edit1.Text) then
  begin
    Edit1.SetFocus;
    Edit1.Clear;
    ShowMessage('Input Mismatch!');
    if CnSpinEdit3.Value>0 then
      if CnValidateImage1.ValidateCount>CnSpinEdit3.Value then
      begin
        ShowMessage('Test Times out, Terminate');
        Application.Terminate;
      end;  
  end
  else
  begin
    ShowMessage('Match Ok!');
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  CnValidateImage1.CaseSensitive:=TCheckBox(Sender).Checked;
end;

procedure TForm1.CnSpinEdit1Change(Sender: TObject);
begin
  CnValidateImage1.FontRandomSeed:=CnSpinEdit1.Value
end;

procedure TForm1.CnSpinEdit2Change(Sender: TObject);
begin
  CnValidateImage1.NoiseCount:=CnSpinEdit2.Value
end;

procedure TForm1.CnSpinEdit4Change(Sender: TObject);
begin
  CnValidateImage1.ValueLength := CnSpinEdit4.Value
end;

procedure TForm1.chkFixStyleClick(Sender: TObject);
begin
  CnValidateImage1.FixStyle := chkFixStyle.Checked;
end;

procedure TForm1.chkFixColorClick(Sender: TObject);
begin
  CnValidateImage1.FixColor := chkFixColor.Checked;
end;

procedure TForm1.chkFixPosClick(Sender: TObject);
begin
  CnValidateImage1.FixPosition := chkFixPos.Checked;
end;

end.
