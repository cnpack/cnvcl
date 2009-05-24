unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CnFloatConvert, StdCtrls;

type
  TForm1 = class(TForm)
   Edit1: TEdit;
   CheckBox1: TCheckBox;
   CheckBox2: TCheckBox;
   Button1: TButton;
   Label1: TLabel;
   rdoBin: TRadioButton;
   rdoOct: TRadioButton;
   rdoHex: TRadioButton;
   procedure Button1Click(Sender: TObject);
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
  if rdoBin.Checked then
    ShowMessage(FloatDecimalToBinExtended(StrToFloat(Edit1.Text), CheckBox1.Checked,
      CheckBox2.Checked))
  else if rdoOct.Checked then
    ShowMessage(FloatDecimalToOctExtended(StrToFloat(Edit1.Text), CheckBox1.Checked,
      CheckBox2.Checked))
  else
    ShowMessage(FloatDecimalToHexExtended(StrToFloat(Edit1.Text), CheckBox1.Checked,
      CheckBox2.Checked));
end;

end.
