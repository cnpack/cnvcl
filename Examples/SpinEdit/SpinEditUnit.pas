unit SpinEditUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin;

type
  TFormSpin = class(TForm)
    btnCreate: TButton;
    SpinEdit1: TSpinEdit;
    procedure btnCreateClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSpin: TFormSpin;

implementation

uses CnSpin;

{$R *.DFM}

procedure TFormSpin.btnCreateClick(Sender: TObject);
var
  Spin: TCnSpinEdit;
begin
  Spin := TCnSpinEdit.Create(Self);
  Spin.Parent := Self;
  Spin.Left := 200;
  Spin.Top := 200;
end;

end.
