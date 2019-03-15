unit UnitIDCard;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFormIDCard = class(TForm)
    lblIDCard: TLabel;
    edtIDCard: TEdit;
    btnCheck: TButton;
    procedure btnCheckClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormIDCard: TFormIDCard;

implementation

uses
  CnCommon;

{$R *.DFM}

procedure TFormIDCard.btnCheckClick(Sender: TObject);
begin
  if CheckChineseIDCardNumber(edtIDCard.Text) then
    ShowMessage('Check OK')
  else
    ShowMessage('Check Failed');
end;

end.
