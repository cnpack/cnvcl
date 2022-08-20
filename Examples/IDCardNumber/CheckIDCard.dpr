program CheckIDCard;

uses
  Forms,
  UnitIDCard in 'UnitIDCard.pas' {FormIDCard},
  CnCommon in '..\..\Source\Common\CnCommon.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormIDCard, FormIDCard);
  Application.Run;
end.
