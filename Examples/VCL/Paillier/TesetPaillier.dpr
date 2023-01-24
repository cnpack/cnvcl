program TesetPaillier;

uses
  Forms,
  UnitPaillier in 'UnitPaillier.pas' {FormPaillier},
  CnPaillier in '..\..\..\Source\Crypto\CnPaillier.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormPaillier, FormPaillier);
  Application.Run;
end.
