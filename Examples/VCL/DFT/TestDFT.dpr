program TestDFT;

uses
  Forms,
  UnitDFT in 'UnitDFT.pas' {FormDFT},
  CnDFT in '..\..\..\Source\Crypto\CnDFT.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormDFT, FormDFT);
  Application.Run;
end.
