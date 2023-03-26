program FuzzyMatch;

uses
  Forms,
  UnitMatch in 'UnitMatch.pas' {FormFuzzy},
  CnStrings in '..\..\..\Source\Common\CnStrings.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormFuzzy, FormFuzzy);
  Application.Run;
end.
