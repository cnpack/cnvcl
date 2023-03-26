program TestStrings;

uses
  Forms,
  UnitStrings in 'UnitStrings.pas' {FormStrings};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormStrings, FormStrings);
  Application.Run;
end.
