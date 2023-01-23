program TestStrings;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitStrings in 'UnitStrings.pas' {FormStrings};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormStrings, FormStrings);
  Application.Run;
end.
