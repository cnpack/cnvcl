program TestZip;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitZip in 'UnitZip.pas' {FormZip};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormZip, FormZip);
  Application.Run;
end.
