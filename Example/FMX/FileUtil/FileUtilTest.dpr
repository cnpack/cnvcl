program FileUtilTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitFile in 'UnitFile.pas' {FormFile};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormFile, FormFile);
  Application.Run;
end.
