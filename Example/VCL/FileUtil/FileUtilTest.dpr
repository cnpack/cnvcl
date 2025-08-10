program FileUtilTest;

uses
  Forms,
  UnitFile in 'UnitFile.pas' {FormFile};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormFile, FormFile);
  Application.Run;
end.
