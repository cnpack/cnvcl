program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit1 in 'Unit1.pas' {FormCrypt};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCrypt, FormCrypt);
  Application.Run;
end.
