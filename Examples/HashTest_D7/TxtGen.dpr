program TxtGen;

uses
  Forms,
  fTxtGenUnit in 'fTxtGenUnit.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
