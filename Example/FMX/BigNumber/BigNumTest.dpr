program BigNumTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMain in 'UnitMain.pas' {FormBigNumber};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBigNumber, FormBigNumber);
  Application.Run;
end.
