program GIF;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitGIF in 'UnitGIF.pas' {frmGIFDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmGIFDemo, frmGIFDemo);
  Application.Run;
end.
