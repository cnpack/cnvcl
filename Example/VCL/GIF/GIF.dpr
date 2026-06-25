program GIF;

uses
  Forms,
  UnitGIF in 'UnitGIF.pas' {frmGIFDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmGIFDemo, frmGIFDemo);
  Application.Run;
end.
