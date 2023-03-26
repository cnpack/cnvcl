program TCPForwarder;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitForm in 'UnitForm.pas' {FormForwarder};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormForwarder, FormForwarder);
  Application.Run;
end.
