program TCPForwarder;

uses
  Forms,
  UnitForm in 'UnitForm.pas' {FormForwarder},
  CnTCPForwarder in '..\..\..\Source\NetComm\CnTCPForwarder.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormForwarder, FormForwarder);
  Application.Run;
end.
