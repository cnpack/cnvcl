program TCPForwarder;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitForm in 'UnitForm.pas' {FormForwarder},
  CnTCPForwarder in '..\..\..\Source\NetComm\CnTCPForwarder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormForwarder, FormForwarder);
  Application.Run;
end.
