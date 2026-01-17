program UDPDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UDPDemoFrm in 'UDPDemoFrm.pas' {Form1},
  CnUDP in '..\..\..\Source\NetComm\CnUDP.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
