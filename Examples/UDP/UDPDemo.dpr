program UDPDemo;

uses
  Forms,
  UDPDemoFrm in 'UDPDemoFrm.pas' {Form1},
  CnUDP in '..\..\Source\NetComm\CnUDP.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
