program UDPDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  UDPDemoFrm in 'UDPDemoFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
