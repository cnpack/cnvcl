program CnTrayIconDemo;

uses
  Forms,
  CnTrayIconMainUnit in 'CnTrayIconMainUnit.pas' {TrayIconDemoFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTrayIconDemoFrm, TrayIconDemoFrm);
  Application.Run;
end.
