program Demo;

uses
  Forms,
  CnDialUpDemo in 'CnDialUpDemo.pas' {FrmCnDialUpDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFrmCnDialUpDemo, FrmCnDialUpDemo);
  Application.Run;
end.
