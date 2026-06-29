program JPEGDemo;

uses
  Forms,
  UnitJPEGDemo in 'UnitJPEGDemo.pas' {FormJPEGDemo};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormJPEGDemo, FormJPEGDemo);
  Application.Run;
end.
