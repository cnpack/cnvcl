program JPEGDemo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitJPEGDemo in 'UnitJPEGDemo.pas' {FormJPEGDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormJPEGDemo, FormJPEGDemo);
  Application.Run;
end.
