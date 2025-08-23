program CnGLPaintBox;

uses
  Forms,
  CnGLPaintBoxDemo in 'CnGLPaintBoxDemo.pas' {frmMain},
  CnOpenGLPaintBox in '..\..\..\Source\Graphic\CnOpenGLPaintBox.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
