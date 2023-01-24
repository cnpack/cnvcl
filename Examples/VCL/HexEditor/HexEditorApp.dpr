program HexEditorApp;

uses
  Forms,
  HexEditorUnit in 'HexEditorUnit.pas' {FormHexEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormHexEditor, FormHexEditor);
  Application.Run;
end.
