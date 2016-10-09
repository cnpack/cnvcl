program NetDecl;

uses
  Forms,
  UnitNetDecl in 'UnitNetDecl.pas' {FormNetDecl},
  CnNetDecls in '..\..\Source\NetComm\CnNetDecls.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormNetDecl, FormNetDecl);
  Application.Run;
end.
