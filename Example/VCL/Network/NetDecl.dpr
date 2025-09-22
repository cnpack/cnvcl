program NetDecl;

uses
  Forms,
  UnitNetDecl in 'UnitNetDecl.pas' {FormNetDecl},
  CnNetwork in '..\..\..\Source\NetComm\CnNetwork.pas',
  CnSocket in '..\..\..\Source\NetComm\CnSocket.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormNetDecl, FormNetDecl);
  Application.Run;
end.
