program AACtrl;

uses
  Forms,
  UnitAACtrl in 'UnitAACtrl.pas' {FormAACtrl};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormAACtrl, FormAACtrl);
  Application.Run;
end.
