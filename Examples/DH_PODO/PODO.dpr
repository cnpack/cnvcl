program PODO;

uses
  Forms,
  CnPODOFormMain in 'CnPODOFormMain.pas' {frmPodoMain},
  CnPODOConsts in 'CnPODOConsts.pas',
  CnPODOUtils in 'CnPODOUtils.pas',
  CnDHibernateAbout in '..\..\Source\DbReport\CnDHibernateAbout.pas' {CnFormDHibernateAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'CnPack - PODO Éú³ÉÆ÷';
  Application.CreateForm(TfrmPodoMain, frmPodoMain);
  Application.Run;
end.
