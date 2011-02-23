program CnMultiLangMerger;

uses
  Forms,
  Unit1 in 'Unit1.pas' {CnMultiLangMergeFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TCnMultiLangMergeFrm, CnMultiLangMergeFrm);
  Application.Run;
end.
