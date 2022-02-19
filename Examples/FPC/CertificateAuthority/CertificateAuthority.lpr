program CertificateAuthority;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitCertificateAuthority in 'UnitCertificateAuthority.pas' {FormCA};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCA, FormCA);
  Application.Run;
end.
