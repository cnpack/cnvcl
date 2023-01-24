program CertificateAuthority;

uses
  Forms,
  UnitCertificateAuthority in 'UnitCertificateAuthority.pas' {FormCA},
  CnCertificateAuthority in '..\..\..\Source\Crypto\CnCertificateAuthority.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormCA, FormCA);
  Application.Run;
end.
