program CertificateAuthority;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitCertificateAuthority in 'UnitCertificateAuthority.pas' {FormCA},
  CnCertificateAuthority in '..\..\Source\Crypto\CnCertificateAuthority.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCA, FormCA);
  Application.Run;
end.
