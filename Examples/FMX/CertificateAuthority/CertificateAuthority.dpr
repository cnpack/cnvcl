program CertificateAuthority;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitCertificateAuthority in 'UnitCertificateAuthority.pas' {FormCA};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCA, FormCA);
  Application.Run;
end.
