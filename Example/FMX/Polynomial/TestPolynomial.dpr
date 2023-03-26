program TestPolynomial;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitPolynomial in 'UnitPolynomial.pas' {FormPolynomial};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormPolynomial, FormPolynomial);
  Application.Run;
end.
