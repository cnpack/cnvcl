program TestPolynomial;

uses
  Forms,
  UnitPolynomial in 'UnitPolynomial.pas' {FormPolynomial},
  CnPolynomial in '..\..\Source\Crypto\CnPolynomial.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormPolynomial, FormPolynomial);
  Application.Run;
end.
