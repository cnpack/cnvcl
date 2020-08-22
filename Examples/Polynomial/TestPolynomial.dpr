program TestPolynomial;

uses
  Forms,
  UnitPolynomial in 'UnitPolynomial.pas' {FormPolynomial},
  CnPolynomial in '..\..\Source\Common\CnPolynomial.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormPolynomial, FormPolynomial);
  Application.Run;
end.
