program Socks5Proxy;

uses
  Forms,
  UnitSocks5 in 'UnitSocks5.pas' {FormSocks5};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormSocks5, FormSocks5);
  Application.Run;
end.
