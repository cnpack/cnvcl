program NativeDecl;

uses
  Forms,
  UnitNative in 'UnitNative.pas' {FormNative},
  CnNativeDecl in '..\..\Source\Crypto\CnNativeDecl.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormNative, FormNative);
  Application.Run;
end.
