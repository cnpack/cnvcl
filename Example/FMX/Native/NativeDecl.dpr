program NativeDecl;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitNative in 'UnitNative.pas' {FormNative},
  CnNative in '..\..\..\Source\Crypto\CnNative.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormNative, FormNative);
  Application.Run;
end.
