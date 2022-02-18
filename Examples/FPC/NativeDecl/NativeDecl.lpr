program NativeDecl;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitNative in 'UnitNative.pas' {FormNative},
  CnNativeDecl in '..\..\..\Source\Crypto\CnNativeDecl.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormNative, FormNative);
  Application.Run;
end.
