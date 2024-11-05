program NativeDecl;

{$MODE Delphi}

uses
  Forms, Interfaces,
  UnitNative in 'UnitNative.pas' {FormNative},
  CnNative in '..\..\..\Source\Crypto\CnNative.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormNative, FormNative);
  Application.Run;
end.
