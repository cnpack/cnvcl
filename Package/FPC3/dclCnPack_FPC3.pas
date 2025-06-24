{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit dclCnPack_FPC3;

{$warn 5023 off : no warning about unused units}
interface

uses
  CnLangEditors, CnLangReg, CnLangTranslator, CnTransEditor, CnTransFilter, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CnLangReg', @CnLangReg.Register);
end;

initialization
  RegisterPackage('dclCnPack_FPC3', @Register);
end.
