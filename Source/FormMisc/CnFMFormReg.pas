unit CnFMFormReg;

interface

uses
  Classes, SysUtils, CnFMForm, CnFMQQPane, CnFMButton, CnFMAppStore;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CnPack Form Misc', [
    TCnFMForm, TCnFMGlassPanel, TCnFMMenuLabel, TCnFMTitleImg, TCnFMMenuImage,
    TCnFMQQMember, TCnFMQQGroup, TCnFMQQGroupWTitle, TCnFMQQPanel, TCnFMButton,
    TCnFMAppStore]);
end;

end.
