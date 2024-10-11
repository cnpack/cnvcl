unit UnitDSA;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, CnNative, CnDSA, CnBigNumber, CnCommon;

type
  TFormDSA = class(TForm)
    pgcDSA: TPageControl;
    tsDSA: TTabSheet;
    btnGenDSAParam: TButton;
    lblDSAP: TLabel;
    edtDSAP: TEdit;
    lblDSAQ: TLabel;
    edtDSAQ: TEdit;
    lblDSAG: TLabel;
    edtDSAG: TEdit;
    btnVerifyDSAParam: TButton;
    btnGenDSAKeys: TButton;
    btnVerifyDSAKeys: TButton;
    lblDSAPriv: TLabel;
    edtDSAPriv: TEdit;
    lblDSAPub: TLabel;
    edtDSAPub: TEdit;
    bvl1: TBevel;
    bvl2: TBevel;
    lblDSAHash: TLabel;
    edtDSAText: TEdit;
    btnDSASignHash: TButton;
    btnDSAVerifyHash: TButton;
    edtDSASignS: TEdit;
    lblDSASignS: TLabel;
    lblDSASignR: TLabel;
    edtDSASignR: TEdit;
    lblDSAHashType: TLabel;
    cbbDSAHashType: TComboBox;
    lblPrimeType: TLabel;
    cbbDSAType: TComboBox;
    procedure btnGenDSAParamClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnVerifyDSAParamClick(Sender: TObject);
    procedure btnGenDSAKeysClick(Sender: TObject);
    procedure btnVerifyDSAKeysClick(Sender: TObject);
    procedure btnDSASignHashClick(Sender: TObject);
    procedure btnDSAVerifyHashClick(Sender: TObject);
  private
    FDSAParam: TCnDSADomainParameter;
    FDSAPriv: TCnDSAPrivateKey;
    FDSAPub: TCnDSAPublicKey;
    FDSASignature: TCnDSASignature;
    FDataHash: TCnBigNumber;
  public
    procedure SetParams;
    procedure SetKeys;
    procedure SetSign;
  end;

var
  FormDSA: TFormDSA;

implementation

{$R *.DFM}

procedure TFormDSA.btnGenDSAParamClick(Sender: TObject);
begin
  if CnDSAGenerateParameter(FDSAParam, TCnDSAPrimeType(cbbDSAType.ItemIndex)) then
  begin
    ShowMessage('DSA Domain Param Generate OK');
    edtDSAP.Text := FDSAParam.P.ToHex;
    edtDSAQ.Text := FDSAParam.Q.ToHex;
    edtDSAG.Text := FDSAParam.G.ToHex;
  end
  else
    ShowMessage('DSA Domain Param Generate Fail');
end;

procedure TFormDSA.FormCreate(Sender: TObject);
begin
  FDSAParam := TCnDSADomainParameter.Create;
  FDSAPriv := TCnDSAPrivateKey.Create;
  FDSAPub := TCnDSAPublicKey.Create;
  FDSASignature := TCnDSASignature.Create;
  FDataHash := TCnBigNumber.Create;

  edtDSAP.Text := '95D8E3C9FC1E748F63C83EFD90EB7ED6871AF087F975FF64048028880C036' +
    '5C505506FFB6EF74911F1164B24EDDACF2D07B14BA84E38A0AC39C4FB8A4C9B816EA36C2EE2' +
    'CF4E276D7BBA5F6A76EC3447C7BC4EBD190575C54814FEDB84FC4EEA456921CC1E3FAAA4B96' +
    'FFABC7A8C00E6427D47A032C5EBEBF0F86192BF25635B';

  edtDSAQ.Text := 'B18265035E348B7F9993893D99E9CECFC45AFA33';

  edtDSAG.Text := '67636611F85C6706C1C53D33553050941A1B5399AA6EA6C9A398ACE01862E' +
    '6AF491A2BC9B65B977756EAAE11CB3755CDC45905AACD10290A1BC1E99AF819A9A9EC8C987F' +
    '98171EADD4F952EAE538B312612EBD68A88054E6F6D0B5B4EA253A033636F56C007D4FA5454' +
    'CC40EBEE1794B7B3DACC41878FFC899A457899DF95994';

  cbbDSAType.ItemIndex := 0;
  cbbDSAHashType.ItemIndex := 0;
end;

procedure TFormDSA.FormDestroy(Sender: TObject);
begin
  FDataHash.Free;
  FDSASignature.Free;
  FDSAPub.Free;
  FDSAPriv.Free;
  FDSAParam.Free;
end;

procedure TFormDSA.btnVerifyDSAParamClick(Sender: TObject);
begin
  SetParams;

  if CnDSAVerifyParameter(FDSAParam) then
    ShowMessage('DSA Domain Param Verify OK')
  else
    ShowMessage('DSA Domain Param Verify Fail');
end;

procedure TFormDSA.btnGenDSAKeysClick(Sender: TObject);
begin
  SetParams;

  if CnDSAGenerateKeys(FDSAParam, FDSAPriv, FDSAPub) then
  begin
    ShowMessage('DSA Keys Generate OK');
    edtDSAPriv.Text := FDSAPriv.ToHex;
    edtDSAPub.Text := FDSAPub.ToHex;
  end
  else
    ShowMessage('DSA Keys Generate Fail');
end;

procedure TFormDSA.btnVerifyDSAKeysClick(Sender: TObject);
begin
  SetParams;
  SetKeys;

  if CnDSAVerifyKeys(FDSAParam, FDSAPriv, FDSAPub) then
    ShowMessage('DSA Keys Verify OK')
  else
    ShowMessage('DSA Keys Verify Fail');
end;

procedure TFormDSA.SetKeys;
begin
  FDSAPriv.SetHex(edtDSAPriv.Text);
  FDSAPub.SetHex(edtDSAPub.Text);
end;

procedure TFormDSA.SetParams;
begin
  FDSAParam.P.SetHex(edtDSAP.Text);
  FDSAParam.Q.SetHex(edtDSAQ.Text);
  FDSAParam.G.SetHex(edtDSAG.Text);
end;

procedure TFormDSA.SetSign;
begin
  FDSASignature.S.SetHex(edtDSASignS.Text);
  FDSASignature.R.SetHex(edtDSASignR.Text);
end;

procedure TFormDSA.btnDSASignHashClick(Sender: TObject);
var
  Data: TBytes;
begin
  SetParams;
  SetKeys;

{$IFDEF UNICODE}
  Data := TEncoding.UTF8.GetBytes(edtDSAText.Text);
{$ELSE}
  Data := AnsiToBytes(CnAnsiToUtf8(edtDSAText.Text));
{$ENDIF}

  if CnDSASignBytes(Data, FDSAParam, FDSAPriv, FDSASignature,
    TCnDSAHashType(cbbDSAHashType.ItemIndex)) then
  begin
    edtDSASignS.Text := FDSASignature.S.ToHex;
    edtDSASignR.Text := FDSASignature.R.ToHex;
    ShowMessage('Sign Hash Data OK');
  end
  else
    ShowMessage('Sign Hash Data Fail');
end;

procedure TFormDSA.btnDSAVerifyHashClick(Sender: TObject);
var
  Data: TBytes;
begin
  SetParams;
  SetKeys;
  SetSign;

{$IFDEF UNICODE}
  Data := TEncoding.UTF8.GetBytes(edtDSAText.Text);
{$ELSE}
  Data := AnsiToBytes(CnAnsiToUtf8(edtDSAText.Text));
{$ENDIF}

  if CnDSAVerifyBytes(Data, FDSAParam, FDSAPub, FDSASignature,
    TCnDSAHashType(cbbDSAHashType.ItemIndex)) then
    ShowMessage('Verify Hash OK')
  else
    ShowMessage('Verify Hash Fail');
end;

end.
