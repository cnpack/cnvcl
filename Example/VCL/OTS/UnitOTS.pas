unit UnitOTS;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnOTS;

type
  TFormOTS = class(TForm)
    pgc1: TPageControl;
    tsSM3OTS: TTabSheet;
    btnGenSM3OTSKeys: TButton;
    mmoSM3OTSPrivateKey: TMemo;
    mmoSM3OTSPublicKey: TMemo;
    lblSM3OTSMessage: TLabel;
    lblSM3OTSPrivate: TLabel;
    lblSM3OTSPublic: TLabel;
    mmoSM3OTSMessage: TMemo;
    btnSM3OTSSign: TButton;
    btnSM3OTSVerify: TButton;
    lblSM3OTSVerifyKey: TLabel;
    mmoSM3OTSVerificationKey: TMemo;
    lblSM3OTSSignature: TLabel;
    mmoSM3OTSSignature: TMemo;
    tsSHA256OTS: TTabSheet;
    btnGenSHA256OTSKeys: TButton;
    mmoSHA256OTSPrivateKey: TMemo;
    mmoSHA256OTSPublicKey: TMemo;
    lblSHA256OTSMessage: TLabel;
    lblSHA256OTSPrivate: TLabel;
    lblSHA256OTSPublic: TLabel;
    mmoSHA256OTSMessage: TMemo;
    btnSHA256OTSSign: TButton;
    btnSHA256OTSVerify: TButton;
    lblSHA256OTSVerifyKey: TLabel;
    mmoSHA256OTSVerificationKey: TMemo;
    lblSHA256OTSSignature: TLabel;
    mmoSHA256OTSSignature: TMemo;
    procedure btnGenSM3OTSKeysClick(Sender: TObject);
    procedure btnSM3OTSSignClick(Sender: TObject);
    procedure btnSM3OTSVerifyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnGenSHA256OTSKeysClick(Sender: TObject);
    procedure btnSHA256OTSSignClick(Sender: TObject);
    procedure btnSHA256OTSVerifyClick(Sender: TObject);
  private
    FSM3OTSPrivateKey: TCnOTSSM3PrivateKey;
    FSM3OTSPublicKey: TCnOTSSM3PublicKey;
    FSM3OTSSignature: TCnOTSSM3Signature;
    FSM3OTSVerification: TCnOTSSM3VerificationKey;
    FSHA256OTSPrivateKey: TCnOTSSHA256PrivateKey;
    FSHA256OTSPublicKey: TCnOTSSHA256PublicKey;
    FSHA256OTSSignature: TCnOTSSHA256Signature;
    FSHA256OTSVerification: TCnOTSSHA256VerificationKey;
  public
    procedure DumpSM3OTSPrivateKey;
    procedure DumpSM3OTSPublicKey;
    procedure DumpSM3OTSSignature;
    procedure DumpSM3OTSVerification;
    procedure DumpSHA256OTSPrivateKey;
    procedure DumpSHA256OTSPublicKey;
    procedure DumpSHA256OTSSignature;
    procedure DumpSHA256OTSVerification;
  end;

var
  FormOTS: TFormOTS;

implementation

{$R *.DFM}

uses
  CnSM3, CnSHA2;

procedure TFormOTS.btnGenSM3OTSKeysClick(Sender: TObject);
begin
  if CnOTSSM3GenerateKeys(FSM3OTSPrivateKey, FSM3OTSPublicKey) then
  begin
    DumpSM3OTSPrivateKey;
    DumpSM3OTSPublicKey;
    ShowMessage('Generate Keys OK');
  end;
end;

procedure TFormOTS.DumpSM3OTSPrivateKey;
var
  I: Integer;
begin
  mmoSM3OTSPrivateKey.Clear;
  for I := Low(TCnOTSSM3PrivateKey) to High(TCnOTSSM3PrivateKey) div 2 do
    mmoSM3OTSPrivateKey.Lines.Add(SM3Print(FSM3OTSPrivateKey[I * 2]) + ', ' + SM3Print(FSM3OTSPrivateKey[I * 2 + 1]));
end;

procedure TFormOTS.DumpSM3OTSPublicKey;
var
  I: Integer;
begin
  mmoSM3OTSPublicKey.Clear;
  for I := Low(TCnOTSSM3PublicKey) to High(TCnOTSSM3PublicKey) div 2 do
    mmoSM3OTSPublicKey.Lines.Add(SM3Print(FSM3OTSPublicKey[I * 2]) + ', ' + SM3Print(FSM3OTSPublicKey[I * 2 + 1]));
end;

procedure TFormOTS.DumpSM3OTSSignature;
var
  I: Integer;
begin
  mmoSM3OTSSignature.Clear;
  for I := Low(TCnOTSSM3Signature) to High(TCnOTSSM3Signature) do
    mmoSM3OTSSignature.Lines.Add(SM3Print(FSM3OTSSignature[I]));
end;

procedure TFormOTS.DumpSM3OTSVerification;
var
  I: Integer;
begin
  mmoSM3OTSVerificationKey.Clear;
  for I := Low(TCnOTSSM3VerificationKey) to High(TCnOTSSM3VerificationKey) do
    mmoSM3OTSVerificationKey.Lines.Add(SM3Print(FSM3OTSVerification[I]));
end;

procedure TFormOTS.btnSM3OTSSignClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSM3OTSMessage.Lines.Text;
  CnOTSSM3SignData(@S[1], Length(S), FSM3OTSPrivateKey, FSM3OTSPublicKey,
    FSM3OTSSignature, FSM3OTSVerification);
  DumpSM3OTSSignature;
  DumpSM3OTSVerification;
end;

procedure TFormOTS.btnSM3OTSVerifyClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSM3OTSMessage.Lines.Text;
  if CnOTSSM3VerifyData(@S[1], Length(S), FSM3OTSSignature, FSM3OTSPublicKey, FSM3OTSVerification) then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');
end;

procedure TFormOTS.FormCreate(Sender: TObject);
begin
  FillChar(FSM3OTSPrivateKey[0], SizeOf(TCnOTSSM3PrivateKey), 0);
  FillChar(FSM3OTSPublicKey[0], SizeOf(TCnOTSSM3PublicKey), 0);
  FillChar(FSM3OTSSignature[0], SizeOf(TCnOTSSM3Signature), 0);
  FillChar(FSM3OTSVerification[0], SizeOf(TCnOTSSM3VerificationKey), 0);

  FillChar(FSHA256OTSPrivateKey[0], SizeOf(TCnOTSSHA256PrivateKey), 0);
  FillChar(FSHA256OTSPublicKey[0], SizeOf(TCnOTSSHA256PublicKey), 0);
  FillChar(FSHA256OTSSignature[0], SizeOf(TCnOTSSHA256Signature), 0);
  FillChar(FSHA256OTSVerification[0], SizeOf(TCnOTSSHA256VerificationKey), 0);
end;

procedure TFormOTS.DumpSHA256OTSPrivateKey;
var
  I: Integer;
begin
  mmoSHA256OTSPrivateKey.Clear;
  for I := Low(TCnOTSSHA256PrivateKey) to High(TCnOTSSHA256PrivateKey) div 2 do
    mmoSHA256OTSPrivateKey.Lines.Add(SHA256Print(FSHA256OTSPrivateKey[I * 2]) + ', ' + SHA256Print(FSHA256OTSPrivateKey[I * 2 + 1]));
end;

procedure TFormOTS.DumpSHA256OTSPublicKey;
var
  I: Integer;
begin
  mmoSHA256OTSPublicKey.Clear;
  for I := Low(TCnOTSSHA256PublicKey) to High(TCnOTSSHA256PublicKey) div 2 do
    mmoSHA256OTSPublicKey.Lines.Add(SHA256Print(FSHA256OTSPublicKey[I * 2]) + ', ' + SHA256Print(FSHA256OTSPublicKey[I * 2 + 1]));
end;

procedure TFormOTS.DumpSHA256OTSSignature;
var
  I: Integer;
begin
  mmoSHA256OTSSignature.Clear;
  for I := Low(TCnOTSSHA256Signature) to High(TCnOTSSHA256Signature) do
    mmoSHA256OTSSignature.Lines.Add(SHA256Print(FSHA256OTSSignature[I]));
end;

procedure TFormOTS.DumpSHA256OTSVerification;
var
  I: Integer;
begin
  mmoSHA256OTSVerificationKey.Clear;
  for I := Low(TCnOTSSHA256VerificationKey) to High(TCnOTSSHA256VerificationKey) do
    mmoSHA256OTSVerificationKey.Lines.Add(SHA256Print(FSHA256OTSVerification[I]));
end;


procedure TFormOTS.btnGenSHA256OTSKeysClick(Sender: TObject);
begin
  if CnOTSSHA256GenerateKeys(FSHA256OTSPrivateKey, FSHA256OTSPublicKey) then
  begin
    DumpSHA256OTSPrivateKey;
    DumpSHA256OTSPublicKey;
    ShowMessage('Generate Keys OK');
  end;
end;

procedure TFormOTS.btnSHA256OTSSignClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSHA256OTSMessage.Lines.Text;
  CnOTSSHA256SignData(@S[1], Length(S), FSHA256OTSPrivateKey, FSHA256OTSPublicKey,
    FSHA256OTSSignature, FSHA256OTSVerification);
  DumpSHA256OTSSignature;
  DumpSHA256OTSVerification;
end;

procedure TFormOTS.btnSHA256OTSVerifyClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSHA256OTSMessage.Lines.Text;
  if CnOTSSHA256VerifyData(@S[1], Length(S), FSHA256OTSSignature, FSHA256OTSPublicKey, FSHA256OTSVerification) then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');
end;

end.
