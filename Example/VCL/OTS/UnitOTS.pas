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
    tsSM3MOTS: TTabSheet;
    btnGenSM3MOTSKeys: TButton;
    mmoSM3MOTSPrivateKey: TMemo;
    mmoSM3MOTSPublicKey: TMemo;
    lblSM3MOTSMessage: TLabel;
    lblSM3MOTSPrivate: TLabel;
    lblSM3MOTSPublic: TLabel;
    mmoSM3MOTSMessage: TMemo;
    btnSM3MOTSSign: TButton;
    btnSM3MOTSVerify: TButton;
    lblSM3MOTSSignature: TLabel;
    mmoSM3MOTSSignature: TMemo;
    tsSHA256MOTS: TTabSheet;
    btnGenSHA256MOTSKeys: TButton;
    mmoSHA256MOTSPrivateKey: TMemo;
    mmoSHA256MOTSPublicKey: TMemo;
    lblSHA256MOTSMessage: TLabel;
    lblSHA256MOTSPrivate: TLabel;
    lblSHA256MOTSPublic: TLabel;
    mmoSHA256MOTSMessage: TMemo;
    btnSHA256MOTSSign: TButton;
    btnSHA256MOTSVerify: TButton;
    lblSHA256MOTSSignature: TLabel;
    mmoSHA256MOTSSignature: TMemo;
    tsSM3WOTS: TTabSheet;
    btnGenSM3WOTSKeys: TButton;
    mmoSM3WOTSPrivateKey: TMemo;
    mmoSM3WOTSPublicKey: TMemo;
    lblSM3WOTSMessage: TLabel;
    lblSM3WOTSPrivate: TLabel;
    lblSM3WOTSPublic: TLabel;
    mmoSM3WOTSMessage: TMemo;
    btnSM3WOTSSign: TButton;
    btnSM3WOTSVerify: TButton;
    lblSM3WOTSSignature: TLabel;
    mmoSM3WOTSSignature: TMemo;
    tsSHA256WOTS: TTabSheet;
    btnGenSHA256WOTSKeys: TButton;
    mmoSHA256WOTSPrivateKey: TMemo;
    mmoSHA256WOTSPublicKey: TMemo;
    lblSHA256WOTSMessage: TLabel;
    lblSHA256WOTSPrivate: TLabel;
    lblSHA256WOTSPublic: TLabel;
    mmoSHA256WOTSMessage: TMemo;
    btnSHA256WOTSSign: TButton;
    btnSHA256WOTSVerify: TButton;
    lblSHA256WOTSSignature: TLabel;
    mmoSHA256WOTSSignature: TMemo;
    tsSM3WOTSPlus: TTabSheet;
    btnGenSM3WOTSPlusKeys: TButton;
    mmoSM3WOTSPlusPrivateKey: TMemo;
    mmoSM3WOTSPlusPublicKey: TMemo;
    lblSM3WOTSPlusMessage: TLabel;
    lblSM3WOTSPlusPrivate: TLabel;
    lblSM3WOTSPlusPublic: TLabel;
    mmoSM3WOTSPlusMessage: TMemo;
    btnSM3WOTSPlusSign: TButton;
    btnSM3WOTSPlusVerify: TButton;
    lblSM3WOTSPlusSignature: TLabel;
    mmoSM3WOTSPlusSignature: TMemo;
    lblSM3WOTSPlusSalt: TLabel;
    mmoSM3WOTSPlusMask: TMemo;
    tsSHA256WOTSPlus: TTabSheet;
    btnGenSHA256WOTSPlusKeys: TButton;
    mmoSHA256WOTSPlusPrivateKey: TMemo;
    mmoSHA256WOTSPlusPublicKey: TMemo;
    lblSHA256WOTSPlusMessage: TLabel;
    lblSHA256WOTSPlusPrivate: TLabel;
    lblSHA256WOTSPlusPublic: TLabel;
    mmoSHA256WOTSPlusMessage: TMemo;
    btnSHA256WOTSPlusSign: TButton;
    btnSHA256WOTSPlusVerify: TButton;
    lblSHA256WOTSPlusSignature: TLabel;
    mmoSHA256WOTSPlusSignature: TMemo;
    lblSHA256WOTSPlusMask: TLabel;
    mmoSHA256WOTSPlusMask: TMemo;
    procedure btnGenSM3OTSKeysClick(Sender: TObject);
    procedure btnSM3OTSSignClick(Sender: TObject);
    procedure btnSM3OTSVerifyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnGenSHA256OTSKeysClick(Sender: TObject);
    procedure btnSHA256OTSSignClick(Sender: TObject);
    procedure btnSHA256OTSVerifyClick(Sender: TObject);
    procedure btnGenSM3WOTSKeysClick(Sender: TObject);
    procedure btnSM3WOTSSignClick(Sender: TObject);
    procedure btnSM3WOTSVerifyClick(Sender: TObject);
    procedure btnGenSHA256WOTSKeysClick(Sender: TObject);
    procedure btnSHA256WOTSSignClick(Sender: TObject);
    procedure btnSHA256WOTSVerifyClick(Sender: TObject);
    procedure btnGenSM3MOTSKeysClick(Sender: TObject);
    procedure btnSM3MOTSSignClick(Sender: TObject);
    procedure btnSM3MOTSVerifyClick(Sender: TObject);
    procedure btnGenSHA256MOTSKeysClick(Sender: TObject);
    procedure btnSHA256MOTSSignClick(Sender: TObject);
    procedure btnSHA256MOTSVerifyClick(Sender: TObject);
    procedure btnGenSM3WOTSPlusKeysClick(Sender: TObject);
    procedure btnSM3WOTSPlusSignClick(Sender: TObject);
    procedure btnSM3WOTSPlusVerifyClick(Sender: TObject);
    procedure btnGenSHA256WOTSPlusKeysClick(Sender: TObject);
    procedure btnSHA256WOTSPlusSignClick(Sender: TObject);
    procedure btnSHA256WOTSPlusVerifyClick(Sender: TObject);
  private
    FSM3OTSPrivateKey: TCnOTSSM3PrivateKey;
    FSM3OTSPublicKey: TCnOTSSM3PublicKey;
    FSM3OTSSignature: TCnOTSSM3Signature;
    FSM3OTSVerification: TCnOTSSM3VerificationKey;
    FSHA256OTSPrivateKey: TCnOTSSHA256PrivateKey;
    FSHA256OTSPublicKey: TCnOTSSHA256PublicKey;
    FSHA256OTSSignature: TCnOTSSHA256Signature;
    FSHA256OTSVerification: TCnOTSSHA256VerificationKey;
    FSM3MOTSPrivateKey: TCnMOTSSM3PrivateKey;
    FSM3MOTSPublicKey: TCnMOTSSM3PublicKey;
    FSM3MOTSSignature: TCnMOTSSM3Signature;
    FSHA256MOTSPrivateKey: TCnMOTSSHA256PrivateKey;
    FSHA256MOTSPublicKey: TCnMOTSSHA256PublicKey;
    FSHA256MOTSSignature: TCnMOTSSHA256Signature;
    FSM3WOTSPrivateKey: TCnWOTSSM3PrivateKey;
    FSM3WOTSPublicKey: TCnWOTSSM3PublicKey;
    FSM3WOTSSignature: TCnWOTSSM3Signature;
    FSHA256WOTSPrivateKey: TCnWOTSSHA256PrivateKey;
    FSHA256WOTSPublicKey: TCnWOTSSHA256PublicKey;
    FSHA256WOTSSignature: TCnWOTSSHA256Signature;
    FSM3WOTSPlusPrivateKey: TCnWOTSPlusSM3PrivateKey;
    FSM3WOTSPlusPublicKey: TCnWOTSPlusSM3PublicKey;
    FSM3WOTSPlusSignature: TCnWOTSPlusSM3Signature;
    FSM3WOTSPlusMask: TCnWOTSPlusSM3Mask;
    FSHA256WOTSPlusPrivateKey: TCnWOTSPlusSHA256PrivateKey;
    FSHA256WOTSPlusPublicKey: TCnWOTSPlusSHA256PublicKey;
    FSHA256WOTSPlusSignature: TCnWOTSPlusSHA256Signature;
    FSHA256WOTSPlusMask: TCnWOTSPlusSHA256Mask;
  public
    procedure DumpSM3OTSPrivateKey;
    procedure DumpSM3OTSPublicKey;
    procedure DumpSM3OTSSignature;
    procedure DumpSM3OTSVerification;
    procedure DumpSHA256OTSPrivateKey;
    procedure DumpSHA256OTSPublicKey;
    procedure DumpSHA256OTSSignature;
    procedure DumpSHA256OTSVerification;
    procedure DumpSM3MOTSPrivateKey;
    procedure DumpSM3MOTSPublicKey;
    procedure DumpSM3MOTSSignature;
    procedure DumpSHA256MOTSPrivateKey;
    procedure DumpSHA256MOTSPublicKey;
    procedure DumpSHA256MOTSSignature;
    procedure DumpSM3WOTSPrivateKey;
    procedure DumpSM3WOTSPublicKey;
    procedure DumpSM3WOTSSignature;
    procedure DumpSHA256WOTSPrivateKey;
    procedure DumpSHA256WOTSPublicKey;
    procedure DumpSHA256WOTSSignature;
    procedure DumpSM3WOTSPlusPrivateKey;
    procedure DumpSM3WOTSPlusPublicKey;
    procedure DumpSM3WOTSPlusSignature;
    procedure DumpSM3WOTSPlusMask;
    procedure DumpSHA256WOTSPlusPrivateKey;
    procedure DumpSHA256WOTSPlusPublicKey;
    procedure DumpSHA256WOTSPlusSignature;
    procedure DumpSHA256WOTSPlusMask;
  end;

var
  FormOTS: TFormOTS;

implementation

{$R *.DFM}

uses
  CnSM3, CnSHA2;

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

  FillChar(FSM3MOTSPrivateKey[0], SizeOf(TCnMOTSSM3PrivateKey), 0);
  FillChar(FSM3MOTSPublicKey[0], SizeOf(TCnMOTSSM3PublicKey), 0);
  FillChar(FSM3MOTSSignature[0], SizeOf(TCnMOTSSM3Signature), 0);

  FillChar(FSM3WOTSPrivateKey[0], SizeOf(TCnWOTSSM3PrivateKey), 0);
  FillChar(FSM3WOTSPublicKey[0], SizeOf(TCnWOTSSM3PublicKey), 0);
  FillChar(FSM3WOTSSignature[0], SizeOf(TCnWOTSSM3Signature), 0);

  FillChar(FSHA256WOTSPrivateKey[0], SizeOf(TCnWOTSSHA256PrivateKey), 0);
  FillChar(FSHA256WOTSPublicKey[0], SizeOf(TCnWOTSSHA256PublicKey), 0);
  FillChar(FSHA256WOTSSignature[0], SizeOf(TCnWOTSSHA256Signature), 0);

  FillChar(FSM3WOTSPlusPrivateKey[0], SizeOf(TCnWOTSPlusSM3PrivateKey), 0);
  FillChar(FSM3WOTSPlusPublicKey[0], SizeOf(TCnWOTSPlusSM3PublicKey), 0);
  FillChar(FSM3WOTSPlusSignature[0], SizeOf(TCnWOTSPlusSM3Signature), 0);
  FillChar(FSM3WOTSPlusMask[0], SizeOf(TCnWOTSPlusSM3Mask), 0);

  FillChar(FSHA256WOTSPlusPrivateKey[0], SizeOf(TCnWOTSPlusSHA256PrivateKey), 0);
  FillChar(FSHA256WOTSPlusPublicKey[0], SizeOf(TCnWOTSPlusSHA256PublicKey), 0);
  FillChar(FSHA256WOTSPlusSignature[0], SizeOf(TCnWOTSPlusSHA256Signature), 0);
  FillChar(FSHA256WOTSPlusMask[0], SizeOf(TCnWOTSPlusSHA256Mask), 0);
end;

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

procedure TFormOTS.DumpSM3MOTSPrivateKey;
var
  I: Integer;
begin
  mmoSM3MOTSPrivateKey.Clear;
  for I := Low(TCnMOTSSM3PrivateKey) to High(TCnMOTSSM3PrivateKey) do
    mmoSM3MOTSPrivateKey.Lines.Add(SM3Print(FSM3MOTSPrivateKey[I]));
end;

procedure TFormOTS.DumpSM3MOTSPublicKey;
var
  I: Integer;
begin
  mmoSM3MOTSPublicKey.Clear;
  for I := Low(TCnMOTSSM3PublicKey) to High(TCnMOTSSM3PublicKey) do
    mmoSM3MOTSPublicKey.Lines.Add(SM3Print(FSM3MOTSPublicKey[I]));
end;

procedure TFormOTS.DumpSM3MOTSSignature;
var
  I: Integer;
begin
  mmoSM3MOTSSignature.Clear;
  for I := Low(TCnMOTSSM3Signature) to High(TCnMOTSSM3Signature) do
    mmoSM3MOTSSignature.Lines.Add(SM3Print(FSM3MOTSSignature[I]));
end;

procedure TFormOTS.btnGenSM3MOTSKeysClick(Sender: TObject);
begin
  if CnMOTSSM3GenerateKeys(FSM3MOTSPrivateKey, FSM3MOTSPublicKey) then
  begin
    DumpSM3MOTSPrivateKey;
    DumpSM3MOTSPublicKey;
    ShowMessage('Generate Keys OK');
  end;
end;

procedure TFormOTS.btnSM3MOTSSignClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSM3MOTSMessage.Lines.Text;
  CnMOTSSM3SignData(@S[1], Length(S), FSM3MOTSPrivateKey, FSM3MOTSSignature);
  DumpSM3MOTSSignature;
end;

procedure TFormOTS.btnSM3MOTSVerifyClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSM3MOTSMessage.Lines.Text;
  if CnMOTSSM3VerifyData(@S[1], Length(S), FSM3MOTSSignature, FSM3MOTSPublicKey) then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');
end;

procedure TFormOTS.DumpSHA256MOTSPrivateKey;
var
  I: Integer;
begin
  mmoSHA256MOTSPrivateKey.Clear;
  for I := Low(TCnMOTSSHA256PrivateKey) to High(TCnMOTSSHA256PrivateKey) do
    mmoSHA256MOTSPrivateKey.Lines.Add(SHA256Print(FSHA256MOTSPrivateKey[I]));
end;

procedure TFormOTS.DumpSHA256MOTSPublicKey;
var
  I: Integer;
begin
  mmoSHA256MOTSPublicKey.Clear;
  for I := Low(TCnMOTSSHA256PublicKey) to High(TCnMOTSSHA256PublicKey) do
    mmoSHA256MOTSPublicKey.Lines.Add(SHA256Print(FSHA256MOTSPublicKey[I]));
end;

procedure TFormOTS.DumpSHA256MOTSSignature;
var
  I: Integer;
begin
  mmoSHA256MOTSSignature.Clear;
  for I := Low(TCnMOTSSHA256Signature) to High(TCnMOTSSHA256Signature) do
    mmoSHA256MOTSSignature.Lines.Add(SHA256Print(FSHA256MOTSSignature[I]));
end;

procedure TFormOTS.btnGenSHA256MOTSKeysClick(Sender: TObject);
begin
  if CnMOTSSHA256GenerateKeys(FSHA256MOTSPrivateKey, FSHA256MOTSPublicKey) then
  begin
    DumpSHA256MOTSPrivateKey;
    DumpSHA256MOTSPublicKey;
    ShowMessage('Generate Keys OK');
  end;
end;

procedure TFormOTS.btnSHA256MOTSSignClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSHA256MOTSMessage.Lines.Text;
  CnMOTSSHA256SignData(@S[1], Length(S), FSHA256MOTSPrivateKey, FSHA256MOTSSignature);
  DumpSHA256MOTSSignature;
end;

procedure TFormOTS.btnSHA256MOTSVerifyClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSHA256MOTSMessage.Lines.Text;
  if CnMOTSSHA256VerifyData(@S[1], Length(S), FSHA256MOTSSignature, FSHA256MOTSPublicKey) then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');
end;

procedure TFormOTS.DumpSM3WOTSPrivateKey;
var
  I: Integer;
begin
  mmoSM3WOTSPrivateKey.Clear;
  for I := Low(TCnWOTSSM3PrivateKey) to High(TCnWOTSSM3PrivateKey) do
    mmoSM3WOTSPrivateKey.Lines.Add(SM3Print(FSM3WOTSPrivateKey[I]));
end;

procedure TFormOTS.DumpSM3WOTSPublicKey;
var
  I: Integer;
begin
  mmoSM3WOTSPublicKey.Clear;
  for I := Low(TCnWOTSSM3PublicKey) to High(TCnWOTSSM3PublicKey) do
    mmoSM3WOTSPublicKey.Lines.Add(SM3Print(FSM3WOTSPublicKey[I]));
end;

procedure TFormOTS.DumpSM3WOTSSignature;
var
  I: Integer;
begin
  mmoSM3WOTSSignature.Clear;
  for I := Low(TCnWOTSSM3Signature) to High(TCnWOTSSM3Signature) do
    mmoSM3WOTSSignature.Lines.Add(SM3Print(FSM3WOTSSignature[I]));
end;

procedure TFormOTS.btnGenSM3WOTSKeysClick(Sender: TObject);
begin
  if CnWOTSSM3GenerateKeys(FSM3WOTSPrivateKey, FSM3WOTSPublicKey) then
  begin
    DumpSM3WOTSPrivateKey;
    DumpSM3WOTSPublicKey;
    ShowMessage('Generate Keys OK');
  end;
end;

procedure TFormOTS.btnSM3WOTSSignClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSM3WOTSMessage.Lines.Text;
  CnWOTSSM3SignData(@S[1], Length(S), FSM3WOTSPrivateKey, FSM3WOTSSignature);
  DumpSM3WOTSSignature;
end;

procedure TFormOTS.btnSM3WOTSVerifyClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSM3WOTSMessage.Lines.Text;
  if CnWOTSSM3VerifyData(@S[1], Length(S), FSM3WOTSSignature, FSM3WOTSPublicKey) then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');
end;

procedure TFormOTS.DumpSHA256WOTSPrivateKey;
var
  I: Integer;
begin
  mmoSHA256WOTSPrivateKey.Clear;
  for I := Low(TCnWOTSSHA256PrivateKey) to High(TCnWOTSSHA256PrivateKey) do
    mmoSHA256WOTSPrivateKey.Lines.Add(SHA256Print(FSHA256WOTSPrivateKey[I]));
end;

procedure TFormOTS.DumpSHA256WOTSPublicKey;
var
  I: Integer;
begin
  mmoSHA256WOTSPublicKey.Clear;
  for I := Low(TCnWOTSSHA256PublicKey) to High(TCnWOTSSHA256PublicKey) do
    mmoSHA256WOTSPublicKey.Lines.Add(SHA256Print(FSHA256WOTSPublicKey[I]));
end;

procedure TFormOTS.DumpSHA256WOTSSignature;
var
  I: Integer;
begin
  mmoSHA256WOTSSignature.Clear;
  for I := Low(TCnWOTSSHA256Signature) to High(TCnWOTSSHA256Signature) do
    mmoSHA256WOTSSignature.Lines.Add(SHA256Print(FSHA256WOTSSignature[I]));
end;

procedure TFormOTS.btnGenSHA256WOTSKeysClick(Sender: TObject);
begin
  if CnWOTSSHA256GenerateKeys(FSHA256WOTSPrivateKey, FSHA256WOTSPublicKey) then
  begin
    DumpSHA256WOTSPrivateKey;
    DumpSHA256WOTSPublicKey;
    ShowMessage('Generate Keys OK');
  end;
end;

procedure TFormOTS.btnSHA256WOTSSignClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSHA256WOTSMessage.Lines.Text;
  CnWOTSSHA256SignData(@S[1], Length(S), FSHA256WOTSPrivateKey, FSHA256WOTSSignature);
  DumpSHA256WOTSSignature;
end;

procedure TFormOTS.btnSHA256WOTSVerifyClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSHA256WOTSMessage.Lines.Text;
  if CnWOTSSHA256VerifyData(@S[1], Length(S), FSHA256WOTSSignature, FSHA256WOTSPublicKey) then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');
end;

procedure TFormOTS.btnGenSM3WOTSPlusKeysClick(Sender: TObject);
begin
  if CnWOTSPlusSM3GenerateKeys(FSM3WOTSPlusPrivateKey, FSM3WOTSPlusPublicKey, FSM3WOTSPlusMask) then
  begin
    DumpSM3WOTSPlusPrivateKey;
    DumpSM3WOTSPlusPublicKey;
    DumpSM3WOTSPlusMask;
    ShowMessage('Generate Keys OK');
  end;
end;

procedure TFormOTS.DumpSM3WOTSPlusPrivateKey;
var
  I: Integer;
begin
  mmoSM3WOTSPlusPrivateKey.Clear;
  for I := Low(TCnWOTSPlusSM3PrivateKey) to High(TCnWOTSPlusSM3PrivateKey) - 1 do
    mmoSM3WOTSPlusPrivateKey.Lines.Add(SM3Print(FSM3WOTSPlusPrivateKey[I]));
end;

procedure TFormOTS.DumpSM3WOTSPlusPublicKey;
var
  I: Integer;
begin
  mmoSM3WOTSPlusPublicKey.Clear;
  for I := Low(TCnWOTSPlusSM3PublicKey) to High(TCnWOTSPlusSM3PublicKey) - 1 do
    mmoSM3WOTSPlusPublicKey.Lines.Add(SM3Print(FSM3WOTSPlusPublicKey[I]));
end;

procedure TFormOTS.DumpSM3WOTSPlusSignature;
var
  I: Integer;
begin
  mmoSM3WOTSPlusSignature.Clear;
  for I := Low(TCnWOTSPlusSM3Signature) to High(TCnWOTSPlusSM3Signature) - 1 do
    mmoSM3WOTSPlusSignature.Lines.Add(SM3Print(FSM3WOTSPlusSignature[I]));
end;

procedure TFormOTS.DumpSM3WOTSPlusMask;
var
  I: Integer;
begin
  mmoSM3WOTSPlusMask.Clear;
  for I := Low(FSM3WOTSPlusMask) to High(FSM3WOTSPlusMask) do
    mmoSM3WOTSPlusMask.Lines.Add(SM3Print(FSM3WOTSPlusMask[I]));
end;

procedure TFormOTS.btnSM3WOTSPlusSignClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSM3WOTSPlusMessage.Lines.Text;
  CnWOTSPlusSM3SignData(@S[1], Length(S), FSM3WOTSPlusPrivateKey, FSM3WOTSPlusMask, FSM3WOTSPlusSignature);
  DumpSM3WOTSPlusSignature;
end;

procedure TFormOTS.btnSM3WOTSPlusVerifyClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSM3WOTSPlusMessage.Lines.Text;
  if CnWOTSPlusSM3VerifyData(@S[1], Length(S), FSM3WOTSPlusSignature, FSM3WOTSPlusPublicKey, FSM3WOTSPlusMask) then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');
end;

procedure TFormOTS.btnGenSHA256WOTSPlusKeysClick(Sender: TObject);
begin
  if CnWOTSPlusSHA256GenerateKeys(FSHA256WOTSPlusPrivateKey, FSHA256WOTSPlusPublicKey, FSHA256WOTSPlusMask) then
  begin
    DumpSHA256WOTSPlusPrivateKey;
    DumpSHA256WOTSPlusPublicKey;
    DumpSHA256WOTSPlusMask;
    ShowMessage('Generate Keys OK');
  end;
end;

procedure TFormOTS.DumpSHA256WOTSPlusPrivateKey;
var
  I: Integer;
begin
  mmoSHA256WOTSPlusPrivateKey.Clear;
  for I := Low(TCnWOTSPlusSHA256PrivateKey) to High(TCnWOTSPlusSHA256PrivateKey) - 1 do
    mmoSHA256WOTSPlusPrivateKey.Lines.Add(SHA256Print(FSHA256WOTSPlusPrivateKey[I]));
end;

procedure TFormOTS.DumpSHA256WOTSPlusPublicKey;
var
  I: Integer;
begin
  mmoSHA256WOTSPlusPublicKey.Clear;
  for I := Low(TCnWOTSPlusSHA256PublicKey) to High(TCnWOTSPlusSHA256PublicKey) - 1 do
    mmoSHA256WOTSPlusPublicKey.Lines.Add(SHA256Print(FSHA256WOTSPlusPublicKey[I]));
end;

procedure TFormOTS.DumpSHA256WOTSPlusSignature;
var
  I: Integer;
begin
  mmoSHA256WOTSPlusSignature.Clear;
  for I := Low(TCnWOTSPlusSHA256Signature) to High(TCnWOTSPlusSHA256Signature) - 1 do
    mmoSHA256WOTSPlusSignature.Lines.Add(SHA256Print(FSHA256WOTSPlusSignature[I]));
end;

procedure TFormOTS.DumpSHA256WOTSPlusMask;
var
  I: Integer;
begin
  mmoSHA256WOTSPlusMask.Clear;
  for I := Low(FSHA256WOTSPlusMask) to High(FSHA256WOTSPlusMask) do
    mmoSHA256WOTSPlusMask.Lines.Add(SHA256Print(FSHA256WOTSPlusMask[I]));
end;

procedure TFormOTS.btnSHA256WOTSPlusSignClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSHA256WOTSPlusMessage.Lines.Text;
  CnWOTSPlusSHA256SignData(@S[1], Length(S), FSHA256WOTSPlusPrivateKey, FSHA256WOTSPlusMask, FSHA256WOTSPlusSignature);
  DumpSHA256WOTSPlusSignature;
end;

procedure TFormOTS.btnSHA256WOTSPlusVerifyClick(Sender: TObject);
var
  S: AnsiString;
begin
  S := mmoSHA256WOTSPlusMessage.Lines.Text;
  if CnWOTSPlusSHA256VerifyData(@S[1], Length(S), FSHA256WOTSPlusSignature, FSHA256WOTSPlusPublicKey, FSHA256WOTSPlusMask) then
    ShowMessage('Verify OK')
  else
    ShowMessage('Verify Fail');
end;

end.
