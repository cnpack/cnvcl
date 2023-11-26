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
  private
    FSM3OTSPrivateKey: TCnOTSSM3PrivateKey;
    FSM3OTSPublicKey: TCnOTSSM3PublicKey;
    FSM3OTSSignature: TCnOTSSM3Signature;
    FSM3OTSVerification: TCnOTSSM3VerificationKey;
    FSHA256OTSPrivateKey: TCnOTSSHA256PrivateKey;
    FSHA256OTSPublicKey: TCnOTSSHA256PublicKey;
    FSHA256OTSSignature: TCnOTSSHA256Signature;
    FSHA256OTSVerification: TCnOTSSHA256VerificationKey;
    FSM3WOTSPrivateKey: TCnWOTSSM3PrivateKey;
    FSM3WOTSPublicKey: TCnWOTSSM3PublicKey;
    FSM3WOTSSignature: TCnWOTSSM3Signature;
    FSHA256WOTSPrivateKey: TCnWOTSSHA256PrivateKey;
    FSHA256WOTSPublicKey: TCnWOTSSHA256PublicKey;
    FSHA256WOTSSignature: TCnWOTSSHA256Signature;
  public
    procedure DumpSM3OTSPrivateKey;
    procedure DumpSM3OTSPublicKey;
    procedure DumpSM3OTSSignature;
    procedure DumpSM3OTSVerification;
    procedure DumpSHA256OTSPrivateKey;
    procedure DumpSHA256OTSPublicKey;
    procedure DumpSHA256OTSSignature;
    procedure DumpSHA256OTSVerification;
    procedure DumpSM3WOTSPrivateKey;
    procedure DumpSM3WOTSPublicKey;
    procedure DumpSM3WOTSSignature;
    procedure DumpSHA256WOTSPrivateKey;
    procedure DumpSHA256WOTSPublicKey;
    procedure DumpSHA256WOTSSignature;
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

  FillChar(FSM3WOTSPrivateKey[0], SizeOf(TCnWOTSSM3PrivateKey), 0);
  FillChar(FSM3WOTSPublicKey[0], SizeOf(TCnWOTSSM3PublicKey), 0);
  FillChar(FSM3WOTSSignature[0], SizeOf(TCnWOTSSM3Signature), 0);

  FillChar(FSHA256WOTSPrivateKey[0], SizeOf(TCnWOTSSHA256PrivateKey), 0);
  FillChar(FSHA256WOTSPublicKey[0], SizeOf(TCnWOTSSHA256PublicKey), 0);
  FillChar(FSHA256WOTSSignature[0], SizeOf(TCnWOTSSHA256Signature), 0);
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

end.
