unit UnitSecretSharing;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnContainers, CnBigNumber;

type
  TFormSecretSharing = class(TForm)
    pgc1: TPageControl;
    tsInt64Shamir: TTabSheet;
    grpInt64Shamir: TGroupBox;
    btnInt64ShamirSample: TButton;
    mmoInt64Shamir: TMemo;
    btnBNShamirSample: TButton;
    mmoBNShamir: TMemo;
    btnBNShamirSample2: TButton;
    btnInt64ShamirSample2: TButton;
    tsFeldmanVSS: TTabSheet;
    grpFeldman: TGroupBox;
    btnFeldmanGen: TButton;
    btnInt64FeldmanCheckParam: TButton;
    btnFeldmanCheckParam2: TButton;
    btnInt64FeldmanGen: TButton;
    btnInt64Feldman: TButton;
    mmoInt64Feldman: TMemo;
    btnFeldmanSample: TButton;
    mmoBNFeldman: TMemo;
    procedure btnInt64ShamirSampleClick(Sender: TObject);
    procedure btnBNShamirSampleClick(Sender: TObject);
    procedure btnBNShamirSample2Click(Sender: TObject);
    procedure btnInt64ShamirSample2Click(Sender: TObject);
    procedure btnFeldmanGenClick(Sender: TObject);
    procedure btnInt64FeldmanCheckParamClick(Sender: TObject);
    procedure btnFeldmanCheckParam2Click(Sender: TObject);
    procedure btnInt64FeldmanGenClick(Sender: TObject);
    procedure btnInt64FeldmanClick(Sender: TObject);
    procedure btnFeldmanSampleClick(Sender: TObject);
  private
    function CheckDHGenerator(const HexPrime: string): Boolean;
  public

  end;

var
  FormSecretSharing: TFormSecretSharing;

implementation

uses
  CnSecretSharing;

{$R *.DFM}

procedure TFormSecretSharing.btnInt64ShamirSampleClick(Sender: TObject);
var
  I: Integer;
  S, P: Int64;
  Shares, X, Y: TCnInt64List;
begin
  S := 23333;
  P := 0;

  Shares := TCnInt64List.Create;
  if CnInt64ShamirSplit(S, 5, 3, Shares, P) then
  begin
    mmoInt64Shamir.Lines.Clear;
    mmoInt64Shamir.Lines.Add('Prime: ' + IntToStr(P));
    mmoInt64Shamir.Lines.Add('Secret: ' + IntToStr(S));
    mmoInt64Shamir.Lines.Add('');

    for I := 0 to Shares.Count - 1 do
      mmoInt64Shamir.Lines.Add(IntToStr(I + 1) + ', ' + IntToStr(Shares[I]));
  end;

  X := TCnInt64List.Create;
  Y := TCnInt64List.Create;

  X.Add(1);
  X.Add(3);
  X.Add(5);
  Y.Add(Shares[0]);
  Y.Add(Shares[2]);
  Y.Add(Shares[4]);

  if CnInt64ShamirReconstruct(P, X, Y, S) then
    ShowMessage(IntToStr(S));

  Y.Free;
  X.Free;
  Shares.Free;
end;

procedure TFormSecretSharing.btnBNShamirSampleClick(Sender: TObject);
var
  I: Integer;
  S, P: TCnBigNumber;
  Orders, Shares, X, Y: TCnBigNumberList;
begin
  S := TCnBigNumber.FromDec('23333333333333874874874874253253');
  P := TCnBigNumber.Create;

  Orders := TCnBigNumberList.Create;
  Shares := TCnBigNumberList.Create;
  if CnShamirSplit(S, 5, 3, Orders, Shares, P) then
  begin
    mmoBNShamir.Lines.Clear;
    mmoBNShamir.Lines.Add('Prime: ' + P.ToString);
    mmoBNShamir.Lines.Add('Secret: ' + S.ToString);
    mmoBNShamir.Lines.Add('');

    for I := 0 to Shares.Count - 1 do
      mmoBNShamir.Lines.Add(Orders[I].ToString + ', ' + Shares[I].ToString);
  end;

  X := TCnBigNumberList.Create;
  Y := TCnBigNumberList.Create;

  X.Add.SetWord(1);
  X.Add.SetWord(3);
  X.Add.SetWord(5);
  BigNumberCopy(Y.Add, Shares[0]);
  BigNumberCopy(Y.Add, Shares[2]);
  BigNumberCopy(Y.Add, Shares[4]);

  if CnShamirReconstruct(P, X, Y, S) then
    ShowMessage(S.ToDec);

  Y.Free;
  X.Free;
  Shares.Free;
  Orders.Free;
  P.Free;
  S.Free;
end;

procedure TFormSecretSharing.btnBNShamirSample2Click(Sender: TObject);
var
  S, P: TCnBigNumber;
  X, Y: TCnBigNumberList;
begin
  S := TCnBigNumber.FromDec('0');
  P := TCnBigNumber.FromDec('65537');

  X := TCnBigNumberList.Create;
  Y := TCnBigNumberList.Create;

  X.Add.SetWord(1);
  X.Add.SetWord(3);
  X.Add.SetWord(5);
  Y.Add.SetWord(12311);
  Y.Add.SetWord(22738);
  Y.Add.SetWord(54614);

  if CnShamirReconstruct(P, X, Y, S) then
    ShowMessage(S.ToDec);

  Y.Free;
  X.Free;
  P.Free;
  S.Free;
end;

procedure TFormSecretSharing.btnInt64ShamirSample2Click(Sender: TObject);
var
  S, P: Int64;
  X, Y: TCnInt64List;
begin
  S := 23333;
  P := 65537;

  X := TCnInt64List.Create;
  Y := TCnInt64List.Create;

  X.Add(1);
  X.Add(3);
  X.Add(5);
  Y.Add(12311);
  Y.Add(22738);
  Y.Add(54614);

  if CnInt64ShamirReconstruct(P, X, Y, S) then
    ShowMessage(IntToStr(S));

  Y.Free;
  X.Free;
end;

procedure TFormSecretSharing.btnFeldmanGenClick(Sender: TObject);
var
  P, Q, G: TCnBigNumber;
begin
  P := TCnBigNumber.Create;
  Q := TCnBigNumber.Create;
  G := TCnBigNumber.Create;

//  if CnInt64FeldmanVssGeneratePrime(P, G) then
//  begin
//    ShowMessage('Get');
//  end;

  G.Free;
  Q.Free;
  P.Free;
end;

function TFormSecretSharing.CheckDHGenerator(
  const HexPrime: string): Boolean;
var
  P, Q, G, R: TCnBigNumber;
begin
  P := TCnBigNumber.FromHex(HexPrime);
  Q := TCnBigNumber.Create;

  BigNumberCopy(Q, P);
  Q.SubWord(1);
  Q.ShiftRightOne;

  G := TCnBigNumber.Create;
  G.SetWord(2);

  // 检查 G^Q mod P = 1
  R := TCnBigNumber.Create;
  BigNumberPowerMod(R, G, Q, P);

  Result := R.IsOne;

  R.Free;
  G.Free;
  Q.Free;
  P.Free;
end;

procedure TFormSecretSharing.btnInt64FeldmanCheckParamClick(
  Sender: TObject);
var
  T: TCnBigNumber;
  R: TCnBigNumber;
begin
  T := TCnBigNumber.Create;
  R := TCnBigNumber.Create;

  T.SetHex(CN_PRIME_FFDHE_2048);
  T.SubWord(1);
  T.ShiftRightOne;
  if BigNumberIsProbablyPrime(T, 30) then
    ShowMessage('(FFDHE_2048 - 1) / 2 is a Prime');

  T.SetHex(CN_PRIME_FFDHE_3072);
  T.SubWord(1);
  T.ShiftRightOne;
  if BigNumberIsProbablyPrime(T, 25) then
    ShowMessage('(FFDHE_3072 - 1) / 2 is a Prime');

  T.SetHex(CN_PRIME_FFDHE_4096);
  T.SubWord(1);
  T.ShiftRightOne;
  if BigNumberIsProbablyPrime(T, 20) then
    ShowMessage('(FFDHE_4096 - 1) / 2 is a Prime');

  T.SetHex(CN_PRIME_FFDHE_6144);
  T.SubWord(1);
  T.ShiftRightOne;
  if BigNumberIsProbablyPrime(T, 15) then
    ShowMessage('(FFDHE_6144 - 1) / 2 is a Prime');

  T.SetHex(CN_PRIME_FFDHE_8192);
  T.SubWord(1);
  T.ShiftRightOne;
  if BigNumberIsProbablyPrime(T, 10) then
    ShowMessage('(FFDHE_8192 - 1) / 2 is a Prime');

  T.Free;
  R.Free;
end;

procedure TFormSecretSharing.btnFeldmanCheckParam2Click(Sender: TObject);
begin
  // RFC 7919 中定义的大素数 P 与生成元 2 并不符合 CnPrimeNumber 中的要求

  // 对于 P = 11, Q = 5 的情况，2 符合本原根 2^5 mod 11 <> 1 的要求
  // 3 符合生成元的 3^5 mod 11 = 1 的要求 ，
  // DH 中到底（11，3）可用还是（11，2）可用？ 貌似是后者，但前者的意义是啥？

  if CheckDHGenerator(CN_PRIME_FFDHE_2048) then
    ShowMessage('FFDHE_2048 Generator is 2');

  if CheckDHGenerator(CN_PRIME_FFDHE_3072) then
    ShowMessage('FFDHE_3072 Generator is 2');

  if CheckDHGenerator(CN_PRIME_FFDHE_4096) then
    ShowMessage('FFDHE_4096 Generator is 2');

  if CheckDHGenerator(CN_PRIME_FFDHE_6144) then
    ShowMessage('FFDHE_6144 Generator is 2');

  if CheckDHGenerator(CN_PRIME_FFDHE_8192) then
    ShowMessage('FFDHE_8192 Generator is 2');
end;

procedure TFormSecretSharing.btnInt64FeldmanGenClick(Sender: TObject);
var
  P, G: Int64;
begin
  if CnInt64FeldmanVssGeneratePrime(P, G) then
    ShowMessage(Format('Get Prime %u with Generator %d', [P, G]));
end;

procedure TFormSecretSharing.btnInt64FeldmanClick(Sender: TObject);
var
  I: Integer;
  S, P, G: Int64;
  Shares, Comms, X, Y: TCnInt64List;
begin
//  S := 3; // 23333;
//  P := 11;
//  G := 3;
  S := 23333;
  P := 0;
  G := 0;

  Shares := TCnInt64List.Create;
  Comms := TCnInt64List.Create;
  if CnInt64FeldmanVssSplit(S, 5, 3, Shares, Comms, P, G) then
  begin
    mmoInt64Feldman.Lines.Clear;
    mmoInt64Feldman.Lines.Add('Prime: ' + IntToStr(P));
    mmoInt64Feldman.Lines.Add('Secret: ' + IntToStr(S));
    mmoInt64Feldman.Lines.Add('');

    for I := 0 to Shares.Count - 1 do
      mmoInt64Feldman.Lines.Add(IntToStr(I + 1) + ', ' + IntToStr(Shares[I]));

    mmoInt64Feldman.Lines.Add('');
    mmoInt64Feldman.Lines.Add('Commitments:');
    for I := 0 to Comms.Count - 1 do
      mmoInt64Feldman.Lines.Add(IntToStr(Comms[I]));
  end
  else
    Exit;

  mmoInt64Feldman.Lines.Add('');
  mmoInt64Feldman.Lines.Add('Verify:');
  for I := 0 to Shares.Count - 1 do
  begin
    if CnInt64FeldmanVssVerify(P, G, I + 1, Shares[I], Comms) then
      mmoInt64Feldman.Lines.Add('Share #' + IntToStr(I + 1) + ' Verify OK')
    else
      mmoInt64Feldman.Lines.Add('Share #' + IntToStr(I + 1) + ' Verify Fail');
  end;

  X := TCnInt64List.Create;
  Y := TCnInt64List.Create;

  X.Add(1);
  X.Add(3);
  X.Add(5);
  Y.Add(Shares[0]);
  Y.Add(Shares[2]);
  Y.Add(Shares[4]);

  if CnInt64FeldmanVssReconstruct(P, G, X, Y, Comms, S) then
    ShowMessage(IntToStr(S));

  Y.Free;
  X.Free;
  Shares.Free;
end;

procedure TFormSecretSharing.btnFeldmanSampleClick(Sender: TObject);
var
  I: Integer;
  S, P, G, O: TCnBigNumber;
  Orders, Shares, Comms, X, Y: TCnBigNumberList;
begin
  S := TCnBigNumber.FromDec('23333333333333874874874874253253');
  P := TCnBigNumber.FromHex(CN_PRIME_FFDHE_2048);
  G := TCnBigNumber.FromDec('2');

  Orders := TCnBigNumberList.Create;
  Shares := TCnBigNumberList.Create;
  Comms := TCnBigNumberList.Create;

  if CnFeldmanVssSplit(S, 5, 3, Orders, Shares, Comms, P, G) then
  begin
    mmoBNFeldman.Lines.Clear;
    mmoBNFeldman.Lines.Add('Prime: ' + P.ToString);
    mmoBNFeldman.Lines.Add('Secret: ' + S.ToString);
    mmoBNFeldman.Lines.Add('');

    for I := 0 to Shares.Count - 1 do
      mmoBNFeldman.Lines.Add(Orders[I].ToString + ', ' + Shares[I].ToString);

    mmoBNFeldman.Lines.Add('');
    mmoBNFeldman.Lines.Add('Verify:');

    O := TCnBigNumber.Create;
    for I := 0 to Shares.Count - 1 do
    begin
      O.SetWord(I + 1);
      if CnFeldmanVssVerify(P, G, O, Shares[I], Comms) then
        mmoBNFeldman.Lines.Add('Share #' + IntToStr(I + 1) + ' Verify OK')
      else
        mmoBNFeldman.Lines.Add('Share #' + IntToStr(I + 1) + ' Verify Fail');
    end;
    O.Free;
  end
  else
    Exit;

  X := TCnBigNumberList.Create;
  Y := TCnBigNumberList.Create;

  X.Add.SetWord(1);
  X.Add.SetWord(3);
  X.Add.SetWord(5);
  BigNumberCopy(Y.Add, Shares[0]);
  BigNumberCopy(Y.Add, Shares[2]);
  BigNumberCopy(Y.Add, Shares[4]);

  if CnFeldmanVssReconstruct(P, G, X, Y, Comms, S) then
    ShowMessage(S.ToDec);

  Y.Free;
  X.Free;
  Comms.Free;
  Shares.Free;
  Orders.Free;
  P.Free;
  S.Free;
  G.Free;
end;

end.
