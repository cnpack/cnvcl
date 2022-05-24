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
    procedure btnInt64ShamirSampleClick(Sender: TObject);
    procedure btnBNShamirSampleClick(Sender: TObject);
  private

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

end.
