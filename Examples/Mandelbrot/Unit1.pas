unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CnGraphics, CnMandelbrotImage, CnBigDecimal;

type
  TFormMandelbrot = class(TForm)
    lblMark: TLabel;
    grpInfo: TGroupBox;
    edtMaxY: TEdit;
    edtMinY: TEdit;
    edtMinX: TEdit;
    edtMaxX: TEdit;
    lblDigits: TLabel;
    lblPoint: TLabel;
    cbbMode: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure cbbModeChange(Sender: TObject);
  private
    FImage: TCnMandelbrotImage;
    procedure ImageClick(Sender: TObject);
    function OnFloatColor1(Sender: TObject; X, Y: Extended;
      XZ, YZ: Extended; Count: Integer): TColor;
    function OnFloatColor2(Sender: TObject; X, Y: Extended;
      XZ, YZ: Extended; Count: Integer): TColor;
    function OnDecimalColor1(Sender: TObject; X, Y: TCnBigDecimal;
      XZ, YZ: TCnBigDecimal; Count: Integer): TColor;
    function OnDecimalColor2(Sender: TObject; X, Y: TCnBigDecimal;
      XZ, YZ: TCnBigDecimal; Count: Integer): TColor;
    function OnBinaryColor1(Sender: TObject; X, Y: TCnBigBinary;
      XZ, YZ: TCnBigBinary; Count: Integer): TColor;
    function OnBinaryColor2(Sender: TObject; X, Y: TCnBigBinary;
      XZ, YZ: TCnBigBinary; Count: Integer): TColor;
    procedure ShowEdges;
  public
    { Public declarations }
  end;

var
  FormMandelbrot: TFormMandelbrot;

implementation

{$R *.DFM}

const
  ENLARGE_FACTOR_DECIMAL = 10;
  ENLARGE_FACTOR_BINARY = 4;

procedure TFormMandelbrot.FormCreate(Sender: TObject);
begin
  FImage := TCnMandelbrotImage.Create(Self);
  with FImage do
  begin
    Parent := Self;
    Lock;
    Left := 10;
    Top := 10;
    Width := 800;
    Height := 800;
    Anchors := [akLeft, akTop, akBottom, akRight];
    ShowAxis := True;
    //Mode := mmBigDecimal;
    OnClick := ImageClick;
    OnColor := OnFloatColor2;
    OnDecimalColor := OnDecimalColor2;
    OnBinaryColor := OnBinaryColor2;
    UnLock;
  end;
  cbbMode.ItemIndex := Ord(FImage.Mode);
  ShowEdges;
end;

procedure TFormMandelbrot.ImageClick(Sender: TObject);
var
  P: TPoint;
  R, I: Extended;
  OW, OH: Extended;
  Img: TCnMandelbrotImage;
  DR, DI, DOW, DOH, XD1, XD2, YD1, YD2: TCnBigDecimal;
  BR, BI, BOW, BOH, XB1, XB2, YB1, YB2: TCnBigBinary;
begin
  Img := Sender as TCnMandelbrotImage;
  P := Img.ScreenToClient(Point(Mouse.CursorPos.X, Mouse.CursorPos.Y));
  if Img.Mode = mmFloat then
  begin
    Img.GetComplexValues(P.x, P.y, R, I);
    lblPoint.Caption := Format('X %d Y %d ***** %8.8f + %8.8f i', [P.x, P.y, R, I]);

    OW := Img.MaxX - Img.MinX;
    OH := Img.MaxY - Img.MinY;

    Img.Lock;
    Img.SetRect(R - OW / (2 * ENLARGE_FACTOR_DECIMAL), R + OW / (2 * ENLARGE_FACTOR_DECIMAL),
      I - OH / (2 * ENLARGE_FACTOR_DECIMAL), I + OH / (2 * ENLARGE_FACTOR_DECIMAL));
    Img.UnLock;
  end
  else if Img.Mode = mmBigDecimal then
  begin
    DR := TCnBigDecimal.Create;
    DI := TCnBigDecimal.Create;

    Img.GetComplexDecimal(P.x, P.y, DR, DI);
    lblPoint.Caption := Format('X %d Y %d ***** %s + %s i', [P.x, P.y, DR.ToString, DI.ToString]);

    DOW := TCnBigDecimal.Create;
    DOH := TCnBigDecimal.Create;
    BigDecimalSub(DOW, Img.MaxDX, Img.MinDX);
    BigDecimalSub(DOH, Img.MaxDY, Img.MinDY);

    DOW.DivWord(2 * ENLARGE_FACTOR_DECIMAL);   // TODO: 要逐渐将精度放大
    DOH.DivWord(2 * ENLARGE_FACTOR_DECIMAL);

    XD1 := TCnBigDecimal.Create;
    XD2 := TCnBigDecimal.Create;
    YD1 := TCnBigDecimal.Create;
    YD2 := TCnBigDecimal.Create;

    BigDecimalCopy(XD1, DR);
    BigDecimalSub(XD1, XD1, DOW);

    BigDecimalCopy(YD1, DI);
    BigDecimalSub(YD1, YD1, DOH);

    BigDecimalCopy(XD2, DR);
    BigDecimalAdd(XD2, XD2, DOW);

    BigDecimalCopy(YD2, DI);
    BigDecimalAdd(YD2, YD2, DOH);

    Img.SetRect(XD1, XD2, YD1, YD2);

    XD1.Free;
    XD2.Free;
    YD1.Free;
    YD2.Free;
    DOW.Free;
    DOH.Free;
    DR.Free;
    DI.Free;
  end
  else if Img.Mode = mmBigBinary then
  begin
    BR := TCnBigBinary.Create;
    BI := TCnBigBinary.Create;

    Img.GetComplexBinary(P.x, P.y, BR, BI);
    lblPoint.Caption := Format('X %d Y %d ***** %s + %s i', [P.x, P.y, BR.ToString, BI.ToString]);

    BOW := TCnBigBinary.Create;
    BOH := TCnBigBinary.Create;
    BigBinarySub(BOW, Img.MaxBX, Img.MinBX);
    BigBinarySub(BOH, Img.MaxBY, Img.MinBY);

    BOW.DivWord(2 * ENLARGE_FACTOR_BINARY);   // TODO: 要逐渐将精度放大
    BOH.DivWord(2 * ENLARGE_FACTOR_BINARY);

    XB1 := TCnBigBinary.Create;
    XB2 := TCnBigBinary.Create;
    YB1 := TCnBigBinary.Create;
    YB2 := TCnBigBinary.Create;

    BigBinaryCopy(XB1, BR);
    BigBinarySub(XB1, XB1, BOW);

    BigBinaryCopy(YB1, BI);
    BigBinarySub(YB1, YB1, BOH);

    BigBinaryCopy(XB2, BR);
    BigBinaryAdd(XB2, XB2, BOW);

    BigBinaryCopy(YB2, BI);
    BigBinaryAdd(YB2, YB2, BOH);

    Img.SetRect(XB1, XB2, YB1, YB2);

    XB1.Free;
    XB2.Free;
    YB1.Free;
    YB2.Free;
    BOW.Free;
    BOH.Free;
    BR.Free;
    BI.Free;
  end;
  ShowEdges;
end;

function TFormMandelbrot.OnDecimalColor1(Sender: TObject; X, Y, XZ,
  YZ: TCnBigDecimal; Count: Integer): TColor;
var
  R: Byte;
begin
  if Count > CN_MANDELBROT_MAX_COUNT then
    Result := clNavy  // 收敛，用深蓝色
  else
  begin
    if 3 * Count > 255 then
      R := 0
    else
      R := 255 - 3 * Byte(Count); // 次数越多越黑
    Result := RGB(R, R, R);
  end;
end;

function TFormMandelbrot.OnDecimalColor2(Sender: TObject; X, Y, XZ,
  YZ: TCnBigDecimal; Count: Integer): TColor;
begin
  if Count > CN_MANDELBROT_MAX_COUNT then
    Result := clNavy  // 收敛，用深蓝色
  else
  begin
    // 用 Count 做色相
    Result := HSLRangeToRGB(Count, 240, 120);
  end;
end;

function TFormMandelbrot.OnFloatColor1(Sender: TObject; X, Y, XZ,
  YZ: Extended; Count: Integer): TColor;
var
  R: Byte;
begin
  if Count > CN_MANDELBROT_MAX_COUNT then
    Result := clNavy  // 收敛，用深蓝色
  else
  begin
    if 3 * Count > 255 then
      R := 0
    else
      R := 255 - 3 * Byte(Count); // 次数越多越黑
    Result := RGB(R, R, R);
  end;
end;

function TFormMandelbrot.OnFloatColor2(Sender: TObject; X, Y, XZ,
  YZ: Extended; Count: Integer): TColor;
begin
  if Count > CN_MANDELBROT_MAX_COUNT then
    Result := clNavy  // 收敛，用深蓝色
  else
  begin
    // 用 Count 做色相
    Result := HSLRangeToRGB(Count, 240, 120);
  end;
end;

procedure TFormMandelbrot.ShowEdges;
begin
  if FImage.Mode = mmFloat then
  begin
    edtMinX.Text := FloatToStr(FImage.MinX);
    edtMaxX.Text := FloatToStr(FImage.MaxX);
    edtMinY.Text := FloatToStr(FImage.MinY);
    edtMaxY.Text := FloatToStr(FImage.MaxY);
  end
  else if FImage.Mode = mmBigDecimal then
  begin
    edtMinX.Text := FImage.MinDX.ToString;
    edtMaxX.Text := FImage.MaxDX.ToString;
    edtMinY.Text := FImage.MinDY.ToString;
    edtMaxY.Text := FImage.MaxDY.ToString;
  end
  else if FImage.Mode = mmBigBinary then
  begin
    edtMinX.Text := FImage.MinBX.ToString;
    edtMaxX.Text := FImage.MaxBX.ToString;
    edtMinY.Text := FImage.MinBY.ToString;
    edtMaxY.Text := FImage.MaxBY.ToString;
  end;
  lblDigits.Caption := 'Digits: ' + IntToStr(FImage.GetCurrentCalcDigits);
end;

procedure TFormMandelbrot.cbbModeChange(Sender: TObject);
begin
  FImage.Mode := TCnMandelbrotMode(cbbMode.ItemIndex);
  ShowEdges;
end;

function TFormMandelbrot.OnBinaryColor1(Sender: TObject; X, Y, XZ,
  YZ: TCnBigBinary; Count: Integer): TColor;
var
  R: Byte;
begin
  if Count > CN_MANDELBROT_MAX_COUNT then
    Result := clNavy  // 收敛，用深蓝色
  else
  begin
    if 3 * Count > 255 then
      R := 0
    else
      R := 255 - 3 * Byte(Count); // 次数越多越黑
    Result := RGB(R, R, R);
  end;
end;

function TFormMandelbrot.OnBinaryColor2(Sender: TObject; X, Y, XZ,
  YZ: TCnBigBinary; Count: Integer): TColor;
begin
  if Count > CN_MANDELBROT_MAX_COUNT then
    Result := clNavy  // 收敛，用深蓝色
  else
  begin
    // 用 Count 做色相
    Result := HSLRangeToRGB(Count, 240, 120);
  end;
end;

end.
