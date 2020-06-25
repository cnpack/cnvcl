unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFormMandelbrot = class(TForm)
    lblMark: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    procedure ImageClick(Sender: TObject);
    function OnFloatColor(Sender: TObject; X, Y: Extended;
      XZ, YZ: Extended; Count: Integer): TColor;
  public
    { Public declarations }
  end;

var
  FormMandelbrot: TFormMandelbrot;

implementation

uses
  CnMandelbrotImage, CnBigRational;

{$R *.DFM}

const
  ENLARGE_FACTOR = 10;

procedure TFormMandelbrot.FormCreate(Sender: TObject);
begin
  with TCnMandelbrotImage.Create(Self) do
  begin
    Parent := Self;
    Left := 10;
    Top := 10;
    Width := 800;
    Height := 800;
    Anchors := [akLeft, akTop, akBottom, akRight];
    ShowAxis := True;
    OnClick := ImageClick;
    OnColor := OnFloatColor;
  end;
end;

procedure TFormMandelbrot.ImageClick(Sender: TObject);
var
  P: TPoint;
  R, I: Extended;
  OW, OH: Extended;
  Img: TCnMandelbrotImage;
  RR, RI, ROW, ROH, X1, X2, Y1, Y2: TCnBigRational;
begin
  Img := Sender as TCnMandelbrotImage;
  P := Img.ScreenToClient(Point(Mouse.CursorPos.X, Mouse.CursorPos.Y));
  if Img.Mode = mmFloat then
  begin
    Img.GetComplexValues(P.x, P.y, R, I);
    lblMark.Caption := Format('X %d Y %d ***** %8.8f + %8.8f i', [P.x, P.y, R, I]);

    OW := Img.MaxX - Img.MinX;
    OH := Img.MaxY - Img.MinY;

    Img.MinX := R - OW / (2 * ENLARGE_FACTOR);
    Img.MinY := I - OH / (2 * ENLARGE_FACTOR);

    Img.MaxX := R + OW / (2 * ENLARGE_FACTOR);
    Img.MaxY := I + OH / (2 * ENLARGE_FACTOR);
  end
  else if Img.Mode = mmBigRational then
  begin
    RR := TCnBigRational.Create;
    RI := TCnBigRational.Create;

    Img.GetComplexRational(P.x, P.y, RR, RI);
    Caption := Format('X %d Y %d ***** %s + %s i', [P.x, P.y, RR.ToDec(20), RI.ToDec(20)]);

    ROW := TCnBigRational.Create;
    ROH := TCnBigRational.Create;

    X1 := TCnBigRational.Create;
    X2 := TCnBigRational.Create;
    Y1 := TCnBigRational.Create;
    Y2 := TCnBigRational.Create;

    X1.Assign(RR);
    X1.Sub(ROW);
    X1.Divide(2 * ENLARGE_FACTOR);

    Y1.Assign(RI);
    Y1.Sub(ROH);
    Y1.Divide(2 * ENLARGE_FACTOR);

    X2.Assign(RR);
    X2.Add(ROW);
    X2.Divide(2 * ENLARGE_FACTOR);

    Y2.Assign(RI);
    Y2.Add(ROH);
    Y2.Divide(2 * ENLARGE_FACTOR);

    Img.SetRect(X1, X2, Y1, Y2);

    X1.Free;
    X2.Free;
    Y1.Free;
    Y2.Free;
    ROW.Free;
    ROH.Free;
    RR.Free;
    RI.Free;
  end
  else
  begin

  end;
end;

function TFormMandelbrot.OnFloatColor(Sender: TObject; X, Y, XZ,
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
    // R := R * 10;
    Result := RGB(R, R, R);
  end;
end;

end.
