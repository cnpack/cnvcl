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
    // InfiniteMode := True;
    OnClick := ImageClick;
  end;
end;

procedure TFormMandelbrot.ImageClick(Sender: TObject);
var
  P: TPoint;
  R, I: Extended;
  OW, OH: Extended;
  Img: TCnMandelbrotImage;
  RR, RI, ROW, ROH, X1, X2, Y1, Y2: TCnBigRationalNumber;
begin
  Img := Sender as TCnMandelbrotImage;
  P := Img.ScreenToClient(Point(Mouse.CursorPos.X, Mouse.CursorPos.Y));
  if not Img.InfiniteMode then
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
  else
  begin
    RR := TCnBigRationalNumber.Create;
    RI := TCnBigRationalNumber.Create;

    Img.GetComplexRational(P.x, P.y, RR, RI);
    Caption := Format('X %d Y %d ***** %s + %s i', [P.x, P.y, RR.ToDec(20), RI.ToDec(20)]);

    ROW := TCnBigRationalNumber.Create;
    ROH := TCnBigRationalNumber.Create;

    X1 := TCnBigRationalNumber.Create;
    X2 := TCnBigRationalNumber.Create;
    Y1 := TCnBigRationalNumber.Create;
    Y2 := TCnBigRationalNumber.Create;

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

  end;
end;

end.
