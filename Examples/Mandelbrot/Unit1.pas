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
  CnMandelbrotImage;

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
  end;
end;

procedure TFormMandelbrot.ImageClick(Sender: TObject);
var
  P: TPoint;
  R, I: Extended;
  OW, OH: Extended;
  Img: TCnMandelbrotImage;
begin
  Img := Sender as TCnMandelbrotImage;
  P := Img.ScreenToClient(Point(Mouse.CursorPos.X, Mouse.CursorPos.Y));
  Img.GetComplexValues(P.x, P.y, R, I);
  lblMark.Caption := Format('X %d Y %d ***** %8.8f + %8.8f i', [P.x, P.y, R, I]);

  OW := Img.MaxX - Img.MinX;
  OH := Img.MaxY - Img.MinY;

  Img.MinX := R - OW / (2 * ENLARGE_FACTOR);
  Img.MinY := I - OH / (2 * ENLARGE_FACTOR);

  Img.MaxX := R + OW / (2 * ENLARGE_FACTOR);
  Img.MaxY := I + OH / (2 * ENLARGE_FACTOR);
end;

end.
