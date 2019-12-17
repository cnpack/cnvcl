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
  R, I: Double;
begin
  P := (Sender as TControl).ScreenToClient(Point(Mouse.CursorPos.X, Mouse.CursorPos.Y));
  (Sender as TCnMandelbrotImage).GetComplexValues(P.x, P.y, R, I);
  lblMark.Caption := Format('X %d Y %d ***** %8.8f + %8.8f i', [P.x, P.y, R, I]);
end;

end.
