unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CnFitCurve, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    sb1: TScrollBar;
    sb2: TScrollBar;
    sb3: TScrollBar;
    sb4: TScrollBar;
    sb5: TScrollBar;
    sb6: TScrollBar;
    sb7: TScrollBar;
    sb8: TScrollBar;
    sb9: TScrollBar;
    sb10: TScrollBar;
    sb11: TScrollBar;
    sb12: TScrollBar;
    sb13: TScrollBar;
    sb14: TScrollBar;
    sb15: TScrollBar;
    sb16: TScrollBar;
    pb1: TPaintBox;
    procedure sb1Change(Sender: TObject);
    procedure pb1Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.sb1Change(Sender: TObject);
begin
  pb1.Invalidate;
end;

procedure TForm1.pb1Paint(Sender: TObject);
var
  i: Integer;
  bmp: TBitmap;
  curve: TCnFitCurve;
begin
  bmp := nil;
  curve := nil;
  try
    bmp := TBitmap.Create;
    curve := TCnFitCurve.Create;
    bmp.Width := pb1.Width;
    bmp.Height := pb1.Height;
    bmp.Canvas.Brush.Color := clBlack;
    bmp.Canvas.Brush.Style := bsSolid;
    bmp.Canvas.FillRect(Rect(0, 0, bmp.Width, bmp.Height));
    for i := 0 to 15 do
      curve.AddPoint(i * (bmp.Width - 1) / 15,
        TScrollBar(FindComponent('sb' + IntToStr(i + 1))).Position);
    for i := 0 to bmp.Width - 1 do
      bmp.Canvas.Pixels[i, Round(curve.Calc(i) / sb1.Max * bmp.Height)] := clWhite;
    pb1.Canvas.Draw(0, 0, bmp);
  finally
    bmp.Free;
    curve.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  sb1Change(nil);
  DoubleBuffered := True;
end;

end.
