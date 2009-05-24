unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnWaterImage, jpeg, ComCtrls, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    CnWaterImage1: TCnWaterImage;
    lbl1: TLabel;
    TrackBar1: TTrackBar;
    lbl2: TLabel;
    TrackBar2: TTrackBar;
    lbl3: TLabel;
    TrackBar3: TTrackBar;
    lbl4: TLabel;
    TrackBar4: TTrackBar;
    btn1: TButton;
    btn2: TButton;
    lbl5: TLabel;
    TrackBar5: TTrackBar;
    tmr1: TTimer;
    procedure tbChanged(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CnWaterImage1BeforeRender(Sender: TObject; ABitmap: TBitmap);
    procedure tmr1Timer(Sender: TObject);
  private
    { Private declarations }
    FY: Integer;
    FControlUpdating: Boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FY := CnWaterImage1.Height;
  if not FControlUpdating then
  begin
    FControlUpdating := True;
    try
      TrackBar1.Position := CnWaterImage1.RandomDelay div 100;
      TrackBar2.Position := CnWaterImage1.RandomBlob div 50;
      TrackBar3.Position := CnWaterImage1.TrackBlob div 50;
      TrackBar4.Position := CnWaterImage1.ClickBlob div 50;
      TrackBar5.Position := CnWaterImage1.Damping div 5;
    finally
      FControlUpdating := False;
    end;
  end;
end;

procedure TForm1.tbChanged(Sender: TObject);
begin
  if not FControlUpdating then
  begin
    FControlUpdating := True;
    try
      CnWaterImage1.RandomDelay := TrackBar1.Position * 100;
      CnWaterImage1.RandomBlob := TrackBar2.Position * 50;
      CnWaterImage1.TrackBlob := TrackBar3.Position * 50;
      CnWaterImage1.ClickBlob := TrackBar4.Position * 50;
      CnWaterImage1.Damping := TrackBar5.Position * 5;
    finally
      FControlUpdating := False;
    end;
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
begin
  CnWaterImage1.ClearWater;
end;

procedure TForm1.btn2Click(Sender: TObject);
begin
  CnWaterImage1.Blob(-1, -1, 2, Random(1500) + 500);
end;

procedure TForm1.CnWaterImage1BeforeRender(Sender: TObject;
  ABitmap: TBitmap);
begin
  with ABitmap.Canvas do
  begin
    Font.Name := 'Tahoma';
    Font.Size := 20;
    Font.Color := clRed;
    Brush.Style := bsClear;
    TextOut(50, FY, 'Scrolling Text...');
  end;
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  Dec(FY);
  if FY < -30 then
    FY := CnWaterImage1.Height;
end;

end.
