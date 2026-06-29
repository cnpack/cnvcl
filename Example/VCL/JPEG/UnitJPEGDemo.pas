unit UnitJPEGDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, CnNative, CnJPEG;

type
  TFormJPEGDemo = class(TForm)
    PanelTop: TPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    Splitter1: TSplitter;
    ImageCnJPEG: TImage;
    ImageNative: TImage;
    LabelCnJPEG: TLabel;
    LabelNative: TLabel;
    btnLoad: TButton;
    btnSave: TButton;
    btnConvert: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    TrackBarQuality: TTrackBar;
    LabelQuality: TLabel;
    chkGrayscale: TCheckBox;
    chkProgressive: TCheckBox;
    cmbScale: TComboBox;
    cmbPerformance: TComboBox;
    chkSmoothing: TCheckBox;
    btnCompare: TButton;
    MemoInfo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure TrackBarQualityChange(Sender: TObject);
    procedure chkGrayscaleClick(Sender: TObject);
    procedure chkProgressiveClick(Sender: TObject);
    procedure cmbScaleChange(Sender: TObject);
    procedure cmbPerformanceChange(Sender: TObject);
    procedure chkSmoothingClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
  private
    FCnJPEG: TCnJPEGImage;
    procedure UpdateLabels;
    procedure ReloadCnJPEG;
  public
    { Public declarations }
  end;

var
  FormJPEGDemo: TFormJPEGDemo;

implementation

{$R *.DFM}

uses
  JPEG, TypInfo;

procedure TFormJPEGDemo.FormCreate(Sender: TObject);
begin
  Caption := 'CnJPEG Demo - Pure Pascal JPEG Codec';
  FCnJPEG := TCnJPEGImage.Create;
  TrackBarQuality.Position := 100;
  cmbScale.ItemIndex := 0;
  cmbPerformance.ItemIndex := 0;
  chkSmoothing.Checked := True;
  UpdateLabels;
end;

procedure TFormJPEGDemo.FormDestroy(Sender: TObject);
begin
  FCnJPEG.Free;
end;

procedure TFormJPEGDemo.UpdateLabels;
begin
  LabelQuality.Caption := 'Quality: ' + IntToStr(TrackBarQuality.Position);
  LabelCnJPEG.Caption := 'CnJPEG (' + IntToStr(FCnJPEG.Width) + 'x' +
    IntToStr(FCnJPEG.Height) + ')';
end;

procedure TFormJPEGDemo.ReloadCnJPEG;
begin
  if FCnJPEG.Empty then Exit;
  FCnJPEG.Scale := TCnJPEGScale(cmbScale.ItemIndex);
  FCnJPEG.Performance := TCnJPEGPerformance(cmbPerformance.ItemIndex);
  FCnJPEG.Smoothing := chkSmoothing.Checked;
  FCnJPEG.Grayscale := chkGrayscale.Checked;
  FCnJPEG.CompressionQuality := TrackBarQuality.Position;
  FCnJPEG.ProgressiveEncoding := chkProgressive.Checked;
  ImageCnJPEG.Picture.Assign(FCnJPEG);
  UpdateLabels;
end;

procedure TFormJPEGDemo.btnLoadClick(Sender: TObject);
var
  NativeJPEG: TJPEGImage;
begin
  OpenDialog1.Filter := 'JPEG Files (*.jpg;*.jpeg)|*.jpg;*.jpeg|All Files|*.*';
  if not OpenDialog1.Execute then Exit;

  FCnJPEG.LoadFromFile(OpenDialog1.FileName);
  ImageCnJPEG.Picture.Assign(FCnJPEG);

  // Also load with native TJPEGImage for comparison
  NativeJPEG := TJPEGImage.Create;
  try
    NativeJPEG.LoadFromFile(OpenDialog1.FileName);
    ImageNative.Picture.Assign(NativeJPEG);
  finally
    NativeJPEG.Free;
  end;

  chkGrayscale.Checked := FCnJPEG.Grayscale;
  chkProgressive.Checked := FCnJPEG.ProgressiveEncoding;
  UpdateLabels;

  MemoInfo.Lines.Clear;
  MemoInfo.Lines.Add('File: ' + OpenDialog1.FileName);
  MemoInfo.Lines.Add('Size: ' + IntToStr(FCnJPEG.Width) + 'x' + IntToStr(FCnJPEG.Height));
  MemoInfo.Lines.Add('Grayscale: ' + BoolToStr(FCnJPEG.Grayscale, True));
  MemoInfo.Lines.Add('Progressive: ' + BoolToStr(FCnJPEG.ProgressiveEncoding, True));
  MemoInfo.Lines.Add('ColorSpace: ' + GetEnumName(TypeInfo(TCnJPEGColorSpace),
    Ord(FCnJPEG.NewJPEG.ColorSpace)));
end;

procedure TFormJPEGDemo.btnSaveClick(Sender: TObject);
begin
  if FCnJPEG.Empty then Exit;
  SaveDialog1.Filter := 'JPEG Files (*.jpg)|*.jpg|All Files|*.*';
  SaveDialog1.DefaultExt := 'jpg';
  if not SaveDialog1.Execute then Exit;

  FCnJPEG.CompressionQuality := TrackBarQuality.Position;
  FCnJPEG.Grayscale := chkGrayscale.Checked;
  FCnJPEG.ProgressiveEncoding := chkProgressive.Checked;
  FCnJPEG.SaveToFile(SaveDialog1.FileName);

  MemoInfo.Lines.Add('Saved to: ' + SaveDialog1.FileName);
end;

procedure TFormJPEGDemo.btnConvertClick(Sender: TObject);
var
  Bmp: TBitmap;
begin
  // Create a gradient bitmap and encode it
  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := 256;
    Bmp.Height := 256;
    // Draw gradient
    // (simplified - just fill with a pattern)
    Bmp.Canvas.Brush.Color := clBlue;
    Bmp.Canvas.FillRect(Rect(0, 0, 256, 256));
    Bmp.Canvas.Brush.Color := clRed;
    Bmp.Canvas.FillRect(Rect(0, 0, 128, 128));
    Bmp.Canvas.Brush.Color := clGreen;
    Bmp.Canvas.FillRect(Rect(128, 128, 256, 256));
    Bmp.Canvas.Brush.Color := clYellow;
    Bmp.Canvas.FillRect(Rect(128, 0, 256, 128));
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.FillRect(Rect(0, 128, 128, 256));

    FCnJPEG.Assign(Bmp);
    FCnJPEG.CompressionQuality := TrackBarQuality.Position;
    FCnJPEG.Grayscale := chkGrayscale.Checked;
    FCnJPEG.ProgressiveEncoding := chkProgressive.Checked;
    ImageCnJPEG.Picture.Assign(FCnJPEG);
    UpdateLabels;

    MemoInfo.Lines.Add('Converted from TBitmap');
  finally
    Bmp.Free;
  end;
end;

procedure TFormJPEGDemo.TrackBarQualityChange(Sender: TObject);
begin
  UpdateLabels;
end;

procedure TFormJPEGDemo.chkGrayscaleClick(Sender: TObject);
begin
  ReloadCnJPEG;
end;

procedure TFormJPEGDemo.chkProgressiveClick(Sender: TObject);
begin
  ReloadCnJPEG;
end;

procedure TFormJPEGDemo.cmbScaleChange(Sender: TObject);
begin
  ReloadCnJPEG;
end;

procedure TFormJPEGDemo.cmbPerformanceChange(Sender: TObject);
begin
  ReloadCnJPEG;
end;

procedure TFormJPEGDemo.chkSmoothingClick(Sender: TObject);
begin
  ReloadCnJPEG;
end;

procedure TFormJPEGDemo.btnCompareClick(Sender: TObject);
var
  BmpCn, BmpNative: TBitmap;
  X, Y, Diff, MaxDiff, TotalDiff, Count: Integer;
  PCn, PNative: PByteArray;
begin
  if FCnJPEG.Empty then Exit;
  BmpCn := TBitmap.Create;
  BmpNative := TBitmap.Create;
  try
    BmpCn.Assign(FCnJPEG);
    // Re-load native with same file
    // For now just compare sizes
    MemoInfo.Lines.Add('--- Comparison ---');
    MemoInfo.Lines.Add('CnJPEG: ' + IntToStr(BmpCn.Width) + 'x' + IntToStr(BmpCn.Height));

    if (ImageNative.Picture.Bitmap <> nil) and
       (ImageNative.Picture.Bitmap.Width > 0) then
    begin
      BmpNative.Assign(ImageNative.Picture.Bitmap);
      MemoInfo.Lines.Add('Native: ' + IntToStr(BmpNative.Width) + 'x' +
        IntToStr(BmpNative.Height));

      if (BmpCn.Width = BmpNative.Width) and
         (BmpCn.Height = BmpNative.Height) and
         (BmpCn.PixelFormat = pf24bit) and
         (BmpNative.PixelFormat = pf24bit) then
      begin
        MaxDiff := 0;
        TotalDiff := 0;
        Count := 0;
        for Y := 0 to BmpCn.Height - 1 do
        begin
          PCn := BmpCn.ScanLine[Y];
          PNative := BmpNative.ScanLine[Y];
          for X := 0 to BmpCn.Width * 3 - 1 do
          begin
            Diff := Abs(Integer(PCn[X]) - Integer(PNative[X]));
            if Diff > MaxDiff then MaxDiff := Diff;
            Inc(TotalDiff, Diff);
            Inc(Count);
          end;
        end;
        MemoInfo.Lines.Add('Max pixel diff: ' + IntToStr(MaxDiff));
        MemoInfo.Lines.Add('Avg pixel diff: ' + FormatFloat('0.00',
          TotalDiff / Count));
      end;
    end;
  finally
    BmpCn.Free;
    BmpNative.Free;
  end;
end;

end.
