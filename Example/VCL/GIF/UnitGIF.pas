unit UnitGIF;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, CnGIF;

type
  TfrmGIFDemo = class(TForm)
    pbDisplay: TPaintBox;
    btnLoad: TButton;
    dlgOpen: TOpenDialog;
    btnPrevFrame: TButton;
    btnNextFrame: TButton;
    btnPlayPause: TButton;
    btnSaveFrame: TButton;
    btnSavePaintBox: TButton;
    lblInfo: TLabel;
    lblFrame: TLabel;
    tmrAnimation: TTimer;
    lblFileName: TLabel;
    dlgSave: TSaveDialog;
    procedure btnLoadClick(Sender: TObject);
    procedure pbDisplayPaint(Sender: TObject);
    procedure btnPrevFrameClick(Sender: TObject);
    procedure btnNextFrameClick(Sender: TObject);
    procedure btnPlayPauseClick(Sender: TObject);
    procedure tmrAnimationTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbDisplayResize(Sender: TObject);
    procedure btnSaveFrameClick(Sender: TObject);
    procedure btnSavePaintBoxClick(Sender: TObject);
  private
    FGIF: TCnGIFImage;
    FPlaying: Boolean;
    FFileName: string;
    procedure UpdateInfo;
  public
  end;

var
  frmGIFDemo: TfrmGIFDemo;

implementation

{$R *.DFM}

{ TfrmGIFDemo }

procedure TfrmGIFDemo.FormCreate(Sender: TObject);
begin
  FGIF := TCnGIFImage.Create;
  FPlaying := False;
  dlgOpen.Filter := 'GIF 文件(*.gif)|*.gif|所有文件(*.*)|*.*';
  dlgSave.Filter := 'GIF 文件(*.gif)|*.gif';
  dlgSave.DefaultExt := 'gif';
end;

procedure TfrmGIFDemo.FormDestroy(Sender: TObject);
begin
  FGIF.Free;
end;

procedure TfrmGIFDemo.btnLoadClick(Sender: TObject);
begin
  if not dlgOpen.Execute then
    Exit;

  FFileName := dlgOpen.FileName;
  try
    FGIF.LoadFromFile(FFileName);
    FGIF.CurrentFrame := 0;
    FPlaying := False;
    btnPlayPause.Caption := '播放';
    tmrAnimation.Enabled := False;
    UpdateInfo;
    pbDisplay.Invalidate;
    btnPrevFrame.Enabled := FGIF.FrameCount > 1;
    btnNextFrame.Enabled := FGIF.FrameCount > 1;
    btnPlayPause.Enabled := FGIF.FrameCount > 1;
    btnSaveFrame.Enabled := not FGIF.Empty;
  except
    on E: Exception do
      ShowMessage('加载失败: ' + E.Message);
  end;
end;

procedure TfrmGIFDemo.pbDisplayPaint(Sender: TObject);
const
  S = '请点击"加载 GIF"按钮';
var
  R: TRect;
begin
  with pbDisplay.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(ClientRect);
  end;

  if FGIF.Empty then
  begin
    with pbDisplay.Canvas do
      TextOut((pbDisplay.Width - TextWidth(S)) div 2,
        (pbDisplay.Height - TextHeight(S)) div 2, S);
    Exit;
  end;

  R := Bounds(0, 0, FGIF.Width, FGIF.Height);
  FGIF.Draw(pbDisplay.Canvas, R);
end;

procedure TfrmGIFDemo.btnPrevFrameClick(Sender: TObject);
begin
  if FGIF.FrameCount = 0 then Exit;
  if FPlaying then
  begin
    FPlaying := False;
    btnPlayPause.Caption := '播放';
    tmrAnimation.Enabled := False;
  end;
  if FGIF.CurrentFrame > 0 then
    FGIF.CurrentFrame := FGIF.CurrentFrame - 1
  else
    FGIF.CurrentFrame := FGIF.FrameCount - 1;
  pbDisplay.Invalidate;
  UpdateInfo;
end;

procedure TfrmGIFDemo.btnNextFrameClick(Sender: TObject);
begin
  if FGIF.FrameCount = 0 then Exit;
  if FPlaying then
  begin
    FPlaying := False;
    btnPlayPause.Caption := '播放';
    tmrAnimation.Enabled := False;
  end;
  if FGIF.CurrentFrame < FGIF.FrameCount - 1 then
    FGIF.CurrentFrame := FGIF.CurrentFrame + 1
  else
    FGIF.CurrentFrame := 0;
  pbDisplay.Invalidate;
  UpdateInfo;
end;

procedure TfrmGIFDemo.btnPlayPauseClick(Sender: TObject);
begin
  if FGIF.FrameCount = 0 then Exit;

  FPlaying := not FPlaying;
  if FPlaying then
  begin
    btnPlayPause.Caption := '暂停';
    tmrAnimation.Interval := FGIF.Frames[FGIF.CurrentFrame].Delay * 10;
    if tmrAnimation.Interval < 50 then
      tmrAnimation.Interval := 100;
    tmrAnimation.Enabled := True;
  end
  else
  begin
    btnPlayPause.Caption := '播放';
    tmrAnimation.Enabled := False;
  end;
end;

procedure TfrmGIFDemo.tmrAnimationTimer(Sender: TObject);
begin
  if FGIF.FrameCount = 0 then Exit;

  if FGIF.CurrentFrame < FGIF.FrameCount - 1 then
    FGIF.CurrentFrame := FGIF.CurrentFrame + 1
  else
    FGIF.CurrentFrame := 0;

  pbDisplay.Invalidate;
  UpdateInfo;

  tmrAnimation.Interval := FGIF.Frames[FGIF.CurrentFrame].Delay * 10;
  if tmrAnimation.Interval < 50 then
    tmrAnimation.Interval := 100;
end;

procedure TfrmGIFDemo.UpdateInfo;
begin
  if FGIF.Empty then
  begin
    lblFileName.Caption := '';
    lblInfo.Caption := '';
    lblFrame.Caption := '';
    Exit;
  end;

  lblFileName.Caption := ExtractFileName(FFileName);
  lblInfo.Caption := Format('%d x %d  帧数: %d  循环: %d',
    [FGIF.Width, FGIF.Height, FGIF.FrameCount, FGIF.AnimationLoopCount]);
  lblFrame.Caption := Format('帧: %d / %d  延迟: %dms',
    [FGIF.CurrentFrame + 1, FGIF.FrameCount,
     FGIF.Frames[FGIF.CurrentFrame].Delay * 10]);
end;

procedure TfrmGIFDemo.pbDisplayResize(Sender: TObject);
begin
  pbDisplay.Invalidate;
end;

procedure TfrmGIFDemo.btnSaveFrameClick(Sender: TObject);
begin
  if FGIF.Empty then
  begin
    ShowMessage('请先加载 GIF');
    Exit;
  end;

  if not dlgSave.Execute then
    Exit;

  try
    FGIF.SaveCurrentFrameToGIFFile(dlgSave.FileName);
    ShowMessage('已保存: ' + dlgSave.FileName);
  except
    on E: Exception do
      ShowMessage('保存失败: ' + E.Message);
  end;
end;

procedure TfrmGIFDemo.btnSavePaintBoxClick(Sender: TObject);
var
  Bmp: TBitmap;
  DC: HDC;
begin
  if not dlgSave.Execute then
    Exit;

  // Ensure the PaintBox content is up to date
  pbDisplay.Repaint;

  Bmp := TBitmap.Create;
  try
    Bmp.PixelFormat := pf24bit;
    Bmp.Width := pbDisplay.Width;
    Bmp.Height := pbDisplay.Height;
    if (Bmp.Width <= 0) or (Bmp.Height <= 0) then
    begin
      ShowMessage('PaintBox size is invalid.');
      Exit;
    end;

    // Capture the PaintBox pixels from the parent window's DC
    DC := GetDC(pbDisplay.Parent.Handle);
    try
      if not BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height,
        DC, pbDisplay.Left, pbDisplay.Top, SRCCOPY) then
      begin
        ShowMessage('BitBlt Failed to Capture PaintBox Pixels.');
        Exit;
      end;
    finally
      if ReleaseDC(pbDisplay.Parent.Handle, DC) = 0 then
        ShowMessage('ReleaseDC Failed.');
    end;

    try
      FGIF.SaveBitmapToGIFFile(dlgSave.FileName, Bmp);
      ShowMessage('Saved: ' + dlgSave.FileName);
    except
      on E: Exception do
        ShowMessage('Save failed: ' + E.Message);
    end;
  finally
    Bmp.Free;
  end;
end;

end.
