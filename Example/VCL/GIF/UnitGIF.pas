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
    lblInfo: TLabel;
    lblFrame: TLabel;
    tmrAnimation: TTimer;
    lblFileName: TLabel;
    procedure btnLoadClick(Sender: TObject);
    procedure pbDisplayPaint(Sender: TObject);
    procedure btnPrevFrameClick(Sender: TObject);
    procedure btnNextFrameClick(Sender: TObject);
    procedure btnPlayPauseClick(Sender: TObject);
    procedure tmrAnimationTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbDisplayResize(Sender: TObject);
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
        (pbDisplay.Height - TextHeight(S)) div 2, 'S);
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

end.
