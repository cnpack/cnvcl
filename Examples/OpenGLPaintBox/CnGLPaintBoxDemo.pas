unit CnGLPaintBoxDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  CnOpenGLPaintBox, StdCtrls, ExtCtrls, OpenGL, ComCtrls, Math, Dialogs;

type
  TMainTests = (mtDot, mtLine, mtPolyline, mtCurve, mtRectEllipse, mtArc, mtText, mtImage);
  TBenchTests = (btLines, btRectangles, btEllipses, btPoints, btTexts);

  TfrmMain = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    glcMain: TCnOpenGLPaintBox;
    Panel1: TPanel;
    chkAntialiasing: TCheckBox;
    btnDot: TButton;
    cboLineStipple: TComboBox;
    TabSheet2: TTabSheet;
    btnLine: TButton;
    btnPolyline: TButton;
    btnCurve: TButton;
    btnRectEllipse: TButton;
    btnArc: TButton;
    btnText: TButton;
    btnImage: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    chkTranslate: TCheckBox;
    chkRotate: TCheckBox;
    chkScale: TCheckBox;
    Label4: TLabel;
    trackPenWidth: TTrackBar;
    Label5: TLabel;
    trackFillAlpha: TTrackBar;
    glcBench: TCnOpenGLPaintBox;
    imgBench: TImage;
    lblGLPaintBoxBench: TLabel;
    lblGDIBench: TLabel;
    GroupBox1: TGroupBox;
    Label6: TLabel;
    btnBenchLines: TButton;
    btnBenchRects: TButton;
    btnBenchEllipses: TButton;
    btnBenchPoints: TButton;
    btnBenchTexts: TButton;
    GroupBox2: TGroupBox;
    btnBuildList: TButton;
    btnExecuteList: TButton;
    Label7: TLabel;
    trackPenWidth2: TTrackBar;
    rdoPenWidth1: TRadioButton;
    rdoPenWidth2: TRadioButton;
    Label8: TLabel;
    pnlPenColor2: TPanel;
    pnlPen: TPanel;
    pnlBrush: TPanel;
    lbl1: TLabel;
    dlgColor1: TColorDialog;
    procedure FormCreate(Sender: TObject);
    procedure MainTestClick(Sender: TObject);
    procedure BenchTestClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure glcMainResize(Sender: TObject);
    procedure btnBuildListClick(Sender: TObject);
    procedure btnExecuteListClick(Sender: TObject);
    procedure pnlPenColor2Click(Sender: TObject);
  private
    { Private declarations }
    ImageTex: TBitmap;
    OldMainTest: TMainTests;

    BenchTest: TBenchTests;
    BenchList: GLuint;

    procedure GLPaintBoxBench;
    procedure GDIBench;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

const
  cBenchLines = 20000;
  cBenchRects = 20000;
  cBenchEllipses = 20000;
  cBenchPoints = 200000;
  cBenchTexts = 10000;

{$R *.dfm}

function StartPrecisionTimer: Int64;
begin
  QueryPerformanceCounter(Result);
end;

function StopPrecisionTimer(const precisionTimer: Int64): Double;
var
  cur, freq: Int64;
begin
  QueryPerformanceCounter(cur);
  QueryPerformanceFrequency(freq);
  Result := (cur - precisionTimer) / freq;
end;

procedure TfrmMain.BenchTestClick(Sender: TObject);
var
  t: Int64;
begin
  BenchTest := TBenchTests(TComponent(Sender).Tag);
  if rdoPenWidth1.Checked then
  begin
    glcBench.PenWidth := 1;
    imgBench.Canvas.Pen.Width := 1;
  end
  else
  begin
    glcBench.PenWidth := 2;
    imgBench.Canvas.Pen.Width := 2;
  end;

  Application.ProcessMessages;
  Randomize;
  t := StartPrecisionTimer;
  GLPaintBoxBench;
  lblGLPaintBoxBench.Caption := Format('GLPaintBox: %.3f msec', [StopPrecisionTimer(t) * 1000]);

  Application.ProcessMessages;
  Randomize;
  t := StartPrecisionTimer;
  GDIBench;
  lblGDIBench.Caption := Format('GDI: %.3f msec', [StopPrecisionTimer(t) * 1000]);
end;

procedure TfrmMain.btnBuildListClick(Sender: TObject);
begin
  glcBench.CreateList(BenchList).ListBegin(BenchList).Line(0, 0, 50, 50).
    FrameRect(30, 20, 100, 150).Ellipse(100, 35, 33, 100).Arc(250, 250, 100, 50, 90, 180).ListEnd;
  btnExecuteList.Enabled := True;
  btnBuildList.Enabled := False;
end;

procedure TfrmMain.btnExecuteListClick(Sender: TObject);
var
  penColor: ARGB;
begin
  penColor := TColorToARGB(pnlPenColor2.Color);
  glcBench.RenderingBegin.ListExecute(BenchList, 0, penColor, trackPenWidth2.Position).RenderingEnd;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  glcBench.Initialize(False);
  glcMain.Initialize(True);
  // 读取例子图片，采用预先构建纹理的方式，速度更快
  ImageTex := TBitmap.Create;
  ImageTex.LoadFromFile('test.bmp');
  OldMainTest := mtLine;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ImageTex.Free;
end;

procedure TfrmMain.GDIBench;
var
  i: Integer;
  r: TRect;
begin
  with imgBench.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := clWhite;
    Rectangle(0, 0, 380, 380);
    Brush.Style := bsClear;
    case BenchTest of
      btLines:
        for i := 1 to cBenchLines do
        begin
          Pen.Color := Random(256 * 256 * 256);
          MoveTo(Random(380), Random(380));
          LineTo(Random(380), Random(380));
        end;
      btEllipses:
        for i := 1 to cBenchEllipses do
        begin
          Pen.Color := Random(256 * 256 * 256);
          Ellipse(Random(380), Random(380), Random(380), Random(380));
        end;
      btRectangles:
        for i := 1 to cBenchRects do
        begin
          Brush.Color := Random(256 * 256 * 256);
          r := Rect(Random(380), Random(380), Random(380), Random(380));
          FillRect(r);
        end;
      btPoints:
        for i := 1 to cBenchPoints do
          Pixels[Random(380), Random(380)] := Random(256 * 256 * 256);
      btTexts:
        for i := 1 to cBenchTexts do
        begin
          Font.Color := Random(256 * 256 * 256);
          TextOut(Random(380), Random(380), 'Hello');
        end;
    end;
  end;
end;

procedure TfrmMain.GLPaintBoxBench;
var
  i: Integer;
begin
  glcBench.RenderingBegin;
  case BenchTest of
    btLines:
      begin
        glcBench.BeginLines;
        for i := 1 to cBenchLines do
          glcBench.SetPenColorWin(Random(256 * 256 * 256), 255, False).
            Lines(Random(380), Random(380), Random(380), Random(380));
        glcBench.EndLines;
      end;
    btEllipses:
      for i := 1 to cBenchEllipses do
        glcBench.SetPenColorWin(Random(256 * 256 * 256), 255, False).
          EllipseRect(Random(380), Random(380), Random(380), Random(380));
    btRectangles:
      for i := 1 to cBenchRects do
        glcBench.SetBrushColorWin(Random(256 * 256 * 256), 255, False).
          FillRect(Random(380), Random(380), Random(380), Random(380));
    btPoints:
      begin
        glcBench.BeginPixels;
        for i := 1 to cBenchPoints do
          glcBench.SetPenColorWin(Random(256 * 256 * 256), 255, False).Pixels(Random(380), Random(380));
        glcBench.EndPixels;
      end;
    btTexts:
      for i := 1 to cBenchTexts do
      begin
        glcBench.DefaultFont.WinColor := Random(256 * 256 * 256);
        glcBench.TextOutASCII('Hello', Random(380), Random(380));
      end;
  end;
  glcBench.RenderingEnd;
end;

procedure TfrmMain.glcMainResize(Sender: TObject);
begin
  if PageControl1.ActivePageIndex = 1 then
    MainTestClick(nil);
end;

procedure TfrmMain.MainTestClick(Sender: TObject);
const
  ColorArray: array[0..3] of TColor = (clRed, clBlue, clTeal, clAqua);
  ImageZoom: array[0..9] of Single = (0.1, 0.3, 0.5, 0.75, 0.8, 1, 1.5, 2, 3, 4);

var
  i: Integer;
  fillColor: ARGB;
  vertices: array[0..9] of TGLPointF;

  function RandomRange(const AFrom, ATo: Integer): Integer;
  begin
    if AFrom > ATo then
      Result := Random(AFrom - ATo) + ATo
    else
      Result := Random(ATo - AFrom) + AFrom;
  end;

  procedure GenRandomVertices;
  var
    i: Integer;
  begin
    Randomize;
    for i := 0 to 9 do
    begin
      vertices[i].x := RandomRange(-20, glcMain.Width - 20);
      vertices[i].y := RandomRange(-10, glcMain.Height - 10);
    end;
  end;
begin
  if Sender is TButton then
    OldMainTest := TMainTests(TComponent(Sender).Tag);
  glcMain.Antialiasing := chkAntialiasing.Checked;

  fillColor := TColorToARGB(pnlBrush.Color, trackFillAlpha.Position);
  glcMain.RenderingBegin. // 开始绘制
    SetBrushColor(fillColor).SetPenWidth(trackPenWidth.Position).
    SetPenColorWin(pnlPen.Color).LineStipple(TLineStippleStyle(cboLineStipple.ItemIndex));

  // 当有大量连续坐标变换操作时，先BeginUpdateTransformation，可以最后一次更新，提高速度
  glcMain.BeginUpdateTransformation.ResetTransformation; // 开始更新坐标变换，并重置
  if chkTranslate.Checked then
    glcMain.SetTranslateX(50).SetTranslateY(50); // 各移动50像素
  if chkScale.Checked then
    glcMain.SetEqualScale(2.0); // x与y轴都放大两倍
  if chkRotate.Checked then
    glcMain.SetRotation(20); // 度, degree
  glcMain.EndUpdateTransformation;

  case OldMainTest of
    mtDot:
      begin
        Randomize;
        // 绘制比别的点大一点的原点
        glcMain.SetPenWidth(trackPenWidth.Position + 2).PlotPixel(0, 0).SetPenWidth(trackPenWidth.Position);
        glcMain.BeginPixels; //开始绘制点，当连续绘制大量点时，使用BeginPixels和EndPixels速度更快
        for i := 0 to 100 do
          glcMain.Pixels(RandomRange(-20, glcMain.Width - 20), RandomRange(-10, glcMain.Height - 10));
        glcMain.EndPixels;
      end;
    mtLine:
      begin
        Randomize;
        glcMain.SetPenWidth(trackPenWidth.Position + 2).Line(0, 0, 100, 0).
          Line(0, 0, 0, 100).SetPenWidth(trackPenWidth.Position); // 绘制坐标轴,比其它直线粗一点
        glcMain.BeginLines; //用途同BeginPixels
        for i := 0 to 30 do
          glcMain.Lines(RandomRange(-20, glcMain.Width - 20), RandomRange(-10, glcMain.Height - 10),
            RandomRange(-20, glcMain.Width - 20), RandomRange(-10, glcMain.Height - 10));
        glcMain.EndLines;
      end;
    mtPolyline:
      begin
        Randomize;
        glcMain.SetPenWidth(trackPenWidth.Position + 2).Line(0, 0, 100, 0).
          Line(0, 0, 0, 100).SetPenWidth(trackPenWidth.Position); // 绘制坐标轴,比其它直线粗一点
        GenRandomVertices; // 生成随机点
        glcMain.
          Polyline(TGLPointsF(@vertices[0]), 5). // 折线
          Polygon(TGLPointsF(@vertices[5]), 5); // 多边形

        vertices[0].X := RandomRange(-20, glcMain.Width div 2 - 20);
        vertices[0].Y := RandomRange(-20, glcMain.Height div 2 - 20);
        vertices[1].X := RandomRange(-20, glcMain.Width div 2 - 20);
        vertices[1].Y := RandomRange(glcMain.Height div 2 - 10, glcMain.Height - 10);
        vertices[2].X := RandomRange(glcMain.Width div 2 - 10, glcMain.Width - 20);
        vertices[2].Y := RandomRange(glcMain.Height div 2 - 10, glcMain.Height - 10);
        vertices[3].X := RandomRange(glcMain.Width div 2 - 10, glcMain.Width - 20);
        vertices[3].Y := RandomRange(-20, glcMain.Height div 2 - 20);
        glcMain.FillPolygon(TGLPointsF(@vertices[0]), 4, True {显示边框}); // 填充多边形
      end;
    mtCurve:
      begin
        Randomize;
        glcMain.SetPenWidth(trackPenWidth.Position + 2).Line(0, 0, 100, 0).
          Line(0, 0, 0, 100).SetPenWidth(trackPenWidth.Position); // 绘制坐标轴,比其它直线粗一点
        GenRandomVertices; // 生成随机点
        glcMain.
          Curve(TGLPointsF(@vertices[0]), 5).
          FillClosedCurve(TGLPointsF(@vertices[5]), 5, True {显示边框});
      end;
    mtRectEllipse:
      begin
        Randomize;
        glcMain.SetPenWidth(trackPenWidth.Position + 2).Line(0, 0, 100, 0).
          Line(0, 0, 0, 100).SetPenWidth(trackPenWidth.Position); // 绘制坐标轴,比其它直线粗一点
        for i := 0 to 3 do
        begin
          glcMain.FrameRect(RandomRange(-20, glcMain.Width - 20), RandomRange(-10, glcMain.Height - 10),
            RandomRange(-20, glcMain.Width - 20), RandomRange(-10, glcMain.Height - 10));
          glcMain.FillRect(RandomRange(-20, glcMain.Width - 20), RandomRange(-10, glcMain.Height - 10),
            RandomRange(-20, glcMain.Width - 20), RandomRange(-10, glcMain.Height - 10), True {显示边框});
          glcMain.EllipseRect(RandomRange(-20, glcMain.Width - 20), RandomRange(-10, glcMain.Height - 10),
            RandomRange(-20, glcMain.Width - 20), RandomRange(-10, glcMain.Height - 10));
          glcMain.FillEllipseRect(RandomRange(-20, glcMain.Width - 20), RandomRange(-10, glcMain.Height - 10),
            RandomRange(-20, glcMain.Width - 20), RandomRange(-10, glcMain.Height - 10), True {显示边框});
        end;
      end;
    mtArc:
      begin
        Randomize;
        glcMain.SetPenWidth(trackPenWidth.Position + 2).Line(0, 0, 100, 0).
          Line(0, 0, 0, 100).SetPenWidth(trackPenWidth.Position); // 绘制坐标轴,比其它直线粗一点
        for i := 0 to 7 do
        begin
          glcMain.Arc(i * 150 + 80, 200, (i + 3) * 10, (i * 2 + 2) * 5, RandomRange(0, 180), RandomRange(50, 180));
          glcMain.FillPie(i * 150 + 80, 500, (i + 3) * 10, (i * 2 + 2) * 5, RandomRange(0, 180), RandomRange(50, 180), True);
        end;
      end;
    mtText:
      begin
        Randomize;
        glcMain.SetPenWidth(trackPenWidth.Position + 2).Line(0, 0, 100, 0).
          Line(0, 0, 0, 100).SetPenWidth(trackPenWidth.Position); // 绘制坐标轴,比其它直线粗一点
        glcMain.RecreateDefaultFont;
        for i := 0 to 3 do
        begin
          glcMain.TextOut('这是可以利用OpenGL硬件加速的画布控件。',
            RandomRange(-20, glcMain.Width - 100), RandomRange(-10, glcMain.Height - 10));
          glcMain.TextOutASCII('abcdefghijklmnopqrstuvwxyz 0123456789 !@#$%^&*()_+', // 只输出ASCII字符使用这个方法更快
            RandomRange(-20, glcMain.Width - 100), RandomRange(-10, glcMain.Height - 10));
          glcMain.DefaultFont.Size := RandomRange(6, 20);
          if i <= 3 then
          begin
            glcMain.DefaultFont.Styles := glcMain.DefaultFont.Styles + [TFontStyle(i)];
            glcMain.DefaultFont.Color := TColorToARGB(ColorArray[i]);
          end;
          glcMain.DefaultFont.Update; // 更改字体后必须使用Update更新
        end;
      end;
    mtImage:
      begin
        Randomize;
        glcMain.SetPenWidth(trackPenWidth.Position + 2).Line(0, 0, 100, 0).
          Line(0, 0, 0, 100).SetPenWidth(trackPenWidth.Position); // 绘制坐标轴,比其它直线粗一点
        for i := 0 to 9 do
          glcMain.DrawBitmap(ImageTex, i * 70 + 40, i * 30 + 100,
            ImageZoom[i], ImageZoom[i]);
      end;
  end;
  glcMain.RenderingEnd; // 结束绘制
end;

procedure TfrmMain.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePageIndex = 1 then
    MainTestClick(nil);
end;

procedure TfrmMain.pnlPenColor2Click(Sender: TObject);
begin
  if dlgColor1.Execute then
    (Sender as TPanel).Color := dlgColor1.Color;
end;

end.

