{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2011 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, CnVCLBase, CnImage, StdCtrls, Buttons, ExtDlgs, CnGraphics,
  jpeg, Spin;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    PageControl: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Label3: TLabel;
    sbRed: TScrollBar;
    Label4: TLabel;
    sbGreen: TScrollBar;
    Label5: TLabel;
    sbBlue: TScrollBar;
    Label6: TLabel;
    sbBrightness: TScrollBar;
    Label7: TLabel;
    sbContrast: TScrollBar;
    Label8: TLabel;
    sbSaturation: TScrollBar;
    cbGrayscale: TCheckBox;
    cbInvert: TCheckBox;
    cbColorize: TCheckBox;
    spColor: TShape;
    btnBackReset: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    cbBackSmooth: TCheckBox;
    ColorDialog: TColorDialog;
    spBackTranColor: TShape;
    Label1: TLabel;
    cbBackTran: TCheckBox;
    cbBackTranColor: TCheckBox;
    cbbBackMode: TComboBox;
    btnBack: TButton;
    TabSheet6: TTabSheet;
    Label2: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    cbbGrad: TComboBox;
    spStartColor: TShape;
    spEndColor: TShape;
    Label11: TLabel;
    sbGradAlpha: TScrollBar;
    lbGradColor: TListBox;
    Label12: TLabel;
    spMiddleColor: TShape;
    Label13: TLabel;
    seGradPos: TSpinEdit;
    btnGradAdd: TBitBtn;
    btnGradDel: TBitBtn;
    btnGradClear: TBitBtn;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    sbFilter1: TScrollBar;
    rgFilterOne: TRadioGroup;
    sbFilter2: TScrollBar;
    Label14: TLabel;
    Label15: TLabel;
    TabSheet9: TTabSheet;
    seCore00: TSpinEdit;
    seCore01: TSpinEdit;
    seCore02: TSpinEdit;
    seCore10: TSpinEdit;
    seCore11: TSpinEdit;
    seCore12: TSpinEdit;
    seCore20: TSpinEdit;
    seCore21: TSpinEdit;
    seCore22: TSpinEdit;
    rbDefine: TRadioButton;
    rbWave: TRadioButton;
    Label16: TLabel;
    Label17: TLabel;
    sbWave1: TScrollBar;
    sbWave2: TScrollBar;
    Label18: TLabel;
    sbWave3: TScrollBar;
    cbWave: TCheckBox;
    btnDefine: TButton;
    Label19: TLabel;
    edtFont: TEdit;
    btnFont: TButton;
    Label20: TLabel;
    seFontSize: TSpinEdit;
    spFontColor: TShape;
    cbFontShadow: TCheckBox;
    Label21: TLabel;
    seFontX: TSpinEdit;
    Label22: TLabel;
    seFontY: TSpinEdit;
    cbFontGrad: TCheckBox;
    cbFontText: TCheckBox;
    btnFontText: TButton;
    cbFontOutline: TCheckBox;
    cbFontNoise: TCheckBox;
    sbFontNoise: TScrollBar;
    Label24: TLabel;
    sbFontAlpha: TScrollBar;
    FontDialog: TFontDialog;
    Label25: TLabel;
    cbbFont: TComboBox;
    cbFontClear: TCheckBox;
    spFontBkColor: TShape;
    spFontShadow: TShape;
    rbBackBW: TRadioButton;
    rbBackColor: TRadioButton;
    spBackColor: TShape;
    Label26: TLabel;
    cbbPen: TComboBox;
    rgPen: TRadioGroup;
    Label27: TLabel;
    Label28: TLabel;
    sbPen1: TScrollBar;
    sbPen2: TScrollBar;
    Label29: TLabel;
    spPen: TShape;
    cbFlip: TCheckBox;
    cbFlop: TCheckBox;
    sbVShift: TScrollBar;
    cbVShift: TCheckBox;
    cbHShift: TCheckBox;
    sbHShift: TScrollBar;
    cbRotate: TCheckBox;
    Label30: TLabel;
    Label31: TLabel;
    sbRotateX: TScrollBar;
    sbRotateY: TScrollBar;
    Label32: TLabel;
    sbRotateAngle: TScrollBar;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Bevel1: TBevel;
    Label37: TLabel;
    cbPenSmooth: TCheckBox;
    sbBackAlpha: TScrollBar;
    cbBackAlpha: TCheckBox;
    cbRotateSmooth: TCheckBox;
    btnBlend: TButton;
    Label38: TLabel;
    Label39: TLabel;
    sbAlphaStart: TScrollBar;
    sbAlphaEnd: TScrollBar;
    Label41: TLabel;
    cbbGradBlend: TComboBox;
    rbGradBlend: TRadioButton;
    rbBlendFore: TRadioButton;
    Label40: TLabel;
    Label42: TLabel;
    sbDstX: TScrollBar;
    sbDstY: TScrollBar;
    Label43: TLabel;
    sbForeAlpha: TScrollBar;
    TabSheet10: TTabSheet;
    Label44: TLabel;
    Label45: TLabel;
    sbLightX: TScrollBar;
    sbLightY: TScrollBar;
    Label46: TLabel;
    Label47: TLabel;
    sbLightW: TScrollBar;
    sbLightH: TScrollBar;
    Label48: TLabel;
    sbLightAngle: TScrollBar;
    Label49: TLabel;
    sbLightAlpha: TScrollBar;
    Label50: TLabel;
    spLight: TShape;
    cbLightBack: TCheckBox;
    cbFontLight: TCheckBox;
    seFontShadow: TSpinEdit;
    cbFontSpray: TCheckBox;
    seFontSpray: TSpinEdit;
    cbRed: TCheckBox;
    cbGreen: TCheckBox;
    cbBlue: TCheckBox;
    Label23: TLabel;
    procedure btnBackClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbTestPaint(Sender: TObject);
    procedure UpdateImage(Sender: TObject);
    procedure cbBackSmoothClick(Sender: TObject);
    procedure spColorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; x, y: Integer);
    procedure btnBackResetClick(Sender: TObject);
    procedure btnGradAddClick(Sender: TObject);
    procedure btnGradDelClick(Sender: TObject);
    procedure btnGradClearClick(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure btnFontTextClick(Sender: TObject);
    procedure btnBlendClick(Sender: TObject);
  private
    { Private declarations }
    pbTest: TCnPaintBox;
    Back: TCnBitmap;
    Bmp: TCnBitmap;
    Fore: TCnBitmap;
    Grad: TCnGradientColor;
    Light: TCnLighting;
    UpdateCount: Integer;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure DrawGraphic;
    procedure UpdateImg(Page: Integer = -1);
    procedure UpdateGradList;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

const
  csBoxWidth = 20;

procedure TMainForm.FormCreate(Sender: TObject);
var
  TestName: string;
begin
  pbTest := TCnPaintBox.Create(Self);
  pbTest.Font.Name := Font.Name;
  pbTest.Font.Charset := Font.Charset;
  pbTest.OnPaint := pbTestPaint;
  pbTest.Align := alClient;
  pbTest.Parent := Panel1;
  Back := TCnBitmap.Create;
  Bmp := TCnBitmap.Create;
  Fore := TCnBitmap.Create;
  Grad := TCnGradientColor.Create;
  Light := TCnLighting.Create;
  UpdateCount := 0;
  PageControl.ActivePageIndex := 0;
  cbbBackMode.ItemIndex := 0;
  cbbGrad.ItemIndex := 0;
  cbbFont.ItemIndex := 1;
  cbbPen.ItemIndex := 1;
  cbbGradBlend.ItemIndex := 0;
  UpdateImg(2);
  UpdateImg(9);
  TestName := ExtractFilePath(Application.ExeName) + 'Test.bmp';
  if FileExists(TestName) then
    Back.LoadFromFile(TestName);
  TestName := ExtractFilePath(Application.ExeName) + 'Test.jpg';
  if FileExists(TestName) then
    Fore.LoadFromFile(TestName);
  UpdateImg;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Light.Free;
  Grad.Free;
  Fore.Free;
  Back.Free;
  Bmp.Free;
end;

procedure TMainForm.pbTestPaint(Sender: TObject);
var
  w, h: Integer;

  procedure DrawBox;
  const
    BkColors: array[Boolean] of TColor = (clWhite, clBlack);
  var
    i, j: Integer;
  begin
    for i := 0 to pbTest.Width div csBoxWidth do
      for j := 0 to pbTest.Height div csBoxWidth do
        pbTest.Face.FillRect(Bounds(i * csBoxWidth, j * csBoxWidth, csBoxWidth,
          csBoxWidth), BkColors[Odd(i + j)]);
  end;

  procedure DrawBmp(ABmp: TCnBitmap);
  begin
    if cbBackAlpha.Checked then
      pbTest.Face.DrawModeEx(ABmp, TCnDrawMode(cbbBackMode.ItemIndex),
        sbBackAlpha.Position)
    else
      pbTest.Face.DrawMode(ABmp, TCnDrawMode(cbbBackMode.ItemIndex));
  end;
begin
  if rbBackBW.Checked then
    DrawBox
  else
    pbTest.Face.Fill(spBackColor.Brush.Color);
  case PageControl.ActivePageIndex of
    0: DrawBmp(Back);
    1: DrawBmp(Bmp);
    2: pbTest.Face.DrawGradientEx(Grad, Bounds(20, 20, pbTest.Width - 40,
      pbTest.Height - 40), sbGradAlpha.Position);
    3, 4: DrawBmp(Bmp);
    5: pbTest.Face.TextOut((pbTest.Width - pbTest.Face.TextWidth(edtFont.Text)) div 2,
        (pbTest.Height - pbTest.Face.TextHeight(edtFont.Text)) div 2, edtFont.Text);
    6: DrawGraphic;
    7: if cbRotate.Checked then
        pbTest.Face.Rotate(Point(sbRotateX.Position * pbTest.Width div 100,
          sbRotateY.Position * pbTest.Height div 100), Bmp, sbRotateAngle.Position)
      else
        DrawBmp(Bmp);
    8: if rbGradBlend.Checked then
        pbTest.Face.AlphaDrawGrad(Back, TCnGradStyle(cbbGradBlend.ItemIndex),
          True, sbAlphaStart.Position, sbAlphaEnd.Position)
      else
      begin
        w := pbTest.Width * sbDstX.Position div 100;
        h := pbTest.Height * sbDstY.Position div 100;
        pbTest.Face.AlphaDrawEx(Bounds((pbTest.Width - w) div 2, (pbTest.Height
          - h) div 2, w, h), Fore, Back, sbForeAlpha.Position, True);
      end;
    9:
      begin
        if cbLightBack.Checked then DrawBmp(Back);
        pbTest.Face.Lighting(pbTest.ClientRect, Light);
      end;
  end;
end;

procedure TMainForm.BeginUpdate;
begin
  Inc(UpdateCount);
end;

procedure TMainForm.EndUpdate;
begin
  Dec(UpdateCount);
  if UpdateCount = 0 then UpdateImg;
end;

procedure TMainForm.UpdateImg(Page: Integer = -1);
var
  Core: TFilterCore;
  APage: Integer;
  Channels: TColorChannels;
begin
  if UpdateCount > 0 then Exit;
  if Page < 0 then
    APage := PageControl.ActivePageIndex
  else
    APage := Page;
  case APage of
    0:
      begin
        Back.Transparent := cbBackTran.Checked;
        if cbBackTranColor.Checked then
          Back.TransparentColor := spBackTranColor.Brush.Color
        else
          Back.TransparentColor := clDefault;
      end;
    1:
      begin
        Bmp.Assign(Back);
        Channels := [];
        if cbRed.Checked then Include(Channels, ccRed);
        if cbGreen.Checked then Include(Channels, ccGreen);
        if cbBlue.Checked then Include(Channels, ccBlue);
        if (sbRed.Position <> 0) or (sbGreen.Position <> 0) or
          (sbBlue.Position <> 0) then
          Bmp.RGB(sbRed.Position, sbGreen.Position, sbBlue.Position);
        if sbBrightness.Position <> 0 then
          Bmp.Brightness(sbBrightness.Position, Channels);
        if sbContrast.Position <> 0 then
          Bmp.Contrast(sbContrast.Position, Channels);
        if sbSaturation.Position <> 0 then
          Bmp.Saturation(sbSaturation.Position, Channels);
        if cbGrayscale.Checked then
          Bmp.Grayscale(Channels);
        if cbInvert.Checked then
          Bmp.Invert(Channels);
        if cbColorize.Checked then
          Bmp.Colorize(spColor.Brush.Color);
      end;
    2:
      begin
        Grad.ColorStart := spStartColor.Brush.Color;
        Grad.ColorEnd := spEndColor.Brush.Color;
        Grad.Style := TCnGradStyle(cbbGrad.ItemIndex);
      end;
    3:
      begin
        Bmp.Assign(Back);
        case rgFilterOne.ItemIndex of
          1: Bmp.Blur;
          2: Bmp.GaussianBlur(sbFilter1.Position);
          3: Bmp.Sharpen;
          4: Bmp.SharpenMore(sbFilter1.Position);
          5: Bmp.Spray(sbFilter1.Position);
          6: Bmp.Emboss;
          7: Bmp.Mosaic(sbFilter1.Position, sbFilter2.Position);
          8: Bmp.Twist(sbFilter1.Position);
          9: Bmp.AddColorNoise(sbFilter1.Position * 255 div sbFilter1.Max);
          10: Bmp.AddMonoNoise(sbFilter1.Position * 255 div sbFilter1.Max);
          11: Bmp.Levels(0, 255, sbFilter1.Position * 255 div sbFilter1.Max,
            sbFilter2.Position * 255 div sbFilter1.Max);
          12: Bmp.Posterize(sbFilter1.Position);
          13: Bmp.HeightMap(sbFilter1.Position);
          14: Bmp.Marble(sbFilter1.Position, sbFilter2.Position * 2);
        end;
      end;
    4:
      begin
        Bmp.Assign(Back);
        if rbDefine.Checked then
        begin
          Core[0][0] := seCore00.Value;
          Core[0][1] := seCore01.Value;
          Core[0][2] := seCore02.Value;
          Core[1][0] := seCore10.Value;
          Core[1][1] := seCore11.Value;
          Core[1][2] := seCore12.Value;
          Core[2][0] := seCore20.Value;
          Core[2][1] := seCore21.Value;
          Core[2][2] := seCore22.Value;
          Bmp.ApplyFilter(Core);
        end
        else
          Bmp.Wave(sbWave1.Position / 2, sbWave2.Position / 2, sbWave3.Position / 2,
            cbWave.Checked);
      end;
    5:
      begin
        pbTest.Face.Font.Size := seFontSize.Value;
        pbTest.Face.Font.Color := spFontColor.Brush.Color;
        pbTest.Face.Font.Quality := TFontQuality(cbbFont.ItemIndex);
        pbTest.Face.FontClear := not cbFontClear.Checked;
        pbTest.Face.FontBkColor := spFontBkColor.Brush.Color;
        pbTest.Face.Font.Alpha := sbFontAlpha.Position;
        if cbFontShadow.Checked then
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx + [fsShadow]
        else
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx - [fsShadow];
        pbTest.Face.Font.Shadow.Color := spFontShadow.Brush.Color;
        pbTest.Face.Font.Shadow.OffsetX := seFontX.Value;
        pbTest.Face.Font.Shadow.OffsetY := seFontY.Value;
        pbTest.Face.Font.Shadow.Blur := seFontShadow.Value;
        if cbFontGrad.Checked then
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx + [fsGradient]
        else
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx - [fsGradient];
        pbTest.Face.Font.Gradient.Assign(Grad);
        if cbFontLight.Checked then
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx + [fsLighting]
        else
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx - [fsLighting];
        pbTest.Face.Font.Lighting.Assign(Light);
        if cbFontText.Checked then
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx + [fsTexture]
        else
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx - [fsTexture];
        if cbFontOutline.Checked then
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx + [fsOutline]
        else
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx - [fsOutline];
        if cbFontNoise.Checked then
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx + [fsNoise]
        else
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx - [fsNoise];
        pbTest.Face.Font.Noise := sbFontNoise.Position;
        if cbFontSpray.Checked then
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx + [fsSpray]
        else
          pbTest.Face.Font.StyleEx := pbTest.Face.Font.StyleEx - [fsSpray];
        pbTest.Face.Font.Spray := seFontSpray.Value;
      end;
    6:
      begin
        pbTest.Face.PenWeight := TPenWeight(cbbPen.ItemIndex);
        pbTest.Face.PenColor := spPen.Brush.Color;
      end;
    7:
      begin
        Bmp.Assign(Back);
        if cbFlip.Checked then Bmp.Flip(True);
        if cbFlop.Checked then Bmp.Flip(False);
        if cbVShift.Checked then Bmp.VShift(sbVShift.Position * Bmp.Width div 100);
        if cbHShift.Checked then Bmp.HShift(sbHShift.Position * Bmp.Height div 100);
      end;
    8:
      begin
        if Fore.Empty then
        begin
          Fore.Assign(Back);
          Fore.Invert;
          Fore.Flip(False);
        end;
      end;
    9:
      begin
        Light.OffsetX := sbLightX.Position;
        Light.OffsetY := sbLightY.Position;
        Light.Width := sbLightW.Position;
        Light.Height := sbLightH.Position;
        Light.Color := spLight.Brush.Color;
        Light.Angle := sbLightAngle.Position;
        Light.Alpha := sbLightAlpha.Position;
      end;
  end;
  pbTest.Repaint;
end;

procedure TMainForm.DrawGraphic;
var
  w, h: Double;
  Rect: TRectF;
  Points: TPointFArray;
  i: Integer;
  x, y: Double;
  iw, ih, ix, iy: Integer;
  iRect: TRect;
  iPoints: array of TPoint;
begin
  if cbPenSmooth.Checked then
  begin
    w := sbPen1.Position * pbTest.Width / 100;
    h := sbPen2.Position * pbTest.Height / 100;
    Rect := RectF((pbTest.Width - w) / 2, (pbTest.Height - h) / 2,
      (pbTest.Width + w) / 2, (pbTest.Height + h) / 2);
    case rgPen.ItemIndex of
      0: pbTest.Face.DrawLineF(10, 10, w, h, pbTest.Face.PenColor);
      1: pbTest.Face.DrawRectF(Rect);
      2: pbTest.Face.EllipseF(Rect);
      3:
        begin
          SetLength(Points, sbPen1.Position div 5);
          for i := Low(Points) to High(Points) do
            Points[i] := PointF(Random(pbTest.Width), Random(pbTest.Height));
          pbTest.Face.PolylineF(Points);
          Points := nil;
        end;
      4:
        begin
          for i := 0 to Round(w) - 1 do
          begin
            x := Round(pbTest.Width - w) div 2 + i;
            y := pbTest.Height * 0.9 - Sqr(x - pbTest.Width div 2) / pbTest.Height
              * sbPen2.Position / 10;
            if i = 0 then
              pbTest.Face.MoveToF(x, y)
            else
              pbTest.Face.LineToF(x, y);
          end;
        end;
      5:
        begin
          for i := 10 to pbTest.Width - 10 do
          begin
            x := i;
            y := pbTest.Height / 2 + h * Sin(x * sbPen1.Position / 500 * PI) / 2;
            if i = 10 then
              pbTest.Face.MoveToF(x, y)
            else
              pbTest.Face.LineToF(x, y);
          end;
        end;
    end;
  end
  else
  begin
    iw := sbPen1.Position * pbTest.Width div 100;
    ih := sbPen2.Position * pbTest.Height div 100;
    iRect := Classes.Rect((pbTest.Width - iw) div 2, (pbTest.Height - ih) div 2,
      (pbTest.Width + iw) div 2, (pbTest.Height + ih) div 2);
    pbTest.Face.Canvas.Pen.Color := spPen.Brush.Color;
    pbTest.Face.Canvas.Pen.Style := psSolid;
    pbTest.Face.Canvas.Pen.Width := 1;
    pbTest.Face.Canvas.Brush.Style := bsClear;
    case rgPen.ItemIndex of
      0:
        begin
          pbTest.Face.Canvas.MoveTo(10, 10);
          pbTest.Face.Canvas.LineTo(iw, ih);
        end;
      1: pbTest.Face.Canvas.Rectangle(iRect);
      2: pbTest.Face.Canvas.Ellipse(iRect);
      3:
        begin
          SetLength(iPoints, sbPen1.Position div 5);
          for i := Low(iPoints) to High(iPoints) do
            iPoints[i] := Point(Random(pbTest.Width), Random(pbTest.Height));
          pbTest.Face.Canvas.Polyline(iPoints);
          iPoints := nil;
        end;
      4:
        begin
          for i := 0 to iw - 1 do
          begin
            ix := (pbTest.Width - iw) div 2 + i;
            iy := Round(pbTest.Height * 0.9 - Sqr(ix - pbTest.Width div 2) /
              pbTest.Height * sbPen2.Position / 10);
            if i = 0 then
              pbTest.Face.Canvas.MoveTo(ix, iy)
            else
              pbTest.Face.Canvas.LineTo(ix, iy);
          end;
        end;
      5:
        begin
          for i := 10 to pbTest.Width - 10 do
          begin
            ix := i;
            iy := Round(pbTest.Height / 2 + ih * Sin(ix * sbPen1.Position / 500 * PI) /
              2);
            if i = 10 then
              pbTest.Face.Canvas.MoveTo(ix, iy)
            else
              pbTest.Face.Canvas.LineTo(ix, iy);
          end;
        end;
    end;
  end;
end;

procedure TMainForm.spColorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; x, y: Integer);
begin
  if (Sender is TShape) and (Button = mbLeft) then
  begin
    ColorDialog.Color := TShape(Sender).Brush.Color;
    if ColorDialog.Execute then
      TShape(Sender).Brush.Color := ColorDialog.Color;
  end;
  UpdateImg;
end;

procedure TMainForm.btnBackClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    Back.LoadFromFile(OpenPictureDialog.FileName);
  UpdateImg;
end;

procedure TMainForm.UpdateImage(Sender: TObject);
begin
  UpdateImg;
end;

procedure TMainForm.cbBackSmoothClick(Sender: TObject);
begin
  if Sender is TCheckBox then
  begin
    pbTest.Face.SmoothFilter := TCheckBox(Sender).Checked;
    cbBackSmooth.Checked := pbTest.Face.SmoothFilter;
    cbRotateSmooth.Checked := pbTest.Face.SmoothFilter;
    UpdateImg;
  end;
end;

procedure TMainForm.btnBackResetClick(Sender: TObject);
var
  i: Integer;
begin
  BeginUpdate;
  try
    for i := 0 to TabSheet2.ControlCount - 1 do
      if TabSheet2.Controls[i] is TScrollBar then
        TScrollBar(TabSheet2.Controls[i]).Position := 0
      else if TabSheet2.Controls[i] is TCheckBox then
        with TCheckBox(TabSheet2.Controls[i]) do
          Checked := Tag <> 0;
  finally
    EndUpdate;
  end;
end;

procedure TMainForm.UpdateGradList;
var
  i: Integer;
begin
  lbGradColor.Clear;
  for i := 0 to Grad.ColorMiddle.Count - 1 do
    lbGradColor.Items.Add(Format('$%s Pos:%d%%',
      [IntToHex(Grad.ColorMiddle[i].Color, 8), Grad.ColorMiddle[i].Pos]));
  UpdateImg;
end;

procedure TMainForm.btnGradAddClick(Sender: TObject);
begin
  Grad.ColorMiddle.Add(spMiddleColor.Brush.Color, seGradPos.Value);
  spMiddleColor.Brush.Color := RandomColor;
  seGradPos.Value := Random(csMaxGradPos);
  UpdateGradList;
end;

procedure TMainForm.btnGradDelClick(Sender: TObject);
begin
  if lbGradColor.ItemIndex >= 0 then
  begin
    Grad.ColorMiddle.Delete(lbGradColor.ItemIndex);
    UpdateGradList;
  end;
end;

procedure TMainForm.btnGradClearClick(Sender: TObject);
begin
  Grad.ColorMiddle.Clear;
  UpdateGradList;
end;

procedure TMainForm.btnFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(pbTest.Face.Font);
  if FontDialog.Execute then
  begin
    BeginUpdate;
    try
      pbTest.Face.Font.Assign(FontDialog.Font);
      seFontSize.Value := pbTest.Face.Font.Size;
      spFontColor.Brush.Color := pbTest.Face.Font.Color;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TMainForm.btnFontTextClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    pbTest.Face.Font.Texture.LoadFromFile(OpenPictureDialog.FileName);
    UpdateImg;
  end;
end;

procedure TMainForm.btnBlendClick(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    Fore.LoadFromFile(OpenPictureDialog.FileName);
  UpdateImg;
end;

end.

