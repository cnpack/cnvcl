{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnGauge;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ��·��������ؼ�ʵ�ֵ�Ԫ
* ��Ԫ���ߣ����о� (iamdream@yeah.net)
* ��    ֲ��CnPack ������ (master@cnpack.org)
* ��    ע������ʹ�ý���ɫ����ʾ���ȣ�������ʹ��ͼƬ��
* ����ƽ̨��PWinXP + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.05.27 V0.1
*               ��ֲ��Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics;

type
{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnGauge = class(TCustomControl)
  private
    FAutoResume: Boolean;  // �Զ�����
    FGradient:   Boolean;  // ʹ�ý���ɫ
    FDrawFrame:  Boolean;  // ���߿�
    FUseGraph:   Boolean;  // ʹ��ͼƬ
    FShowPer:    Boolean;  // ��ʾ�ٷֱ�
    FBevelWidth: Integer;  // �߿��������֮�䱣���հ�
    FMinValue,
    FMaxValue,
    FProgress,
    FBarWidth: Integer;
    FBackColor,
    FForeColor,
    FFrameColor,
    FGradientColor: TColor;
    FBmpBuf:     TBitmap;  // ����λͼ
    FBackGraph:  TBitmap;  // ����λͼ
    FForeGraph:  TBitmap;  // ǰ��λͼ
    procedure SetMinValue(Value: Integer);
    procedure SetMaxValue(Value: Integer);
    procedure SetProgress(Value: Integer);
    procedure SetBarWidth(Value: Integer);
    procedure SetBackColor(Value: TColor);
    procedure SetForeColor(Value: TColor);
    procedure SetFrameColor(Value: TColor);
    procedure SetGradient(Value: Boolean);
    procedure SetGradientColor(Value: TColor);
    procedure SetDrawFrame(Value: Boolean);
    procedure SetBevelWidth(Value: Integer);
    procedure SetUseGraph(Value: Boolean);
    function GetBackGraph: TGraphic;
    procedure SetBackGraph(Value: TGraphic);
    function GetForeGraph: TGraphic;
    procedure SetForeGraph(Value: TGraphic);
    procedure SetShowPer(Value: Boolean);
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
  protected
    procedure Paint; override;
    procedure DoDrawFrame(ACanvas: TCanvas; ARect: TRect);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StepIt(iLength: Integer = 1);
  published
    property AutoResume: Boolean read FAutoResume write FAutoResume default False;
    {* �Ƿ��Զ�����}
    property Gradient: Boolean read FGradient write SetGradient default False;
    {* �Ƿ�ʹ�ý���ɫ}
    property DrawFrame: Boolean read FDrawFrame write SetDrawFrame default True;
    {* �Ƿ���Ʊ߿�}
    property UseGraph: Boolean read FUseGraph write SetUseGraph default False;
    {* �Ƿ�ʹ��ͼƬ}
    property ShowPercent: Boolean read FShowPer write SetShowPer default False;
    {* �Ƿ���ʾ�ٷֱ�}
    property BevelWidth: Integer read FBevelWidth write SetBevelWidth default 1;
    {* �ڱ߿��������֮�䱣���Ŀհף�Ĭ�� 1  ����}
    property MinValue: Integer read FMinValue write SetMinValue default 0;
    {* ��Сֵ}
    property MaxValue: Integer read FMaxValue write SetMaxValue default 100;
    {* ���ֵ}
    property Progress: Integer read FProgress write SetProgress default 0;
    {* ��ǰֵ}
    property BarWidth: Integer read FBarWidth write SetBarWidth default 0;
    {* ��������ǰ��ɫ��Ŀ�ȣ���Ϊ0����ʾ���̶���ȣ���ͷ������}
    property BackColor: TColor  read FBackColor  write SetBackColor  default clBtnFace;
    {* ����ɫ}
    property ForeColor: TColor  read FForeColor  write SetForeColor  default clBlue;
    {* ǰ��ɫ}
    property FrameColor: TColor read FFrameColor write SetFrameColor default clNavy;
    {* �߿���ɫ}
    property GradientColor: TColor read FGradientColor write SetGradientColor default clWhite;
    {* ����ɫ���������ʱ�Ǵӽ���ɫ��ǰ��ɫ}
    property BackGraph: TGraphic read GetBackGraph write SetBackGraph;
    {* ����ͼƬ}
    property ForeGraph: TGraphic read GetForeGraph write SetForeGraph;
    {* ������ǰ��ͼƬ}

    property Action;
    property Align;
    property Constraints;
    property Ctl3D;
    property Font;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation

{ TCnGauge }

constructor TCnGauge.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width     := 200;
  Height    := 20;
  FDrawFrame := True;
  FBevelWidth := 1;
  FMinValue := 0;
  FMaxValue := 100;
  FProgress := 0;
  FBarWidth := 0;
  FBackColor := clBtnFace;
  FForeColor := clBlue;
  FFrameColor := clNavy;
  FGradientColor := clWhite;
  FBmpBuf     := TBitmap.Create;
  FBmpBuf.PixelFormat := pf24bit; // for QuickDrawShade()
  FBackGraph  := TBitmap.Create;
  FForeGraph  := TBitmap.Create;
  with Font do
  begin
    Color := clYellow;
    Name  := 'Times New Roman';
    Size  := 10;
    Style := [fsBold];
  end;
end;

destructor TCnGauge.Destroy;
begin
  FBmpBuf.Free;
  FBackGraph.Free;
  FForeGraph.Free;
  inherited;
end;

procedure TCnGauge.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  if Visible then
    DoDrawFrame(Canvas, ClientRect);
end;

procedure TCnGauge.DoDrawFrame(ACanvas: TCanvas; ARect: TRect);
begin
  if FDrawFrame then
  begin
    with ACanvas do
    begin
      if Self.Ctl3D then
      begin
        Pen.Color := clBtnShadow;
        MoveTo(ARect.Left, ARect.Bottom);
        LineTo(ARect.Left, ARect.Top);
        LineTo(ARect.Right -1, ARect.Top);
        Pen.Color := clBtnHighlight;
        LineTo(ARect.Right -1, ARect.Bottom -1);
        LineTo(ARect.Left, ARect.Bottom -1);
      end 
      else 
      begin
        Brush.Color := FrameColor;
        FrameRect(ARect);
      end;
    end;
  end;
end;

procedure TCnGauge.Paint;

  function GetGradualColor(ACanvas: TCanvas; aRGB, gRGB: Longint; iValue, iWidth: Integer): TColor;
  var
    r, g, b, gr, gg, gb: Byte;
  begin
    r := GetRValue(aRGB);
    g := GetGValue(aRGB);
    b := GetBValue(aRGB);
    gr:= GetRValue(gRGB);
    gg:= GetGValue(gRGB);
    gb:= GetBValue(gRGB);
    r := r + (gr - r) * iValue div iWidth;
    g := g + (gg - g) * iValue div iWidth;
    b := b + (gb - b) * iValue div iWidth;
    Result := Windows.GetNearestColor(ACanvas.Handle, RGB(r, g, b));
  end;

  procedure DrawShade(ACanvas: TCanvas; ARect: TRect);
  var
    i, iWidth, iLeft, iStart, iSpace: Integer;
    AColor, GColor: Longint;
  begin
    iWidth := ARect.Right - ARect.Left;
    if iWidth <= 0 then
      Exit;

    AColor := ColorToRGB(ForeColor);  //Ӧ��ת��һ�£�����Ͳ�����ȷ����clMenu֮����ɫ��ȫ�ɺ�ɫ�ˣ�
    GColor := ColorToRGB(GradientColor); //Ӧ��ת��һ�£�����Ͳ�����ȷ����clMenu֮����ɫ��ȫ�ɺ�ɫ�ˣ�
    iLeft  := ARect.Left;
    iSpace := 0;
    Inc(iSpace, Integer(FDrawFrame));
    Inc(iSpace, FBevelWidth);
    if iLeft < iSpace then
      iStart := iSpace - iLeft else iStart := 0;
    for i := iStart to iWidth -1 do
    begin
      ACanvas.Brush.Color := GetGradualColor(ACanvas, AColor, GColor, iWidth -i, iWidth);
      ACanvas.FillRect(Rect(iLeft + i, ARect.Top, iLeft + i +1, ARect.Bottom));
    end;
  end;

  procedure QuickDrawShade(Bmp: TBitmap; ARect: TRect);
  var
    r, g, b, gr, gg, gb: Byte;
    x, y, iWidth, iValue, iLeft, aRGB, gRGB, iSpace: Integer;
    p: PByte;
  begin
    aRGB := ColorToRGB(ForeColor);   //Ӧ��ת��һ�£�����Ͳ�����ȷ����clMenu֮����ɫ��ȫ�ɺ�ɫ�ˣ�
    gRGB := ColorToRGB(GradientColor);  //Ӧ��ת��һ�£�����Ͳ�����ȷ����clMenu֮����ɫ��ȫ�ɺ�ɫ�ˣ�
    r := GetRValue(aRGB);
    g := GetGValue(aRGB);
    b := GetBValue(aRGB);
    gr:= GetRValue(gRGB);
    gg:= GetGValue(gRGB);
    gb:= GetBValue(gRGB);
    iWidth := ARect.Right - ARect.Left;
    if iWidth <= 0 then
      Exit;

    iSpace := 0;
    Inc(iSpace, Integer(FDrawFrame));
    Inc(iSpace, FBevelWidth);
    for y := ARect.Top to ARect.Bottom -1 do
    begin
      p := Bmp.ScanLine[y];
      x := ARect.Left;
      if x < iSpace then
        x := iSpace;

      iLeft := x - ARect.Left;
      Inc(p, x *3);
      while iLeft < iWidth do
      begin
        iValue := iWidth - iLeft;
        p^ := b + (gb - b) * iValue div iWidth;
        Inc(p);
        p^ := g + (gg - g) * iValue div iWidth;
        Inc(p);
        p^ := r + (gr - r) * iValue div iWidth;
        Inc(p);
        Inc(iLeft);
      end;
    end;
  end;

  procedure DrawPercentText(ACanvas: TCanvas; ARect: TRect);
  var
    sPer: string;
    rtNeed, rtCopy: TRect;
    bmpOver: TBitmap;
    iLeft, iTop: Integer;
  begin
    if csLoading in ComponentState then
      Exit;

    sPer := Format('%d%%', [(Progress - Self.MinValue) * 100 div (FMaxValue - FMinValue)]);
    with ACanvas do
    begin
      //Brush.Style := bsClear;
      Font.Assign(Self.Font);
      DrawText(Handle, PChar(sPer), Length(sPer), rtNeed, DT_SINGLELINE or DT_CALCRECT);
      iLeft := ARect.Left + (ARect.Right - ARect.Left - rtNeed.Right + rtNeed.Left) div 2;
      iTop  := ARect.Top + (ARect.Bottom - ARect.Top - rtNeed.Bottom + rtNeed.Top) div 2;
      rtCopy  := Rect(iLeft, iTop, iLeft + rtNeed.Right - rtNeed.Left, iTop + rtNeed.Bottom - rtNeed.Top);
      bmpOver := TBitmap.Create;
      try
        bmpOver.Width  := rtNeed.Right - rtNeed.Left;
        bmpOver.Height := rtNeed.Bottom - rtNeed.Top;
        with bmpOver.Canvas do
        begin
          Brush.Color := TColor(0);
          Brush.Style := bsSolid;
          FillRect(ClipRect);
          Font.Assign(Self.Font);
          Brush.Style := bsClear;
          rtNeed := ClipRect;
          DrawText(Handle, PChar(sPer), Length(sPer), rtNeed, DT_SINGLELINE or DT_VCENTER or DT_CENTER);
        end;
        CopyMode := cmSrcInvert;
        Draw(rtCopy.Left, rtCopy.Top, bmpOver);
      finally
        bmpOver.Free;
      end;
    end;
  end;

var
  ARect, PerRect: TRect;
begin
  with FBmpBuf.Canvas do
  begin
    Lock;
    try
      FBmpBuf.Width  := ClientWidth;
      FBmpBuf.Height := ClientHeight;
      ARect := ClipRect;

      //��䱳��
      if FDrawFrame then
        InflateRect(ARect, -1, -1);
      if UseGraph and not BackGraph.Empty then
      begin
        StretchDraw(ARect, BackGraph);
      end else begin
        Brush.Color := BackColor;
        FillRect(ARect);
      end;

      // ��������
      if FBevelWidth > 0 then
        InflateRect(ARect, - FBevelWidth, - FBevelWidth);

      PerRect := ARect;
      ARect.Right := ARect.Left + (ARect.Right - ARect.Left) * (Progress - MinValue) div (MaxValue - MinValue);
      if BarWidth > 0 then
        ARect.Left  := ARect.Right - BarWidth;
      if UseGraph and not ForeGraph.Empty then
      begin
        StretchDraw(ARect, ForeGraph);
      end
      else
      begin
        if Gradient then
        begin
          //DrawShade(FBmpBuf.Canvas, ARect);
          QuickDrawShade(FBmpBuf, ARect);
        end
        else
        begin
          Brush.Color := ForeColor;
          FillRect(ARect);
        end;
      end;

      //���ٷֱ�
      if ShowPercent then
        DrawPercentText(FBmpBuf.Canvas, PerRect);
      //�����
      DoDrawFrame(FBmpBuf.Canvas, ClipRect);
    finally
      Unlock;
    end;
  end;
  Canvas.Draw(0, 0, FBmpBuf);
end;

procedure TCnGauge.SetBackColor(Value: TColor);
begin
  if Value <> FBackColor then
  begin
    FBackColor := Value;
    Paint;
  end;
end;

procedure TCnGauge.SetForeColor(Value: TColor);
begin
  if Value <> FForeColor then
  begin
    FForeColor := Value;
    Paint;
  end;
end;

procedure TCnGauge.SetFrameColor(Value: TColor);
begin
  if Value <> FFrameColor then
  begin
    FFrameColor := Value;
    Paint;
  end;
end;

procedure TCnGauge.SetMaxValue(Value: Integer);
begin
  if (Value <> FMaxValue) and (Value > FMinValue) then
  begin
    FMaxValue := Value;
    if FProgress > FMaxValue then
      FProgress := FMaxValue;
    Paint;
  end;
end;

procedure TCnGauge.SetMinValue(Value: Integer);
begin
  if (Value <> FMinValue) and (Value >= 0) and (Value < FMaxValue) then
  begin
    FMinValue := Value;
    if FProgress < FMinValue then
      FProgress := FMinValue;
    Paint;
  end;
end;

procedure TCnGauge.SetProgress(Value: Integer);
begin
  if (Value <> FProgress) and (Value >= FMinValue) and (Value <= FMaxValue) then
  begin
    FProgress := Value;
    Paint;
  end;
end;

procedure TCnGauge.SetBarWidth(Value: Integer);
begin
  if Value <> FBarWidth then
  begin
    FBarWidth := Value;
    Paint;
  end;
end;

procedure TCnGauge.StepIt(iLength: Integer);
begin
  Inc(FProgress, iLength);
  if FProgress > FMaxValue then
  begin
    if FAutoResume then
      FProgress := FMinValue
    else
      FProgress := FMaxValue;
  end;
  Paint;
end;

procedure TCnGauge.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TCnGauge.SetGradient(Value: Boolean);
begin
  if Value <> FGradient then
  begin
    FGradient := Value;
    Paint;
  end;
end;

procedure TCnGauge.SetGradientColor(Value: TColor);
begin
  if Value <> FGradientColor then
  begin
    FGradientColor := Value;
    if Gradient then
      Paint;
  end;
end;

procedure TCnGauge.SetDrawFrame(Value: Boolean);
begin
  if Value <> FDrawFrame then
  begin
    FDrawFrame := Value;
    Paint;
  end;
end;

procedure TCnGauge.SetBevelWidth(Value: Integer);
begin
  if Value <> FBevelWidth then
  begin
    FBevelWidth := Value;
    Paint;
  end;
end;

procedure TCnGauge.SetUseGraph(Value: Boolean);
begin
  if Value <> FUseGraph then
  begin
    FUseGraph := Value;
    Paint;
  end;
end;

function TCnGauge.GetBackGraph: TGraphic;
begin
  Result := FBackGraph;
end;

procedure TCnGauge.SetBackGraph(Value: TGraphic);
begin
  FBackGraph.Assign(Value);
  if UseGraph then
    Paint;
end;

function TCnGauge.GetForeGraph: TGraphic;
begin
  Result := FForeGraph;
end;

procedure TCnGauge.SetForeGraph(Value: TGraphic);
begin
  FForeGraph.Assign(Value);
  if UseGraph then
    Paint;
end;

procedure TCnGauge.SetShowPer(Value: Boolean);
begin
  if Value <> FShowPer then
  begin
    FShowPer := Value;
    Paint;
  end;
end;

end.
