{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2007 CnPack 开发组                       }
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

unit CnSkinForm;

interface

uses
  Windows, Messages, Classes, Controls, SysUtils, Graphics, Forms, ExtCtrls;

const
  CN_MSG_NCREPAINT = CM_BASE + $0110;

type
  TWindowButton = (wbNone, wbClose, wbMaximized, wbMinimized);

  TCnSkinForm = class(TComponent)
  private
    FForm: TForm;
    FDownButton: TWindowButton;
    FOverButton: TWindowButton;
    FSaveWndProc: TWndMethod;
    FTimer: TTimer;
    procedure NcPaint(Active: Boolean);
    procedure SetOverButton(Value: TWindowButton);
    procedure CheckOverButton(Sender: TObject);
  protected
    procedure ThemeChange;
    procedure NewWndProc(var Message: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  CnSkinTheme;

const
  HTOVERBUTTON = HTHELP + 100;
  TransparentColor = clFuchsia;

var
  CnSkinForms: TList;

constructor TCnSkinForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FForm := AOwner as TForm;
  FSaveWndProc := FForm.WindowProc;
  FForm.WindowProc := NewWndProc;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 333;
  FTimer.OnTimer := CheckOverButton;
  CnSkinForms.Add(Self);
end;

destructor TCnSkinForm.Destroy;
begin
  FTimer.Free;
  CnSkinForms.Remove(Self);
  inherited Destroy;
end;

procedure TCnSkinForm.SetOverButton(Value: TWindowButton);
begin
  if Value <> FOverButton then
  begin
    FOverButton := Value;
    // FTimer.Enabled := FOverButton <> wbNone;
    NcPaint(FForm.Active);
  end;
end;

procedure TCnSkinForm.CheckOverButton(Sender: TObject);
var
  I: Integer;
  Pt: TPoint;
begin
  GetCursorPos(Pt);
  Dec(Pt.X, FForm.Left);
  Dec(Pt.Y, FForm.Top);
  I := FForm.Width - CnSkinThemes.CurrentSkin.ButtonRight - CnSkinThemes.CurrentSkin.ButtonSize;
  if FForm.BorderStyle <> bsDialog then
  begin
    if biMaximize in FForm.BorderIcons then
      Dec(I, CnSkinThemes.CurrentSkin.ButtonSize);
    if biMinimize in FForm.BorderIcons then
      Dec(I, CnSkinThemes.CurrentSkin.ButtonSize);
  end;
  if not PtInRect(Rect(I, CnSkinThemes.CurrentSkin.ButtonTop,
    FForm.Width - CnSkinThemes.CurrentSkin.ButtonRight,
    CnSkinThemes.CurrentSkin.ButtonTop +
    CnSkinThemes.CurrentSkin.ButtonSize), Pt) then
  begin
    SetOverButton(wbNone);
    PostMessage(FForm.Handle, CN_MSG_NCREPAINT, 0, 0);
  end;
end;

procedure TCnSkinForm.NcPaint(Active: Boolean);
var
  Canvas: TCanvas;
  R, SrcR, DestR: TRect;
  TB, LB, RB, BB: TBitmap;
  Flags: Longint;
  X, Y: Integer;
  Icon: TIcon;

  procedure BtnPaint(Btn: TWindowButton);
  begin
    Y := 0;
    if FOverButton = Btn then
    begin
      Inc(Y, CnSkinThemes.CurrentSkin.ButtonSize);
      if FDownButton = Btn then Inc(Y, CnSkinThemes.CurrentSkin.ButtonSize);
    end else
      if not Active then
        Inc(Y, CnSkinThemes.CurrentSkin.ButtonSize * 3);
    SrcR := Rect(X, Y, X + CnSkinThemes.CurrentSkin.ButtonSize,
      Y + CnSkinThemes.CurrentSkin.ButtonSize);
    OffsetRect(DestR, - CnSkinThemes.CurrentSkin.ButtonSize, 0);
    TB.Canvas.BrushCopy(DestR, CnSkinThemes.CurrentSkin.WindowBtnBmp, SrcR, TransparentColor);
  end;

begin
  TB := TBitmap.Create;
  TB.Width := FForm.Width;
  TB.Height := CnSkinThemes.CurrentSkin.CaptionHeight;
  LB := TBitmap.Create;
  LB.Width := CnSkinThemes.CurrentSkin.BorderSize;
  LB.Height := FForm.Height - CnSkinThemes.CurrentSkin.CaptionHeight - CnSkinThemes.CurrentSkin.BorderSize;
  RB := TBitmap.Create;
  RB.Width := CnSkinThemes.CurrentSkin.BorderSize;
  RB.Height := LB.Height;
  BB := TBitmap.Create;
  BB.Width := FForm.Width;
  BB.Height := CnSkinThemes.CurrentSkin.BorderSize;
  Icon := nil;
  Canvas := TCanvas.Create;
  try
    R := Rect(0, 0, CnSkinThemes.CurrentSkin.WindowBmp.Width, CnSkinThemes.CurrentSkin.WindowBmp.Height div 2);
    if not Active then
      OffsetRect(R, 0, R.Bottom);
    TB.Canvas.Brush.Style := bsClear;
    SrcR := Rect(R.Left, R.Top, R.Left + CnSkinThemes.CurrentSkin.CaptionHeight, R.Top + CnSkinThemes.CurrentSkin.CaptionHeight);
    DestR := Rect(0, 0, CnSkinThemes.CurrentSkin.CaptionHeight, TB.Height);
    TB.Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.WindowBmp.Canvas, SrcR);
    OffsetRect(SrcR, R.Right - R.Left - CnSkinThemes.CurrentSkin.CaptionHeight, 0);
    OffsetRect(DestR, TB.Width - CnSkinThemes.CurrentSkin.CaptionHeight, 0);
    TB.Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.WindowBmp.Canvas, SrcR);
    SrcR.Right := Srcr.Left;
    SrcR.Left := R.Left + CnSkinThemes.CurrentSkin.CaptionHeight;
    DestR.Right := DestR.Left;
    DestR.Left := CnSkinThemes.CurrentSkin.CaptionHeight;
    TB.Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.WindowBmp.Canvas, SrcR);
    DestR := Rect(0, 0, CnSkinThemes.CurrentSkin.ButtonSize, CnSkinThemes.CurrentSkin.ButtonSize);
    OffsetRect(DestR, TB.Width - CnSkinThemes.CurrentSkin.ButtonRight, CnSkinThemes.CurrentSkin.ButtonTop);
    X := 0;
    BtnPaint(wbClose);
    if FForm.BorderStyle <> bsDialog then
    begin
      Inc(X, CnSkinThemes.CurrentSkin.ButtonSize);
      if biMaximize in FForm.BorderIcons then
      begin
        if FForm.WindowState = wsNormal then
          Inc(X, CnSkinThemes.CurrentSkin.ButtonSize);
        BtnPaint(wbMaximized);
        if FForm.WindowState <> wsNormal then
          Inc(X, CnSkinThemes.CurrentSkin.ButtonSize);
        Inc(X, CnSkinThemes.CurrentSkin.ButtonSize);
      end
      else
        Inc(X, CnSkinThemes.CurrentSkin.ButtonSize * 2);
      if biMinimize in FForm.BorderIcons then
        BtnPaint(wbMinimized);
    end;
    Flags := DT_LEFT or DT_VCENTER or DT_SINGLELINE;
    DestR.Right := DestR.Left - CnSkinThemes.CurrentSkin.BorderSize;
    DestR.Left := CnSkinThemes.CurrentSkin.BorderSize + 2;
    if FForm.BorderStyle <> bsDialog then
    begin
      if not FForm.Icon.Empty then
        Icon := FForm.Icon else
        if FForm = Application.MainForm then
          Icon := Application.Icon;
      if Assigned(Icon) then
      begin
        DrawIconEx(TB.Canvas.Handle, DestR.Left,
          CnSkinThemes.CurrentSkin.ButtonTop + 2,
          Icon.Handle, 16, 16, 0, 0, DI_NORMAL or DT_VCENTER);
        Inc(DestR.Left, 18);
      end;
    end;
    TB.Canvas.Font := FForm.Font;
    TB.Canvas.Font.Size := 11;
    TB.Canvas.Font.Style := [fsBold];
    if Active then
      TB.Canvas.Font.Color := CnSkinThemes.CurrentSkin.ActiveCaptionColor
    else
      TB.Canvas.Font.Color := CnSkinThemes.CurrentSkin.InactiveCaptionColor;

    DrawText(TB.Canvas.Handle, PChar(FForm.Caption), -1, DestR, Flags);
    SrcR := Rect(R.Left, R.Top + CnSkinThemes.CurrentSkin.CaptionHeight, R.Left
      + CnSkinThemes.CurrentSkin.BorderSize, R.Bottom - CnSkinThemes.CurrentSkin.BorderSize);
    DestR := Rect(0, 0, LB.Width, LB.Height);
    LB.Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.WindowBmp.Canvas, SrcR);
    SrcR.Right := R.Right;
    SrcR.Left := R.Right - CnSkinThemes.CurrentSkin.BorderSize;
    RB.Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.WindowBmp.Canvas, SrcR);
    SrcR := Rect(R.Left, R.Bottom - CnSkinThemes.CurrentSkin.BorderSize, R.Left + CnSkinThemes.CurrentSkin.BorderSize, R.Bottom);
    DestR := Rect(0, 0, CnSkinThemes.CurrentSkin.BorderSize, BB.Height);
    BB.Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.WindowBmp.Canvas, SrcR);
    OffsetRect(SrcR, R.Right - R.Left - CnSkinThemes.CurrentSkin.BorderSize, 0);
    OffsetRect(DestR, BB.Width - CnSkinThemes.CurrentSkin.BorderSize, 0);
    BB.Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.WindowBmp.Canvas, SrcR);
    SrcR.Right := SrcR.Left;
    SrcR.Left := R.Left + CnSkinThemes.CurrentSkin.BorderSize;
    DestR.Right := DestR.Left;
    DestR.Left := CnSkinThemes.CurrentSkin.BorderSize;
    BB.Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.WindowBmp.Canvas, SrcR);
    Canvas.Handle := GetWindowDC(FForm.Handle);
    try
      ExcludeClipRect(Canvas.Handle, CnSkinThemes.CurrentSkin.BorderSize, CnSkinThemes.CurrentSkin.CaptionHeight,
        FForm.Width - CnSkinThemes.CurrentSkin.BorderSize, FForm.Height - CnSkinThemes.CurrentSkin.BorderSize);
      R := Rect(0, 0, TB.Width, TB.Height);
      Canvas.CopyRect(R, TB.Canvas, R);
      SrcR := Rect(0, 0, LB.Width, LB.Height);
      DestR := SrcR;
      OffsetRect(DestR, 0, CnSkinThemes.CurrentSkin.CaptionHeight);
      Canvas.CopyRect(DestR, LB.Canvas, SrcR);
      OffsetRect(DestR, FForm.Width - CnSkinThemes.CurrentSkin.BorderSize, 0);
      Canvas.CopyRect(DestR, RB.Canvas, SrcR);
      SrcR := Rect(0, 0, BB.Width, BB.Height);
      DestR := SrcR;
      OffsetRect(DestR, 0, FForm.Height - CnSkinThemes.CurrentSkin.BorderSize);
      Canvas.CopyRect(DestR, BB.Canvas, SrcR);
    finally
      ReleaseDC(FForm.Handle, Canvas.Handle);
    end;
  finally
    TB.Free;
    LB.Free;
    RB.Free;
    BB.Free;
    Canvas.Free;
  end;
end;

procedure TCnSkinForm.ThemeChange;
var
  SaveCW, SaveCH: Integer;
  Flags: Longint;
  Rgn: HRGN;
begin
  FForm.Color := CnSkinThemes.CurrentSkin.FaceColor;
  SaveCW := FForm.ClientWidth;
  SaveCH := FForm.ClientHeight;
  Flags := GetWindowLong(FForm.Handle, GWL_STYLE);
  if not CnSkinThemes.Active then
    SetWindowLong(FForm.Handle, GWL_STYLE, Flags or WS_CAPTION)
  else
    SetWindowLong(FForm.Handle, GWL_STYLE, Flags and not WS_CAPTION);
    
  FForm.ClientWidth := SaveCW;
  FForm.ClientHeight := SaveCH;
  if CnSkinThemes.CurrentSkin.RgnSize <> 0 then
  begin
    Rgn := CreateRoundRectRgn(0, 0, FForm.Width + 1, FForm.Height + CnSkinThemes.CurrentSkin.RgnSize,
      CnSkinThemes.CurrentSkin.RgnSize, CnSkinThemes.CurrentSkin.RgnSize);
    SetWindowRgn(FForm.Handle, Rgn, True);
    DeleteObject(Rgn);
  end
  else
    SetWindowRgn(FForm.Handle, 0, True);
end;

procedure TCnSkinForm.NewWndProc(var Message: TMessage);
var
  Pt: TPoint;
  WP: PWindowPos;
  Btn: TWindowButton;
  Right, Bottom: Integer;
  Rgn: HRGN;
begin
  if (not CnSkinThemes.Active) or (FForm.BorderStyle = bsNone) or
    (FForm.BorderStyle > bsDialog) then
    FSaveWndProc(Message)
  else
    case Message.Msg of
      WM_NCACTIVATE:
      begin
        Message.Result := 1;
        NcPaint(TWMNcActivate(Message).Active);
      end;

      WM_NCCALCSIZE:
        begin
          FSaveWndProc(Message);
          if Message.WParam <> 0 then
          begin
            WP := TWMNCCalcSize(Message).CalcSize_Params^.lppos;
            with TWMNCCalcSize(Message).CalcSize_Params^.rgrc[0] do
            begin
              Inc(Top, CnSkinThemes.CurrentSkin.CaptionHeight);
              Dec(Bottom, CnSkinThemes.CurrentSkin.BorderSize);
              Inc(Left, CnSkinThemes.CurrentSkin.BorderSize);
              Dec(Right, CnSkinThemes.CurrentSkin.BorderSize);
            end;

            TWMNCCalcSize(Message).CalcSize_Params^.rgrc[1] := TWMNCCalcSize(Message).CalcSize_Params^.rgrc[0];
            Message.Result := WVR_VALIDRECTS;
          end;
        end;
      WM_NCHITTEST:
      begin
        FSaveWndProc(Message);
        Btn := wbNone;
        with TWMNCHitTest(Message) do
          Pt := Point(XPos - FForm.Left, YPos - FForm.Top);

        // 判断是否落在系统图标内
        if FForm.BorderStyle <> bsDialog then
        begin
          if not FForm.Icon.Empty or ((FForm = Application.MainForm) and not Application.Icon.Empty) then
          begin
            if PtInRect(Rect(CnSkinThemes.CurrentSkin.BorderSize, CnSkinThemes.CurrentSkin.ButtonTop,
              CnSkinThemes.CurrentSkin.BorderSize + 2 + 16, CnSkinThemes.CurrentSkin.ButtonTop + 16), Pt) then
            begin
              Message.Result := HTSYSMENU;
              Exit;
            end;
          end;
        end;

        Right := FForm.Width - CnSkinThemes.CurrentSkin.ButtonRight;
        Bottom := CnSkinThemes.CurrentSkin.ButtonTop + CnSkinThemes.CurrentSkin.ButtonSize;
        if PtInRect(Rect(Right - CnSkinThemes.CurrentSkin.ButtonSize, CnSkinThemes.CurrentSkin.ButtonTop, Right, Bottom), Pt) then
        begin
          // 鼠标坐标在最右边的关闭按钮
          Btn := wbClose;
          Message.Result := HTCLOSE;
        end
        else
          if FForm.BorderStyle <> bsDialog then
          begin
            if biMaximize in FForm.BorderIcons then
            begin
              Dec(Right, CnSkinThemes.CurrentSkin.ButtonSize);
              if PtInRect(Rect(Right - CnSkinThemes.CurrentSkin.ButtonSize, CnSkinThemes.CurrentSkin.ButtonTop, Right, Bottom), Pt) then
              begin
                // 在最大化按钮区域内
                Btn := wbMaximized;
                Message.Result := HTMAXBUTTON;
              end;
            end;
            if biMinimize in FForm.BorderIcons then
            begin
              Dec(Right, CnSkinThemes.CurrentSkin.ButtonSize);
              if PtInRect(Rect(Right - CnSkinThemes.CurrentSkin.ButtonSize, CnSkinThemes.CurrentSkin.ButtonTop, Right, Bottom), Pt) then
              begin
                // 在最小化按钮区域内
                Btn := wbMinimized;
                Message.Result := HTMINBUTTON;
              end;
            end;
          end;
        SetOverButton(Btn);
        if PtInRect(Rect(CnSkinThemes.CurrentSkin.BorderSize, CnSkinThemes.CurrentSkin.BorderSize, Right - CnSkinThemes.CurrentSkin.ButtonSize, CnSkinThemes.CurrentSkin.CaptionHeight), Pt) then
          Message.Result := HTCAPTION;
      end;

      // 被WM_ENTERMENULOOP消息激活而通知重画的消息
      CN_MSG_NCREPAINT: NcPaint(FForm.Active);
      CM_TEXTCHANGED: NcPaint(FForm.Active);
      WM_NCPAINT: NcPaint(FForm.Active);
      WM_SETTEXT: NcPaint(FForm.Active);
      WM_NCMOUSEMOVE: CheckOverButton(Self);
      // 必须处理此消息并且送出重画消息来重画，否则标题栏上会出现原有按钮造成错乱
      WM_ENTERMENULOOP: PostMessage(FForm.Handle, CN_MSG_NCREPAINT, 0, 0);
      $00AE: Message.Result := 1;
      //WM_INITMENU: PostMessage(FForm.Handle, CN_MSG_NCREPAINT, 0, 0);
      WM_NCLBUTTONDOWN:
      begin
        if FOverButton <> wbNone then
          TWMNCHitMessage(Message).HitTest := HTOVERBUTTON;
        FSaveWndProc(Message);
        if FDownButton <> FOverButton then
        begin
          FDownButton := FOverButton;
          NcPaint(FForm.Active);
        end;
      end;

      WM_NCLBUTTONUP:
      begin
        if FDownButton <> wbNone then
        begin
          case FDownButton of
            wbClose: FForm.Close;
            wbMaximized:
              if FForm.WindowState <> wsNormal then
                FForm.WindowState := wsNormal
              else
                FForm.WindowState := wsMaximized;
            wbMinimized: Application.Minimize;
          end;
          FDownButton := wbNone;
          NcPaint(FForm.Active);
        end;
      end;

      WM_SIZE:
      begin
        FSaveWndProc(Message);
        if CnSkinThemes.CurrentSkin.RgnSize > 0 then
        begin
          Rgn := CreateRoundRectRgn(0, 0, FForm.Width + 1,
            FForm.Height + CnSkinThemes.CurrentSkin.RgnSize,
            CnSkinThemes.CurrentSkin.RgnSize, CnSkinThemes.CurrentSkin.RgnSize);
          SetWindowRgn(FForm.Handle, Rgn, True);
          DeleteObject(Rgn);
        end;
//        NcPaint(FForm.Active);
      end;

    else
      FSaveWndProc(Message);
    end;
end;

{procedure ResetSkin(SkinPath: string; IniFile: TIniFile);
var
  I: Integer;
begin
  if FileExists(SkinPath + 'window.bmp') then
    CnSkinThemes.CurrentSkin.WindowBmp.LoadFromFile(SkinPath + 'window.bmp');
  if FileExists(SkinPath + 'window_button.bmp') then
  begin
    CnSkinThemes.CurrentSkin.WindowBtnBmp.LoadFromFile(SkinPath + 'window_button.bmp');
    ButtonSize := CnSkinThemes.CurrentSkin.WindowBtnBmp.Width div 4;
  end;
  if Assigned(IniFile) then
  begin
    CnSkinThemes.CurrentSkin.CaptionHeight := IniFile.ReadInteger('Parameter', 'CnSkinThemes.CurrentSkin.CaptionHeight',
      CnSkinThemes.CurrentSkin.CaptionHeight);
    BorderSize := IniFile.ReadInteger('Parameter', 'BorderSize', BorderSize);
    ButtonRight := IniFile.ReadInteger('Parameter', 'ButtonRight', ButtonRight);
    ButtonTop := IniFile.ReadInteger('Parameter', 'ButtonTop', ButtonTop);
    RgnSize := IniFile.ReadInteger('Parameter', 'RgnSize', 0);
    ActiveCaption := TColor(IniFile.ReadInteger('Color', 'ActiveCaption',
      ActiveCaption));
    InactiveCaption := TColor(IniFile.ReadInteger('Color', 'InactiveCaption',
      InactiveCaption));
  end else
  begin
    RgnSize := 0;
  end;
  for I := 0 to CnSkinForms.Count -1 do
    TCnSkinForm(CnSkinForms[I]).ThemeChange;   
end;                                          }

initialization
  CnSkinForms := TList.Create;

finalization
  CnSkinForms.Free;

end.
