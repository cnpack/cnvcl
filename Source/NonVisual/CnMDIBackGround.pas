{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2019 CnPack 开发组                       }
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

unit CnMDIBackGround;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：MDI 主窗体画背景单元
* 单元作者：Shenloqi
* 备    注：
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串支持本地化处理方式
* 修改记录：2004.06.08
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Messages, Classes, Controls, Forms, StdCtrls, ExtCtrls,
  Graphics, CnConsts, CnClasses, CnCompConsts;

type
  TCnBMPDisplayStyle = (dsNormal, dsTiled, dsStretched, dsCentered, dsNone);

  TPaintImageEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TCnMDIBackGround = class(TCnComponent)
  private
    { Private declarations }
    OldWndProc: TFarProc;
    NewWndProc: Pointer;

    OldMDIWndProc: TFarProc;
    NewMDIWndProc: Pointer;

    FBitmap: TBitmap;
    FDisplayStyle: TCnBMPDisplayStyle;
    FColor: TColor;

    FBuffer: TBitmap;

    FBorderLeft: Integer;
    FBorderRight: Integer;
    FBorderBottom: Integer;
    FBorderTop: Integer;

    FOnPaintImage: TPaintImageEvent;

    procedure SetBitmap(const Value: TBitmap);
    procedure SetDStyle(const Value: TCnBMPDisplayStyle);
    procedure SetMDIColor(const Value: TColor);
  protected
    { Protected declarations }
    procedure HookWndProc(var AMsg: TMessage);
    procedure HookWnd;
    procedure UnHookWnd;

    procedure HookMDIWndProc(var AMsg: TMessage);
    procedure HookMDIWin;
    procedure UnhookMDIWin;

    procedure PaintImage(const Msg, wParam, lParam: DWORD);
    procedure DoPaintImage(ACanvas: TCanvas);

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure DrawImage(ACanvas: TCanvas; AImage: TImage);
    procedure DrawLabel(ACanvas: TCanvas; ALabel: TLabel);
  published
    { Published declarations }
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property BorderBottom: Integer read FBorderBottom write FBorderBottom;
    property BorderLeft: Integer read FBorderLeft write FBorderLeft;
    property BorderRight: Integer read FBorderRight write FBorderRight;
    property BorderTop: Integer read FBorderTop write FBorderTop;
    property Color: TColor read FColor write SetMDIColor default clappWorkspace;
    property DisplayStyle: TCnBMPDisplayStyle read FDisplayStyle write SetDStyle default dsNone;

    property OnPaintImage: TPaintImageEvent read FOnPaintImage write FOnPaintImage;
  end;

  TCnWinControlHookList = class(TObject)
  private
    FWinControl: TWinControl;
    FHooks: TList;
  public
    constructor Create(aWinControl: TWinControl);
    destructor Destroy; override;
    property WinControl: TWinControl read FWinControl;
    procedure AddHook(oldHook: TFarProc);
    function GetNextHook: TFarProc;
    function Count: integer;
  end;

procedure PushOldProc(aWinControl: TWinControl; OldHook: TFarProc);

function PopOldProc(aWinControl: TWinControl): TFarProc;

implementation

uses
  Math;

var
  FormList: TList;

procedure PushOldProc(aWinControl: TWinControl; OldHook: TFarProc);
var
  iloop: Integer;
  wHook: TCnWinControlHookList;
  bfound: Boolean;
begin
  bfound := False;
  wHook := nil;
  for iloop := 0 to FormList.Count - 1 do
  begin
    wHook := TCnWinControlHookList(FormList[iloop]);
    bfound := wHook.WinControl = aWinControl;
    if bfound then
      Break;
  end;

  if bfound then
    wHook.AddHook(OldHook)
  else
  begin
    if Assigned(aWinControl) then
    begin
      wHook := TCnWinControlHookList.Create(aWinControl);
      FormList.Add(wHook);
      wHook.AddHook(oldhook);
    end
  end
end;

function PopOldProc(aWinControl: TWinControl): TFarProc;
var
  iloop: Integer;
  wHook: TCnWinControlHookList;
  bfound: Boolean;
begin
  bfound := False;
  wHook := nil;
  for iloop := 0 to FormList.Count - 1 do
  begin
    wHook := TCnWinControlHookList(FormList[iloop]);
    bfound := wHook.WinControl = aWinControl;
    if bfound then
      Break;
  end;

  if bfound then
  begin
    Result := wHook.GetNextHook;
    if wHook.Count = 0 then
    begin
      FormList.Delete(iloop);
      wHook.Free;
    end
  end
  else
    Result := nil;
end;


function _Width(const Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

function _Height(const Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

{ TCnMDIBackGround }

constructor TCnMDIBackGround.Create(AOwner: TComponent);
begin
  inherited;

  if not ((AOwner is TForm) and (TForm(AOwner).FormStyle = fsMDIForm)) then
    raise Exception.Create('TCnMDIBackGround''s Owner MUST be MDIForm.');

  NewWndProc := nil;
  OldWndProc := nil;

  OldMDIWndProc := nil;
  NewMDIWndProc := nil;

  FBitmap := TBitmap.Create;
  FBuffer := TBitmap.Create;

  FColor := clAppWorkSpace;
  FDisplayStyle := dsNone;

  HookWnd;
end;

destructor TCnMDIBackGround.Destroy;
begin
  UnHookWnd;

  FBitmap.Free;
  FBuffer.Free;

  inherited;
end;

procedure TCnMDIBackGround.DoPaintImage(ACanvas: TCanvas);
begin
  if Assigned(FOnPaintImage) then
    FOnPaintImage(Self, ACanvas)
end;

procedure TCnMDIBackGround.DrawImage(ACanvas: TCanvas; AImage: TImage);
var
  DescRect, Rect: TRect;
  Buffer: TBitmap;
  cx, cy: Integer;
begin
  if not Assigned(AImage) then
    Exit;
  if AImage.Picture.Graphic.Empty then
    Exit;

  CopyRect(Rect, AImage.ClientRect);
  OffsetRect(Rect, AImage.Left, AImage.Top);

  //忽略 Proportional 和 IncrementalDisplay
  if AImage.AutoSize then
  begin
    ACanvas.Draw(Rect.Left, Rect.Top, AImage.Picture.Graphic);
    Exit
  end
  else if AImage.Stretch then
  begin
    ACanvas.StretchDraw(Rect, AImage.Picture.Graphic);
    Exit
  end;

  Buffer := TBitmap.Create;
  try
    Buffer.Height := AImage.Picture.Height;
    Buffer.Width := AImage.Picture.Width;
    Buffer.Canvas.Draw(0, 0, AImage.Picture.Graphic);

    if AImage.Center then
    begin
      cx := (AImage.Width - Buffer.Width) div 2;
      cy := (AImage.Height - Buffer.Height) div 2;

      Rect := Classes.Rect(Rect.Left + Max(cx, 0),
        Rect.Top + Max(cy, 0),
        Rect.Right - Max(cx, 0),
        Rect.Bottom - Max(cy, 0));
      DescRect := Rect;
      OffsetRect(DescRect, Max(-cx, 0) - DescRect.Left, Max(-cy, 0) - DescRect.Top)
    end
    else
    begin
      cx := Min(AImage.Width, Buffer.Width);
      cy := Min(AImage.Height, Buffer.Height);

      Rect := Classes.Rect(Rect.Left,
        Rect.Top,
        Rect.Left + cx,
        Rect.Top + cy);
      DescRect := Rect;
      OffsetRect(DescRect, - DescRect.Left, - DescRect.Top)
    end;

    ACanvas.CopyRect(Rect, Buffer.Canvas, DescRect)
  finally
    Buffer.Free;
  end
end;

procedure TCnMDIBackGround.DrawLabel(ACanvas: TCanvas; ALabel: TLabel);
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  CalcRect, Rect: TRect;
begin
  if not Assigned(ALabel) then
    Exit;

  CopyRect(Rect, ALabel.ClientRect);
  OffsetRect(Rect, ALabel.Left, ALabel.Top);

  with ACanvas do
  begin
    if not ALabel.Transparent then
    begin
      Brush.Color := ALabel.Color;
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;

    Brush.Style := bsClear;
    Font := ALabel.Font;
    if ALabel.Layout <> tlTop then
    begin
      CalcRect := Rect;
      DrawText(Handle, PChar(ALabel.Caption), Length(ALabel.Caption), CalcRect,
        ALabel.DrawTextBiDiModeFlags(DT_EXPANDTABS or WordWraps[ALabel.WordWrap]
          or Alignments[ALabel.Alignment] or DT_NOPREFIX or DT_CALCRECT));
      if ALabel.Layout = tlBottom then
        OffsetRect(Rect, 0, _Height(Rect) - _Height(CalcRect))
      else
        OffsetRect(Rect, 0, (_Height(Rect) - _Height(CalcRect)) div 2);
    end;

    DrawText(Handle, PChar(ALabel.Caption), Length(ALabel.Caption), Rect,
      ALabel.DrawTextBiDiModeFlags(DT_EXPANDTABS or WordWraps[ALabel.WordWrap]
      or Alignments[ALabel.Alignment] or DT_NOPREFIX));
  end
end;

procedure TCnMDIBackGround.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnMDIBackGroundName;
  Author := SCnPack_Shenloqi;
  Email := SCnPack_ShenloqiEmail;
  Comment := SCnMDIBackGroundComment;
end;

procedure TCnMDIBackGround.HookMDIWin;
begin
  if csDesigning in ComponentState then
    Exit;

  if not Assigned(NewMDIWndProc) then
  begin
    OldMDIWndProc := TFarProc(GetWindowLong(TForm(Owner).ClientHandle, GWL_WNDPROC));
    NewMDIWndProc := MakeObjectInstance(HookMDIWndProc);
    SetWindowLong(TForm(Owner).ClientHandle, GWL_WNDPROC, LongInt(NewMDIWndProc));
  end
end;

procedure TCnMDIBackGround.HookMDIWndProc(var AMsg: TMessage);
begin
  with AMsg do
  begin
    Result := CallWindowProc(OldMDIWndProc, TForm(Owner).ClientHandle, Msg, wParam, lParam);

    if Msg in [WM_PAINT{, WM_NCPAINT, WM_ERASEBKGND}] then
      PaintImage(Msg, wParam, lParam);
  end
end;

procedure TCnMDIBackGround.HookWnd;
begin
  if csDesigning in ComponentState then
    Exit;
  if TForm(Owner).FormStyle <> fsMDIForm then
    Exit;

  if not Assigned(NewWndProc) then
  begin
    OldWndProc := TFarProc(GetWindowLong(TForm(Owner).Handle, GWL_WNDPROC));
    NewWndProc := MakeObjectInstance(HookWndProc);
    SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LongInt(NewWndProc));
    PushOldProc(TForm(Owner), OldWndProc);
    HookMDIWin
  end
end;

procedure TCnMDIBackGround.HookWndProc(var AMsg: TMessage);
begin
  case AMsg.Msg of
    WM_DESTROY:
      begin
        AMsg.Result := CallWindowProc(OldWndProc, TForm(Owner).Handle, AMsg.Msg, AMsg.wParam, AMsg.lParam);
        UnHookWnd;
        Exit
      end;
  end;

  AMsg.Result := CallWindowProc(OldWndProc, TForm(Owner).Handle, AMsg.Msg, AMsg.wParam, AMsg.lParam);

  case aMsg.Msg of
    //WM_ERASEBKGND,
    //WM_NCPAINT,
    WM_PAINT: PaintImage(AMsg.Msg, AMsg.wParam, AMsg.lParam)
  end;
end;

procedure TCnMDIBackGround.PaintImage(const Msg, wParam, lParam: DWORD);
var
  ACanvas: TCanvas;
  DC: HDC;
  cx, cy: Integer;
  wRect, DescRect: TRect;
  x, y: Integer;

  procedure _ClearBuffer;
  begin
    FBuffer.Canvas.FillRect(Rect(0, 0, FBuffer.Width, FBuffer.Height))
  end;

  procedure _BufferToDC;
  begin
    BitBlt(DC,
      0,
      0,
      _Width(wRect),
      _Height(wRect),
      FBuffer.Canvas.Handle,
      0,
      0,
      SRCCOPY);
  end;

begin
  if csDesigning in ComponentState then
    Exit;
  if TForm(Owner).FormStyle <> fsMDIForm then
    Exit;

  GetWindowRect(TForm(Owner).ClientHandle, wRect);

  FBuffer.Height := _Height(wRect);
  FBuffer.Width := _Width(wRect);

  if FBitmap.Empty then
  begin
    DC := GetDC(TForm(Owner).ClientHandle);
    try
      ACanvas := FBuffer.Canvas;
      ACanvas.Brush.Color := FColor;

      _ClearBuffer;

      DoPaintImage(ACanvas);

      _BufferToDC;

      Exit
    finally
      ReleaseDC(TForm(Owner).ClientHandle, DC)
    end
  end;
  if (FBitmap.Width = 0) or (FBitmap.Height = 0) then
    Exit;

  DescRect.Left := FBorderLeft;
  DescRect.Top := FBorderTop;
  DescRect.Right := _Width(wRect) - FBorderRight;
  DescRect.Bottom := _Height(wRect) - FBorderBottom;

  DC := GetDC(TForm(Owner).ClientHandle);
  try
    ACanvas := FBuffer.Canvas;
    ACanvas.Brush.Color := FColor;

    case FDisplayStyle of
      dsNormal, dsTiled, dsStretched, dsCentered:
        begin
          case FDisplayStyle of
            dsNormal:
              begin
                _ClearBuffer;

                BitBlt(FBuffer.Canvas.Handle,
                  DescRect.Left,
                  DescRect.Top,
                  Min(FBitmap.Width, _Width(DescRect)),
                  Min(FBitmap.Height, _Height(DescRect)),
                  FBitmap.Canvas.Handle,
                  0,
                  0,
                  SRCCOPY);

                DoPaintImage(ACanvas);

              end;

            dsTiled:
              begin
                _ClearBuffer;

                cx := DescRect.Right;
                cy := DescRect.Bottom;

                y := DescRect.Top;
                while y < cy do
                begin
                  x := DescRect.Left;
                  while x < cx do
                  begin
                    BitBlt(FBuffer.Canvas.Handle,
                      x,
                      y,
                      Min(DescRect.Right - x, FBitmap.Width),
                      Min(DescRect.Bottom - y, FBitmap.Height),
                      FBitmap.Canvas.Handle,
                      0,
                      0,
                      SRCCOPY);

                    Inc(x, FBitmap.Width)
                  end;

                  Inc(y, FBitmap.Height)
                end;

                DoPaintImage(ACanvas);

              end;

            dsStretched:
              begin
                _ClearBuffer;

                cx := (wRect.Right - wRect.Left - FBorderLeft - FBorderRight);
                cy := (wRect.Bottom - wRect.Top - FBorderTop - FBorderBottom);

                FBuffer.Canvas.StretchDraw(Rect(DescRect.Left,DescRect.Top,cx,cy), FBitmap); ///Edit By LXY
                DoPaintImage(ACanvas);

              end;

            dsCentered:
              begin
                _ClearBuffer;

                cx := (_Width(DescRect) - FBitmap.Width) div 2;
                cy := (_Height(DescRect) - FBitmap.Height) div 2;

                BitBlt(FBuffer.Canvas.Handle,
                  Max(DescRect.Left, cx),
                  Max(DescRect.Top, cy),
                  Min(FBitmap.Width, _Width(DescRect)),
                  Min(FBitmap.Height, _Height(DescRect)),
                  FBitmap.Canvas.Handle,
                  Max(0, -cx),
                  Max(0, -cy),
                  SRCCOPY);

                DoPaintImage(ACanvas);

              end
          end
        end;
      dsNone:
        begin
          _ClearBuffer;

          DoPaintImage(ACanvas);

        end;
    end; // end case

    _BufferToDC;

  finally
    ReleaseDC(TForm(Owner).ClientHandle, DC)
  end
end;

procedure TCnMDIBackGround.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TCnMDIBackGround.SetDStyle(const Value: TCnBMPDisplayStyle);
begin
  if FDisplayStyle <> Value then
  begin
    FDisplayStyle := Value;
    TForm(Owner).Invalidate;
  end
end;

procedure TCnMDIBackGround.SetMDIColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    TForm(Owner).Invalidate;
  end
end;

procedure TCnMDIBackGround.UnhookMDIWin;
begin
  if csDesigning in ComponentState then
    Exit;

  if Assigned(NewMDIWndProc) then
  begin
    SetWindowLong(TForm(Owner).ClientHandle, GWL_WNDPROC, LongInt(OldMDIWndProc));
    if Assigned(NewMDIWndProc) then
      FreeObjectInstance(NewMDIWndProc);
    NewMDIWndProc := nil;
    OldMDIWndProc := nil;
  end
end;

procedure TCnMDIBackGround.UnHookWnd;
begin
  if csDesigning in ComponentState then
    Exit;

  if Assigned(NewWndProc) then
  begin
    SetWindowLong(TForm(Owner).Handle, GWL_WNDPROC, LongInt(PopOldProc(TForm(Owner))));
    if Assigned(NewWndProc) then
      FreeObjectInstance(NewWndProc);
    NewWndProc := nil;
    OldWndProc := nil;
  end;

  UnHookMDIWin;
end;

{ TCnWinControlHookList }

procedure TCnWinControlHookList.AddHook(oldHook: TFarProc);
begin
  FHooks.add(oldHook)
end;

function TCnWinControlHookList.Count: integer;
begin
  Result := FHooks.Count
end;

constructor TCnWinControlHookList.Create(aWinControl: TWinControl);
begin
  FWinControl := aWinControl;
  FHooks := TList.Create
end;

destructor TCnWinControlHookList.Destroy;
begin
  FHooks.Free;
  inherited;
end;

function TCnWinControlHookList.GetNextHook: TFarProc;
begin
  Result := FHooks[FHooks.Count - 1];
  FHooks.Delete(FHooks.Count - 1);
end;

initialization
  FormList := TList.Create;

finalization
  FormList.Free;

end.

