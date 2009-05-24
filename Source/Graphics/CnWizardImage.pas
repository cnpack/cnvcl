{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2009 CnPack 开发组                       }
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

{******************************************************************************}
{ Unit Note:                                                                   }
{    This file is derived from Marley Software                                 }
{                                                                              }
{ Original author:                                                             }
{    http://marleyware.com/marley/twizardtree.htm                              }
{    Marley <pablo@marleyware.com>                                             }
{******************************************************************************}

unit CnWizardImage;
{* |<PRE>
================================================================================
* 软件名称：界面组件包
* 单元名称：向导界面图像控件
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：该控件基于 Mr. Marley 的 WizardTree 控件修改而来，增加了大量的改进
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnWizardImage.pas,v 1.11 2009/01/02 08:27:39 liuxiao Exp $
* 修改记录：2003.04.06 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms;

type
  TCnWizardItem = class;
  TCnWizardItems = class;
  TCnWizardImage = class;
  TBackGroundMode = (bmNormal, bmCenter, bmTiled, bmStretched);

{ TCnWizardItem }

  TCnWizardItem = class(TCollectionItem)
  private
    FCaption: TCaption;
    FBackGround: TPicture;
    FWizardItems: TCnWizardItems;
    FComment: TStrings;
    FBackGroundMode: TBackGroundMode;
    FBackGroundX: Integer;
    FBackGroundY: Integer;
    FVisible: Boolean;
    procedure Changed;
    procedure OnChange(Sender: TObject);
    procedure SetCaption(const Value: TCaption);
    procedure SetBackGround(const Value: TPicture);
    procedure SetComment(const Value: TStrings);
    procedure SetBackGroundMode(const Value: TBackGroundMode);
    procedure SetBackGroundTransparent(const Value: Boolean);
    function GetBackGroundTransparent: Boolean;
    procedure SetBackGroundX(const Value: Integer);
    procedure SetBackGroundY(const Value: Integer);
    procedure SetVisible(const Value: Boolean);
    function BackEmpty: Boolean;
    function GetBackGround: TPicture;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property WizardItems: TCnWizardItems read FWizardItems;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Caption: TCaption read FCaption write SetCaption;
    property Comment: TStrings read FComment write SetComment;
    property BackGround: TPicture read GetBackGround write SetBackGround;
    property BackGroundTransparent: Boolean read GetBackGroundTransparent
      write SetBackGroundTransparent default False;
    property BackGroundMode: TBackGroundMode read FBackGroundMode write
      SetBackGroundMode default bmNormal;
    property BackGroundX: Integer read FBackGroundX write SetBackGroundX default 0;
    property BackGroundY: Integer read FBackGroundY write SetBackGroundY default 0;
  end;

{ TCnWizardItems }

  TCnWizardItems = class(TOwnedCollection)
  private
    FWizardImage: TCnWizardImage;
    function GetItem(Index: Integer): TCnWizardItem;
    procedure SetItem(Index: Integer; const Value: TCnWizardItem);
    procedure Changed;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TCnWizardImage);
    property Items[Index: Integer]: TCnWizardItem read GetItem write SetItem; default;
    property WizardImage: TCnWizardImage read FWizardImage write FWizardImage;
  end;

{ TCnWizardImage }

  THeightRate = 0..100;
  TOnChanging = procedure(Sender: TObject; NewItemIndex: Integer; var AllowChange:
    Boolean) of object;
  TOnChange = procedure(Sender: TObject) of object;

  TCnWizardImage = class(TGraphicControl)
  private
    { Private declarations }
    FItemIndex: Integer;
    FTopMargin: Integer;
    FHorizontalSpace: Integer;
    FBoxWidth: Integer;
    FLeftMargin: Integer;
    FBoxHeight: Integer;
    FSelectedBoxColor: TColor;
    FBoxColor: TColor;
    FLineColor: TColor;
    FOnChange: TOnChange;
    FOnChanging: TOnChanging;
    FItems: TCnWizardItems;
    FBottomColor: TColor;
    FTopColor: TColor;
    FSelectedFont: TFont;
    FBackGroundMode: TBackGroundMode;
    FBackGround: TPicture;
    FBackGroundX: Integer;
    FBackGroundY: Integer;
    FCommentHeight: THeightRate;
    FCommentFont: TFont;
    FTreeHeight: THeightRate;
    FUpdateCount: Integer;
    procedure SetBoxColor(const Value: TColor);
    procedure SetBoxHeight(const Value: Integer);
    procedure SetBoxWidth(const Value: Integer);
    procedure SetHorizontalSpace(const Value: Integer);
    procedure SetItemIndex(const Value: Integer);
    procedure SetItems(const Value: TCnWizardItems);
    procedure SetLeftMargin(const Value: Integer);
    procedure SetLineColor(const Value: TColor);
    procedure SetSelectedBoxColor(const Value: TColor);
    procedure SetTopMargin(const Value: Integer);
    procedure SetBottomColor(const Value: TColor);
    procedure SetSelectedFont(const Value: TFont);
    procedure SetTopColor(const Value: TColor);
    function GetBackGround: TPicture;
    procedure SetBackGround(const Value: TPicture);
    procedure SetBackGroundMode(const Value: TBackGroundMode);
    function GetBackGroundTransparent: Boolean;
    procedure SetBackGroundTransparent(const Value: Boolean);
    procedure SetBackGroundX(const Value: Integer);
    procedure SetBackGroundY(const Value: Integer);
    procedure SetCommentFont(const Value: TFont);
    procedure SetCommentHeight(const Value: THeightRate);
    procedure SetTreeHeight(const Value: THeightRate);
    function BackEmpty: Boolean;
  private
    FMemBmp: TBitmap;
    FMemBmpValid: Boolean;
    procedure DrawBackGnd;
    procedure DrawMemBmp;
    procedure CheckMemBmp;
  private
    FRects: TList;
    procedure AllocateRects;
    procedure FillBox(ACanvas: TCanvas; Index: Integer; Live: Boolean);
  protected
    { Protected declarations }
    procedure Changed; overload;
    procedure Changed(Sender: TObject); overload;
    procedure Paint; override;
    procedure Click; override;
    property UpdateCount: Integer read FUpdateCount;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Font;
    property ShowHint;
    property Hint;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property Visible;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Height default 240;
    property Width default 120;
    property Items: TCnWizardItems read FItems write SetItems;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property LineColor: TColor read FLineColor write SetLineColor default clWhite;
    property SelectedBoxColor: TColor read FSelectedBoxColor write
      SetSelectedBoxColor default clLime;
    property BoxColor: TColor read FBoxColor write SetBoxColor default clGray;
    property HorizontalSpace: Integer read FHorizontalSpace write SetHorizontalSpace
      default 4;
    property TreeHeight: THeightRate read FTreeHeight write SetTreeHeight default 65;
    property BoxHeight: Integer read FBoxHeight write SetBoxHeight default 17;
    property BoxWidth: Integer read FBoxWidth write SetBoxWidth default 16;
    property LeftMargin: Integer read FLeftMargin write SetLeftMargin default 8;
    property TopMargin: Integer read FTopMargin write SetTopMargin default 12;
    property SelectedFont: TFont read FSelectedFont write SetSelectedFont;
    property TopColor: TColor read FTopColor write SetTopColor default clBlue;
    property BottomColor: TColor read FBottomColor write SetBottomColor default
      clBlack;
    property BackGround: TPicture read GetBackGround write SetBackGround;
    property BackGroundMode: TBackGroundMode read FBackGroundMode write
      SetBackGroundMode default bmNormal;
    property BackGroundX: Integer read FBackGroundX write SetBackGroundX default 0;
    property BackGroundY: Integer read FBackGroundY write SetBackGroundY default 0;
    property BackGroundTransparent: Boolean read GetBackGroundTransparent
      write SetBackGroundTransparent default False;
    property CommentFont: TFont read FCommentFont write SetCommentFont;
    property CommentHeight: THeightRate read FCommentHeight write SetCommentHeight default 25;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnChanging: TOnChanging read FOnChanging write FOnChanging;
    property OnChange: TOnChange read FOnChange write FOnChange;
  end;

implementation

{$R-}

uses
  Math;

procedure DrawTiled(Canvas: TCanvas; Rect: TRect; G: TGraphic);
var
  R, Rows, C, Cols: Integer;
begin
  if (G <> nil) and (not G.Empty) then
  begin
    Rows := ((Rect.Bottom - Rect.Top) div G.Height) + 1;
    Cols := ((Rect.Right - Rect.Left) div G.Width) + 1;
    for R := 1 to Rows do
      for C := 1 to Cols do
        Canvas.Draw(Rect.Left + (C - 1) * G.Width, Rect.Top + (R - 1) * G.Height,
          G);
  end;
end;

procedure DrawBackGround(Canvas: TCanvas; Rect: TRect;
  G: TGraphic; Mode: TBackGroundMode);
begin
  if (G <> nil) and (not G.Empty) then
  begin
    case Mode of
      bmTiled:
        DrawTiled(Canvas, Rect, G);
      bmStretched:
        Canvas.StretchDraw(Rect, G);
      bmCenter:
        Canvas.Draw((Rect.Right + Rect.Left - G.Width) div 2,
          (Rect.Bottom + Rect.Top - G.Height) div 2, G);
      bmNormal:
        Canvas.Draw(Rect.Left, Rect.Top, G);
    end;
  end;
end;

{ TCnWizardItem }

procedure TCnWizardItem.Assign(Source: TPersistent);
begin
  if Source is TCnWizardItem then
  begin
    FCaption := TCnWizardItem(Source).FCaption;
    FComment.Assign(TCnWizardItem(Source).FComment);
    FVisible := TCnWizardItem(Source).FVisible;
    BackGround := TCnWizardItem(Source).FBackGround;
    FBackGroundMode := TCnWizardItem(Source).FBackGroundMode;
    FBackGroundX := TCnWizardItem(Source).FBackGroundX;
    FBackGroundY := TCnWizardItem(Source).FBackGroundY;
  end
  else
    inherited;
end;

procedure TCnWizardItem.Changed;
begin
  if Assigned(FWizardItems) then
    FWizardItems.Changed;
end;

procedure TCnWizardItem.OnChange(Sender: TObject);
begin
  Changed;
end;

constructor TCnWizardItem.Create(Collection: TCollection);
begin
  inherited;
  FWizardItems := TCnWizardItems(Collection);
  if Assigned(FWizardItems) then
    FCaption := Format('Step%d', [FWizardItems.Count - 1]);
  FComment := TStringList.Create;
  TStringList(FComment).OnChange := OnChange;
  FVisible := True;
  FBackGroundMode := bmNormal;
  FBackGroundX := 0;
  FBackGroundY := 0;
end;

destructor TCnWizardItem.Destroy;
begin
  FBackGround.Free;
  FComment.Free;
  inherited;
end;

function TCnWizardItem.GetDisplayName: string;
begin
  Result := FCaption;
end;

procedure TCnWizardItem.SetCaption(const Value: TCaption);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TCnWizardItem.SetComment(const Value: TStrings);
begin
  FComment.Assign(Value);
  Changed;
end;

function TCnWizardItem.GetBackGround: TPicture;
begin
  if not Assigned(FBackGround) then
  begin
    FBackGround := TPicture.Create;
    FBackGround.OnChange := OnChange;
  end;
  Result := FBackGround;
end;

procedure TCnWizardItem.SetBackGround(const Value: TPicture);
begin
  if not Assigned(Value) or not Assigned(Value.Graphic) or Value.Graphic.Empty then
    FreeAndNil(FBackGround)
  else
    BackGround.Assign(Value);
  Changed;
end;

procedure TCnWizardItem.SetBackGroundMode(const Value: TBackGroundMode);
begin
  FBackGroundMode := Value;
  Changed;
end;

function TCnWizardItem.GetBackGroundTransparent: Boolean;
begin
  Result := not BackEmpty;
  if Result then
    Result := FBackGround.Graphic.Transparent;
end;

procedure TCnWizardItem.SetBackGroundTransparent(const Value: Boolean);
begin
  if not BackEmpty then
    FBackGround.Graphic.Transparent := Value;
end;

procedure TCnWizardItem.SetBackGroundX(const Value: Integer);
begin
  if FBackGroundX <> Value then
  begin
    FBackGroundX := Value;
    Changed;
  end;
end;

procedure TCnWizardItem.SetBackGroundY(const Value: Integer);
begin
  if FBackGroundY <> Value then
  begin
    FBackGroundY := Value;
    Changed;
  end;
end;

procedure TCnWizardItem.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

function TCnWizardItem.BackEmpty: Boolean;
begin
  Result := not Assigned(FBackGround) or not Assigned(FBackGround.Graphic)
    or FBackGround.Graphic.Empty;
end;

{ TCnWizardItems }

procedure TCnWizardItems.Changed;
begin
  if Assigned(FWizardImage) then
    FWizardImage.Changed;
end;

constructor TCnWizardItems.Create(AOwner: TCnWizardImage);
begin
  inherited Create(AOwner, TCnWizardItem);
  if Assigned(AOwner) and (csDesigning in AOwner.ComponentState) then
  begin
    BeginUpdate;
    try
      with TCnWizardItem(Add) do
      begin
        Caption := 'Start';
        Comment.Text := 'Welcome to wizard!';
      end;
      with TCnWizardItem(Add) do
        Comment.Text := Caption;
      with TCnWizardItem(Add) do
        Comment.Text := Caption;
      with TCnWizardItem(Add) do
      begin
        Caption := 'Finish';
        Comment.Text := 'Finished.';
      end;
    finally
      EndUpdate;
    end;
  end;
  FWizardImage := AOwner;
end;

function TCnWizardItems.GetItem(Index: Integer): TCnWizardItem;
begin
  Result := TCnWizardItem(inherited GetItem(Index));
end;

procedure TCnWizardItems.SetItem(Index: Integer; const Value: TCnWizardItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TCnWizardItems.Update(Item: TCollectionItem);
begin
  inherited;
  Changed;
end;

{ TCnWizardImage }

constructor TCnWizardImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  FItems := TCnWizardItems.Create(Self);
  FRects := TList.Create;
  FMemBmp := TBitmap.Create;
  FMemBmpValid := False;
  Font.Color := clWhite;
  FSelectedFont := TFont.Create;
  FSelectedFont.Assign(Font);
  FSelectedFont.Style := [fsBold];
  FCommentFont := TFont.Create;
  FCommentFont.Assign(Font);
  FBackGroundMode := bmNormal;
  FBackGroundX := 0;
  FBackGroundY := 0;
  FTopColor := clBlue;
  FBottomColor := clBlack;
  FItemIndex := 0;
  FBoxColor := clGray;
  FSelectedBoxColor := clLime;
  FLineColor := clWhite;
  FHorizontalSpace := 4;
  FTreeHeight := 65;
  FCommentHeight := 25;
  FBoxHeight := 17;
  FBoxWidth := 16;
  FTopMargin := 12;
  FLeftMargin := 8;
  Width := 120;
  Height := 240;

  Font.OnChange := Changed;
  FSelectedFont.OnChange := Changed;
  FCommentFont.OnChange := Changed;
end;

destructor TCnWizardImage.Destroy;
begin
  FSelectedFont.Free;
  FMemBmp.Free;
  FItems.Clear;
  AllocateRects;
  FItems.Free;
  FRects.Free;
  FCommentFont.Free;
  inherited;
end;

procedure TCnWizardImage.Changed;
begin
  FMemBmpValid := False;
  if FItemIndex > FItems.Count - 1 then
    FItemIndex := FItems.Count - 1;
  if ([csLoading, csDestroying, csReading, csUpdating, csWriting] *
    ComponentState = []) and (UpdateCount = 0) then
    Refresh;
end;

procedure TCnWizardImage.Changed(Sender: TObject);
begin
  Changed;
end;

procedure TCnWizardImage.SetItemIndex(const Value: Integer);
begin
  if (Value >= -1) and (Value < FItems.Count) then
    if (Value <> FItemIndex) then
    begin
      FItemIndex := Value;
      Changed;
    end;
end;

procedure TCnWizardImage.Click;
var
  Index: Integer;
  P: TPoint;
  AllowChange: Boolean;
begin
  inherited;
  GetCursorPos(P);
  P := ScreenToClient(P);
  for Index := 0 to FRects.Count - 1 do
  begin
    if PtInRect(TRect(FRects[Index]^), P) then
    begin
      if ItemIndex <> Index then
      begin
        AllowChange := True;
        if Assigned(FOnChanging) then
          FOnChanging(Self, Index, AllowChange);
        if AllowChange then
        begin
          ItemIndex := Index;
          if Assigned(FOnChange) then
            FOnChange(Self);
        end;
      end;
      Break;
    end;
  end;
end;

procedure TCnWizardImage.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  Changed;
end;

procedure TCnWizardImage.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TCnWizardImage.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed;
end;

procedure TCnWizardImage.AllocateRects;
var
  Index: Integer;
  P: Pointer;
begin
  if FRects.Count < FItems.Count then
    for Index := FRects.Count to FItems.Count - 1 do
    begin
      GetMem(P, SizeOf(TRect));
      FRects.Add(P);
    end
  else if FRects.Count > FItems.Count then
    for Index := FRects.Count - 1 downto FItems.Count do
    begin
      FreeMem(FRects[Index]);
      FRects.Delete(Index);
    end;
end;

procedure TCnWizardImage.FillBox(ACanvas: TCanvas; Index: Integer; Live: Boolean);
var
  BoxRect: TRect;
  TextRect: TRect;
  Text: string;
begin
  if (Index < 0) or (Index >= FItems.Count) then
    Exit;
  ACanvas.Brush.Style := bsSolid;
  if Live then
    ACanvas.Brush.Color := SelectedBoxColor
  else
    ACanvas.Brush.Color := BoxColor;
  BoxRect := TRect(FRects[Index]^);
  TextRect := BoxRect;
  BoxRect.Right := BoxRect.Left + BoxWidth;
  TextRect.Left := BoxRect.Right + HorizontalSpace;
  if (Index = 0) or (Index = FItems.Count - 1) then
    Inc(TextRect.Left, HorizontalSpace + BoxWidth div 2);
  ACanvas.FillRect(BoxRect);
  ACanvas.Brush.Style := bsClear;

  if Live then
    ACanvas.Font := FSelectedFont
  else
    ACanvas.Font := Font;
  Text := FItems[Index].Caption;
  ACanvas.TextOut(TextRect.Left, TextRect.Top + ((BoxHeight -
    ACanvas.TextHeight(Text)) div 2), Text);
  TRect(FRects[Index]^) := Rect(BoxRect.Left, BoxRect.Top, BoxRect.Left + BoxWidth +
    HorizontalSpace + ACanvas.TextWidth(Text), BoxRect.Top +
    BoxHeight);
  if (Index = 0) or (Index = FItems.Count - 1) then
    Inc(TRect(FRects[Index]^).Right, BoxWidth + HorizontalSpace);

  if Live and (FCommentHeight > 0) then
  begin
    Text := FItems[Index].Comment.Text;
    ACanvas.Font := FCommentFont;
    TextRect := Rect(LeftMargin, Height * (100 - FCommentHeight)
      div 100, Width - LeftMargin, Height);
    DrawText(ACanvas.Handle, PChar(Text), Length(Text), TextRect,
      DT_EXPANDTABS or DT_WORDBREAK or DT_LEFT);
  end;
end;

procedure TCnWizardImage.DrawBackGnd;
type
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..0] of TRGBTriple;
var
  pLine: PRGBArray;
  i, j: Integer;
  RGB: TRGBTriple;
  tr, tg, tb: Byte;
  br, bg, bb: Byte;
begin
  FMemBmp.Width := Width;
  FMemBmp.Height := Height;
  FMemBmp.PixelFormat := pf24Bit;

  if FTopColor = FBottomColor then
  begin
    FMemBmp.Canvas.Brush.Color := FTopColor;
    FMemBmp.Canvas.Brush.Style := bsSolid;
    FMemBmp.Canvas.FillRect(Rect(0, 0, Width, Height));
  end
  else
    for i := 0 to FMemBmp.Height - 1 do
    begin
      pLine := FMemBmp.ScanLine[i];
      tr := GetRValue(ColorToRGB(FTopColor));
      tg := GetGValue(ColorToRGB(FTopColor));
      tb := GetBValue(ColorToRGB(FTopColor));
      br := GetRValue(ColorToRGB(FBottomColor));
      bg := GetGValue(ColorToRGB(FBottomColor));
      bb := GetBValue(ColorToRGB(FBottomColor));
      RGB.rgbtRed := tr + (br - tr) * i div FMemBmp.Height;
      RGB.rgbtGreen := tg + (bg - tg) * i div FMemBmp.Height;
      RGB.rgbtBlue := tb + (bb - tb) * i div FMemBmp.Height;
      for j := 0 to FMemBmp.Width - 1 do
        pLine^[j] := RGB;
    end;

  if (FItems.Count > 0) and (FItemIndex >= 0) and not FItems[FItemIndex].BackEmpty then
  begin
    with FMemBmp do
      if FItems[FItemIndex].FBackGroundMode = bmNormal then
        Canvas.Draw(FItems[FItemIndex].BackGroundX, FItems[FItemIndex].BackGroundY,
          FItems[FItemIndex].FBackGround.Graphic)
      else
        DrawBackGround(Canvas, Rect(0, 0, Width, Height),
          FItems[FItemIndex].FBackGround.Graphic,
          FItems[FItemIndex].FBackGroundMode);
  end
  else if not BackEmpty then
  begin
    with FMemBmp do
      if FBackGroundMode = bmNormal then
        Canvas.Draw(BackGroundX, BackGroundY, FBackGround.Graphic)
      else
        DrawBackGround(Canvas, Rect(0, 0, Width, Height), FBackGround.Graphic,
          FBackGroundMode);
  end;
end;

procedure TCnWizardImage.DrawMemBmp;
var
  Index: Integer;
  X, Y: Integer;
  DrawHeight: Integer;
  RSpace: Double;
  VerticalSpace: Integer;
  VisibleCount: Integer;
  CurrCount: Integer;
begin
  AllocateRects;
  DrawBackGnd;
  if FItems.Count < 3 then
    Exit;

  VisibleCount := 0;
  for Index := 1 to FItems.Count - 2 do
  begin
    if FItems[Index].Visible then
      Inc(VisibleCount);
  end;
  if VisibleCount < 1 then
    Exit;

  DrawHeight := Height * FTreeHeight div 100 - TopMargin - BoxHeight;
  RSpace := (DrawHeight - BoxHeight * VisibleCount) / (VisibleCount + 1);
  VerticalSpace := Round(RSpace);
  CurrCount := 0;
  for Index := 0 to FItems.Count - 1 do
  begin
    if (Index = 0) or (Index = FItems.Count - 1) or FItems[Index].Visible then
    begin
      if Index = 0 then
      begin
        X := LeftMargin;
        Y := TopMargin;
      end
      else if Index = FItems.Count - 1 then
      begin
        X := LeftMargin;
        Y := TopMargin + DrawHeight;
      end
      else
      begin
        Inc(CurrCount);
        X := LeftMargin + BoxWidth + HorizontalSpace;
        Y := Round(TopMargin + BoxHeight / 2 + CurrCount * RSpace +
          (CurrCount - 1) * BoxHeight);
      end;
      TRect(FRects[Index]^) := Rect(X, Y, X + BoxWidth + HorizontalSpace +
        FMemBmp.Canvas.TextWidth(FItems[Index].FCaption), Y + BoxHeight);
      FillBox(FMemBmp.Canvas, Index, ItemIndex = Index);
      FMemBmp.Canvas.Pen.Color := LineColor;
      if Index = 0 then
      begin
        FMemBmp.Canvas.MoveTo(X + BoxWidth, Y + BoxHeight div 2);
        FMemBmp.Canvas.LineTo(X + HorizontalSpace + BoxWidth + BoxWidth div 2,
          Y + BoxHeight div 2);
        FMemBmp.Canvas.LineTo(X + HorizontalSpace + BoxWidth + BoxWidth div 2,
          Y + BoxHeight div 2 + VerticalSpace + 1);
      end
      else if Index = FItems.Count - 1 then
      begin
        FMemBmp.Canvas.MoveTo(X + BoxWidth, Y + BoxHeight div 2);
        FMemBmp.Canvas.LineTo(X + HorizontalSpace + BoxWidth + BoxWidth div 2,
          Y + BoxHeight div 2);
      end
      else if (Index < FItems.Count - 1) then
      begin
        FMemBmp.Canvas.MoveTo(X + BoxWidth div 2, Y + BoxHeight);
        FMemBmp.Canvas.LineTo(X + BoxWidth div 2, Min(TopMargin + DrawHeight
          + BoxHeight div 2, Y + BoxHeight + VerticalSpace) + 1);
      end;
    end
    else
    begin
      TRect(FRects[Index]^) := Rect(0, 0, 0, 0);
    end;
  end;
  FMemBmp.Canvas.Pen.Color := LineColor;
  X := LeftMargin + BoxWidth div 2;
  Y := TopMargin + BoxHeight;
  FMemBmp.Canvas.MoveTo(X, Y);
  FMemBmp.Canvas.LineTo(X, Y + DrawHeight - BoxHeight);
end;

procedure TCnWizardImage.CheckMemBmp;
begin
  if not FMemBmpValid then
  begin
    DrawMemBmp;
    FMemBmpValid := True;
  end;
end;

procedure TCnWizardImage.Paint;
begin
  inherited;
  CheckMemBmp;
  Bitblt(Canvas.Handle, 0, 0, Width, Height, FMemBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TCnWizardImage.SetItems(const Value: TCnWizardItems);
begin
  FItems.Assign(Value);
  Changed;
end;

procedure TCnWizardImage.SetBoxColor(const Value: TColor);
begin
  if FBoxColor <> Value then
  begin
    FBoxColor := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetBoxHeight(const Value: Integer);
begin
  if FBoxHeight <> Value then
  begin
    FBoxHeight := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetBoxWidth(const Value: Integer);
begin
  if FBoxWidth <> Value then
  begin
    FBoxWidth := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetHorizontalSpace(const Value: Integer);
begin
  if FHorizontalSpace <> Value then
  begin
    FHorizontalSpace := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetLeftMargin(const Value: Integer);
begin
  if Value <> FLeftMargin then
  begin
    FLeftMargin := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetLineColor(const Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetSelectedBoxColor(const Value: TColor);
begin
  if FSelectedBoxColor <> Value then
  begin
    FSelectedBoxColor := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetTopMargin(const Value: Integer);
begin
  if Value <> FTopMargin then
  begin
    FTopMargin := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetTreeHeight(const Value: THeightRate);
begin
  if FTreeHeight <> Value then
  begin
    FTreeHeight := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetCommentFont(const Value: TFont);
begin
  FCommentFont.Assign(Value);
  Changed;
end;

procedure TCnWizardImage.SetCommentHeight(const Value: THeightRate);
begin
  if FCommentHeight <> Value then
  begin
    FCommentHeight := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetTopColor(const Value: TColor);
begin
  if FTopColor <> Value then
  begin
    FTopColor := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetBottomColor(const Value: TColor);
begin
  if FBottomColor <> Value then
  begin
    FBottomColor := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetSelectedFont(const Value: TFont);
begin
  FSelectedFont.Assign(Value);
  Changed;
end;

function TCnWizardImage.GetBackGround: TPicture;
begin
  if not Assigned(FBackGround) then
  begin
    FBackGround := TPicture.Create;
    FBackGround.OnChange := Changed;
  end;
  Result := FBackGround;
end;

procedure TCnWizardImage.SetBackGround(const Value: TPicture);
begin
  if not Assigned(Value) or not Assigned(Value.Graphic) or Value.Graphic.Empty then
    FreeAndNil(FBackGround)
  else
    BackGround.Assign(Value);
  Changed;
end;

procedure TCnWizardImage.SetBackGroundMode(const Value: TBackGroundMode);
begin
  if FBackGroundMode <> Value then
  begin
    FBackGroundMode := Value;
    Changed;
  end;
end;

function TCnWizardImage.GetBackGroundTransparent: Boolean;
begin
  Result := not BackEmpty;
  if Result then
    Result := FBackGround.Graphic.Transparent;
end;

procedure TCnWizardImage.SetBackGroundTransparent(const Value: Boolean);
begin
  if not BackEmpty then
    FBackGround.Graphic.Transparent := Value;
end;

procedure TCnWizardImage.SetBackGroundX(const Value: Integer);
begin
  if FBackGroundX <> Value then
  begin
    FBackGroundX := Value;
    Changed;
  end;
end;

procedure TCnWizardImage.SetBackGroundY(const Value: Integer);
begin
  if FBackGroundY <> Value then
  begin
    FBackGroundY := Value;
    Changed;
  end;
end;

function TCnWizardImage.BackEmpty: Boolean;
begin
  Result := not Assigned(FBackGround) or not Assigned(FBackGround.Graphic)
    or FBackGround.Graphic.Empty;
end;

end.

