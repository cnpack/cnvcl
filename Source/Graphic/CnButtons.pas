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

unit CnButtons;
{* |<PRE>
================================================================================
* ������ƣ�����ؼ���
* ��Ԫ���ƣ�����ؼ���λͼ��ťʵ�ֵ�Ԫ
* ��Ԫ���ߣ�Bahamut
* ��    ע��
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2014.08.29
*               ���� WordWrap ����
*           2013.07.24
*               ����δ���� Action ������
*           2013.07.03
*               �޲� Glyph ��ĳЩBPL�����¿�߱�Ϊ0�Ӷ���������
*           2009.06.29 
*               �޲��˵������������ Caption Ϊ��ʱ�����ڻ��Ϊ Name �� BUG
*           2007.12.18 V0.2
*               ���� SpeedButton��
*           2007.12.10 V0.1
*               ʵ�ֵ�Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Buttons,
  ImgList, ActnList, CnConsts;

type
  TBtnColorStyle = (bcsCustom, bcsGold, bcsChrome, bcsBlue, bcsRed,
    bcsFlat1, bcsFlat2, bcsAqua);

  TModernBtnStyle = (bsNormal, bsThin, bsFlat, bsModern);
  {* ��ť���Ʒ������������ƽ������Ч��}

  TCnCustomButton = class(TCustomControl)
  private
    FAlignment: TAlignment;
    FShadowColor: TColor;
    FDownColor: TColor;
    FGlyph: TBitmap;
    FHotTrackColor: TColor;
    FKind: TBitBtnKind;
    FLayout: TButtonLayout;
    FLightColor: TColor;
    FModalResult: TModalResult;
    FNumGlyphs: Integer;
    FWordWrap: Boolean;
    FOnClick: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FBtnColorStyle: TBtnColorStyle;
    FModernBtnStyle: TModernBtnStyle;
    FDefault, FCancel: Boolean;
    FDown: Boolean;
    FCursorOnButton: Boolean;
    FDownBold: Boolean;
    FHotTrackBold: Boolean;
    FFlatBorder: Boolean;
    FSpacing: Integer;
    FMargin: Integer;
    FRoundCorner: Boolean;
    procedure SetCancel(const Value: Boolean);
    procedure SetDefault(const Value: Boolean);
    procedure SetFlatBorder(const Value: Boolean);
    procedure SetDownBold(const Value: Boolean);
    procedure SetDownColor(const Value: TColor);
    procedure SetHotTrackBold(const Value: Boolean);
    procedure SetHotTrackColor(const Value: TColor);
    procedure SetSpacing(const Value: Integer);
    procedure SetMargin(const Value: Integer);
    procedure SetRoundCorner(const Value: Boolean);
    procedure RenewBack;
    {* ˢ�µײ�λͼ *}
    procedure GlyphChanged(Sender: TObject);
    procedure SetWordWrap(const Value: Boolean);
    {* 2009-06-05 ��ӣ�����FGlyph��OnChange�¼�������ֱ�ӵ���Glyph�ķ����ؼ��޷��õ�֪ͨ��ʱˢ�� }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetGlyph(const Value: TBitmap);
    procedure SetKind(const Value: TBitBtnKind);
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetLightColor(const Value: TColor);
    procedure SetModalResult(const Value: TModalResult);
    procedure SetNumGlyphs(const Value: Integer);
    procedure SetModernBtnStyle(const Value: TModernBtnStyle);
    procedure SetShadowColor(const Value: TColor);
    procedure SetBtnColorStyle(const Value: TBtnColorStyle);
    procedure DoMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure DoMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure DoEnable(var Message: TMessage); message WM_ENABLE;
    procedure DoFocusChanged(var Msg: TMessage); message CM_FOCUSCHANGED;
    procedure DoKeyDown(var Msg: TMessage); message CN_KEYDOWN;
    procedure DoKeyUp(var Msg: TMessage); message CN_KEYUP;
    procedure DoDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure DoDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMWindowPosChanged(var Message: TMessage); message WM_WINDOWPOSCHANGED;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    property Glyph: TBitmap read FGlyph write SetGlyph;
    property Kind: TBitBtnKind read FKind write SetKind default bkCustom;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property NumGlyphs: Integer read FNumGlyphs write SetNumGlyphs default 0;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;

    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    {* ���ֺ�ͼƬ�����Ҷ��뷽ʽ}
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    {* ��Ӱ��ɫ����������ģʽ�µ����±�Ե���ƣ��Լ� bsModern ʱ�Ľ���Ŀ�갵ɫ}
    property Cancel: Boolean read FCancel write SetCancel default False;
    property BtnColorStyle: TBtnColorStyle read FBtnColorStyle write SetBtnColorStyle default bcsCustom;
    {* ��������һЩԤ�����Ч��}
    property DownColor: TColor read FDownColor write SetDownColor default clNone;
    {* ����ʱ�ı������ɫ}
    property Default: Boolean read FDefault write SetDefault default False;
    property DownBold: Boolean read FDownBold write SetDownBold;
    {* ����ʱ�����Ƿ������ʾ}
    property FlatBorder: Boolean read FFlatBorder write SetFlatBorder;
    {* bsFlat ʱ�Ƿ���Ʊ߿�}
    property HotTrackBold: Boolean read FHotTrackBold write SetHotTrackBold;
    {* �������ʱ�����Ƿ������ʾ}
    property HotTrackColor: TColor read FHotTrackColor write SetHotTrackColor default clNone;
    {* �������ʱ����ɫ}
    property LightColor: TColor read FLightColor write SetLightColor default clWhite;
    {* ������ɫ����������ģʽ�µ����ϱ�Ե���ƣ��Լ� bsModern ʱ�Ľ���Ŀ�����ɫ}
    property Margin: Integer read FMargin write SetMargin default 4;
    {* ͼƬ���ַǾ���ʱ�����Ե�ľ���}
    property ModalResult: TModalResult read FModalResult write SetModalResult default mrNone;
    property RoundCorner: Boolean read FRoundCorner write SetRoundCorner default True;
    {* bsModern ���ʱ�Ƿ���ʾ��ťԲ�ǣ�Ĭ����ʾ}
    property ModernBtnStyle: TModernBtnStyle read FModernBtnStyle
      write SetModernBtnStyle default bsNormal;
    {* ��ť���Ʒ��}
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    {* ͼ�������֮��ľ��룬������Ϊ��λ��Ĭ��Ϊ 4}
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    {* �����Ƿ��Զ�����}
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnButton = class(TCnCustomButton)
  published
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BtnColorStyle;
    property Cancel;
    property Caption;
    property Color;
    property Constraints;
    property Cursor;
    property DownColor;
    property Default;
    property DownBold;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatBorder;
    property Font;
    property Hint;
    property HotTrackBold;
    property HotTrackColor;
    property LightColor;
    property Margin;
    property ModalResult;
    property ModernBtnStyle;

    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RoundCorner;
    property ShadowColor;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap;
    property Name;

    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnBitBtn = class(TCnCustomButton)
  published
    property Action;
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BtnColorStyle;
    property Cancel;
    property Caption;
    property Color;
    property Constraints;
    property Cursor;
    property Default;
    property DownColor;
    property DownBold;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FlatBorder;
    property Font;
    property Glyph;
    property Hint;
    property HotTrackBold;
    property HotTrackColor;
    property Kind;
    property Layout;
    property LightColor;
    property Margin;
    property ModalResult;
    property ModernBtnStyle;
    property NumGlyphs;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RoundCorner;
    property ShowHint;
    property ShadowColor;
    property Spacing;
    property TabOrder;
    property TabStop;
    property Visible;
    property WordWrap;
    property Name;

    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnSpeedButton = class(TGraphicControl)
  private
    FGroupIndex: Integer;
    FGlyph: TBitmap;
    FNumGlyphs: Integer;
    FDown: Boolean;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FTransparent: Boolean;
    FMargin: Integer;
    FCursorOnButton: Boolean;
    FHotTrackBold: Boolean;
    FFlatBorder: Boolean;
    FDownBold: Boolean;
    FBtnColorStyle: TBtnColorStyle;
    FDownColor: TColor;
    FHotTrackColor: TColor;
    FShadowColor: TColor;
    FLightColor: TColor;
    FModernBtnStyle: TModernBtnStyle;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FAlignment: TAlignment;
    FRoundCorner: Boolean;
    procedure UpdateExclusive;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetTransparent(Value: Boolean);
    procedure SetMargin(Value: Integer);
    procedure UpdateTracking;
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetBtnColorStyle(const Value: TBtnColorStyle);
    procedure SetFlatBorder(const Value: Boolean);
    procedure SetLightColor(const Value: TColor);
    procedure SetModernBtnStyle(const Value: TModernBtnStyle);
    procedure SetShadowColor(const Value: TColor);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetDownBold(const Value: Boolean);
    procedure SetDownColor(const Value: TColor);
    procedure SetHotTrackBold(const Value: Boolean);
    procedure SetHotTrackColor(const Value: TColor);
    procedure SetRoundCorner(const Value: Boolean);
    procedure GlyphChanged(Sender: TObject);
    {* 2009-06-05 ��ӣ����� FGlyph �� OnChange �¼�������ֱ�ӵ��� Glyph �ķ����ؼ��޷��õ�֪ͨ��ʱˢ�� }
    function GetFlat: Boolean;
    procedure SetFlat(const Value: Boolean);
  protected
    FState: TButtonState;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetPalette: HPALETTE; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure Paint; override;
    property CursorOnButton: Boolean read FCursorOnButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Action;
    property Align;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    {* ���ֺ�ͼƬ�����Ҷ��뷽ʽ}
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Anchors;
    property BiDiMode;

    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property BtnColorStyle: TBtnColorStyle read FBtnColorStyle write SetBtnColorStyle default bcsCustom;
    property Color;
    property DownColor: TColor read FDownColor write SetDownColor default clNone;
    property DownBold: Boolean read FDownBold write SetDownBold;
    property Flat: Boolean read GetFlat write SetFlat stored False;
    {* ���� SpeedButton ���ṩ�� Flat ���ԣ�ʵ�����ǲ��� FModernBtnStyle Ϊ bsFlat}
    property FlatBorder: Boolean read FFlatBorder write SetFlatBorder;
    property HotTrackBold: Boolean read FHotTrackBold write SetHotTrackBold;
    property HotTrackColor: TColor read FHotTrackColor write SetHotTrackColor default clNone;
    property LightColor: TColor read FLightColor write SetLightColor default clWhite;
    property ModernBtnStyle: TModernBtnStyle read FModernBtnStyle
      write SetModernBtnStyle default bsNormal;

    property Constraints;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Caption;
    property Enabled;
    property Font;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property RoundCorner: Boolean read FRoundCorner write SetRoundCorner default True;
    {* Modern ���ʱ�Ƿ���ʾ��ťԲ�ǣ�Ĭ����ʾ}
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    {* �Ƿ�͸����ʾ��������ֻ�� bsFlat ʱ��Ч}
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

implementation

{$R CNBUTTONS.RES}

var
  FImageList: TImageList = nil;
  {* �������ƻҶ�ͼ��� ImageList}

procedure CopyImage(Glyph: TBitmap; ImageList: TCustomImageList; Index: Integer);
begin
  if Glyph <> nil then
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia;
      Canvas.FillRect(Rect(0, 0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
    end;
end;

procedure GetPreDefinedColors(BtnColorStyle: TBtnColorStyle; var Color, LightColor,
  ShadowColor, DownColor, HotTrackColor: TColor; var ModernStyle: TModernBtnStyle;
  var FlatBorder: Boolean);
begin
  case BtnColorStyle of
    bcsGold:
      begin
        Color := $0000C0C0;
        LightColor := clYellow;
        ShadowColor := clOlive;
        DownColor := clNone;
        HotTrackColor := $0000DFDF;
        ModernStyle := bsModern;
        FlatBorder := False;
      end;

    bcsChrome:
      begin
        Color := clSilver;
        LightColor := clWhite;
        ShadowColor := clGray;
        DownColor := clNone;
        HotTrackColor := clNone;
        ModernStyle := bsModern;
        FlatBorder := False;
      end;

    bcsBlue:
      begin
        Color := $00FF8000;
        LightColor := clAqua;
        ShadowColor := clBlue;
        DownColor := clNone;
        HotTrackColor := clNone;
        ModernStyle := bsModern;
        FlatBorder := False;
      end;

    bcsRed:
      begin
        Color := clRed;
        LightColor := $00C0C0FF;
        ShadowColor := $000000C0;
        DownColor := clNone;
        HotTrackColor := clNone;
        ModernStyle := bsModern;
        FlatBorder := False;
      end;

    bcsAqua:
      begin
        Color := $00ECCE94;
        LightColor := $00FCE6D4;
        ShadowColor := clBlack;
        DownColor := clNone;
        HotTrackColor := clNone;
        ModernStyle := bsModern;
        FlatBorder := False;
      end;

    bcsFlat1:
      begin
        Color := clBtnFace;
        LightColor := $00B59284;
        ShadowColor := $00B59284;
        DownColor := $00B59284;
        HotTrackColor := $00DED3D6;
        ModernStyle := bsFlat;
        FlatBorder := True;
      end;

    bcsFlat2:
      begin
        Color := clBtnFace;
        LightColor := clBlack;
        ShadowColor := clBlack;
        DownColor := $0024DABC;
        HotTrackColor := $008CF6E4;
        ModernStyle := bsFlat;
        FlatBorder := False;
      end;
  end;
end;

procedure PaintButton(Canvas: TCanvas; IsSpeedButton: Boolean;
  Width, Height, NumGlyphs, Spacing, Margin: Integer;
  Glyph: TBitmap; Down, DownBold, HotTrackBold, CursorOnButton, Transparent, Enabled,
  PopupArrow, Focused, Default, FlatBorder, RoundCorner: Boolean; ModernBtnStyle: TModernBtnStyle;
  Color, DownColor, HotTrackColor, LightColor, ShadowColor: TColor;
  Font: TFont; Layout: TButtonLayout; Caption: string; Alignment: TAlignment; Wrap: Boolean);
const
  clDeepShadow = $00404040;
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK or DT_EDITCONTROL);
var
  CaptionHeight, CaptionWidth, GlyphHeight, GlyphWidth: Integer;
  GlyphIndex: Integer;
  Offset: Integer;
  clBackColor: TColor;
  CapX, CapY, GlX, GlY: Integer;
  aRect, oRect, wRect: TRect;
  FArrowGlyph: TPicture;
  UseDisabledBitmap: Boolean;
  MonoBmp: TBitmap;
  OldBrushStyle: TBrushStyle;
  OldPenColor: TColor;
  DrawStyle: LongInt;

  procedure DrawColorFade(StartColor, StopColor: TColor; Left, Top, Right, Bottom: Integer);
  var
    Counter, Buffer, FillStep: Integer;
    bR1, bG1, bB1, bR2, bG2, bB2: byte;
    aColor1, aColor2: LongInt;
    dCurrentR, dCurrentG, dCurrentB, dRStep, dGStep, dBStep: double;
    aOldStyle: TPenStyle;
    Height, DrawBottom: Integer;
  begin
    Height := (Bottom - Top);
    aOldStyle := Canvas.Pen.Style;
    Canvas.Pen.Style := psClear;

    aColor1 := ColorToRGB(StartColor);
    bR1 := GetRValue(aColor1);
    bG1 := GetGValue(aColor1);
    bB1 := GetBValue(aColor1);

    aColor2 := ColorToRGB(StopColor);
    bR2 := GetRValue(aColor2);
    bG2 := GetGValue(aColor2);
    bB2 := GetBValue(aColor2);

    dCurrentR := bR1;
    dCurrentG := bG1;
    dCurrentB := bB1;

    dRStep := (bR2 - bR1) / 31;
    dGStep := (bG2 - bG1) / 31;
    dBStep := (bB2 - bB1) / 31;

    FillStep := (Height div 31) + 1;
    for Counter := 0 to 31 do
    begin
      Buffer := Counter * Height div 31;
      Canvas.Brush.Color := RGB(Trunc(dCurrentR), Trunc(dCurrentG), Trunc(dCurrentB));
      dCurrentR := dCurrentR + dRStep;
      dCurrentG := dCurrentG + dGStep;
      dCurrentB := dCurrentB + dBStep;
      DrawBottom := Top + Buffer + FillStep;
      if DrawBottom > Bottom then
        DrawBottom := Bottom;
      Canvas.FillRect(Rect(Left, Top + Buffer, Right, DrawBottom));
    end;
    Canvas.Pen.Style := aOldStyle;
  end;

  procedure DrawGlyph(AGlyph: TBitmap; DestLeft, DestTop, SrcLeft, SrcTop,
    Width, Height: Integer);  // transparent draw
  var
    APicture: TPicture;
  begin
    if AGlyph = nil then
      Exit;

    APicture := TPicture.Create;
    try
      APicture.Bitmap.Assign(AGlyph);
      APicture.Bitmap.Width := Width;
      APicture.Bitmap.Height := Height;
      APicture.Bitmap.Canvas.Draw(-SrcLeft, -SrcTop, AGlyph);
      APicture.Graphic.Transparent := True;
      Canvas.Draw(DestLeft, DestTop, APicture.Graphic);
    finally
      FreeAndNil(APicture);
    end;
  end;
begin
  if not Enabled then
    Down := False;

  Offset := 0;
  if Down {and (ModernBtnStyle in [bsNormal, bsThin, bsModern])} then
    Offset := 1;

  clBackColor := ColorToRGB(Color);
  if CursorOnButton and (HotTrackColor <> clNone) then
    clBackColor := HotTrackColor;

  if Down and (DownColor <> clNone) then
    clBackColor := DownColor;

  // ��͸��ʱ��䱳��
  Canvas.Brush.Color := clBackColor;
  if not Transparent or (ModernBtnStyle <> bsFlat) then
  begin
    if (ModernBtnStyle = bsModern) and RoundCorner then
    begin
      // Բ��ʱ�������С��������Բ���ⱻ����
      Canvas.FillRect(Rect(2, 2, Width - 2, Height - 2));
    end
    else
      Canvas.FillRect(Rect(0, 0, Width, Height));
  end;

  if FlatBorder and (ModernBtnStyle = bsFlat) then
  begin
    // ��ƽ����Ե����������ڲ�
    OldBrushStyle := Canvas.Brush.Style;
    Canvas.Brush.Style := bsClear;
    OldPenColor := Canvas.Pen.Color;
    Canvas.Pen.Color := ShadowColor;
    Canvas.Rectangle(0, 0, Width, Height);
    Canvas.Brush.Style := OldBrushStyle;
    Canvas.Pen.Color := OldPenColor;
  end;

  if ModernBtnStyle = bsModern then
  begin
    // bsModer ���ֱ�ӻ�����
    DrawColorFade(LightColor, clBackColor, 2, 2, Width - 2, Height div 4);
    DrawColorFade(clBackColor, LightColor, 2, Height div 4, Width - 2, Height - 2);
  end;

  Canvas.Brush.Style := bsClear;

  if ModernBtnStyle <> bsModern then
  begin
    if (ModernBtnStyle = bsThin) or (ModernBtnStyle = bsFlat) and
      (CursorOnButton or Down) then  // Thin����������Ҳ����ƽ�Ļ���Ե���˴�������������
    begin
      // ��һ�λ��Ƚϱ���͹������ǳɫ�����ϣ�������ɫ�����£�����ʱ�෴
      if Down then
        Canvas.Pen.Color := ShadowColor
      else
        Canvas.Pen.Color := LightColor;

      Canvas.MoveTo(0, Height - 1);
      Canvas.LineTo(0, 0);
      Canvas.LineTo(Width, 0);

      if Down then
        Canvas.Pen.Color := LightColor
      else
        Canvas.Pen.Color := ShadowColor;

      Canvas.MoveTo(Width - 1, 1);
      Canvas.LineTo(Width - 1, Height - 1);
      Canvas.LineTo(0, Height - 1);
    end
    else if ModernBtnStyle = bsNormal then
    begin
      // ��һ�λ��ȽϺ�ģ�Normal����͹������ǳɫ�����ϣ�������ɫ�����£��ٺ�ɫ��������
      if Down then // ���µıȽϼ�
      begin
        if IsSpeedButton then // SpeedButton �İ���Ч����һ��
        begin
          Canvas.Pen.Color := clDeepShadow;
          Canvas.MoveTo(0, Height - 1);
          Canvas.LineTo(0, 0);
          Canvas.LineTo(Width - 1, 0);

          Canvas.Pen.Color := ShadowColor;
          Canvas.MoveTo(Width - 3, 1);
          Canvas.LineTo(1, 1);
          Canvas.LineTo(1, Height - 2);

          Canvas.Pen.Color := LightColor;
          Canvas.MoveTo(Width - 1, 0);
          Canvas.LineTo(Width - 1, Height - 1);
          Canvas.LineTo(-1, Height - 1);
        end
        else
        begin
          Canvas.Pen.Color := clDeepShadow;
          Canvas.Rectangle(0, 0, Width, Height);
          Canvas.Pen.Color := ShadowColor;
          Canvas.Rectangle(1, 1, Width - 1, Height - 1);
        end;
      end
      else
      begin
        if Focused or Default then // ���ؿ��
        begin
          Canvas.Pen.Color := clBlack;
          Canvas.Rectangle(0, 0, Width, Height);

          Canvas.Pen.Color := LightColor;
          Canvas.MoveTo(1, Height - 2);
          Canvas.LineTo(1, 1);
          Canvas.LineTo(Width - 2, 1);

          Canvas.Pen.Color := ShadowColor;
          Canvas.MoveTo(Width - 3, 2);
          Canvas.LineTo(Width - 3, Height - 3);
          Canvas.LineTo(1, Height - 3);

          // ȡ ShadowColor �� clBlack ���м�ɫ
          Canvas.Pen.Color := ShadowColor div 2 + clDeepShadow div 2;
          Canvas.MoveTo(Width - 2, 1);
          Canvas.LineTo(Width - 2, Height - 2);
          Canvas.LineTo(0, Height - 2);
        end
        else // ����δ Focused �� Button���� Thin ������ºڿ�
        begin
          Canvas.Pen.Color := LightColor;
          Canvas.MoveTo(0, Height - 1);
          Canvas.LineTo(0, 0);
          Canvas.LineTo(Width - 1, 0);

          Canvas.Pen.Color := ShadowColor;
          Canvas.MoveTo(Width - 2, 1);
          Canvas.LineTo(Width - 2, Height - 2);
          Canvas.LineTo(1, Height - 2);

          Canvas.Pen.Color := clDeepShadow;
          Canvas.MoveTo(Width - 1, 1);
          Canvas.LineTo(Width - 1, Height - 1);
          Canvas.LineTo(0, Height - 1);
        end;
      end;
    end;
  end
  else // ModernBtnStyle = bsModern
  begin
    Canvas.Pen.Color := clBackColor;
    if Down then
      Canvas.Pen.Color := ShadowColor;
    Canvas.Rectangle(1, 1, Width - 1, Height - 1);
    Canvas.Pen.Color := ShadowColor;
    if RoundCorner then
      Canvas.RoundRect(0, 0, Width, Height, 6, 6)
    else
      Canvas.RoundRect(0, 0, Width, Height, 0, 0);
  end;

  Canvas.Font := Font;
  if (Down and DownBold) or (CursorOnButton and HotTrackBold) then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];

  if Glyph.Empty then
    Spacing := 0;

  GlyphHeight := Glyph.Height;
  if NumGlyphs <> 0 then
    GlyphWidth := Glyph.Width div NumGlyphs
  else
    GlyphWidth := 0;

  if Wrap then
  begin
    // ����ͼƬ���Ű淽ʽԤ�ȼ���һ���ɻ�������Ĵ�С���� Wrap ��ʽ�����Ű�
    wRect := Rect(0, 0, Width - Margin * 2, Height - Margin * 2);
    if Layout in [blGlyphLeft, blGlyphRight] then
      wRect.Right := wRect.Right - GlyphWidth - Spacing;
    if Layout in [blGlyphTop, blGlyphBottom] then
      wRect.Bottom := wRect.Bottom - GlyphHeight - Spacing;

    DrawStyle := DT_CENTER or DT_VCENTER or WordWraps[Wrap] or DT_CALCRECT;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), wRect, DrawStyle);
    CaptionHeight := wRect.Bottom - wRect.Top;
    CaptionWidth := wRect.Right - wRect.Left;
  end
  else
  begin
    CaptionHeight := Canvas.TextHeight(Caption);
    CaptionWidth := Canvas.TextWidth(Caption);
  end;

  GlyphIndex := 0;

  MonoBmp := nil;
  UseDisabledBitmap := False;
  if not Enabled then
  begin
    if NumGlyphs >= 2 then
      GlyphIndex := GlyphWidth
    else
      UseDisabledBitmap := True;
  end
  else
  begin
    if CursorOnButton and (NumGlyphs > 3) then
      GlyphIndex := 3 * GlyphWidth;
    if Down and (NumGlyphs > 2) then
      GlyphIndex := 2 * GlyphWidth;
  end;

  CapX := 0;
  CapY := 0;
  GlX := 0;
  GlY := 0;

  case Layout of
  blGlyphLeft:
    begin
      CapY := (Height - CaptionHeight) div 2;
      GlY := (Height - GlyphHeight) div 2;
      case Alignment of
        taLeftJustify:
          begin
            CapX := Margin + GlyphWidth + Spacing;
            GlX := Margin;
          end;
        taRightJustify:
          begin
            CapX := Width - CaptionWidth - Margin;
            GlX := Width - CaptionWidth - Margin - GlyphWidth - Spacing;
          end;
        taCenter:
          begin
            CapX := (Width - CaptionWidth - GlyphWidth - Spacing) div 2 + GlyphWidth + Spacing;
            GlX := (Width - CaptionWidth - GlyphWidth - Spacing) div 2;
          end;
      end;
    end;
  blGlyphRight:
    begin
      CapY := (Height - CaptionHeight) div 2;
      GlY := (Height - GlyphHeight) div 2;
      case Alignment of
        taLeftJustify:
          begin
            CapX := Margin;
            GlX := Spacing + Margin + CaptionWidth;
          end;
        taRightJustify:
          begin
            CapX := Width - Spacing - CaptionWidth - GlyphWidth - Margin;
            GlX := Width - GlyphWidth - Margin;
          end;
        taCenter:
          begin
            CapX := (Width - CaptionWidth - GlyphWidth - Spacing) div 2;
            GlX := (Width - CaptionWidth - GlyphWidth - Spacing) div 2 + CaptionWidth + Spacing;
          end;
      end;
    end;
  blGlyphTop:
    begin
      CapY := (Height - CaptionHeight - GlyphHeight - Spacing) div 2 + GlyphHeight + Spacing;
      GlY := (Height - CaptionHeight - GlyphHeight - Spacing) div 2;
      case Alignment of
        taLeftJustify:
          begin
            CapX := Margin;
            GlX := Margin;
          end;
        taRightJustify:
          begin
            CapX := Width - CaptionWidth - Margin;
            GlX := Width - GlyphWidth - Margin;
          end;
        taCenter:
          begin
            CapX := (Width - CaptionWidth) div 2;
            GlX := (Width - GlyphWidth) div 2;
          end;
      end;
    end;
  blGlyphBottom:
    begin
      CapY := (Height - CaptionHeight - GlyphHeight - Spacing) div 2;
      GlY := (Height - CaptionHeight - GlyphHeight - Spacing) div 2 + CaptionHeight + Spacing;
      case Alignment of
        taLeftJustify:
          begin
            CapX := Margin;
            GlX := Margin;
          end;
        taRightJustify:
          begin
            CapX := Width - CaptionWidth - Margin;
            GlX := Width - GlyphWidth - Margin;
          end;
        taCenter:
          begin
            CapX := (Width - CaptionWidth) div 2;
            GlX := (Width - GlyphWidth) div 2;
          end;
      end;
    end;
  end;

  if Offset > 0 then
  begin
    Inc(CapX, Offset);
    Inc(CapY, Offset);
    Inc(GlX, Offset);
    Inc(GlY, Offset);
  end;

  aRect := Rect(CapX, CapY, CapX + CaptionWidth, CapY + CaptionHeight);
  DrawStyle := DT_CENTER or DT_VCENTER or WordWraps[Wrap];

  // calc rect, and if multi-line, re-adjust rect.
  oRect := aRect;
  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), aRect, DrawStyle or DT_CALCRECT);
  OffsetRect(aRect, 0, (oRect.Bottom - aRect.Bottom) div 2);
  OffsetRect(aRect, (oRect.Right - aRect.Right) div 2, 0);

  if not Enabled then
  begin
    OffsetRect(aRect, 1, 1);
    Canvas.Font.Color := clWhite;
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), aRect, DrawStyle);
    Canvas.Font.Color := clGray;
    OffsetRect(aRect, -1, -1);
  end;

  DrawText(Canvas.Handle, PChar(Caption), Length(Caption), aRect, DrawStyle);
  if not UseDisabledBitmap then
    DrawGlyph(Glyph, GlX, GlY, GlyphIndex, 0, GlyphWidth, GlyphHeight)
  else if Glyph.Handle <> 0 then
  begin
    // DONE: �� ImageList ������ GlyphIndex 0 ���������ɵ� Disable ͼƬ
    if FImageList = nil then
      FImageList := TImageList.Create(nil)
    else
      FImageList.Clear;

    FImageList.Height := Glyph.Height;
    FImageList.Width := Glyph.Width;

    // In some BPL case, Height and Width will be 0 and cause error, so check here.
    if (FImageList.Height > 0) and (FImageList.Width > 0) then
    begin
      // TODO: ���ٽ�����֤���Ʋ���ͻ
      FImageList.Add(Glyph, Glyph);
      FImageList.Draw(Canvas, GlX, GlY, 0, False);
    end;
    FImageList.Clear;
  end;

  if PopupArrow then
  begin
    FArrowGlyph := TPicture.Create;
    try
      FArrowGlyph.Bitmap.LoadFromResourceName(hInstance, 'CNBTNARROW');
      FArrowGlyph.Graphic.Transparent := True;
      Canvas.Draw(Width - 11, Height div 2 - 1, FArrowGlyph.Graphic);
    finally
      FreeAndNil(FArrowGlyph);
    end;
  end;

  MonoBmp.Free;
end;

{ TCnCustomButton }

procedure TCnCustomButton.Click;
var
  Form: TCustomForm;
begin
  if Visible and Enabled then
  begin
    if FModalResult <> mrNone then
    begin
      Form := GetParentForm(Self);
      if Form <> nil then
        Form.ModalResult := FModalResult;
    end;

    if Assigned(PopupMenu) then
      PopupMenu.PopUp(ClientToScreen(Point(0, Height)).X, ClientToScreen(Point(0, Height)).Y);

    if Assigned(FOnClick) and (Action <> nil) and (@FOnClick <> @Action.OnExecute) then
      FOnClick(Self)
    else if not (csDesigning in ComponentState) and (ActionLink <> nil) then
    begin
{$IFDEF COMPILER5}
      ActionLink.Execute;
{$ELSE}
      ActionLink.Execute(Self);
{$ENDIF}
    end
    else if Assigned(FOnClick) then
      FOnClick(Self);
  end;
end;

procedure TCnCustomButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCnCustomButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

constructor TCnCustomButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Height := 25;
  Width := 75;

  ControlStyle := [csSetCaption, csCaptureMouse];

  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChanged;
  { 2009-06-05 ��ӣ�����FGlyph��onchange�¼�������ֱ�ӵ���Glyph�ķ����ؼ��޷��õ�֪ͨ��ʱˢ�� }
  FSpacing := 4;
  FMargin := 4;
  FLightColor := clWhite;
  FShadowColor := clGray;
  FDownColor := clNone;
  FModernBtnStyle := bsNormal;
  FKind := bkCustom;
  TabStop := True;
  FBtnColorStyle := bcsCustom;
  FHotTrackColor := clNone;
  FAlignment := taCenter;
  FNumGlyphs := 1;
  FDefault := False;
  FCancel := False;
  FRoundCorner := True;
  Color := clBtnFace;
end;

procedure TCnCustomButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  //Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
    (* 2008-07-22 ע�͵���ԭ���ǰ���WS_EX_TRANSPARENT���Ĵ����޷���ӦWM_WINDOWPOSCHANGED��Ϣ *)
end;

destructor TCnCustomButton.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited;
end;

procedure TCnCustomButton.DoDialogChar(var Message: TCMDialogChar);
begin
  if IsAccel(Message.CharCode, Caption) and
   (Parent <> nil) {and Parent.Showing} and CanFocus then
  begin
    FDown := False;
    Invalidate;
    Click;
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TCnCustomButton.DoDialogKey(var Message: TCMDialogKey);
begin
  FDown := False;
  Invalidate;

  if ((Message.CharCode = VK_RETURN) and FDefault) or
    ((Message.CharCode = VK_ESCAPE) and FCancel) and
    (KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
  begin
    FDown := False;
    Invalidate;
    Click;
    Message.Result := 1;
  end
  else
    inherited;
end;

procedure TCnCustomButton.DoEnable(var Message: TMessage);
begin
  SetEnabled(Message.WParam <> 0);
end;

procedure TCnCustomButton.DoFocusChanged(var Msg: TMessage);
begin
  if (GetFocus() <> Self.Handle) and FDown then
    PostMessage(Handle, WM_KEYUP, VK_RETURN, 0)
  else
    Invalidate;

  inherited;
end;

procedure TCnCustomButton.DoKeyDown(var Msg: TMessage);
begin
  if not Enabled then
    Exit;

  if Msg.WParam in [VK_SPACE, VK_RETURN] then
  begin
    FDown := True;
    Invalidate;
  end
  else // if Msg.WParam in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
    inherited; // ����ȫ�� inherited ������������� ShortCut����л KADU
end;

procedure TCnCustomButton.DoKeyUp(var Msg: TMessage);
var
  IsClick: Boolean;
begin
  IsClick := FDown;
  FDown := False;
  Invalidate;

  if Enabled then
    if (Msg.WParam in [VK_SPACE, VK_RETURN]) and IsClick then
      Click
    else if Msg.WParam in [VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN] then
      inherited;
end;

procedure TCnCustomButton.DoMouseEnter(var Msg: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);

  FCursorOnButton := True;
  Invalidate;
end;

procedure TCnCustomButton.DoMouseLeave(var Msg: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);

  FCursorOnButton := False;
  Invalidate;
end;

procedure TCnCustomButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  if Enabled then
  begin
    FDown := True;
    SetFocus;
    Invalidate;
  end;
end;

procedure TCnCustomButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  IsClick: Boolean;
begin
  inherited;
  IsClick := FDown;

  FDown := False;
  Invalidate;

  if IsClick and FCursorOnButton then
    Click;
end;

procedure TCnCustomButton.Paint;
var
  Bmp: TBitmap;
begin
  if (csLoading in ComponentState) or (Parent = nil) then
    Exit;

  // 2009-06-29����ж��ж��Ƿ�������ڣ�������VisibleΪFalseʱ��������޷�ˢ�¿ؼ�
  if not Visible and not (csDesigning in ComponentState) then
    Exit;

  if FModernBtnStyle = bsModern then // �ִ�ģʽֱ�ӻ����Ա���Բ�ǵ�ɫ���⣬��������˸
  begin
    if FRoundCorner then // Բ��ʱ����͸������
      RenewBack;

    PaintButton(Canvas, False, Width, Height, FNumGlyphs, FSpacing, FMargin, FGlyph, FDown, FDownBold,
              FHotTrackBold, FCursorOnButton, False, Enabled,
              Assigned(PopupMenu), Focused, FDefault, FFlatBorder, FRoundCorner, FModernBtnStyle, Color, FDownColor,
              FHotTrackColor, FLightColor, FShadowColor, Font, FLayout,
              Caption, FAlignment, FWordWrap);
  end
  else // ����ģʽ����һ���Ի��ƣ�������˸
  begin
    Bmp := TBitmap.Create;
    Bmp.Width := Width;
    Bmp.Height := Height;

    PaintButton(Bmp.Canvas, False, Width, Height, FNumGlyphs, FSpacing, FMargin, FGlyph, FDown, FDownBold,
              FHotTrackBold, FCursorOnButton{ or Focused}, False, Enabled,
              Assigned(PopupMenu), Focused, FDefault, FFlatBorder, FRoundCorner, FModernBtnStyle, Color, FDownColor,
              FHotTrackColor, FLightColor, FShadowColor, Font, FLayout,
              Caption, FAlignment, FWordWrap);

    Canvas.Draw(0, 0, Bmp);
    Bmp.Free;
  end;

  if Focused and Enabled then
  begin
    if FModernBtnStyle = bsNormal then
      Canvas.DrawFocusRect(Rect(4, 4, Width - 4, Height - 4))
    else
      Canvas.DrawFocusRect(Rect(2, 2, Width - 2, Height - 2));
  end;
end;

procedure TCnCustomButton.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  Invalidate;
end;

procedure TCnCustomButton.SetBtnColorStyle(const Value: TBtnColorStyle);
var
  AColor: TColor;
begin
  FBtnColorStyle := Value;
  if Value = bcsCustom then
    Exit;

  GetPreDefinedColors(Value, AColor, FLightColor, FShadowColor, FDownColor,
    FHotTrackColor, FModernBtnStyle, FFlatBorder);
  Color := AColor;

  Invalidate;
end;

procedure TCnCustomButton.SetGlyph(const Value: TBitmap);
begin
  if Value <> nil then
  begin
    FGlyph.Assign(Value);
    if Value.Height <> 0 then
      FNumGlyphs := Value.Width div Value.Height
    else
      FNumGlyphs := 0;
  end
  else
  begin
    FGlyph.Height := 0;
    FNumGlyphs := 0;
  end;

  FKind := bkCustom;
  Invalidate;
end;

procedure TCnCustomButton.SetKind(const Value: TBitBtnKind);
begin
  if Value <> bkCustom then
    FNumGlyphs := 2;

  case Value of
    bkOK:
      begin
        ModalResult := mrOK;
        FGlyph.LoadFromResourceName(hInstance, 'CNBTNOK');
        Caption := '&OK';
      end;

    bkCancel:
      begin
        ModalResult := mrCancel;
        FGlyph.LoadFromResourceName(hInstance, 'CNBTNCANCEL');
        Caption := '&Cancel';
      end;

    bkHelp:
      begin
        ModalResult := mrNone;
        FGlyph.LoadFromResourceName(hInstance, 'CNBTNHELP');
        Caption := '&Help';
      end;

    bkYes:
      begin
        ModalResult := mrYes;
        FGlyph.LoadFromResourceName(hInstance, 'CNBTNYES');
        Caption := '&Yes';
      end;

    bkNo:
      begin
        ModalResult := mrNo;
        FGlyph.LoadFromResourceName(hInstance, 'CNBTNNO');
        Caption := '&No';
      end;

    bkClose:
      begin
        ModalResult := mrNone;
        FGlyph.LoadFromResourceName(hInstance, 'CNBTNCLOSE');
        Caption := '&Close';
      end;

    bkAbort:
      begin
        ModalResult := mrAbort;
        FGlyph.LoadFromResourceName(hInstance, 'CNBTNABORT');
        Caption := '&Abort';
      end;

    bkRetry:
      begin
        ModalResult := mrRetry;
        FGlyph.LoadFromResourceName(hInstance, 'CNBTNRETRY');
        Caption := '&Retry';
      end;

    bkIgnore:
      begin
        ModalResult := mrIgnore;
        FGlyph.LoadFromResourceName(hInstance, 'CNBTNIGNORE');
        Caption := '&Ignore';
      end;

    bkAll:
      begin
        ModalResult := mrAll;
        FGlyph.LoadFromResourceName(hInstance, 'CNBTNALL');
        Caption := '&All';
      end;
  end;

  FKind := Value;
  Invalidate;
end;

procedure TCnCustomButton.SetLayout(const Value: TButtonLayout);
begin
  FLayout := Value;
  Invalidate;
end;

procedure TCnCustomButton.SetLightColor(const Value: TColor);
begin
  if FLightColor <> Value then
  begin
    FLightColor := Value;
    FBtnColorStyle := bcsCustom;
    Invalidate;
  end;
end;

procedure TCnCustomButton.SetModalResult(const Value: TModalResult);
begin
  FModalResult := Value;
  FKind := bkCustom;
end;

procedure TCnCustomButton.SetNumGlyphs(const Value: Integer);
begin
  FNumGlyphs := Value;
  Invalidate;
end;

procedure TCnCustomButton.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    FBtnColorStyle := bcsCustom;
    Invalidate;
  end;
end;

procedure TCnCustomButton.SetModernBtnStyle(const Value: TModernBtnStyle);
begin
  if FModernBtnStyle <> Value then
  begin
    FModernBtnStyle := Value;
    FBtnColorStyle := bcsCustom;
    Invalidate;
  end;
end;

procedure TCnCustomButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

procedure TCnCustomButton.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then
    Click;
end;

procedure TCnCustomButton.SetCancel(const Value: Boolean);
begin
  FCancel := Value;
  if FCancel then
    Default := False;
end;

procedure TCnCustomButton.SetDefault(const Value: Boolean);
var
  Form: TCustomForm;
begin
  FDefault := Value;
  if FDefault then
    Cancel := False;
  if HandleAllocated then
  begin
    Form := GetParentForm(Self);
    if Form <> nil then
      Form.Perform(CM_FOCUSCHANGED, 0, Longint(Form.ActiveControl));
  end;
end;

procedure TCnCustomButton.SetFlatBorder(const Value: Boolean);
begin
  if FFlatBorder <> Value then
  begin
    FFlatBorder := Value;
    FBtnColorStyle := bcsCustom;
    Invalidate;
  end;
end;

procedure TCnCustomButton.ActionChange(Sender: TObject;
  CheckDefaults: Boolean);
begin
  inherited;
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(Glyph, ActionList.Images, ImageIndex);
    end;
end;

procedure TCnCustomButton.SetDownBold(const Value: Boolean);
begin
  if FDownBold <> Value then
  begin
    FDownBold := Value;
    if FDown then
      Invalidate;
  end;
end;

procedure TCnCustomButton.SetDownColor(const Value: TColor);
begin
  if FDownColor <> Value then
  begin
    FDownColor := Value;
    if FDown then
      Invalidate;
  end;
end;

procedure TCnCustomButton.SetHotTrackBold(const Value: Boolean);
begin
  if FHotTrackBold <> Value then
  begin
    FHotTrackBold := Value;
    if FCursorOnButton then
      Invalidate;
  end;
end;

procedure TCnCustomButton.SetHotTrackColor(const Value: TColor);
begin
  if FHotTrackColor <> Value then
  begin
    FHotTrackColor := Value;
    if FCursorOnButton or Focused then
      Invalidate;
  end;
end;

procedure TCnCustomButton.SetSpacing(const Value: Integer);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TCnCustomButton.SetMargin(const Value: Integer);
begin
  if FMargin <> Value then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TCnCustomButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if (FModernBtnStyle = bsModern) and FRoundCorner then
    Message.Result := 0;
end;

procedure TCnCustomButton.SetRoundCorner(const Value: Boolean);
begin
  if FRoundCorner <> Value then
  begin
    FRoundCorner := Value;
    Invalidate;
  end;
end;

procedure TCnCustomButton.RenewBack;
var
  ABitmap: TBitmap;
  FBackBitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;
  FBackBitmap := TBitmap.Create;
  try
    FBackBitmap.Width := Width;
    FBackBitmap.Height := Height;
    ABitmap.PixelFormat := pf24bit;
    ABitmap.Width := -Parent.ClientOrigin.X + ClientOrigin.X + Width;
    ABitmap.Height := -Parent.ClientOrigin.Y + ClientOrigin.Y + Height;
    ABitmap.Canvas.Brush.Color := Parent.Brush.Color;
    ABitmap.Canvas.FillRect(Rect(0, 0, ABitmap.Width, ABitmap.Height));

    SendMessage(Parent.Handle, WM_PAINT, ABitmap.Canvas.Handle, 0);
    if not (csDesigning in ComponentState) then // 2008��08��03������жϣ�����������ڻ���ִ���
      Application.ProcessMessages;
    FBackBitmap.Canvas.Draw(Parent.ClientOrigin.X - ClientOrigin.x,
      Parent.ClientOrigin.Y - ClientOrigin.Y, ABitmap);

    Canvas.Draw(0, 0, FBackBitmap);
  finally
    FreeAndNil(FBackBitmap);
    FreeAndNil(ABitmap);
  end;
end;

procedure TCnCustomButton.WMWindowPosChanged(var Message: TMessage);
begin
  Invalidate;
  inherited;
  // 2008��08��03����ӣ�������̳�ԭ��Ϣ������ʹ�ؼ��޷��ı��С- -
end;

procedure TCnCustomButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TCnCustomButton.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
end;

{ TCnSpeedButton }

constructor TCnSpeedButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;
  FAlignment := taCenter;
  FSpacing := 4;
  FMargin := 4;

  FLayout := blGlyphLeft;
  FNumGlyphs := 1;
  FLightColor := clWhite;
  FShadowColor := clGray;
  FDownColor := clNone;
  FModernBtnStyle := bsNormal;
  FRoundCorner := True;
  FBtnColorStyle := bcsCustom;
  FHotTrackColor := clNone;
  FTransparent := False;
  FGlyph := TBitmap.Create;
  FGlyph.OnChange := GlyphChanged;
  { 2009-06-05 ��ӣ�����FGlyph��onchange�¼�������ֱ�ӵ���Glyph�ķ����ؼ��޷��õ�֪ͨ��ʱˢ�� }
end;

destructor TCnSpeedButton.Destroy;
begin
  FGlyph.Free;
  inherited Destroy;
end;

procedure TCnSpeedButton.Click;
begin
  inherited Click;
end;

procedure TCnSpeedButton.Paint;
var
  Down: Boolean;
begin
  if (csLoading in ComponentState) or (Parent = nil) then
    Exit;

  // 2009-06-29����ж��ж��Ƿ�������ڣ�������VisibleΪFALSEʱ��������޷�ˢ�¿ؼ�
  if not Visible and not (csDesigning in ComponentState) then
    Exit;

  if not Enabled then
  begin
    FState := bsDisabled;
    FDragging := False;
  end
  else if FState = bsDisabled then
    if FDown and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;

  Down := FDown;
  case FState of
    bsUp:
      begin
        if not Enabled then
          Enabled := True;
        if FDown then
        begin
          Down := False;
          FDown := False;
        end;
      end;
    bsDisabled:
      begin
        if Enabled then
          Enabled := False;
        if FDown then
        begin
          Down := False;
          FDown := False;
        end;
      end;
    bsDown:
      begin
        if not Enabled then
          Enabled := True;
        if not FDown then
        begin
          Down := True;  // FDown ��������Ϣ����
          // FDown := True;
        end;
      end;
    bsExclusive:
      begin
        if not Enabled then
          Enabled := True;
        if not FDown then
        begin
          Down := True;
          FDown := True;
        end;
      end;
  end;

  PaintButton(Canvas, True, Width, Height, FNumGlyphs, FSpacing, FMargin, FGlyph, Down, FDownBold,
              FHotTrackBold, CursorOnButton{ or Focused}, Transparent and (ModernBtnStyle <> bsModern), Enabled,
              Assigned(PopupMenu), False, False, FFlatBorder, FRoundCorner, FModernBtnStyle, Color, FDownColor,
              FHotTrackColor, FLightColor, FShadowColor, Font, FLayout,
              Caption, FAlignment, False);
end;

procedure TCnSpeedButton.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos(P);
    FCursorOnButton := not (FindDragTarget(P, True) = Self);
    if FCursorOnButton then
      Perform(CM_MOUSELEAVE, 0, 0)
    else
      Perform(CM_MOUSEENTER, 0, 0);
  end;
end;

procedure TCnSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button = mbLeft) and Enabled then
  begin
    if not FDown then
    begin
      FState := bsDown;
      Invalidate;
    end;
    FDragging := True;
  end;
end;

procedure TCnSpeedButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then NewState := bsUp
    else NewState := bsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then NewState := bsExclusive else NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end
  else if not FCursorOnButton then
    UpdateTracking;
end;

procedure TCnSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      { Redraw face in-case mouse is captured }
      FState := bsUp;
      FCursorOnButton := False;
      if DoClick and not (FState in [bsExclusive, bsDown]) then
        Invalidate;
    end
    else
      if DoClick then
      begin
        SetDown(not FDown);
        if FDown then Repaint;
      end
      else
      begin
        if FDown then FState := bsExclusive;
        Repaint;
      end;
    if DoClick then
      Click;
    UpdateTracking;
  end;
end;

function TCnSpeedButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

function TCnSpeedButton.GetGlyph: TBitmap;
begin
  Result := FGlyph;
end;

procedure TCnSpeedButton.SetGlyph(Value: TBitmap);
begin
  if Value <> nil then
  begin
    FGlyph.Assign(Value);
    if Value.Height <> 0 then
      FNumGlyphs := Value.Width div Value.Height
    else
      FNumGlyphs := 0;
  end
  else
  begin
    FGlyph.Height := 0;
    FNumGlyphs := 0;
  end;

  Invalidate;
end;

function TCnSpeedButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FNumGlyphs;
end;

procedure TCnSpeedButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < Low(TNumGlyphs) then
    Value := Low(TNumGlyphs)
  else if Value > High(TNumGlyphs) then
    Value := High(TNumGlyphs);

  if Value <> FNumGlyphs then
  begin
    FNumGlyphs := Value;
    Invalidate;
  end;
end;

procedure TCnSpeedButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TCnSpeedButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then Invalidate;
      FState := bsExclusive
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
    if Value then
      UpdateExclusive;
  end;
end;

procedure TCnSpeedButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

procedure TCnSpeedButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TCnSpeedButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= 0) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TCnSpeedButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

procedure TCnSpeedButton.SetTransparent(Value: Boolean);
begin
  if not (csLoading in ComponentState) and (FModernBtnStyle <> bsFlat) then
    Value := False;

  if Value <> FTransparent then
  begin
    FTransparent := Value;
    if Value then
      ControlStyle := ControlStyle - [csOpaque] else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TCnSpeedButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

procedure TCnSpeedButton.WMLButtonDblClk(var Message: TWMLButtonDown);
begin
  inherited;
  if FDown then DblClick;
end;

procedure TCnSpeedButton.CMEnabledChanged(var Message: TMessage);
begin
  UpdateTracking;
  Repaint;
end;

procedure TCnSpeedButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TCnSpeedButton;
begin
  if Integer(Message.WParam) = FGroupIndex then
  begin
    Sender := TCnSpeedButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

procedure TCnSpeedButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and Enabled and Visible and
      (Parent <> nil) and Parent.Showing then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

procedure TCnSpeedButton.CMFontChanged(var Message: TMessage);
begin
  Repaint;
end;

procedure TCnSpeedButton.CMTextChanged(var Message: TMessage);
begin
  Repaint;
end;

procedure TCnSpeedButton.CMSysColorChange(var Message: TMessage);
begin
  Repaint;
end;

procedure TCnSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FCursorOnButton and Enabled and (DragMode <> dmAutomatic)
    and (GetCapture = 0) then
  begin
    FCursorOnButton := True;
    Invalidate;
  end;

  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TCnSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FCursorOnButton and Enabled and not FDragging then
  begin
    FCursorOnButton := False;
    Invalidate;
  end;

  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TCnSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
        (ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
        CopyImage(Glyph, ActionList.Images, ImageIndex);
    end;
end;

procedure TCnSpeedButton.SetBtnColorStyle(const Value: TBtnColorStyle);
var
  AColor: TColor;
begin
  FBtnColorStyle  := Value;
  if Value = bcsCustom then
    Exit;

  GetPreDefinedColors(Value, AColor, FLightColor, FShadowColor, FDownColor,
    FHotTrackColor, FModernBtnStyle, FFlatBorder);
  Color := AColor;

  Invalidate;
end;

procedure TCnSpeedButton.SetFlatBorder(const Value: Boolean);
begin
  if FFlatBorder <> Value then
  begin
    FFlatBorder := Value;
    FBtnColorStyle := bcsCustom;
    Invalidate;
  end;
end;

procedure TCnSpeedButton.SetLightColor(const Value: TColor);
begin
  if FLightColor <> Value then
  begin
    FLightColor := Value;
    FBtnColorStyle := bcsCustom;
    Invalidate;
  end;
end;

procedure TCnSpeedButton.SetModernBtnStyle(const Value: TModernBtnStyle);
begin
  if FModernBtnStyle <> Value then
  begin
    FModernBtnStyle := Value;
    FBtnColorStyle := bcsCustom;
    if Value <> bsFlat then
      Transparent := False;
    Invalidate;
  end;
end;

procedure TCnSpeedButton.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    FBtnColorStyle := bcsCustom;
    Invalidate;
  end;
end;

procedure TCnSpeedButton.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

procedure TCnSpeedButton.SetDownBold(const Value: Boolean);
begin
  if FDownBold <> Value then
  begin
    FDownBold := Value;
    if FDown then
      Invalidate;
  end;
end;

procedure TCnSpeedButton.SetDownColor(const Value: TColor);
begin
  if FDownColor <> Value then
  begin
    FDownColor := Value;
    Transparent := False;
    if FDown then
      Invalidate;
  end;
end;

procedure TCnSpeedButton.SetHotTrackBold(const Value: Boolean);
begin
  if FHotTrackBold <> Value then
  begin
    FHotTrackBold := Value;
    if FCursorOnButton then
      Invalidate;
  end;
end;

procedure TCnSpeedButton.SetHotTrackColor(const Value: TColor);
begin
  if FHotTrackColor <> Value then
  begin
    FHotTrackColor := Value;
    Transparent := False;
    if FCursorOnButton then
      Invalidate;
  end;
end;

procedure TCnSpeedButton.SetRoundCorner(const Value: Boolean);
begin
  if FRoundCorner <> Value then
  begin
    FRoundCorner := Value;
    Invalidate;
  end;
end;

procedure TCnSpeedButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

function TCnSpeedButton.GetFlat: Boolean;
begin
  Result := FModernBtnStyle = bsFlat;
end;

procedure TCnSpeedButton.SetFlat(const Value: Boolean);
begin
  if Flat <> Value then
  begin
    if Value then
      ModernBtnStyle := bsFlat
    else
      ModernBtnStyle := bsNormal;
  end;
end;

initialization

finalization
  FreeAndNil(FImageList);

end.
