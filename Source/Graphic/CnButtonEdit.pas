{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnButtonEdit;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：CnButtonEdit 单元
* 单元作者：dingbaosheng (yzdbs@msn.com)
* 备    注：
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2007.05.02 V1.0
*               LiuXiao 移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, Menus;

type
  TButtonKind = (bkCustom, bkLookup, bkDropDown, bkAccept, bkReject,
    bkFolder, bkFind);

{$IFDEF SUPPORT_32_AND_64}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TCnButtonEdit = class(TCustomMemo)
  private
    FButtonVisible: Boolean;
    FButtonFlat: Boolean;
    FButtonKind: TButtonKind;
    FOnButtonClick: TNotifyEvent;
    function GetButtonKind: TButtonKind;
    procedure SetButtonKind(const Value: TButtonKind);
    function GetButtonGlyph: TBitmap;
    procedure SetButtonGlyph(Value: TBitmap);
    procedure SetButtonVisible(const Value: Boolean);
    procedure SetButtonBounds;
    procedure SetButtonFlat(const Value: Boolean);
    function GetButtonHint: string;
    procedure SetButtonHint(const Value: string);
  protected
    FButton: TSpeedButton;
    procedure BtnClickHandler(Sender: TObject); virtual;
    procedure UpdateFormatRect;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure CMEnabledChanged(var Msg: TWMNoParams); message CM_ENABLEDCHANGED;
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ButtonVisible: Boolean read FButtonVisible write SetButtonVisible default True;
    property ButtonFlat: Boolean read FButtonFlat write SetButtonFlat;
    property ButtonHint: string read GetButtonHint write SetButtonHint;
    property ButtonKind: TButtonKind read GetButtonKind write SetButtonKind;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property ButtonPic: TBitmap read GetButtonGlyph write SetButtonGlyph;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  end;

implementation

{$R CnButtonEdit.RES}

const
  BtnEdtResNames: array[TButtonKind] of PChar = (nil,
    'BTNEDT_LOOKUP',
    'BTNEDT_DROPDOWN',
    'BTNEDT_ACCEPT',
    'BTNEDT_REJECT',
    'BTNEDT_FOLDER',
    'BTNEDT_FIND');

var
  BtnEdtGlyphs: array[TButtonKind] of TBitmap;

function GetBtnEdtGlyph(Kind: TButtonKind): TBitmap;
begin
  if BtnEdtGlyphs[Kind] = nil then
  begin
    BtnEdtGlyphs[Kind] := TBitmap.Create;
    BtnEdtGlyphs[Kind].LoadFromResourceName(HInstance, BtnEdtResNames[Kind]);
  end;
  Result := BtnEdtGlyphs[Kind];
end;

function TCnButtonEdit.GetButtonGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

procedure TCnButtonEdit.SetButtonGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
  FButtonKind := bkCustom;
end;

function TCnButtonEdit.GetButtonKind: TButtonKind;
begin
  Result := FButtonKind;
end;

procedure TCnButtonEdit.SetButtonKind(const Value: TButtonKind);
begin
  if (Value <> FButtonKind) then
  begin
    FButtonKind := Value;
    if FButtonKind <> bkCustom then
      FButton.Glyph := GetBtnEdtGlyph(Value);
  end
end;

constructor TCnButtonEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Height := 21;
  Width := 121;
  WordWrap := False;
  WantReturns := False;
  FButtonVisible := True;
  FButton := TSpeedButton.Create(Self);

  with FButton do
  begin
    Parent := Self;
    FButtonKind := bkLookup; //设置为...图片
    Glyph := GetBtnEdtGlyph(FButtonKind);
    Align := alRight;
    Spacing := -1;
    ShowHint := True;
    Margin := -1;
    OnClick := BtnClickHandler;
  end;
end;

destructor TCnButtonEdit.Destroy;
begin

  inherited;
end;

procedure TCnButtonEdit.CreateHandle;
begin
  inherited CreateHandle;
  UpdateFormatRect;
end;

procedure TCnButtonEdit.UpdateFormatRect;
var
  Rect: TRect;
begin
  Rect := ClientRect;
  if FButtonVisible then
    Dec(Rect.Right, FButton.Height)
  else
    Inc(Rect.Right, FButton.Height);
  SendMessage(Handle, EM_SETRECTNP, 0, Longint(@Rect));
end;

procedure TCnButtonEdit.WMSize(var Msg: TWMSize);
begin
  inherited;
  FButton.Width := FButton.Height;
  UpdateFormatRect;
end;

procedure TCnButtonEdit.WMSetCursor(var Msg: TWMSetCursor);
var
  P: TPoint;
  PosWidth: Integer;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  PosWidth := ClientWidth;

  if FButtonVisible then
    PosWidth := PosWidth - FButton.Width;

  if (P.X >= PosWidth) then
    SetCursor(Screen.Cursors[crDefault])
  else
    inherited;
end;

procedure TCnButtonEdit.CMEnabledChanged(var Msg: TWMNoParams);
begin
  inherited;
  FButton.Enabled := Enabled;
end;

procedure TCnButtonEdit.SetButtonBounds;
begin
  if not FButtonVisible then
    FButton.Width := 0
  else
    FButton.Width := Height - 1;
  UpdateFormatRect;
  if not (csLoading in ComponentState) then
  begin
    SendMessage(Handle, EM_SETMARGINS, EC_LEFTMARGIN, 0);
    SendMessage(Handle, EM_SETMARGINS, EC_RIGHTMARGIN, MakeLong(0, 2));
  end;
end;

procedure TCnButtonEdit.SetButtonVisible(const Value: Boolean);
begin
  if FButtonVisible <> Value then
  begin
    FButtonVisible := Value;
    FButton.Visible := Value;
    SetButtonBounds;
    Invalidate;
  end;
end;

procedure TCnButtonEdit.SetButtonFlat(const Value: Boolean);
begin
  if FButtonFlat <> Value then
  begin
    FButtonFlat := Value;
    FButton.Flat := Value;
    Invalidate;
  end;
end;

function TCnButtonEdit.GetButtonHint: string;
begin
  Result := FButton.Hint;
end;

procedure TCnButtonEdit.SetButtonHint(const Value: string);
begin
  FButton.Hint := Value;
end;

procedure TCnButtonEdit.BtnClickHandler(Sender: TObject);
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self);
end;

procedure FreeBtnEdtGlyph;
var
  Kind: TButtonKind;
begin
  for Kind := Low(TButtonKind) to High(TButtonKind) do
  begin
    if BtnEdtGlyphs[Kind] <> nil then
    begin
      BtnEdtGlyphs[Kind].Free;
      BtnEdtGlyphs[Kind] := nil;
    end;
  end;
end;

initialization

finalization
  FreeBtnEdtGlyph;

end.
