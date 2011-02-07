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

unit CnSkinTheme;

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics, Forms, Controls,
  CnSkinStyle;

const
  CM_THEMECHANGE = CM_BASE + $CE0;
  SBSIZE = 17;
  
  {$EXTERNALSYM COLOR_MENUHILIGHT}
  COLOR_MENUHILIGHT = 29;
  {$EXTERNALSYM COLOR_MENUBAR}
  COLOR_MENUBAR = 30;

  clSystemColor = $FF000000;
  clMenuHighlight = TColor(clSystemColor or COLOR_MENUHILIGHT);
  clMenuBar = TColor(clSystemColor or COLOR_MENUBAR);

type
  TScrollBarButton = (sbNone, sbUp, sbDown, sbLeft, sbRight, sbButton, sbButtonH);

  TCnSkinThemes = class(TObject)
  private
    FSkins: TList;
    FControls: TList;
    FOldSkinIndex: Integer;
    FSkinIndex: Integer;
    FCurrentSkin: TCnSkinStyle;
    FActive: Boolean;
    function GetSkinCount: Integer;
    procedure SetSkinIndex(const Value: Integer);
    function GetSkins(Index: Integer): TCnSkinStyle;
    procedure SetActive(const Value: Boolean);
  protected
    procedure LoadStyle(Skin: TCnSkinStyle);
  public
    constructor Create; virtual;
    {* 构造函数}
    destructor Destroy; override;
    {* 析构函数}
    procedure AddSkin(Skin: TCnSkinStyle);
    {* 添加一个 SkinStyle 到列表中，一般不需直接调用}
    procedure RemoveSkin(Skin: TCnSkinStyle);
    {* 从列表中删除一个 SkinStyle，一般不需直接调用}
    property Active: Boolean read FActive write SetActive;
    {* 是否使能皮肤效果}
    property Controls: TList read FControls;
    {* 所有 CnSkin 界面组件的实例列表，供通知之用}
    property SkinCount: Integer read GetSkinCount;
    {* 已记录的 SkinStyle 的数量}
    property SkinIndex: Integer read FSkinIndex write SetSkinIndex;
    {* 当前 SkinStyle 的索引号，从 0 到 SkinCount - 1}
    property Skins[Index: Integer]: TCnSkinStyle read GetSkins;
    {* 已记录的 SkinStyle 列表}
    property CurrentSkin: TCnSkinStyle read FCurrentSkin;
    {* 当前使用的 SkinStyle，是一内部实例，内容从 Skins 的当前实例复制而来}
  end;

function CnSkinThemes: TCnSkinThemes;
{* 全局函数，返回 CnSkinThemes 的实例}

function CnGetScrollInfo(Control: TWinControl; I: Integer; var I1, I2: Integer;
  Kind: TScrollBarKind): Boolean;

procedure CnDrawScrollBar(Canvas: TCanvas; R: TRect; I1, I2: Integer;
  Over, Down: TScrollBarButton; Kind: TScrollBarKind; Enabled: Boolean);

function CnGetScrollCount(Control: TWinControl; Button: TScrollBarButton;
  I, Pos: Integer; Kind: TScrollBarKind): Integer;

implementation

var
  FCnSkinThemes: TCnSkinThemes;

function CnSkinThemes: TCnSkinThemes;
begin
  Result := FCnSkinThemes;
end;

{ TCnSkinThemes }

procedure TCnSkinThemes.AddSkin(Skin: TCnSkinStyle);
begin
  if Skin <> nil then
    FSkins.Add(Skin);
end;

constructor TCnSkinThemes.Create;
begin
  inherited;
  FCnSkinThemes := Self;
  FControls := TList.Create;
  FSkins := TList.Create;
  FSkinIndex := -1;
  FCurrentSkin := TCnSkinStyle.Create(nil);
  FSkins.Clear; // Don't add internal store
end;

destructor TCnSkinThemes.Destroy;
begin
  FCurrentSkin.Free;
  FControls.Free;
  FSkins.Free;
  inherited;
end;

function TCnSkinThemes.GetSkinCount: Integer;
begin
  Result := FSkins.Count;
end;

function TCnSkinThemes.GetSkins(Index: Integer): TCnSkinStyle;
begin
  if (Index < SkinCount) and (Index >= 0) then
  begin;
    Result := TCnSkinStyle(FSkins[Index]);
  end
  else
    Result := nil;
end;

procedure TCnSkinThemes.LoadStyle(Skin: TCnSkinStyle);
begin
  if Skin <> nil then
    FCurrentSkin.Assign(Skin)
  else
  begin
    FCurrentSkin.Clear;
    FActive := False;
    FSkinIndex := -1;
  end;
end;

procedure TCnSkinThemes.RemoveSkin(Skin: TCnSkinStyle);
begin
  FSkins.Remove(Skin);
end;

procedure TCnSkinThemes.SetActive(const Value: Boolean);
var
  I: Integer;
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if not FActive then
    begin
      FOldSkinIndex := SkinIndex;
      SkinIndex := -1;
      for I := 0 to Controls.Count - 1 do
        TWinControl(Controls[I]).Perform(CM_THEMECHANGE, 0, 0);
    end
    else
    begin
      SkinIndex := FOldSkinIndex;
    end;
  end;
end;

procedure TCnSkinThemes.SetSkinIndex(const Value: Integer);
var
  I: Integer;
begin
  if Value <> FSkinIndex then
  begin
    FSkinIndex := Value;
    LoadStyle(Skins[FSkinIndex]);
    
    if Active then 
      for I := 0 to Controls.Count - 1 do
        TWinControl(Controls[I]).Perform(CM_THEMECHANGE, 0, 0);
  end;
end;

{ Other Routines }

function CnGetScrollInfo(Control: TWinControl; I: Integer; var I1, I2: Integer;
  Kind: TScrollBarKind): Boolean;
var
  ScrollInfo: TScrollInfo;
  Count: Integer;
begin
  Result := False;
  FillChar(ScrollInfo, SizeOf(TScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  Windows.GetScrollInfo(Control.Handle, Ord(Kind), ScrollInfo);
  with ScrollInfo do
  begin
    Dec(nMax, nMin - 1);
    Dec(nPos, nMin);
    Count := nMax - Integer(nPage);
    if Count > 0 then
    begin
      Result := True;
      Dec(I, SBSIZE + SBSIZE);
      I2 := I - I * Count div nMax;
      if I2 < 10 then I2 := 10;
      Inc(I1, (I - I2) * nPos div Count);
    end;
  end;
end;

procedure CnDrawScrollBar(Canvas: TCanvas; R: TRect; I1, I2: Integer;
  Over, Down: TScrollBarButton; Kind: TScrollBarKind; Enabled: Boolean);
var
  SrcR, DestR: TRect;
  SrcW, DestW, Offset, W: Integer;

  function GetSrcR(Button: TScrollBarButton): TRect;
  begin
    Result := Rect(0, 0, SrcW, SrcW);
    Offset := 0;
    if not Enabled then
      Offset := SrcW * 3 else
      if Over = Button then
      begin
        Inc(Offset, SrcW);
        if Down = Button then Inc(Offset, SrcW);
      end;
    OffsetRect(Result, 0, Offset);
  end;

  function GetSrcR2(Button: TScrollBarButton): TRect;
  begin
    Result := Rect(0, 0, SrcW, SrcW);
    Offset := 5 + Ord(Button);
    if not Enabled then Inc(Offset, 4);
    OffsetRect(Result, 0, Offset * SrcW);
  end;

begin
  Canvas.Brush.Style := bsClear;
  SrcW := CnSkinThemes.CurrentSkin.ScrollBarBmp.Width;
  if Kind = sbHorizontal then
  begin
    DestW := R.Bottom - R.Top;
    DestR := R;
    DestR.Right := R.Left + DestW;
    Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp.Canvas, GetSrcR(sbLeft));
    Canvas.BrushCopy(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp, GetSrcR2(sbLeft), clFuchsia);
    DestR := R;
    DestR.Left := DestR.Right - DestW;
    Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp.Canvas, GetSrcR(sbRight));
    Canvas.BrushCopy(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp, GetSrcR2(sbRight), clFuchsia);
    DestR.Right := DestR.Left;
    DestR.Left := R.Left + DestW;
    SrcR := Rect(0, 0, SrcW, SrcW);
    OffsetRect(SrcR, 0, 5 * SrcW);
    Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp.Canvas, SrcR);
    if Enabled then
    begin
      SrcR := GetSrcR(sbButton);
      W := SrcW div 3;
      SrcR.Right := W;
      DestR.Left := I1;
      DestR.Right := DestR.Left + W;
      Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp.Canvas, SrcR);
      DestR.Right := I1 + I2;
      DestR.Left := DestR.Right - W;
      OffsetRect(SrcR, W + W, 0);
      Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp.Canvas, SrcR);
      if I2 > W + W then
      begin
        DestR.Right := DestR.Left;
        DestR.Left := I1 + W;
        OffsetRect(SrcR, - W, 0);
        Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp.Canvas, SrcR);
        if I2 > DestW then
        begin
          SrcR := Rect(0, 0, SrcW, SrcW);
          OffsetRect(SrcR, 0, 15 * SrcW);
          DestR.Left := I1 + (I2 - SrcW) div 2;
          DestR.Top := R.Top + (DestW - SrcW) div 2;
          DestR.Right := DestR.Left + SrcW;
          DestR.Bottom := DestR.Top + SrcW;
          Canvas.BrushCopy(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp, SrcR, clFuchsia);
        end;
      end;
    end;
  end
  else
  begin
    DestW := R.Right - R.Left;
    DestR := R;
    DestR.Bottom := R.Top + DestW;
    Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp.Canvas, GetSrcR(sbUp));
    Canvas.BrushCopy(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp, GetSrcR2(sbUp), clFuchsia);
    DestR := R;
    DestR.Top := DestR.Bottom - DestW;
    Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp.Canvas, GetSrcR(sbDown));
    Canvas.BrushCopy(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp, GetSrcR2(sbDown), clFuchsia);
    DestR.Bottom := DestR.Top;
    DestR.Top := R.Top + DestW;
    SrcR := Rect(0, 0, SrcW, SrcW);
    OffsetRect(SrcR, 0, 4 * SrcW);
    Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp.Canvas, SrcR);
    if Enabled then
    begin
      SrcR := GetSrcR(sbButton);
      W := SrcW div 3;
      SrcR.Bottom := SrcR.Top + W;
      DestR.Top := I1;
      DestR.Bottom := DestR.Top + W;
      Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp.Canvas, SrcR);
      DestR.Bottom := I1 + I2;
      DestR.Top := DestR.Bottom - W;
      OffsetRect(SrcR, 0, W + W);
      Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp.Canvas, SrcR);
      if I2 > W + W then
      begin
        DestR.Bottom := DestR.Top;
        DestR.Top := I1 + W;
        OffsetRect(SrcR, 0, - W);
        Canvas.CopyRect(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp.Canvas, SrcR);
        if I2 > DestW then
        begin
          SrcR := Rect(0, 0, SrcW, SrcW);
          OffsetRect(SrcR, 0, 14 * SrcW);
          DestR.Top := I1 + (I2 - SrcW) div 2;
          DestR.Left := R.Left + (DestW - SrcW) div 2;
          DestR.Bottom := DestR.Top + SrcW;
          DestR.Right := DestR.Left + SrcW;
          Canvas.BrushCopy(DestR, CnSkinThemes.CurrentSkin.ScrollBarBmp, SrcR, clFuchsia);
        end;
      end;
    end;
  end;
end;

function CnGetScrollCount(Control: TWinControl; Button: TScrollBarButton;
  I, Pos: Integer; Kind: TScrollBarKind): Integer;
var
  ScrollInfo: TScrollInfo;
  NewPos, Count, I2: Integer;
begin
  Result := 0;
  FillChar(ScrollInfo, SizeOf(TScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(TScrollInfo);
  ScrollInfo.fMask := SIF_ALL;
  Windows.GetScrollInfo(Control.Handle, Ord(Kind), ScrollInfo);
  with ScrollInfo do
  begin
    NewPos := nPos;
    Count := nMax - Integer(nPage);
    Dec(nMax, nMin - 1);
    Dec(nPos, nMin);
    case Button of
      sbUp, sbLeft: if nPos > 0 then Dec(NewPos);

      sbDOwn, sbRight: if nPos <= Count then Inc(NewPos);

      sbNone, sbButton:
      begin
        Dec(I, SBSIZE + SBSIZE);
        I2 := I - I * Count div nMax;
        if I2 < 10 then I2 := 10;
        Dec(Pos, SBSIZE);
        NewPos := Pos * Count div (I - I2 div 2);
        if NewPos < 0 then
          NewPos := 0 else
         if NewPos > Count + 1 then NewPos := Count + 1;
      end;
    end;
    if NewPos <> nPos then  Result := NewPos - nPos;
  end;
end;

initialization
  FCnSkinThemes := TCnSkinThemes.Create;

finalization
  FreeAndNil(FCnSkinThemes);

end.
