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

unit CnSkinMenu;

interface

uses
  Windows, SysUtils, Classes, Menus, Forms, Graphics, ImgList,
  CnSkinTheme, CnCommon;

type
  TCnSkinMenu = class(TComponent)
  private
    FMaxWidth: Integer;
    FColor: TColor;
    FHighLightColor: TColor;
    FForm: TForm;
    FSaveFormCreate: TNotifyEvent;
    procedure FormCreate(Sender: TObject);
    function GetImageList(MenuItem: TMenuItem): TCustomImageList;
    procedure AdvancedDrawMenuItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; State: TOwnerDrawState);
    procedure MeasureMenuItem(Sender: TObject; ACanvas: TCanvas;
      var Width, Height: Integer);
  protected
    procedure DrawBorder(Handle: HWND);
    procedure DoDrawText(MenuItem: TMenuItem; ACanvas: TCanvas;
      const ACaption: string; var Rect: TRect; Selected: Boolean; Flags: Longint);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InstallMenuSkin;
    procedure UnInstallMenuSkin;
  published
    property Color: TColor read FColor write FColor;
    property HighLightColor: TColor read FHighLightColor write FHighLightColor;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth;
  end;

implementation

var
  CnSkinMenus: TList;

constructor TCnSkinMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clMenu;
  FHighLightColor := clMenuHighLight;
  FMaxWidth := -1;
  CnSkinMenus.Add(Self);
  FForm := AOwner as TForm;
  if not (csDesigning in FForm.ComponentState) then
  begin
    if Assigned(FForm.OnCreate) then
      FSaveFormCreate := FForm.OnCreate;
    FForm.OnCreate := FormCreate;
  end;
end;

destructor TCnSkinMenu.Destroy;
begin
  CnSkinMenus.Remove(Self);
  inherited Destroy;
end;

procedure TCnSkinMenu.FormCreate(Sender: TObject);
begin
  InstallMenuSkin;
  if Assigned(FSaveFormCreate) then
    FSaveFormCreate(Sender);
end;

function TCnSkinMenu.GetImageList(MenuItem: TMenuItem): TCustomImageList;
var
  LItem: TMenuItem;
  LMenu: TMenu;
begin
  Result := nil;
  LItem := MenuItem.Parent;
  while (LItem <> nil) and (LItem.SubMenuImages = nil) do
    LItem := LItem.Parent;
  if LItem <> nil then
    Result := LItem.SubMenuImages
  else
  begin
    LMenu := MenuItem.GetParentMenu;
    if LMenu <> nil then
      Result := LMenu.Images;
  end;
end;

procedure TCnSkinMenu.DrawBorder(Handle: HWND);
var
  Canvas: TCanvas;
  RC, RW: TRect;
  X, Y: Integer;
begin
  Canvas := TCanvas.Create;
  Canvas.Handle := GetWindowDC(Handle);
  try
    GetClientRect(Handle, RC);
    GetWindowRect(Handle, RW);
    OffsetRect(RW, -RW.Left, -RW.Top);
    X := (RW.Right - RC.Right) div 2;
    Y := (RW.Bottom - RC.Bottom) div 2;
    ExcludeClipRect(Canvas.Handle, X, Y, RW.Right - X, RW.Bottom - Y);
    Canvas.Pen.Color := CnSkinThemes.CurrentSkin.ShadowColor;
    Canvas.Brush.Color := FColor;
    Canvas.Rectangle(RW);
  finally
    ReleaseDC(Handle, Canvas.Handle);
    Canvas.Free;
  end;
end;

procedure TCnSkinMenu.DoDrawText(MenuItem: TMenuItem; ACanvas: TCanvas;
  const ACaption: string; var Rect: TRect; Selected: Boolean; Flags: Longint);
begin
  with ACanvas do
  begin
    Brush.Style := bsClear;
    if MenuItem.Default then
      Font.Style := Font.Style + [fsBold];
    if not MenuItem.Enabled then
    begin
      OffsetRect(Rect, 1, 1);
      Font.Color := CnSkinThemes.CurrentSkin.LightColor;
      DrawText(Handle, PChar(ACaption), -1, Rect, Flags);
      OffsetRect(Rect, -1, -1);
      Font.Color := CnSkinThemes.CurrentSkin.ShadowColor;
    end
    else if Selected then
      Font.Color := CnSkinThemes.CurrentSkin.LightColor;
    DrawText(Handle, PChar(ACaption), -1, Rect, Flags);
  end;
end;

procedure TCnSkinMenu.AdvancedDrawMenuItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; State: TOwnerDrawState);
var
  MenuItem: TMenuItem;
  Y: Integer;
  Image: TCustomImageList;
  Selected: Boolean;
  Text: string;

  procedure DrawTopLevelMenuItem;
  begin
    Selected := Selected or (odHotLight in State);
    if Selected then
    begin
      ACanvas.Brush.Color := FHighLightColor;
      ACanvas.Pen.Color := CnSkinThemes.CurrentSkin.ShadowColor;
    end else
    begin
      ACanvas.Brush.Color := clMenuBar;
      ACanvas.Pen.Color := clMenuBar;
    end;
    ACanvas.Rectangle(ARect);
    DoDrawText(MenuItem, ACanvas, MenuItem.Caption, ARect, Selected,
      DT_CENTER or DT_VCENTER or DT_SINGLELINE);
  end;
begin
  MenuItem := Sender as TMenuItem;
  Image := GetImageList(MenuItem);
  Selected := odSelected in State;
  if MenuItem.GetParentComponent is TMainMenu then
    DrawTopLevelMenuItem
  else
  begin
    DrawBorder(WindowFromDC(ACanvas.Handle));
    with ACanvas do
    begin
      Pen.Color := CnSkinThemes.CurrentSkin.ShadowColor;
      if Selected and MenuItem.Enabled then
      begin
        Brush.Color := FHighLightColor;
        Rectangle(ARect);
      end else
      begin
        Brush.Color :=  FColor;
        FillRect(ARect);
      end;
      if MenuItem.Caption = '-' then
      begin
        Y := ARect.Top + (ARect.Bottom - ARect.Top -1) div 2;
        MoveTo(ARect.Left, Y);
        LineTo(ARect.Right, Y);
      end else
      begin
        if Assigned(Image) and (MenuItem.ImageIndex >= 0) then
            Image.Draw(ACanvas, ARect.Left + 1, ARect.Top + 1,
              MenuItem.ImageIndex, MenuItem.Enabled)
        else
        begin
          if MenuItem.Enabled then
            Pen.Color := clBlack else
            Pen.Color := clGray;
          if MenuItem.Checked then
          begin
            MoveTo(ARect.Left + 3, ARect.Top + 8);
            LineTo(ARect.Left + 7, ARect.Top + 12);
            LineTo(ARect.Left + 14, ARect.Top + 4);
            MoveTo(AREct.Left + 2, ARect.Top + 9);
            LineTo(ARect.Left + 6, ARect.Top + 13);
            LineTo(ARect.Left + 15, ARect.Top + 5);
          end;
        end;
        if Assigned(Image) then
          Inc(ARect.Left, Image.Width + 4) else
          Inc(ARect.Left, 20);
        Dec(ARect.Right, 16);
        Text := ShortCutToText(MenuItem.ShortCut);
        if Text <> '' then
        begin
          DoDrawText(MenuItem, ACanvas, Text, ARect, Selected,
            DT_VCENTER or DT_SINGLELINE or DT_RIGHT);
          Dec(ARect.Right, ACanvas.TextWidth(Text) + 5);
        end;
        DoDrawText(MenuItem, ACanvas, MenuItem.Caption, ARect, Selected,
          DT_VCENTER or DT_SINGLELINE or DT_LEFT or DT_END_ELLIPSIS);
      end;
    end;
  end;
end;

procedure TCnSkinMenu.MeasureMenuItem(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
var
  MenuItem: TMenuItem;
begin
  MenuItem := Sender as TMenuItem;
  if MenuItem.GetParentComponent is TMainMenu then Exit;
  if GetImageList(MenuItem) = nil then
  begin
    if Assigned(Menuitem.Bitmap) and not MenuItem.Bitmap.Empty then
      Inc(Width, 23) else
      Inc(Width, 38);
  end;
  if (FMaxWidth >= 0) and (Width > FMaxWidth) then
    Width := FMaxWidth;
end;

procedure TCnSkinMenu.InstallMenuSkin;
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  if FForm = nil then
    Exit;

  for I := 0 to FForm.ComponentCount -1 do
    if FForm.Components[I] is TMenuItem then
    begin
      MenuItem := FForm.Components[I] as TMenuItem;
      MenuItem.OnMeasureItem := MeasureMenuItem;
      MenuItem.OnAdvancedDrawItem := AdvancedDrawMenuItem;
    end
    else if FForm.Components[I] is TMenu then
      (FForm.Components[I] as TMenu).OwnerDraw := True;
end;

procedure TCnSkinMenu.UnInstallMenuSkin;
var
  I: Integer;
  MenuItem: TMenuItem;
begin
  if FForm = nil then
    Exit;

  for I := 0 to FForm.ComponentCount -1 do
    if FForm.Components[I] is TMenuItem then
    begin
      MenuItem := FForm.Components[I] as TMenuItem;
      if TMethod(MenuItem.OnMeasureItem).Code = @TCnSkinMenu.MeasureMenuItem then
        MenuItem.OnMeasureItem := nil;
      if TMethod(MenuItem.OnAdvancedDrawItem).Code = @TCnSkinMenu.AdvancedDrawMenuItem then
        MenuItem.OnAdvancedDrawItem := nil;
    end
    else if FForm.Components[I] is TMenu then
      (FForm.Components[I] as TMenu).OwnerDraw := False;
end;

initialization
  CnSkinMenus := TList.Create;

finalization
  CnSkinMenus.Free;

end.
