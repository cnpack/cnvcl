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

unit CnSkinStyle;

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics, Forms, Controls;

type
  ICnSkinParams = interface(IUnknown)
    ['{C4FDB92B-AE10-49F9-918E-F4F7DC9CD1E4}']
    function GetFaceColor: TColor;
    function GetLightColor: TColor;
    function GetMenuHotColor: TColor;
    function GetShadowColor: TColor;
    function GetActiveCaptionColor: TColor;
    function GetInActiveCaptionColor: TColor;
    function GetCaptionHeight: Integer;
    function GetBorderSize: Integer;
    function GetButtonSize: Integer;
    function GetButtonRight: Integer;
    function GetButtonTop: Integer;
    function GetCaptionFontSize: Integer;
    function GetRgnSize: Integer;
    function GetWindowBmp: TBitmap;
    function GetWindowBtnBmp: TBitmap;
    function GetScrollBarBmp: TBitmap;
    function GetButtonBmp: TBitmap;
    function GetCheckBmp: TBitmap;
    function GetComboBmp: TBitmap;
    function GetRadioBmp: TBitmap;

    property FaceColor: TColor read GetFaceColor;
    {* 窗体表面颜色 }
    property LightColor: TColor read GetLightColor;
    {* 高亮颜色 }
    property ShadowColor: TColor read GetShadowColor;
    {* 阴影颜色 }
    property MenuHotColor: TColor read GetMenuHotColor;
    {* 菜单高亮颜色 }
    property ActiveCaptionColor: TColor read GetActiveCaptionColor;
    {* 活动窗体的标题颜色 }
    property InActiveCaptionColor: TColor read GetInActiveCaptionColor;
    {* 非活动窗体的标题颜色 }
    property CaptionHeight: Integer read GetCaptionHeight;
    {* 标题栏高度 }
    property BorderSize: Integer read GetBorderSize;
    {* 窗体边框粗细 }
    property ButtonSize: Integer read GetButtonSize;
    {* 单个标题按钮的尺寸 }
    property ButtonTop: Integer read GetButtonTop;
    {* 标题按钮距顶端的尺寸 }
    property ButtonRight: Integer read GetButtonRight;
    {* 标题按钮距右边的尺寸 }
    property CaptionFontSize: Integer read GetCaptionFontSize;
    {* 标题栏文字尺寸 }
    property RgnSize: Integer read GetRgnSize;
    {* 区域尺寸 }
    property WindowBmp: TBitmap read GetWindowBmp;
    {* 标题栏背景图片 }
    property WindowBtnBmp: TBitmap read GetWindowBtnBmp;  
    {* 窗体系统按钮图片 }  
    property ScrollBarBmp: TBitmap read GetScrollBarBmp;
    {* 滚动条的图片 }
    property ButtonBmp: TBitmap read GetButtonBmp;
    {* 普通按钮图片 }
    property CheckBmp: TBitmap read GetCheckBmp;
    {* 复选框图片 }
    property RadioBmp: TBitmap read GetRadioBmp;
    {* 单选钮图片 }
    property ComboBmp: TBitmap read GetComboBmp;
    {* 下拉框图片 }
  end;

  TCnSkinStyle = class(TComponent, ICnSkinParams)
  private
    FWindowBmp: TBitmap;
    FWindowBtnBmp: TBitmap;
    FScrollBarBmp: TBitmap;
    FButtonBmp: TBitmap;
    FCheckBmp: TBitmap;
    FRadioBmp: TBitmap;
    FComboBmp: TBitmap;
    FShadowColor: TColor;
    FLightColor: TColor;
    FMenuHotColor: TColor;
    FFaceColor: TColor;
    FActiveCaptionColor: TColor;
    FInActiveCaptionColor: TColor;
    FCaptionHeight: Integer;
    FBorderSize: Integer;
    FButtonSize: Integer;
    FButtonRight: Integer;
    FButtonTop: Integer;
    FCaptionFontSize: Integer;
    FRgnSize: Integer;
    function GetFaceColor: TColor;
    function GetLightColor: TColor;
    function GetMenuHotColor: TColor;
    function GetScrollBarBmp: TBitmap;
    function GetShadowColor: TColor;
    function GetActiveCaptionColor: TColor;
    function GetInActiveCaptionColor: TColor;
    procedure SetFaceColor(const Value: TColor);
    procedure SetLightColor(const Value: TColor);
    procedure SetMenuHotColor(const Value: TColor);
    procedure SetScrollBarBmp(const Value: TBitmap);
    procedure SetShadowColor(const Value: TColor);
    procedure SetActiveCaptionColor(const Value: TColor);
    procedure SetInActiveCaptionColor(const Value: TColor);
    procedure SetCaptionHeight(const Value: Integer);
    function GetCaptionHeight: Integer;
    procedure SetBorderSize(const Value: Integer);
    function GetBorderSize: Integer;
    procedure SetButtonSize(const Value: Integer);
    function GetButtonSize: Integer;
    procedure SetButtonRight(const Value: Integer);
    procedure SetButtonTop(const Value: Integer);
    function GetButtonRight: Integer;
    function GetButtonTop: Integer;
    function GetCaptionFontSize: Integer;
    procedure SetCaptionFontSize(const Value: Integer);
    procedure SetRgnSize(const Value: Integer);
    function GetRgnSize: Integer;

    function GetButtonBmp: TBitmap;
    function GetCheckBmp: TBitmap;
    function GetComboBmp: TBitmap;
    function GetRadioBmp: TBitmap;
    procedure SetButtonBmp(const Value: TBitmap);
    procedure SetCheckBmp(const Value: TBitmap);
    procedure SetComboBmp(const Value: TBitmap);
    procedure SetRadioBmp(const Value: TBitmap);
    function GetWindowBmp: TBitmap;
    function GetWindowBtnBmp: TBitmap;
    procedure SetWindowBmp(const Value: TBitmap);
    procedure SetWindowBtnBmp(const Value: TBitmap);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure InitConsts; virtual;
    procedure InitResources; virtual;
    procedure Clear;
  published
    property FaceColor: TColor read GetFaceColor write SetFaceColor;
    {* 窗体表面颜色 }
    property LightColor: TColor read GetLightColor write SetLightColor;
    {* 高亮颜色 }
    property ShadowColor: TColor read GetShadowColor write SetShadowColor;
    {* 阴影颜色 }
    property MenuHotColor: TColor read GetMenuHotColor write SetMenuHotColor;
    {* 菜单高亮颜色 }
    property ActiveCaptionColor: TColor read GetActiveCaptionColor write SetActiveCaptionColor;
    {* 活动窗体的标题颜色 }
    property InActiveCaptionColor: TColor read GetInActiveCaptionColor write SetInActiveCaptionColor;
    {* 非活动窗体的标题颜色 }
    property CaptionHeight: Integer read GetCaptionHeight write SetCaptionHeight;
    {* 标题栏高度 }
    property BorderSize: Integer read GetBorderSize write SetBorderSize;
    {* 窗体边框粗细 }
    property ButtonSize: Integer read GetButtonSize write SetButtonSize;
    {* 单个标题按钮的尺寸 }
    property ButtonTop: Integer read GetButtonTop write SetButtonTop;
    {* 标题按钮距顶端的尺寸 }
    property ButtonRight: Integer read GetButtonRight write SetButtonRight;
    {* 标题按钮距右边的尺寸 }
    property CaptionFontSize: Integer read GetCaptionFontSize write SetCaptionFontSize;
    {* 标题栏文字尺寸 }
    property RgnSize: Integer read GetRgnSize write SetRgnSize;
    {* 区域尺寸 }
    property WindowBmp: TBitmap read GetWindowBmp write SetWindowBmp;
    {* 标题栏背景图片 }
    property WindowBtnBmp: TBitmap read GetWindowBtnBmp write SetWindowBtnBmp;
    {* 窗体系统按钮图片 }
    property ScrollBarBmp: TBitmap read GetScrollBarBmp write SetScrollBarBmp;
    {* 滚动条的图片 }
    property ButtonBmp: TBitmap read GetButtonBmp write SetButtonBmp;
    {* 普通按钮图片 }
    property CheckBmp: TBitmap read GetCheckBmp write SetCheckBmp;
    {* 复选框图片 }
    property RadioBmp: TBitmap read GetRadioBmp write SetRadioBmp;
    {* 单选钮图片 }
    property ComboBmp: TBitmap read GetComboBmp write SetComboBmp;
    {* 下拉框图片 }
  end;

  TCnSkinXPStyle = class(TCnSkinStyle)
  public
    constructor Create(AOwner: TComponent); override;
    procedure InitConsts; override;
    procedure InitResources; override;
  published

  end;

procedure CnReadBmpFromResource(Bmp: TBitmap; const ResName: string);
{* 从资源读入需要的图片}

implementation

uses
  CnSkinTheme;

procedure CnReadBmpFromResource(Bmp: TBitmap; const ResName: string);
var
  Stream: TResourceStream;
begin
  if (Bmp <> nil) and (ResName <> '') then
  begin
    Stream := TResourceStream.Create(HInstance, ResName, 'BMP');
    try
      Bmp.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;

{ TCnSkinStyle }

procedure TCnSkinStyle.Assign(Source: TPersistent);
var
  ASkinStyle: TCnSkinStyle;
begin
  if Source is TCnSkinStyle then
  begin
    ASkinStyle := (Source as TCnSkinStyle);
    FaceColor := ASkinStyle.FaceColor;
    LightColor := ASkinStyle.LightColor;
    ShadowColor := ASkinStyle.ShadowColor;
    MenuHotColor := ASkinStyle.MenuHotColor;
    ActiveCaptionColor := ASkinStyle.ActiveCaptionColor;
    InActiveCaptionColor := ASkinStyle.InActiveCaptionColor;
    CaptionHeight := ASkinStyle.CaptionHeight;
    BorderSize := ASkinStyle.BorderSize;
    ButtonSize := ASkinStyle.ButtonSize;
    ButtonTop := ASkinStyle.ButtonTop;
    ButtonRight := ASkinStyle.ButtonRight;
    CaptionFontSize := ASkinStyle.CaptionFontSize;
    RgnSize  := ASkinStyle.RgnSize;
    WindowBmp.Assign(ASkinStyle.WindowBmp);
    WindowBtnBmp.Assign(ASkinStyle.WindowBtnBmp);
    ScrollBarBmp.Assign(ASkinStyle.ScrollBarBmp);
    ButtonBmp.Assign(ASkinStyle.ButtonBmp);
    CheckBmp.Assign(ASkinStyle.CheckBmp);
    RadioBmp.Assign(ASkinStyle.RadioBmp);
    ComboBmp.Assign(ASkinStyle.ComboBmp);
  end
  else
    inherited;
end;

procedure TCnSkinStyle.Clear;
begin

end;

constructor TCnSkinStyle.Create(AOwner: TComponent);
begin
  inherited;
  FWindowBmp := TBitmap.Create;
  FWindowBtnBmp := TBitmap.Create;
  FScrollBarBmp := TBitmap.Create;
  FButtonBmp := TBitmap.Create;
  FCheckBmp := TBitmap.Create;
  FRadioBmp := TBitmap.Create;
  FComboBmp := TBitmap.Create;
  
  InitConsts;
  InitResources;

  CnSkinThemes.AddSkin(Self);
end;

destructor TCnSkinStyle.Destroy;
begin
  FComboBmp.Free;
  FRadioBmp.Free;
  FCheckBmp.Free;
  FButtonBmp.Free;
  FScrollBarBmp.Free;
  FWindowBtnBmp.Free;
  FWindowBmp.Free;
  inherited;
end;

function TCnSkinStyle.GetActiveCaptionColor: TColor;
begin
  Result := FActiveCaptionColor;
end;

function TCnSkinStyle.GetBorderSize: Integer;
begin
  Result := FBorderSize;
end;

function TCnSkinStyle.GetButtonBmp: TBitmap;
begin
  Result := FButtonBmp;
end;

function TCnSkinStyle.GetButtonRight: Integer;
begin
  Result := FButtonRight;
end;

function TCnSkinStyle.GetButtonSize: Integer;
begin
  Result := FButtonSize;
end;

function TCnSkinStyle.GetButtonTop: Integer;
begin
  Result := FButtonTop;
end;

function TCnSkinStyle.GetCaptionFontSize: Integer;
begin
  Result := FCaptionFontSize;
end;

function TCnSkinStyle.GetCaptionHeight: Integer;
begin
  Result := FCaptionHeight;
end;

function TCnSkinStyle.GetCheckBmp: TBitmap;
begin
  Result := FCheckBmp;
end;

function TCnSkinStyle.GetComboBmp: TBitmap;
begin
  Result := FComboBmp;
end;

function TCnSkinStyle.GetFaceColor: TColor;
begin
  Result := FFaceColor;
end;

function TCnSkinStyle.GetInActiveCaptionColor: TColor;
begin
  Result := FInActiveCaptionColor;
end;

function TCnSkinStyle.GetLightColor: TColor;
begin
  Result := FLightColor;
end;

function TCnSkinStyle.GetMenuHotColor: TColor;
begin
  Result := FMenuHotColor;
end;

function TCnSkinStyle.GetRadioBmp: TBitmap;
begin
  Result := FRadioBmp;
end;

function TCnSkinStyle.GetRgnSize: Integer;
begin
  Result := FRgnSize;
end;

function TCnSkinStyle.GetScrollBarBmp: TBitmap;
begin
  Result := FScrollBarBmp;
end;

function TCnSkinStyle.GetShadowColor: TColor;
begin
  Result := FShadowColor;
end;

function TCnSkinStyle.GetWindowBmp: TBitmap;
begin
  Result := FWindowBmp;
end;

function TCnSkinStyle.GetWindowBtnBmp: TBitmap;
begin
  Result := FWindowBtnBmp;
end;

procedure TCnSkinStyle.InitConsts;
begin
  // 初始化变量
  FShadowColor := clBtnShadow;
  FLightColor := clBtnHighLight;
  FFaceColor := clBtnFace;
  FMenuHotColor := clMenuHighlight;

  FCaptionHeight := 26;
  FBorderSize := 0;
  FButtonSize := 16;
  FButtonRight := 5;
  FButtonTop := 3;
  FCaptionFontSize := 11;
  FRgnSize := 0;
end;

procedure TCnSkinStyle.InitResources;
begin
  // 基类啥都不干
end;

procedure TCnSkinStyle.SetActiveCaptionColor(const Value: TColor);
begin
  if FActiveCaptionColor <> Value then
  begin
    FActiveCaptionColor := Value;
  end;
end;

procedure TCnSkinStyle.SetBorderSize(const Value: Integer);
begin
  FBorderSize := Value;
end;

procedure TCnSkinStyle.SetButtonBmp(const Value: TBitmap);
begin
  FButtonBmp.Assign(Value);
end;

procedure TCnSkinStyle.SetButtonRight(const Value: Integer);
begin
  FButtonRight := Value;
end;

procedure TCnSkinStyle.SetButtonSize(const Value: Integer);
begin
  FButtonSize := Value;
end;

procedure TCnSkinStyle.SetButtonTop(const Value: Integer);
begin
  FButtonTop := Value;
end;

procedure TCnSkinStyle.SetCaptionFontSize(const Value: Integer);
begin
  FCaptionFontSize := Value;
end;

procedure TCnSkinStyle.SetCaptionHeight(const Value: Integer);
begin
  FCaptionHeight := Value;
end;

procedure TCnSkinStyle.SetCheckBmp(const Value: TBitmap);
begin
  FCheckBmp.Assign(Value);
end;

procedure TCnSkinStyle.SetComboBmp(const Value: TBitmap);
begin
  FComboBmp.Assign(Value);
end;

procedure TCnSkinStyle.SetFaceColor(const Value: TColor);
begin
  FFaceColor := Value;
end;

procedure TCnSkinStyle.SetInActiveCaptionColor(const Value: TColor);
begin
  if FInActiveCaptionColor <> Value then
  begin
    FInActiveCaptionColor := Value;
  end;
end;

procedure TCnSkinStyle.SetLightColor(const Value: TColor);
begin
  FLightColor := Value;
end;

procedure TCnSkinStyle.SetMenuHotColor(const Value: TColor);
begin
  FMenuHotColor := Value;
end;

procedure TCnSkinStyle.SetRadioBmp(const Value: TBitmap);
begin
  FRadioBmp.Assign(Value);
end;

procedure TCnSkinStyle.SetRgnSize(const Value: Integer);
begin
  FRgnSize := Value;
end;

procedure TCnSkinStyle.SetScrollBarBmp(const Value: TBitmap);
begin
  FScrollBarBmp.Assign(Value);
end;

procedure TCnSkinStyle.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
end;

procedure TCnSkinStyle.SetWindowBmp(const Value: TBitmap);
begin
  FWindowBmp.Assign(Value);
end;

procedure TCnSkinStyle.SetWindowBtnBmp(const Value: TBitmap);
begin
  FWindowBtnBmp.Assign(Value);
end;

{ TCnSkinXPStyle }

constructor TCnSkinXPStyle.Create(AOwner: TComponent);
begin
  inherited;

end;

procedure TCnSkinXPStyle.InitConsts;
begin
  inherited;
  FCaptionHeight := 30;
  FBorderSize := 4;
  FButtonRight := 5;
  FButtonTop := 5;
  FRgnSize := 14;
end;

procedure TCnSkinXPStyle.InitResources;
begin
  inherited;

end;

end.
