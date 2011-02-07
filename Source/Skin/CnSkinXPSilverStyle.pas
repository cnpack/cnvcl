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

unit CnSkinXPSilverStyle;

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics, Forms, Controls,
  CnSkinStyle;

type
  TCnSkinXPSilverStyle = class(TCnSkinXPStyle)
  public
    procedure InitConsts; override;
    procedure InitResources; override;  
  end;

implementation

{$R CnSkinXPSilverStyle.res}

const
  SCN_SKIN_XPSILVER_BACKGROUND      = 'CN_SKIN_XPSILVER_BACKGROUND';
  SCN_SKIN_XPSILVER_BUTTON          = 'CN_SKIN_XPSILVER_BUTTON';
  SCN_SKIN_XPSILVER_CHECKBOX        = 'CN_SKIN_XPSILVER_CHECKBOX';
  SCN_SKIN_XPSILVER_COMBO           = 'CN_SKIN_XPSILVER_COMBO';
  SCN_SKIN_XPSILVER_RADIO           = 'CN_SKIN_XPSILVER_RADIO';
  SCN_SKIN_XPSILVER_SCROLLBAR       = 'CN_SKIN_XPSILVER_SCROLLBAR';
  SCN_SKIN_XPSILVER_WINDOW          = 'CN_SKIN_XPSILVER_WINDOW';
  SCN_SKIN_XPSILVER_WINDOW_BUTTON   = 'CN_SKIN_XPSILVER_WINDOW_BUTTON';

{ TCnSkinXPSilverStyle }

procedure TCnSkinXPSilverStyle.InitConsts;
begin
  inherited;
  FaceColor := $DBE9EC;
  InactiveCaptionColor := $EEEEEE;
  ActiveCaptionColor := $F0F0F0;
  ShadowColor := $ADB397;
  LightColor := $F5F3F6;
  MenuHotColor := $66BEA4;
end;

procedure TCnSkinXPSilverStyle.InitResources;
begin
  inherited;
  CnReadBmpFromResource(WindowBmp, SCN_SKIN_XPSILVER_WINDOW);
  CnReadBmpFromResource(WindowBtnBmp, SCN_SKIN_XPSILVER_WINDOW_BUTTON);
  CnReadBmpFromResource(ButtonBmp, SCN_SKIN_XPSILVER_BUTTON);
  CnReadBmpFromResource(RadioBmp, SCN_SKIN_XPSILVER_RADIO);
  CnReadBmpFromResource(CheckBmp, SCN_SKIN_XPSILVER_CHECKBOX);
  CnReadBmpFromResource(ComboBmp, SCN_SKIN_XPSILVER_COMBO);
  CnReadBmpFromResource(ScrollBarBmp, SCN_SKIN_XPSILVER_SCROLLBAR);
  if not WindowBtnBmp.Empty then
    ButtonSize := WindowBtnBmp.Width div 4;
end;

end.