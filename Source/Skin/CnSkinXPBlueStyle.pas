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

unit CnSkinXPBlueStyle;

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics, Forms, Controls,
  CnSkinStyle;

type
  TCnSkinXPBlueStyle = class(TCnSkinXPStyle)
  public
    procedure InitConsts; override;
    procedure InitResources; override;
  end;

implementation

{$R CnSkinXPBlueStyle.res}

{ TCnSkinXPBlueStyle }

const
  SCN_SKIN_XPBLUE_BACKGROUND      = 'CN_SKIN_XPBLUE_BACKGROUND';
  SCN_SKIN_XPBLUE_BUTTON          = 'CN_SKIN_XPBLUE_BUTTON';
  SCN_SKIN_XPBLUE_CHECKBOX        = 'CN_SKIN_XPBLUE_CHECKBOX';
  SCN_SKIN_XPBLUE_COMBO           = 'CN_SKIN_XPBLUE_COMBO';
  SCN_SKIN_XPBLUE_RADIO           = 'CN_SKIN_XPBLUE_RADIO';
  SCN_SKIN_XPBLUE_SCROLLBAR       = 'CN_SKIN_XPBLUE_SCROLLBAR';
  SCN_SKIN_XPBLUE_WINDOW          = 'CN_SKIN_XPBLUE_WINDOW';
  SCN_SKIN_XPBLUE_WINDOW_BUTTON   = 'CN_SKIN_XPBLUE_WINDOW_BUTTON';

procedure TCnSkinXPBlueStyle.InitConsts;
begin
  inherited;
  FaceColor := $DBE9EC;
  InactiveCaptionColor := $DDDDDD;
  ActiveCaptionColor := $EEEEEE;
  ShadowColor := $ADB397;
  LightColor := $F5F3F6;
  MenuHotColor := $D58656;
end;

procedure TCnSkinXPBlueStyle.InitResources;
begin
  inherited;
  CnReadBmpFromResource(WindowBmp, SCN_SKIN_XPBLUE_WINDOW);
  CnReadBmpFromResource(WindowBtnBmp, SCN_SKIN_XPBLUE_WINDOW_BUTTON);
  CnReadBmpFromResource(ButtonBmp, SCN_SKIN_XPBLUE_BUTTON);
  CnReadBmpFromResource(RadioBmp, SCN_SKIN_XPBLUE_RADIO);
  CnReadBmpFromResource(CheckBmp, SCN_SKIN_XPBLUE_CHECKBOX);
  CnReadBmpFromResource(ComboBmp, SCN_SKIN_XPBLUE_COMBO);
  CnReadBmpFromResource(ScrollBarBmp, SCN_SKIN_XPBLUE_SCROLLBAR);
  if not WindowBtnBmp.Empty then
    ButtonSize := WindowBtnBmp.Width div 4;
end;

end.