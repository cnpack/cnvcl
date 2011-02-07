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

unit CnSkinXPGreenStyle;

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics, Forms, Controls,
  CnSkinStyle;

type
  TCnSkinXPGreenStyle = class(TCnSkinXPStyle)
  public
    procedure InitConsts; override;
    procedure InitResources; override;
  end;

implementation

{$R CnSkinXPGreenStyle.res}

const
  SCN_SKIN_XPGREEN_BACKGROUND      = 'CN_SKIN_XPGREEN_BACKGROUND';
  SCN_SKIN_XPGREEN_BUTTON          = 'CN_SKIN_XPGREEN_BUTTON';
  SCN_SKIN_XPGREEN_CHECKBOX        = 'CN_SKIN_XPGREEN_CHECKBOX';
  SCN_SKIN_XPGREEN_COMBO           = 'CN_SKIN_XPGREEN_COMBO';
  SCN_SKIN_XPGREEN_RADIO           = 'CN_SKIN_XPGREEN_RADIO';
  SCN_SKIN_XPGREEN_SCROLLBAR       = 'CN_SKIN_XPGREEN_SCROLLBAR';
  SCN_SKIN_XPGREEN_WINDOW          = 'CN_SKIN_XPGREEN_WINDOW';
  SCN_SKIN_XPGREEN_WINDOW_BUTTON   = 'CN_SKIN_XPGREEN_WINDOW_BUTTON';

{ TCnSkinXPGreenStyle }

procedure TCnSkinXPGreenStyle.InitConsts;
begin
  inherited;
  FaceColor := $E7DFE7;
  InactiveCaptionColor := $AAAAAA;
  ActiveCaptionColor := $333333;
  ShadowColor := $B5AEA5;
  LightColor := $F7EFF7;
  MenuHotColor := $C6B6BD;
end;

procedure TCnSkinXPGreenStyle.InitResources;
begin
  inherited;
  CnReadBmpFromResource(WindowBmp, SCN_SKIN_XPGREEN_WINDOW);
  CnReadBmpFromResource(WindowBtnBmp, SCN_SKIN_XPGREEN_WINDOW_BUTTON);
  CnReadBmpFromResource(ButtonBmp, SCN_SKIN_XPGREEN_BUTTON);
  CnReadBmpFromResource(RadioBmp, SCN_SKIN_XPGREEN_RADIO);
  CnReadBmpFromResource(CheckBmp, SCN_SKIN_XPGREEN_CHECKBOX);
  CnReadBmpFromResource(ComboBmp, SCN_SKIN_XPGREEN_COMBO);
  CnReadBmpFromResource(ScrollBarBmp, SCN_SKIN_XPGREEN_SCROLLBAR);
  if not WindowBtnBmp.Empty then
    ButtonSize := WindowBtnBmp.Width div 4;
end;

end.