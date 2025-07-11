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

unit CnGraphRegister;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：界面控件包注册单元
* 单元作者：CnPack开发组
* 备    注：
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2002.03.30 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  {$IFNDEF FPC}
  {$IFDEF COMPILER6_UP} DesignIntf, DesignEditors, {$ELSE} DsgnIntf, {$ENDIF}
  {$ENDIF}
  CnConsts, {$IFNDEF FPC} CnGraphics, CnImage, CnGraphPropEditors, CnCheckTreeView,
  CnWizardImage, CnShellCtrls, {$ENDIF} CnWaterImage, CnPanel, CnEdit, CnSpin,
  {$IFNDEF FPC} CnAOTreeView, CnAACtrls,
  CnAAFont, CnAAFontEditor, CnAAFontDialog, CnTabSet, CnButtonEdit,
  CnSkinMagic, CnButtons, CnHint, CnGauge, CnListBox, CnColorGrid,
  CnMonthCalendar, CnValidateImage, CnErrorProvider, CnLED, {$ENDIF} CnHexEditor;

procedure Register;
{* 控件、组件编辑器、属性编辑器注册过程}

implementation

procedure Register;
begin
  //RegisterComponents(SCnGraphicPalette, [TCnPaintBox]);
  //RegisterComponents(SCnGraphicPalette, [TCnImage]);
{$IFNDEF FPC}
  RegisterComponents(SCnGraphicPalette, [TCnButton, TCnBitBtn, TCnSpeedButton]);
{$ENDIF}
  RegisterComponents(SCnGraphicPalette, [TCnPanel, TCnEdit, TCnSpinEdit]);
{$IFNDEF FPC}
  RegisterComponents(SCnGraphicPalette, [TCnListBox]);
  RegisterComponents(SCnGraphicPalette, [TCnTabSet, TCnButtonEdit]);
  RegisterComponents(SCnGraphicPalette, [TCnCheckTreeView]);
  RegisterComponents(SCnGraphicPalette, [TCnAOTreeView]);
  RegisterComponents(SCnGraphicPalette, [TCnWizardImage]);
  RegisterComponents(SCnGraphicPalette, [TCnValidateImage]);
  RegisterComponents(SCnGraphicPalette, [TCnGauge]);
{$ENDIF}
  RegisterComponents(SCnGraphicPalette, [TCnWaterImage]);
  RegisterComponents(SCnGraphicPalette, [TCnHexEditor]);
{$IFNDEF FPC}
  RegisterComponents(SCnGraphicPalette, [TCnMonthCalendar]);
  RegisterComponents(SCnGraphicPalette, [TCnColorGrid]);
  RegisterComponents(SCnGraphicPalette, [TCnErrorProvider]);
  RegisterComponents(SCnGraphicPalette, [TCnLEDText]);
  RegisterComponents(SCnGraphicPalette, [TCnSkinMagic]);
  RegisterComponents(SCnGraphicPalette, [TCnHint, TCnHintWindow]);
  //RegisterComponents(SCnGraphicPalette, [TCnCheckGroupBox]);
  //RegisterPropertyEditor(TypeInfo(TCnBitmap), nil, '', TCnBitmapProperty);
  RegisterComponents(SCnGraphicPalette, [TCnShellTreeView, TCnShellListView,
    TCnShellChangeNotifier]);

  RegisterComponents(SCnGraphicPalette, [TCnAAFadeText, TCnAALabel, 
    TCnAALinkLabel, TCnAAText, TCnAAScrollText, TCnAAMarqueeText]);
  RegisterComponents(SCnGraphicPalette, [TCnAAFontDialog]);
  RegisterPropertyEditor(TypeInfo(TCnAAEffect), nil, '', TCnAAEffectProperty);
  RegisterComponentEditor(TCnAALabel, TCnAALabelEditor);
{$ENDIF}
end;

end.
