{******************************************************************************}
{                                                                              }
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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

unit CnGraphConsts;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：资源字符串定义单元
* 单元作者：CnPack 开发组
* 备    注：该单元定义了界面类用到的资源字符串
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2002.02.15 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

resourcestring

  // CnErrorProvider
  SCnErrorProviderName = '岿粇矗ボ舱ン';
  SCnErrorProviderComment = '岿粇矗ボ舱ン';

  // CnHint
  SCnHintName = 'Hint舱ン';
  SCnHintComment = 'Hint舱ン';

  // CnHintWindow
  SCnHintWindowName = 'HintWindow舱ン';
  SCnHintWindowComment = 'HintWindow舱ン';

var
  SCnAOCaptionColor: string = '肅︹(&C)';
  SCnAOCaptionFont: string = '砰(&F)';
  SCnAOCaptionOption: string = '砞竚(&O)';

  SCreateDCFromEmptyBmp: string = 'ぃ瓜だ皌 DC';
  SAllocDIBFail: string = '承 DIB 癸钩琡ア毖';
  SCreateDCFail: string = '承 DC ア毖';
  SSelectBmpToDCFail: string = '礚猭盢瓜癸钩匡拒 DC い';
  SBitmapIsEmpty: string = '礚猭砐拜瓜钩计沮';
  SInvalidPixel: string = '礚钩翴 x: %d, y: %d';
  SInvalidPixelF: string = '礚钩翴 x: %f, y: %f';
  SInvalidScanLine: string = '礚苯核絬 Row: %d';
  SInvalidAlphaBitmap: string = ' Alpha 睼矪瞶いノ睼瓜钩ゲ斗籔讽玡瓜钩璓';
  SInvalidForeBitmap: string = '砰籜狾睼矪瞶い玡春瓜籔籜狾ゲ斗璓';
  SReadBmpError: string = '弄瓜计沮岿';

  // CnSkinMagic 钵盽獺
  SCNE_WRITEVMTFAILED: string = '礚猭э糶 VMT 计沮CnSkinMagic 摸爹ア毖';
  SCNE_FINDCLASSDATAFAILED: string = '礚猭莉眔 CnSkingMagic 摸獺';
  SCNE_REGISTERMESSAGEFAILED: string = '礚猭蠢 CnSkinMagic 爹怠';

implementation

end.
