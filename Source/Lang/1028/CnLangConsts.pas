{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2014 CnPack 开发组                       }
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

unit CnLangConsts;
{* |<PRE>
================================================================================
* 软件名称：CnPack 多语包
* 单元名称：多语包常量单元
* 单元作者：CnPack开发组 刘啸 (liuxiao@cnpack.org)
* 备    注：
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2003.08.20 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes;

resourcestring

  SCnLangMgrName = '粂恨瞶竟';
  SCnLangMgrComment = '粂恨瞶竟舱ン';
  SCnIniLangStorageName = 'INI 粂纗舱ン';
  SCnIniLangStorageComment = 'INI 粂纗舱ン';
  SCnHashLangStorageName = '床ゅセ粂纗舱ン';
  SCnHashLangStorageComment = '床ゅセ粂纗舱ンΤ耕е硉';
  SCnLangTranslatorName = '陆亩竟舱ン';
  SCnLangTranslatorComment = '陆亩竟舱ンノㄓ币笆陆亩恨瞶竟';

  SCnMultiInstanceError = 'す砛承 %s 龟ㄒ';

  SCnLoadLangFileError = 'ぃ杆更粂ēゅン: %s';
  SCnInvalidLanguageIDError = '%d ぃ琌猭 Language ID';
  SCnErrorInCheckingLanguage = '╰参粂ē浪代岿叫沽刚闽超 DEP';

  SCnMultiLangPaletteName = 'CnPack MultiLang';
  SCnFormTranslationManager = '&T.陆亩恨瞶竟 ...';
  SCnEditLanguageStrings = '絪胯粂ē兵ヘ...';
  SCnLangExtractStrings = '粂ē才﹃芭禟狾(&T)';
  SCnErrorCaption = '岿粇';
  SCnErrorNoLangManager = 'ゼт粂ē恨瞶竟叫承粂ē恨瞶竟龟ㄒ';
  SCnErrorNoStorage = 'ゼт粂ē纗龟ㄒ叫承粂ē纗龟ㄒ';
  SCnLanguagePath = '叫匡拒粂ēゅン纗隔畖';
  SCnCanNotCreateDir = '礚猭承ヘ魁';

  // 陆亩恨瞶竟怠砰才﹃
  SCnactAddLineCaption = '︽';
  SCnactAddLineHint = '才﹃ソЮ糤︽';
  SCnactClearCaption = '睲';
  SCnactClearHint = '埃讽玡┮Τ陆亩粂ē兵ヘ';
  SCnactCloseCaption = '闽超';
  SCnactCollectFormCaption = '蹲羆';
  SCnactCollectFormHint = '笆筂菌俱兜ヘネΘ┮Τ Form 陆亩才﹃';
  SCnactFilterCaption = '筁耾';
  SCnactFilterHint = '砞竚ネΘ陆亩才﹃惠璶筁耾妮┦';
  SCnactCopyStrsCaption = '狡籹';
  SCnactCopyStrsHint = '盢┮Τゅ狡籹陆亩ゅセ';
  SCnactDelBlankCaption = '';
  SCnactDelBlankHint = '埃┮Τ才﹃┪计㎝才腹︽';
  SCnactDelLineCaption = '︽';
  SCnactDelLineHint = '埃陆亩才﹃讽玡︽';
  SCnactGenStrsCaption = 'ネΘ';
  SCnactGenStrsHint = 'ネΘ讽玡 Form 陆亩才﹃';
  SCnactSaveCurItemCaption = '玂';
  SCnactSaveCurItemHint = '玂讽玡粂ē兵ヘ陆亩才﹃';
  SCnactUpdateStrsCaption = '穝';
  SCnactUpdateStrsHint = '穝讽玡陆亩才﹃';
  SCnCaption = '怠砰陆亩恨瞶竟';
  SCnlbl1Caption = 'ゅン';
  SCnlblIndexCaption = '腹';
  SCnlblLangIDCaption = '粂ē ID';
  SCnlblLangNameCaption = '粂ē';
  SCnStringGridHint = '陆亩ゅ絪胯跋';
  SCntvStoragesHint = '粂ē兵ヘ恨瞶跋';

  SCnStringGridCells10 = '陆亩兵ヘ';
  SCnStringGridCells20 = 'ゅ';
  SCnStringGridCells30 = '陆亩ゅセ';

  // 筁耾砞竚怠砰才﹃
  SCnFilterFrmCaption = '筁耾砞竚';
  SCnFilterCaption = '妮┦ネΘ陆亩才﹃';
  SCnOKCaption     = '絋﹚(&O)';
  SCnCancelCaption = '(&C)';

  SCnWarningCaption = '牡';
  SCnLangInvalidLine = '粂兵ヘい祇瞷獶猭︽┪︽盢砆埃';

{ SCnMultiInstanceError = 'Only one %s Instance is allowed!';

  SCnLoadLangFileError = 'Can''t Load the language File: %s';
  SCnInvalidLanguageIDError = 'Invalid Language ID Error';

  SCnMultiLangPaletteName = 'CnPack MultiLang';
  SCnCreateLangFile = '&Create Language files';
  SCnUpdateLangFile = '&Update Language files';
  SCnFormTranslationManager = 'Form &Translation Manager';    }

const
  SCnCRLF = #13#10;
  SCnBR = '<BR>';
  SCnCommentChar1 = ';';
  SCnCommentChar2 = '#';
  SCnCommentChar3 = '/';

implementation

end.
