{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2010 CnPack 开发组                       }
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

  SCnLangMgrName = '多语管理器';
  SCnLangMgrComment = '多语管理器组件';
  SCnIniLangStorageName = 'INI 多语存储组件';
  SCnIniLangStorageComment = 'INI 多语存储组件';
  SCnHashLangStorageName = '散列文本多语存储组件';
  SCnHashLangStorageComment = '散列文本多语存储组件，有较快的速度';
  SCnLangTranslatorName = '翻译器组件';
  SCnLangTranslatorComment = '翻译器组件，用来启动翻译管理器';

  SCnMultiInstanceError = '只允许创建一个 %s 实例！';

  SCnLoadLangFileError = '不能装载语言文件: %s';
  SCnInvalidLanguageIDError = '%d 不是合法的 Language ID';
  SCnErrorInCheckingLanguage = '系统语言检测出错，请尝试关闭 DEP';

  SCnMultiLangPaletteName = 'CnPack MultiLang';
  SCnFormTranslationManager = '&T.翻译管理器 ...';
  SCnEditLanguageStrings = '编辑语言条目...';
  SCnErrorCaption = '错误';
  SCnErrorNoLangManager = '未找到多语言管理器，请先创建多语言管理器实例';
  SCnErrorNoStorage = '未找到多语言存储实例，请先创建多语言存储实例';
  SCnLanguagePath = '请选择语言文件的存储路径';
  SCnCanNotCreateDir = '无法创建目录';

  // 翻译管理器窗体字符串
  SCnactAddLineCaption = '加行';
  SCnactAddLineHint = '在字符串列表末尾增加一行';
  SCnactClearCaption = '清空';
  SCnactClearHint = '删除当前所有的翻译语言条目';
  SCnactCloseCaption = '关闭';
  SCnactCollectFormCaption = '汇总';
  SCnactCollectFormHint = '自动遍历整个项目，生成所有 Form 的翻译字符串列表';
  SCnactFilterCaption = '过滤';
  SCnactFilterHint = '设置生成翻译字符串列表时需要过滤的属性';
  SCnactCopyStrsCaption = '复制';
  SCnactCopyStrsHint = '将所有原文复制到翻译后文本';
  SCnactDelBlankCaption = '删空';
  SCnactDelBlankHint = '删除所有字符串值为空或者只包含数字和符号的行';
  SCnactDelLineCaption = '删行';
  SCnactDelLineHint = '删除翻译字符串列表的当前行';
  SCnactGenStrsCaption = '生成';
  SCnactGenStrsHint = '生成当前 Form 的待翻译字符串列表';
  SCnactSaveCurItemCaption = '保存';
  SCnactSaveCurItemHint = '保存当前语言条目的翻译字符串列表';
  SCnactUpdateStrsCaption = '更新';
  SCnactUpdateStrsHint = '更新当前待翻译字符串列表';
  SCnCaption = '窗体翻译管理器';
  SCnlbl1Caption = '文件名：';
  SCnlblIndexCaption = '序号：';
  SCnlblLangIDCaption = '语言 ID：';
  SCnlblLangNameCaption = '语言名：';
  SCnStringGridHint = '翻译文字编辑区';
  SCntvStoragesHint = '语言条目管理区';

  SCnStringGridCells10 = '待翻译条目';
  SCnStringGridCells20 = '原文';
  SCnStringGridCells30 = '翻译后文本';

  // 过滤设置窗体字符串
  SCnFilterFrmCaption = '过滤设置';
  SCnFilterCaption = '只为以下属性生成翻译字符串：';
  SCnOKCaption     = '确定(&O)';
  SCnCancelCaption = '取消(&C)';

  SCnWarningCaption = '警告';
  SCnLangInvalidLine = '多语条目中发现非法行或空行，将被删除。';

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
