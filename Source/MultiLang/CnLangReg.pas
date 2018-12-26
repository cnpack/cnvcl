{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2018 CnPack 开发组                       }
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

unit CnLangReg;
{* |<PRE>
================================================================================
* 软件名称：CnPack 多语包
* 单元名称：多语包组件注册单元
* 单元作者：CnPack开发组 刘啸 (liuxiao@cnpack.org)
* 备    注：该单元实现了多语包的组件和编辑器注册
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2003.08.20 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  SysUtils, Classes,
  CnLangCollection, CnLangConsts, CnLangMgr, CnLangStorage, CnLangTranslator,
  CnLangEditors, CnTransEditor, CnHashLangStorage, CnIniLangFileStorage;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CnMutiLang', [TCnLangManager, TCnLangTranslator,
    TCnHashLangFileStorage, TCnIniLangFileStorage]);
  RegisterPropertyEditor(TypeInfo(LongWord), TCnLanguageItem, 'LanguageID',
    TCnLanguageItemProperty);
  RegisterPropertyEditor(TypeInfo(Integer), TCnLangManager, 'CurrentLanguageIndex',
    TCnLangManagerProperty);
  RegisterPropertyEditor(TypeInfo(WideString), TCnCustomLangFileStorage, 'LanguagePath',
    TCnLanguagePathProperty);
  RegisterComponentEditor(TCnLangTranslator, TCnTranslatorEditor);
{$IFDEF DELPHI}
  RegisterComponentEditor(TCnCustomLangStorage, TCnStorageEditor);
{$ENDIF}
{$IFDEF COMPILER6_UP}
  RegisterSelectionEditor(TComponent, TCnLangDesignerEditor);
{$ENDIF}
end;

end.
