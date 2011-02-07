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

unit CnPODOConsts;
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate PODO 生成工具
* 单元名称：PODO 生成工具
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnPODOConsts.pas,v 1.2 2009/01/02 08:27:38 liuxiao Exp $
* 修改记录：2008.08.23 V1.8
*               移植到 Delphi2009
*           2006.09.04 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

const
  PODO_CONNECT_SUCCESS = '连接数据库成功！';
  PODO_CONNECT_FAIL = '连接数据库失败！';
  PODO_SAVE_SUCCESS = '保存文件成功！';
  PODO_SAVE_FAIL = '保存文件失败！';
  PODO_MSGBOX_TITLE = '提示';


  PODO_DATA_TYPE_STRING = 'String';
  PODO_DATA_TYPE_INTEGER = 'Integer';
  PODO_DATA_TYPE_FLOAT = 'Real';
  PODO_DATA_TYPE_DATETIME = 'TDateTime';
  PODO_DATA_TYPE_BOOLEAN = 'Boolean';
  PODO_DATA_TYPE_VARIANT = 'Variant';

  STR_SPACE = ' ';
  STR_NULL = #0;

  FILTER_PODO = '%s.pas';
  FILTER_FILE_NAME = 'PODO_%s';

  PREVIEW_UNIT_HEAD_COMMENT = '(* This unit is created by PODO generator *)';
  PREVIEW_UNIT_NAME = 'unit PODO_%s;';
  PREVIEW_UNIT_MPLUS = '{$M+}';
  PREVIEW_UNIT_INTERFACE = 'interface';
  PREVIEW_UNIT_USES = 'uses';
  PREVIEW_UNIT_BASE_UNIT = '  Classes, SysUtils, CnDHibernateBase;';
  PREVIEW_UNIT_TYPE = 'type';
  PREVIEW_UNIT_CLASS_NAME = '  T%s = class(TCnDHibernateBase)';
  PREVIEW_UNIT_PRIVATE = '  private';
  PREVIEW_UNIT_PRIVATE_ATTR = '    F%s : %s;';
  PREVIEW_UNIT_PUBLISHED = '  published';
  PREVIEW_UNIT_PUBLISHED_ATTR = '    property %s : %s read F%s write F%s;';
  PREVIEW_UNIT_END = '  end;';
  PREVIEW_UNIT_IMPLEMENTATION = 'implementation';
  PREVIEW_UNIT_INITIALIZATION = 'initialization';
  PREVIEW_UNIT_REGISTER_CLASS = '  RegisterClass(T%s);';
  PREVIEW_UNIT_FULL_END = 'end.';

implementation

end.
