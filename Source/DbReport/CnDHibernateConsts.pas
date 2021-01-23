{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
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

unit CnDHibernateConsts; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate基础库
* 单元名称：常量库
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.08.23 V1.8
*               移植到 Delphi2009
*           2006.09.04 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Classes, SysUtils;

const
  DH_VERSION = '1.8 (D6~D2009)';

  // cnt: empty string
  DH_NULL_VAR = ''; 

  // cnt: components registry panel
  DH_REG_PANEL = 'CnPack DHibernate'; 

  // fmt: class name
  DH_CLASS_NAME = 'T%s'; 

  // fmt: get record T-sql with a param
  DH_GET_RECORD = 'select * from %s where %s=:%s'; 

  // fmt: id-generator T-sql
  DH_ID_GENERATOR = 'select * from %s where %s=''%s'''; 

  // fmt: normal selection T-sql
  // for events, add "and" after this
  DH_SELECT = 'select * from %s where 1=1'; 

  // fmt: nnormal deletion T-sql
  // for events, add "and" after this
  DH_DELETE_RECORD = 'delete from %s where 1=1'; 

  // fmt: events
  DH_SEARCH_FILTER = ' and %s=''%s'''; 

  // msg: no record found
  DH_MEM_NO_RECORDS = 'No data found';

  // msg: no fields defined
  DH_INVALID_FIELDS = 'No fields defined'; 

  // cnt: the formatter of date in kinds of databases
  DH_DATE_FMT_STD_16 = '''"''mm''/''dd''/''yyyy''"''';  {"mm/dd/yyyy"}
  DH_DATE_FMT_STD_32 = '''''''dd/mm/yyyy''''''';  {'dd/mm/yyyy'}
  DH_DATE_FMT_ORACLE = '"TO_DATE(''"dd/mm/yyyy"'', ''DD/MM/YYYY'')"';
  DH_DATE_FMT_INTERBASE = '"CAST(''"mm"/"dd"/"yyyy"'' AS DATE)"';
  DH_DATE_FMT_MSSQL = '"CONVERT(datetime, ''"mm"/"dd"/"yyyy"'', 103)"'; 

  // cnt: the true event
  DH_TRUE_EXPRESS = '1=1'; 

  // cnt: the extension ids
  DH_MAX_EXT_STR_ID = 61300;
  DH_CONFIRM_SAVE = DH_MAX_EXT_STR_ID - 126;
  DH_DATABASE_NAME = DH_MAX_EXT_STR_ID - 127; 

  // cnt: strings and formatters
  DH_FLAGS = 'Flags';
  DH_SHOW_CMD = 'ShowCmd';
  DH_MIN_MAX_POS = 'MinMaxPos';
  DH_NORM_POS = 'NormPos';
  DH_PIXELS = 'PixelsPerInch';
  DH_MDI_CHILD = 'MDI Children';
  DH_LIST_COUNT = 'Count';
  DH_ITEM = 'Item%d'; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{$ENDIF SUPPORT_ADO}
end.
