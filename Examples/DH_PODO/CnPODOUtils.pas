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

unit CnPODOUtils;
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate PODO 生成工具
* 单元名称：PODO 生成工具
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnPODOUtils.pas,v 1.2 2009/01/02 08:27:38 liuxiao Exp $
* 修改记录：2008.08.23 V1.8
*               移植到 Delphi2009
*           2006.09.04 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils, DB, CnPODOConsts;

{ change dataType to string }
function DataTypeToString(dataType: TFieldType): string;

{ delete the spaces in string }
function DeleteSpaces(const Str: string): string;

{ change the first letter to upperCase }
function UpperCaseFirst(const Str: string):string;

implementation

function DataTypeToString(dataType: TFieldType): string;
begin
  case dataType of
    ftWideString, ftString, ftFixedChar {$IFDEF BDS2006_UP}, ftFixedWideChar{$ENDIF}: Result := PODO_DATA_TYPE_STRING;
    ftSmallint, ftInteger, ftWord, ftLargeint {$IFDEF BDS2006_UP}, ftOraInterval{$ENDIF}, ftAutoInc: Result := PODO_DATA_TYPE_INTEGER;
    ftBoolean: Result := PODO_DATA_TYPE_BOOLEAN;
    ftFloat, ftCurrency, ftBCD{$IFDEF DELPHI6_UP}, ftFMTBcd{$ENDIF}: Result := PODO_DATA_TYPE_FLOAT;
    ftDate, ftTime, ftDateTime{$IFDEF DELPHI6_UP}, ftTimeStamp{$ENDIF} {$IFDEF BDS2006_UP}, ftOraTimeStamp{$ENDIF}: Result := PODO_DATA_TYPE_DATETIME;
  else
    Result := PODO_DATA_TYPE_VARIANT;
  end;
end;

function DeleteSpaces(const Str: string): string;
begin
  Result := StringReplace(Str, STR_SPACE, EmptyStr, [rfReplaceAll, rfIgnoreCase]);
end;

function UpperCaseFirst(const Str:string):string;
begin
  Result := UpperCase(Str[1]) + Copy(Str, 2, Length(Str) - 1)
end;    

end.

