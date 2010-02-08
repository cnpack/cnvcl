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

unit CnLangTranslator;
{* |<PRE>
================================================================================
* 软件名称：CnPack 多语包
* 单元名称：多语包翻译器单元
* 单元作者：CnPack开发组 刘啸 (liuxiao@cnpack.org)
* 备    注：该单元实现了多语包的翻译器
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
  SysUtils, Classes, CnConsts, CnClasses, CnLangConsts,
  CnLangMgr;

type
  
  ETranslatorError = class (Exception)
  end;

  TCnCustomLangTranslator = class (TCnComponent)
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    
    destructor Destroy; override;
  end;

  TCnLangTranslator = class(TCnCustomLangTranslator)
  end;

implementation

{**************************** TCustomTranslator *******************************}

constructor TCnCustomLangTranslator.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TCnCustomLangTranslator.Destroy;
begin
  inherited;
end;


procedure TCnCustomLangTranslator.GetComponentInfo(var AName, Author,
  Email, Comment: string);
begin
  AName := SCnLangTranslatorName;
  Author := SCnPack_LiuXiao;
  Email := SCnPack_LiuXiaoEmail;
  Comment := SCnLangTranslatorComment;
end;

end.
