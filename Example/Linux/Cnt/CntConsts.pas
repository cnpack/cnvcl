{******************************************************************************}
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

unit CntConsts;
{
  CNT - CnPack NetTool
  常量定义单元
}

interface

{$I CnPack.inc}

const
  SCnCntName = 'CNT';
  SCnCntComment = 'CnPack NetTool - A NetCat-like Command Line Tool';
  SCnCntVersion = '1.0';

  SCnCntUsage = 'Usage: cnt [OPTIONS] [HOST] [PORT]';

  // Default buffer sizes
  CN_CNT_BUF_SIZE = 8192;
  CN_CNT_MAX_BUF_SIZE = 64 * 1024;

  // Default timeouts (milliseconds)
  CN_CNT_DEFAULT_TIMEOUT = 30000;
  CN_CNT_CONNECT_TIMEOUT = 60000;

implementation

end.