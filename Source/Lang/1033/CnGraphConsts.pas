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

unit CnGraphConsts;
{* |<PRE>
================================================================================
* 软件名称：界面控件包
* 单元名称：资源字符串定义单元
* 单元作者：CnPack开发组
* 备    注：该单元定义了界面类用到的资源字符串
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2005.12.24 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

resourcestring

  // CnErrorProvider
  SCnErrorProviderName = 'Error Provider Component';
  SCnErrorProviderComment = 'A Component to Show Error Flag';

  // CnHint
  SCnHintName = 'Hint Component';
  SCnHintComment = 'An Encapsulated Hint Component';

  // CnHintWindow
  SCnHintWindowName = 'HintWindow Component';
  SCnHintWindowComment = 'An Encapsulated HintWindow Component';

var
  SCnAOCaptionColor: string = '&Color';
  SCnAOCaptionFont: string = '&Font';
  SCnAOCaptionOption: string = '&Options';

  SCreateDCFromEmptyBmp: string = 'Can''t create DC for empty bitmap';
  SAllocDIBFail: string = 'Can''t allocate the DIB handle';
  SCreateDCFail: string = 'Can''t create compatible DC';
  SSelectBmpToDCFail: string = 'Can''t select an object into DC';
  SBitmapIsEmpty: string = 'Can''t access data for empty bitmap';
  SInvalidPixel: string = 'Invalid pixel x: %d, y: %d';
  SInvalidPixelF: string = 'Invalid pixel x: %f, y: %f';
  SInvalidScanLine: string = 'Invalid scanline Row: %d';
  SInvalidAlphaBitmap: string = 'In Alpha Blend, Blend Bitmap must be same dimensions as Current Bitmap';
  SInvalidForeBitmap: string = 'Invalid foreground bitmap';
  SReadBmpError: string = 'Read bitmap data error';
  
  // CnSkinMagic Exceptions
  SCNE_WRITEVMTFAILED: string = 'Can''t Write VMT Data, CnSkinMagic Class Register Failed!';
  SCNE_FINDCLASSDATAFAILED: string = 'Can''t Find CnSkingMagic Class Data!';
  SCNE_REGISTERMESSAGEFAILED: string = 'Can''t Register Window Message for CnSkinMagic!';  

implementation

end.
