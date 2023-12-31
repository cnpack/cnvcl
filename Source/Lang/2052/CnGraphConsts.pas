{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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

var
  SCnAOCaptionColor: string = '颜色(&C)';
  SCnAOCaptionFont: string = '字体(&F)';
  SCnAOCaptionOption: string = '设置(&O)';

  SCreateDCFromEmptyBmp: string = '不能为空位图分配 DC';
  SAllocDIBFail: string = '创建 DIB 对象句柄失败';
  SCreateDCFail: string = '创建 DC 失败';
  SSelectBmpToDCFail: string = '无法将位图对象选择到 DC 中';
  SBitmapIsEmpty: string = '无法访问一个空位图的像素数据';
  SInvalidPixel: string = '无效的像素点 x: %d, y: %d';
  SInvalidPixelF: string = '无效的像素点 x: %f, y: %f';
  SInvalidScanLine: string = '无效的扫描线 Row: %d';
  SInvalidAlphaBitmap: string = '在 Alpha 混合处理中，用于混合的图像大小必须与当前图像一致';
  SInvalidForeBitmap: string = '在字体蒙板混合处理中，前景图与蒙板大小必须一致';
  SReadBmpError: string = '读位图数据出错';

  // CnSkinMagic 异常信息
  SCNE_WRITEVMTFAILED: string = '无法改写 VMT 数据，CnSkinMagic 类注册失败';
  SCNE_FINDCLASSDATAFAILED: string = '无法获得 CnSkingMagic 类信息';
  SCNE_REGISTERMESSAGEFAILED: string = '无法替 CnSkinMagic 注册窗口消息';

implementation

end.
