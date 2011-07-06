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

unit CnNativeDecl;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：32位和64位的一些统一声明
* 单元作者：刘啸 (liuxiao@cnpack.org)
* 备    注：Delphi XE 2 支持 32 和 64 以来，开放出的 NativeInt 和 NativeUInt 随
*           当前是 32 位还是 64 而动态变化，影响到的是 Pointer、Reference等东西。
*           考虑到兼容性，固定长度的 32 位 Cardinal/Integer 等和 Pointer 这些就
*           不能再通用了，即使 32 位下也被编译器禁止。因此本单元声明了几个类型，
*           供同时在低版本和高版本的 Delphi 中使用。
*           包装的代码部分在自行分配的可执行的内存空间，避免了 DEP 下出错。
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 XE 2
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnCallBack.pas 761 2011-02-07 14:08:58Z liuxiao@cnpack.org $
* 修改记录：2011.07.06 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Windows, SysUtils;

type
{$IFDEF SUPPORTS_32_AND_64}
  TCnNativeInt     = NativeInt;
  TCnNativeUInt    = NativeUInt;
  TCnNativePointer = NativeUInt;
{$ELSE}
  TCnNativeInt     = Integer;
  TCnNativeUInt    = Cardinal;
  TCnNativePointer = Cardinal;
{$ENDIF}

implementation

end.
