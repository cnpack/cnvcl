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

unit CnCompAboutFrm;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：开发包公共组件关于窗口单元
* 单元作者：CnPack开发组
* 备    注：
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串暂不符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2002.08.06 V1.0
                定义调用接口
================================================================================
|</PRE>}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type

{ TCnCompAboutForm }

  TCnCompAboutForm = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  PCnAuthorInfo = ^TCnAuthorInfo;
  {* 组件作者信息指针 }
  TCnAuthorInfo = record
  {* 组件作者信息记录 }
    Name: string;
    {* 组件作者 }
    Email: string;
    {* 组件作者信箱，可能为空 }
  end;

  PCnAuthorInfoArray = ^TCnAuthorInfoArray;
  {* 组件作者信息动态数组指针 }
  TCnAuthorInfoArray = array of TCnAuthorInfo;
  {* 组件作者信息动态数组 }

function CnShowCompAbout(Sender: TObject; AuthorArray: TCnAuthorInfoArray;
  const Comment: string): Boolean; overload;
{* 显示组件关于窗口过程（多名作者）
 |<PRE>
   Sender: TObject                 - 调用者，可能是组件或组件、属性编辑器
   AuthorArray: TCnAuthorInfoArray - 组件作者信息，动态数组，可以传nil
   Comment: string                 - 组件的说明信息，可能为空
   Result: Boolean                 - 如果该模式窗口返回 mrOK 则为真，否则为假
 |</PRE>}

function CnShowCompAbout(Sender: TObject; const AName, AEmail: string;
  const Comment: string): Boolean; overload;
{* 显示组件关于窗口过程（单一作者）
 |<PRE>
   Sender: TObject                 - 调用者，可能是组件或组件、属性编辑器
   AName: string                   - 组件作者
   AEmail: string                  - 组件作者Email
   Comment: string                 - 组件的说明信息，可能为空
   Result: Boolean                 - 如果该模式窗口返回 mrOK 则为真，否则为假
 |</PRE>}

implementation

uses
  CnConsts;

{$R *.DFM}

// 显示组件关于窗口过程（多名作者）
function CnShowCompAbout(Sender: TObject; AuthorArray: TCnAuthorInfoArray;
  const Comment: string): Boolean;
begin
  with TCnCompAboutForm.Create(nil) do
  try
    // 此处增加用户代码，处理以上参数
    // 注意用 Low(AuthorArray) 和 High(AuthorArray) 来取数组上下界
    // 如果为作者信箱做链接，考虑为空时的情况
    // 显示时用到的其它字符串，尽量从 CnConsts 中取，特别是版本号等
    // 见 CnConsts 中的资源字符串定义
    // 考虑到本地化，单元中（不是窗体）用到的中文字符串最好放到 CnCosnts 中定义
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

// 显示组件关于窗口过程（单一作者）
function CnShowCompAbout(Sender: TObject; const AName, AEmail: string;
  const Comment: string): Boolean; overload;
var
  AuthorArray: TCnAuthorInfoArray;
begin
  SetLength(AuthorArray, 1);
  try
    AuthorArray[0].Name := AName;
    AuthorArray[0].Email := AEmail;
    Result := CnShowCompAbout(Sender, AuthorArray, Comment);
  finally
    AuthorArray := nil;
  end;
end;

end.
