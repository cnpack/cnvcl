{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2017 CnPack 开发组                       }
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

unit CnIISCtrl;

{* |<PRE>
================================================================================
* 软件名称：网络通讯组件包
* 单元名称：实现IIS配置功能单元
* 单元作者：rarnu(rarnu@cnpack.org)
* 备    注：
* 开发平台：Windows2003 Server + Delphi2007 up2
* 兼容测试：Windows2000/XP/2003/Vista + Delphi 7/2006/2007/2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.08.14 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, ComObj, Windows;

type
  TCnIISCtrl = class(TComponent)
  private
    FOnDeleteVirtualDirApp: TNotifyEvent;
    FOnDeleteVirtualDir: TNotifyEvent;
    FOnCreateVirtualDir: TNotifyEvent;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    {* 检查是否存在 .NET FrameWork }
    function CheckDotNetFramework: Boolean;
    {* 删除虚拟目录应用程序名 }
    function DeleteVirtualDirApp(strVirtualDir: string): Boolean;
    {* 删除虚拟目录 }
    function DeleteVirtualDir(strVirtualDir: string): Boolean;
    {* 检测是否有虚拟目录 }
    function CheckVirtualDir(const strVirtualDir: string): Boolean;
    {* 建立虚拟目录 }
    function CreateVirtualDir(const strVirtualDir, strDir, strAppName: string): Boolean;
  published
    {* 删除虚拟目录应用程序名时触发事件 }
    property OnDeleteVirtualDirApp: TNotifyEvent read FOnDeleteVirtualDirApp write FOnDeleteVirtualDirApp;
    {* 删除虚拟目录时触发事件 }
    property OnDeleteVirtualDir: TNotifyEvent read FOnDeleteVirtualDir write FOnDeleteVirtualDir;
    {* 建立虚拟目录时触发事件 }
    property OnCreateVirtualDir: TNotifyEvent read FOnCreateVirtualDir write FOnCreateVirtualDir;
  end;

implementation

{ TCnIISCtrl }

function TCnIISCtrl.CheckDotNetFramework: Boolean;
var
  SysDir: pchar;
begin
  GetMem(SysDir, 250);
  GetSystemDirectory(SysDir, 250);
  if not FileExists(SysDir + '\MSCOREE.DLL') then
    Result := False
  else
    Result := True;
  FreeMem(SysDir);
end;

function TCnIISCtrl.CheckVirtualDir(const strVirtualDir: string): Boolean;
var
  WebSite, WebServer, WebRoot: Variant;
begin
  Result := True;
  try
    WebSite := CreateOleObject('IISNamespace');
    WebSite := WebSite.GetObject('IIsWebService', 'localhost/w3svc');
    WebServer := WebSite.GetObject('IIsWebServer', '1');
    WebRoot := WebServer.GetObject('IIsWebVirtualDir', 'Root');
    WebRoot.GetObject('IIsWebVirtualDir', strVirtualDir);
  except
    Result := False;
  end;
end;

constructor TCnIISCtrl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TCnIISCtrl.CreateVirtualDir(const strVirtualDir, strDir,
  strAppName: string): Boolean;
var
  WebSite, WebServer, WebRoot, vdir: Variant;
begin
  Result := True;
  try
    WebSite := CreateOleObject('IISNamespace');
    WebSite := WebSite.GetObject('IIsWebService', 'localhost/w3svc');
    WebServer := WebSite.GetObject('IIsWebServer', '1');
    WebRoot := WebServer.GetObject('IIsWebVirtualDir', 'Root');
    vdir := WebRoot.Create('IIsWebVirtualDir', strVirtualDir);
    vdir.AccessRead := True;                    // 允许读取
    vdir.AccessScript := True;                  // 执行许可为纯脚本
    vdir.DefaultDoc := 'index.aspx,index.asp';  // 默认文档
    vdir.EnableDirBrowsing := False;            // 允许浏览目录
    vdir.AppFriendlyName := strAppName;         // 应用程序名
    vdir.Path := strDir;                        // 虚拟目录真实路径
    vdir.AppCreate(True);                       // 虚拟目录自动创建应用程序名
    vdir.SetInfo;
  except
    Result := False;
  end;

  (************************************************************)
  (*       IIS 其他各项参数列表于此，可根据需要修改           *)
  (*                                                          *)
  (*  vdir.AccessWrite   := True;   // 允许写入               *)
  (*  vdir.AccessSource  := True;   // 允许脚本资源访问       *)
  (*  vdir.AccessExecute := True;   // 允许可执行文件         *)
  (************************************************************)

  if Assigned(FOnCreateVirtualDir) then
    FOnCreateVirtualDir(Self);
end;

function TCnIISCtrl.DeleteVirtualDir(strVirtualDir: string): Boolean;
var
  WebSite, WebServer, WebRoot: Variant;
begin
  Result := True;
  try
    WebSite := CreateOleObject('IISNamespace');
    WebSite := WebSite.GetObject('IIsWebService', 'localhost/w3svc');
    WebServer := WebSite.GetObject('IIsWebServer', '1');
    WebRoot := WebServer.GetObject('IIsWebVirtualDir', 'Root');
    WebRoot.Delete('IIsWebVirtualDir', strVirtualDir);
  except
    Result := False;
  end;
  if Assigned(FOnDeleteVirtualDir) then
    FOnDeleteVirtualDir(Self);
end;

function TCnIISCtrl.DeleteVirtualDirApp(strVirtualDir: string): Boolean;
var
  WebSite, WebServer, WebRoot, vdir: Variant;
begin
  Result := True;
  try
    WebSite := CreateOleObject('IISNamespace');
    WebSite := WebSite.GetObject('IIsWebService', 'localhost/w3svc');
    WebServer := WebSite.GetObject('IIsWebServer', '1');
    WebRoot := WebServer.GetObject('IIsWebVirtualDir', 'Root');
    vdir := WebRoot.GetObject('IIsWebVirtualDir', strVirtualDir);
    vdir.AppDelete;
    vdir.SetInfo;
  except
    Result := False;
  end;
  if Assigned(FOnDeleteVirtualDirApp) then
    FOnDeleteVirtualDirApp(Self);
end;

end.
