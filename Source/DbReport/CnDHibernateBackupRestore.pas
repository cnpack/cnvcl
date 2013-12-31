{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2014 CnPack 开发组                       }
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

unit CnDHibernateBackupRestore; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate控件库
* 单元名称：备份还原线程控件单元
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
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
  Windows, Messages, SysUtils, Classes, CnDHibernateSQLThread;

type
  TCnDHibernateBackupRestore = class(TComponent)
  private
    FAbout: string;
    FBackupFileName: string;
    FDatabaseName: string;
    FUserPwd: string;
    FUserID: string;
    FDBHost: string;
    FOnFinishBackup: TNotifyEvent;
    FLogicDatabaseName: string;
    FLogicLogName: string;
    FOnFinishRestore: TNotifyEvent;
    FOnBeginBackup: TNotifyEvent;
    FOnBeginRestore: TNotifyEvent; 
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Backup;
    procedure Restore;
  published
    property About: string read FAbout write FAbout;
    property DBHost: string read FDBHost write FDBHost;
    property UserID: string read FUserID write FUserID;
    property UserPwd: string read FUserPwd write FUserPwd;
    property DatabaseName: string read FDatabaseName write FDatabaseName;
    property BackupFileName: string read FBackupFileName write FBackupFileName;
    property OnBeginBackup: TNotifyEvent read FOnBeginBackup write FOnBeginBackup;
    property OnFinishBackup: TNotifyEvent read FOnFinishBackup write FOnFinishBackup;
    property LogicDatabaseName: string read FLogicDatabaseName write FLogicDatabaseName;
    property LogicLogName: string read FLogicLogName write FLogicLogName;
    property OnBeginRestore: TNotifyEvent read FOnBeginRestore write FOnBeginRestore;
    property OnFinishRestore: TNotifyEvent read FOnFinishRestore write FOnFinishRestore;
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDHibernateBackupRestore }

procedure TCnDHibernateBackupRestore.Backup;
var
  backupThread: TCnCustomSQLBackupThread;
begin
  if Assigned(OnBeginBackup) then
    OnBeginBackup(Self);
  backupThread := TCnCustomSQLBackupThread.Create(True);
  with backupThread do
  begin
    FreeOnTerminate := True;
    DBHost := self.FDBHost;
    UserID := self.FUserID;
    UserPwd := self.FUserPwd;
    DatabaseName := self.FDatabaseName;
    BackupFileName := self.FBackupFileName;
    if Assigned(OnFinishBackup) then
      OnFinish := OnFinishBackup;
    Execute;
  end;
end;

constructor TCnDHibernateBackupRestore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCnDHibernateBackupRestore.Destroy;
begin
  inherited Destroy;
end;

procedure TCnDHibernateBackupRestore.Restore;
var
  RestoreThread: TCnCustomSQLRestoreThread;
begin
  if Assigned(OnBeginRestore) then
    OnBeginRestore(Self);
  RestoreThread := TCnCustomSQLRestoreThread.Create(True);
  with RestoreThread do
  begin
    FreeOnTerminate := True;
    DBHost := self.FDBHost;
    UserID := self.FUserID;
    UserPwd := self.FUserPwd;
    DatabaseName := self.FDatabaseName;
    BackupFileName := self.FBackupFileName;
    LogicDatabaseName := Self.FLogicDatabaseName;
    LogicLogName := Self.FLogicLogName;
    if Assigned(OnFinishRestore) then
      OnFinish := OnFinishRestore;
    Execute;
  end;
end; 

{$ENDIF SUPPORT_ADO}
end.
