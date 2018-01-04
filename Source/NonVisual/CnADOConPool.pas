{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2018 CnPack 开发组                       }
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

unit CnADOConPool;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：ADOConnection 对象池单元
* 单元作者：Chinbo（Shenloqi）
* 备    注：
* 开发平台：PWin2000Pro + Delphi 7.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串暂不符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2004.03.18 V1.0
*               创建单元
*           2004.09.18 V1.1
*               公开从父类继承的属性WaitTimeOut
*               修改了ReleaseConnection的实现，不再强制类型转换
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Windows, Messages, SysUtils, Classes, CnObjectPool, ADODB,
  CnConsts, CnCompConsts;

type
  TCnADOConWrapper = class(TCnObjectWrapper)
  private
    function GetConnection: TADOConnection;
  public
    property Connection: TADOConnection read GetConnection;
  end;

  TCnADOConPool = class(TCnCustomObjectPool)
  private
    FConnectionString: WideString;
    procedure SetConnectionString(const Value: WideString);
  protected
    function DoCreateOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; override;
    function DoFreeOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; override;
    function DoGetOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; override;
    function DoReleaseOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; override;
    function DoReInitOne(Wrapper: TCnObjectWrapper;
      var Obj: TObject): Boolean; override;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    function GetConnection(var con: TADOConnection;
      const go: TCnObjectPoolGetOption = goNone): TCnObjectPoolGetResult;
    procedure ReleaseConnection(var con: TADOConnection;
      const ro: TCnObjectPoolReleaseOption = roNone);
  published
    property ConnectionString: WideString
      read FConnectionString
      write SetConnectionString;
    property MinSize;
    property MaxSize;
    property LowLoadCount;
    property PeakCount;
    property PolicyOnBusy;
    property PolicyOnPeak;
    property PolicyOnGet;
    property WaitTimeOut;

    property OnGetOne;
    property OnReleaseOne;
    property OnReInitOne;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

uses
  ActiveX,
  ComObj;

{ TCnADOConWrapper }

function TCnADOConWrapper.GetConnection: TADOConnection;
begin
  Result := nil;
  if Assigned(ObjectWrapped) then
    Result := TADOConnection(ObjectWrapped);
end;

{ TCnADOConPool }

function TCnADOConPool.DoCreateOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  csObjectMgr.Enter;
  try
    CoInitialize(nil);
    try
      Obj := TADOConnection.Create(Self);
      with TADOConnection(Obj) do
      begin
        KeepConnection := True;
        LoginPrompt := False;
        ConnectionString := Self.ConnectionString;
      end;

      Result := inherited DoCreateOne(Wrapper, Obj);

      if Assigned(Wrapper) then
        TCnADOConWrapper(Wrapper).NeedReInit := True;
    finally
      CoUninitialize;
    end;
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnADOConPool.DoFreeOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  csObjectMgr.Enter;
  try
    Result := inherited DoFreeOne(Wrapper, Obj);
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnADOConPool.DoGetOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  csObjectMgr.Enter;
  try
    Result := inherited DoGetOne(Wrapper, Obj);
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnADOConPool.DoReInitOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  Result := True;
  
  csObjectMgr.Enter;
  try
    CoInitialize(nil);
    try
      with TADOConnection(Obj) do
      begin
        Connected := False;
        KeepConnection := True;
        LoginPrompt := False;
        ConnectionString := Self.ConnectionString;
        try
          Connected := True;
        except
          Result := False;
        end;
      end;

      Result := (inherited DoReInitOne(Wrapper, Obj)) and Result;

      if not Result then
        TCnADOConWrapper(Wrapper).NeedReInit := True;

    finally
      CoUninitialize;
    end;
  finally
    csObjectMgr.Leave;
  end;
end;

function TCnADOConPool.DoReleaseOne(Wrapper: TCnObjectWrapper;
  var Obj: TObject): Boolean;
begin
  csObjectMgr.Enter;
  try
    Result := inherited DoReleaseOne(Wrapper, Obj);
  finally
    csObjectMgr.Leave;
  end;
end;

procedure TCnADOConPool.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnADOConPoolName;
  Author := SCnPack_Shenloqi;
  Email := SCnPack_ShenloqiEmail;
  Comment := SCnADOConPoolComment;
end;

function TCnADOConPool.GetConnection(var con: TADOConnection;
  const go: TCnObjectPoolGetOption): TCnObjectPoolGetResult;
var
  Obj: TObject;
begin
  Result := GetOne(Obj, go);
  if Obj is TADOConnection then
    con := TADOConnection(Obj)
  else
  begin
    if Obj <> nil then
      ReleaseOne(Obj, roFree);
    con := nil;
    Result := grGetError;
  end;
end;

procedure TCnADOConPool.ReleaseConnection(var con: TADOConnection;
  const ro: TCnObjectPoolReleaseOption);
begin
  ReleaseOne(con, ro);
end;

procedure TCnADOConPool.SetConnectionString(const Value: WideString);
begin
  csObjectMgr.Enter;
  try
    FConnectionString := Value;
    ReInitAll;
  finally
    csObjectMgr.Leave;
  end;
end;

{$ENDIF SUPPORT_ADO}

end.
