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

unit CnADOUpdateSQLEditor;
{* |<PRE>
================================================================================
* 软件名称：CnPack组件包
* 单元名称：CnADOUpdateSQL属性编辑器单元
* 单元作者：小夏
* 备    注：
* 开发平台：PWin2K SP3 + Delphi 7
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.04.25
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Windows, Messages, SysUtils, Classes, Controls,
  {$IFDEF SUPPORT_CROSS_PLATFORM} Data.Win.ADODB {$ELSE} ADODB {$ENDIF},
  Provider,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors
  {$ELSE}
  Dsgnintf
  {$ENDIF}
  ;

type
  TCnADOUpdateSQLEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

uses
  CnADOUpdateSQLFrm, CnADOUpdateSQL;

procedure TCnADOUpdateSQLEditor.ExecuteVerb(Index: Integer);
begin
  with TCnADOUpdateSQLForm.Create(nil) do
  try
    case TCnADOUpdateSQL(Component).ConnectionType of
      ctConnection:
        if TCnADOUpdateSQL(Component).Connection <> nil then
          Connection := TCnADOUpdateSQL(Component).Connection;
      ctDataSet:
        if TCnADOUpdateSQL(Component).DataSet <> nil then
          Connection := TCustomADODataSet(TCnADOUpdateSQL(Component).DataSet).Connection;
      ctProvider:
        if TCnADOUpdateSQL(Component).Provider <> nil then
          Connection := TCustomADODataSet(TDataSetProvider(TCnADOUpdateSQL(Component).Provider).DataSet).Connection;
    end;
    ModifySQL.Text := TCnADOUpdateSQL(Component).ModifySQL.Text;
    InsertSQL.Text := TCnADOUpdateSQL(Component).InsertSQL.Text;
    DeleteSQL.Text := TCnADOUpdateSQL(Component).DeleteSQL.Text;
    if ShowModal = mrOK then
    begin
      TCnADOUpdateSQL(Component).ModifySQL.Text := ModifySQL.Text;
      TCnADOUpdateSQL(Component).InsertSQL.Text := InsertSQL.Text;
      TCnADOUpdateSQL(Component).DeleteSQL.Text := DeleteSQL.Text;
      Self.Designer.Modified;
    end;
  finally
    Free;
  end;
end;

function TCnADOUpdateSQLEditor.GetVerb(Index: Integer): string;
begin
  if Index in [1] then
    Result := 'Generate SQL';
end;

function TCnADOUpdateSQLEditor.GetVerbCount:Integer;
begin
  Result := $00000005;
end;

{$ENDIF SUPPORT_ADO}
end.
