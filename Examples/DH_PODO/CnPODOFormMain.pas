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

unit CnPODOFormMain;
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate PODO 生成工具
* 单元名称：PODO 生成工具
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id: CnPODOFormMain.pas,v 1.2 2009/01/02 08:27:38 liuxiao Exp $
* 修改记录：2008.08.23 V1.8
*               移植到 Delphi2009
*           2006.09.04 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, AdoConEd, StdCtrls, DB, ADODB, CnPODOConsts, ComCtrls, CnPODOUtils,
  CnDHibernateAbout;

type
  TfrmPodoMain = class(TForm)
    conn: TADOConnection;
    table: TADOTable;
    lblDB: TLabel;
    edtDB: TEdit;
    btnConn: TButton;
    lblTblName: TLabel;
    cbTableName: TComboBox;
    lblTblInfo: TLabel;
    lvInfo: TListView;
    lblPODO: TLabel;
    btnGenerate: TButton;
    btnClose: TButton;
    btnHelp: TButton;
    sdPODO: TSaveDialog;
    mmPodo: TMemo;
    procedure btnConnClick(Sender: TObject);
    procedure cbTableNameSelect(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { get data field and type }
    procedure getFields;
    { generate podo file preview according to fields }
    procedure generatePODO;
  public
    { Public declarations }
  end;

var
  frmPodoMain: TfrmPodoMain;

implementation

{$R *.dfm}

procedure TfrmPodoMain.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPodoMain.btnConnClick(Sender: TObject);
begin
  conn.Close;
  // edtDB.Clear;
  cbTableName.Items.Clear;
  AdoConEd.EditConnectionString(Self.conn);
  edtDB.Text := conn.ConnectionString;
  try
    conn.Open;
    conn.GetTableNames(cbTableName.Items);
    MessageBox(Handle, PODO_CONNECT_SUCCESS, PODO_MSGBOX_TITLE, MB_OK or MB_ICONINFORMATION);
  except
    MessageBox(Handle, PODO_CONNECT_FAIL, PODO_MSGBOX_TITLE, MB_OK or MB_ICONERROR);
  end;
end;

procedure TfrmPodoMain.btnGenerateClick(Sender: TObject);
begin
  // save podo file
  sdPODO.Filter := Format(FILTER_PODO, [Format(FILTER_FILE_NAME, [UpperCase(DeleteSpaces(cbTableName.Text))])]);
  sdPODO.FileName := sdPODO.Filter;
  if not sdPODO.Execute then
    Exit;
  try
    mmPODO.Lines.SaveToFile(sdPODO.FileName);
    MessageBox(Handle, PODO_SAVE_SUCCESS, PODO_MSGBOX_TITLE, MB_OK or MB_ICONINFORMATION);
  except
    MessageBox(Handle, PODO_SAVE_FAIL, PODO_MSGBOX_TITLE, MB_OK or MB_ICONINFORMATION);
  end;
end;

procedure TfrmPodoMain.btnHelpClick(Sender: TObject);
begin
  with TCnFormDHibernateAbout.Create(application) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TfrmPodoMain.cbTableNameSelect(Sender: TObject);
begin
  table.Close;
  lvInfo.Items.Clear;
  mmPODO.Lines.Clear;
  if cbTableName.Text <> EmptyStr then
  begin
    table.TableName := cbTableName.Text;
    table.Open;
    getFields;
    generatePODO;
  end;
end;

procedure TfrmPodoMain.generatePODO;
var
  i: Integer;
  tableName: string;
  fieldName: string;
begin
  // TODO : generate podo file preview
  // delete the spaces in table name
  tableName := DeleteSpaces(cbTableName.Text);
  with mmPODO.Lines do
  begin
    // comment
    Append(PREVIEW_UNIT_HEAD_COMMENT);
    Append(EmptyStr);
    // unit %s;
    Append(Format(PREVIEW_UNIT_NAME, [UpperCase(tableName)]));
    Append(EmptyStr);
    // {$M+}
    Append(PREVIEW_UNIT_MPLUS);
    Append(EmptyStr);
    // interface
    Append(PREVIEW_UNIT_INTERFACE);
    Append(EmptyStr);
    // uses
    Append(PREVIEW_UNIT_USES);
    // Classes, SysUtils, DHibernateBase;
    Append(PREVIEW_UNIT_BASE_UNIT);
    Append(EmptyStr);
    // type
    Append(PREVIEW_UNIT_TYPE);
    // T%s = class(TDHibernateBase)
    Append(Format(PREVIEW_UNIT_CLASS_NAME, [UpperCaseFirst(tableName)]));
    // private
    Append(PREVIEW_UNIT_PRIVATE);
    // F%s : %s
    for i := 0 to lvInfo.Items.Count - 1 do
    begin
      fieldName := UpperCaseFirst(DeleteSpaces(lvInfo.Items[i].Caption));
      Append(Format(PREVIEW_UNIT_PRIVATE_ATTR, [fieldName, lvInfo.Items[i].SubItems[0]]));
    end;
    // published
    Append(PREVIEW_UNIT_PUBLISHED);
    // property %s : %s read F%s write F%s;
    for i := 0 to lvInfo.Items.Count - 1 do
    begin
      fieldName := UpperCaseFirst(DeleteSpaces(lvInfo.Items[i].Caption));
      Append(Format(PREVIEW_UNIT_PUBLISHED_ATTR, [fieldName, lvInfo.Items[i].SubItems[0], fieldName, fieldName]));
    end;
    // end;
    Append(PREVIEW_UNIT_END);
    Append(EmptyStr);
    // implementation
    Append(PREVIEW_UNIT_IMPLEMENTATION);
    Append(EmptyStr);
    // initialization
    Append(PREVIEW_UNIT_INITIALIZATION);
    // registerClass
    Append(Format(PREVIEW_UNIT_REGISTER_CLASS, [UpperCaseFirst(tableName)]));
    Append(EmptyStr);
    // end.
    Append(PREVIEW_UNIT_FULL_END);
  end;
end;

procedure TfrmPodoMain.getFields;
var
  i: Integer;
  item: TListItem;
begin
  for i := 0 to table.FieldCount - 1 do
  begin
    item := lvInfo.Items.Add;
    item.Caption := table.Fields[i].FieldName;
    item.SubItems.Add(DataTypeToString(table.Fields[i].DataType));
  end;
end;

end.

