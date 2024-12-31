{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnADOUpdateSQLFrm;
{* |<PRE>
================================================================================
* 软件名称：CnPack组件包
* 单元名称：CnADOUpdateSQL 属性编辑器窗体
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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ExtCtrls, ComCtrls,
  {$IFDEF SUPPORT_CROSS_PLATFORM} Data.Win.ADODB {$ELSE} ADODB {$ENDIF};

type
  TCnADOUpdateSQLForm = class(TForm)
    PageControl1: TPageControl;
    TabSheetOptions: TTabSheet;
    GroupBox1: TGroupBox;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    cbbTables: TComboBox;
    btnGetTables: TButton;
    btnGetTableFields: TButton;
    btnGenerateSQL: TButton;
    lstKeyFields: TListBox;
    lstUpdateFields: TListBox;
    TabSheetSQL: TTabSheet;
    lbl4: TLabel;
    RadioGroupSQL: TRadioGroup;
    mmoSQLText: TMemo;
    btnHelp: TButton;
    btnCancel: TButton;
    btnOK: TButton;
    pmKeyFields: TPopupMenu;
    mniSelectAll1: TMenuItem;
    mniClearAll1: TMenuItem;
    pmUpdateFields: TPopupMenu;
    mniSelectAll2: TMenuItem;
    mniClearAll2: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure btnGetTablesClick(Sender: TObject);
    procedure btnGetTableFieldsClick(Sender: TObject);
    procedure btnGenerateSQLClick(Sender: TObject);
    procedure RadioGroupSQLClick(Sender: TObject);
    procedure cbbTablesChange(Sender: TObject);
    procedure mmoSQLTextChange(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure mniSelectAll1Click(Sender: TObject);
    procedure mniClearAll1Click(Sender: TObject);
    procedure mniSelectAll2Click(Sender: TObject);
    procedure mniClearAll2Click(Sender: TObject);    
  private
    FConnection: TADOConnection;
    FModifySQL: TStrings;
    FInsertSQL: TStrings;
    FDeleteSQL: TStrings;
    FTableName: AnsiString;
    procedure ClearValue;
    function GetConnection: TADOConnection;
    function GetModifySQL: TStrings;
    function GetInsertSQL: TStrings;
    function GetDeleteSQL: TStrings;
    procedure SetConnection(Value: TADOConnection);
    procedure SetModifySQL(Value: TStrings);
    procedure SetInsertSQL(Value: TStrings);
    procedure SetDeleteSQL(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Connection: TADOConnection read GetConnection write SetConnection;
    property ModifySQL: TStrings read GetModifySQL write SetModifySQL;
    property InsertSQL: TStrings read GetInsertSQL write SetInsertSQL;
    property DeleteSQL: TStrings read GetDeleteSQL write SetDeleteSQL;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{$R *.dfm}

procedure CnListBoxSelectAll(ListBox: TListBox; Select: Boolean = True);
var
  I: Integer;
begin
  if ListBox.MultiSelect then
    for I := 0 to ListBox.Items.Count - 1 do
      ListBox.Selected[I] := Select
  else if not Select then
    ListBox.ItemIndex := -1;
end;

{ TCnADOUpdateSQLForm }

constructor TCnADOUpdateSQLForm.Create(AOwner: TComponent);
begin
  inherited;
  FModifySQL := TStringList.Create;
  FInsertSQL := TStringList.Create;
  FDeleteSQL := TStringList.Create;
end;

destructor TCnADOUpdateSQLForm.Destroy;
begin
  FModifySQL.Free;
  FInsertSQL.Free;
  FDeleteSQL.Free;
  inherited;
end;

procedure TCnADOUpdateSQLForm.FormShow(Sender: TObject);
var
  I, J: Integer;
begin
  if FConnection <> nil then
  begin
    FConnection.GetTableNames(cbbTables.Items);
    if Trim(FModifySQL.Text) <> '' then
    begin
      // 从 ModifySQL 字符串中获取表名
      FTableName := {$IFDEF UNICODE}AnsiString{$ENDIF}(Trim(FModifySQL.Strings[0]));
      System.Delete(FTableName, 1, 7);
      FTableName := {$IFDEF UNICODE}AnsiString{$ENDIF}(Trim({$IFDEF UNICODE}String{$ENDIF}(FTableName)));
      cbbTables.ItemIndex := cbbTables.Items.IndexOf({$IFDEF UNICODE}String{$ENDIF}(FTableName));

      // 根据 FTableName 表名获取字段名
      FConnection.GetFieldNames({$IFDEF UNICODE}String{$ENDIF}(FTableName), lstKeyFields.Items);
      lstUpdateFields.Items.Text := lstKeyFields.Items.Text;

      // 根据 FModifySQL 来选中列表框字段
      J := 0;
      for I := 2 to FModifySQL.Count - 1 do
      begin
        if Trim(UpperCase(FModifySQL.Strings[I])) = 'WHERE' then
        begin
          J := I;
          Break;
        end;
        lstUpdateFields.Selected[lstUpdateFields.Items.IndexOf(Trim(Copy(FModifySQL.Strings[I], 1, Pos('=', FModifySQL.Strings[I]) - 1)))] := True;
      end;
      for I := J + 1 to FModifySQL.Count - 1 do
      begin
        lstUpdateFields.Selected[lstUpdateFields.Items.IndexOf(Trim(Copy(FModifySQL.Strings[I], 1, Pos('=', FModifySQL.Strings[I]) - 1)))] := True;
      end;
    end;
  end;
  btnOK.Enabled := Trim(mmoSQLText.Lines.Text) <> '';
end;

procedure TCnADOUpdateSQLForm.btnGetTablesClick(Sender: TObject);
var
  sText: string;
begin
  sText := cbbTables.Text;
  if FConnection <> nil then
    FConnection.GetTableNames(cbbTables.Items);
  cbbTables.ItemIndex := cbbTables.Items.IndexOf(sText);
  cbbTables.Hint := cbbTables.Text;
  ClearValue;
end;

procedure TCnADOUpdateSQLForm.ClearValue;
begin
  lstKeyFields.Clear;
  lstUpdateFields.Clear;
  FModifySQL.Clear;
  FInsertSQL.Clear;
  FDeleteSQL.Clear;
  mmoSQLText.Clear;
  btnOK.Enabled := Trim(mmoSQLText.Text) <> '';
end;

procedure TCnADOUpdateSQLForm.btnGetTableFieldsClick(Sender: TObject);
begin
  if cbbTables.Text = '' then
  begin
    MessageDlg('Please select Table Name.', mtInformation, [mbOK], 0);
    Exit;
  end;
  FConnection.GetFieldNames(cbbTables.Text, lstKeyFields.Items);
  lstUpdateFields.Items.Text := lstKeyFields.Items.Text;
  CnListBoxSelectAll(lstKeyFields);
  CnListBoxSelectAll(lstUpdateFields);
end;

procedure TCnADOUpdateSQLForm.btnGenerateSQLClick(Sender: TObject);
var
  I, J: Integer;
  sFieldName, sFieldNames, sValuesParams: AnsiString;
begin
  // 必须选择主键（作为执行 SQL 语句的 where 条件）
  if lstKeyFields.SelCount = 0 then
  begin
    ClearValue;
    MessageDlg('Please specify Key Fields.', mtInformation, [mbOK], 0);
    Exit;
  end;

  // 必须选择要更新的字段
  if lstUpdateFields.SelCount = 0 then
  begin
    MessageDlg('Please specify Update Fields.', mtInformation, [mbOK], 0);
    Exit;
  end;

  // 构造 SQL 语句
  FModifySQL.Clear;
  FInsertSQL.Clear;
  FDeleteSQL.Clear;
  FModifySQL.Add('UPDATE ' + cbbTables.Text);
  FModifySQL.Add(' SET ');
  FInsertSQL.Add('INSERT INTO ' + cbbTables.Text);
  sFieldNames := '   (';
  sValuesParams := 'VALUES '#13#10'   (';

  // 构造字段名及字段参数
  J := 0;
  for I := 0 to lstUpdateFields.Items.Count - 1 do
  begin
    if lstUpdateFields.Selected[I] then
    begin
      sFieldName := {$IFDEF UNICODE}AnsiString{$ENDIF}(lstUpdateFields.Items.Strings[I]);
      if (lstUpdateFields.SelCount - 1) <> J then // 不是选中项的最后一项时
      begin
        FModifySQL.Add('    ' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' = :' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ',');
        sFieldNames := sFieldNames + sFieldName + ',';
        sValuesParams := sValuesParams + ':' + sFieldName + ',';
      end
      else
      begin                                  // 是选中项的最后一项时
        FModifySQL.Add('    ' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' = :' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName));
        sFieldNames := sFieldNames + sFieldName;
        sValuesParams := sValuesParams + ':' + sFieldName;
      end;
      Inc(J);
    end;
  end;
  FInsertSQL.Add({$IFDEF UNICODE}String{$ENDIF}(sFieldNames) + ')' + #13 + #10 + {$IFDEF UNICODE}String{$ENDIF}(sValuesParams) + ')');
  FModifySQL.Add(' WHERE ');
  FDeleteSQL.Add('DELETE FROM ' + cbbTables.Text);
  FDeleteSQL.Add(' WHERE ');

  // 构造 Where 条件的主键字段
  J := 0;
  for I := 0 to lstKeyFields.Items.Count - 1 do
  begin
    if lstKeyFields.Selected[I] then
    begin
      sFieldName := {$IFDEF UNICODE}AnsiString{$ENDIF}(lstKeyFields.Items.Strings[I]);
      if (lstKeyFields.SelCount - 1) <> J then //不是选中项的最后一项时
      begin
        FModifySQL.Add('    ' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' = :OLD_' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' and ');
        FDeleteSQL.Add('    ' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' = :OLD_' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' and ');
      end
      else begin                              //是选中项的最后一项时
        FModifySQL.Add('    ' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' = :OLD_' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName));
        FDeleteSQL.Add('    ' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName) + ' = :OLD_' + {$IFDEF UNICODE}String{$ENDIF}(sFieldName));
      end;
      Inc(J);
    end;
  end;

  RadioGroupSQLClick(Sender);
  TabSheetSQL.Show;
end;

procedure TCnADOUpdateSQLForm.RadioGroupSQLClick(Sender: TObject);
begin
  case RadioGroupSQL.ItemIndex of
    0: mmoSQLText.Lines.Text := FModifySQL.Text;
    1: mmoSQLText.Lines.Text := FInsertSQL.Text;
    2: mmoSQLText.Lines.Text := FDeleteSQL.Text;
  end;
end;

procedure TCnADOUpdateSQLForm.cbbTablesChange(Sender: TObject);
begin
  inherited;
  ClearValue;
end;

procedure TCnADOUpdateSQLForm.mmoSQLTextChange(Sender: TObject);
begin
  case RadioGroupSQL.ItemIndex of
    0: FModifySQL.Text := mmoSQLText.Lines.Text;
    1: FInsertSQL.Text := mmoSQLText.Lines.Text;
    2: FDeleteSQL.Text := mmoSQLText.Lines.Text;
  end;
  btnOK.Enabled := Trim(mmoSQLText.Text) <> '';
end;

procedure TCnADOUpdateSQLForm.PageControl1Change(Sender: TObject);
begin
  if PageControl1.ActivePageIndex = 1 then
    mmoSQLText.Lines.Text := FModifySQL.Text;
end;

procedure TCnADOUpdateSQLForm.mniSelectAll1Click(Sender: TObject);
begin
  CnListBoxSelectAll(lstKeyFields);
end;

procedure TCnADOUpdateSQLForm.mniClearAll1Click(Sender: TObject);
begin
  CnListBoxSelectAll(lstKeyFields, False);
end;

procedure TCnADOUpdateSQLForm.mniSelectAll2Click(Sender: TObject);
begin
  CnListBoxSelectAll(lstUpdateFields);
end;

procedure TCnADOUpdateSQLForm.mniClearAll2Click(Sender: TObject);
begin
  CnListBoxSelectAll(lstUpdateFields);
end;

function TCnADOUpdateSQLForm.GetConnection: TADOConnection;
begin
  Result := FConnection;
end;

function TCnADOUpdateSQLForm.GetDeleteSQL: TStrings;
begin
  Result := FDeleteSQL;
end;

function TCnADOUpdateSQLForm.GetInsertSQL: TStrings;
begin
  Result := FInsertSQL;
end;

function TCnADOUpdateSQLForm.GetModifySQL: TStrings;
begin
  Result := FModifySQL;
end;

procedure TCnADOUpdateSQLForm.SetConnection(Value: TADOConnection);
begin
  if Assigned(Value) then
    FConnection := Value;
end;

procedure TCnADOUpdateSQLForm.SetDeleteSQL(Value: TStrings);
begin
  FDeleteSQL.Assign(Value);
end;

procedure TCnADOUpdateSQLForm.SetInsertSQL(Value: TStrings);
begin
  FInsertSQL.Assign(Value);
end;

procedure TCnADOUpdateSQLForm.SetModifySQL(Value: TStrings);
begin
  FModifySQL.Assign(Value);
end;

{$ENDIF SUPPORT_ADO}
end.
