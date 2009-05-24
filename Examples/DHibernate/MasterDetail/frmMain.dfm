object FormMainDetailDemo: TFormMainDetailDemo
  Left = 265
  Top = 146
  BorderStyle = bsDialog
  Caption = 'Master-Detail Demo'
  ClientHeight = 340
  ClientWidth = 272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 41
    Height = 13
    AutoSize = False
    Caption = 'Master'
  end
  object Label2: TLabel
    Left = 16
    Top = 168
    Width = 41
    Height = 13
    AutoSize = False
    Caption = 'Detail'
  end
  object DBGrid1: TDBGrid
    Left = 16
    Top = 24
    Width = 241
    Height = 137
    DataSource = DataSource1
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    ReadOnly = True
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    OnCellClick = DBGrid1CellClick
    Columns = <
      item
        Expanded = False
        FieldName = 'ID'
        Width = 100
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'NAME'
        Width = 100
        Visible = True
      end>
  end
  object DBGrid2: TDBGrid
    Left = 16
    Top = 184
    Width = 241
    Height = 137
    DataSource = DataSource2
    Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgRowSelect, dgConfirmDelete, dgCancelOnExit]
    ReadOnly = True
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'WORK'
        Width = 200
        Visible = True
      end>
  end
  object DHibernateSubQueryAdvance1: TCnDHibernateSubQueryAdvance
    Active = False
    Aggregates = <>
    Params = <>
    Connection = ADOConnection1
    SQL.Strings = (
      'select * from DetailTable')
    CurrentPage = 0
    RowsPerPage = 5
    TableName = 'DetailTable'
    useFormula = True
    PKName = 'ID'
    MainTableName = 'mainTable'
    MainTablePK = 'ID'
    SubTableRefField = 'MainID'
    SubTableName = 'DetailTable'
    SubTablePKName = 'ID'
    Left = 144
    Top = 88
  end
  object DHibernateQueryAdvance1: TCnDHibernateQueryAdvance
    Active = False
    Aggregates = <>
    Params = <>
    Connection = ADOConnection1
    SQL.Strings = (
      'select * from mainTable')
    CurrentPage = 0
    RowsPerPage = 5
    TableName = 'mainTable'
    useFormula = True
    PKName = 'ID'
    Left = 112
    Top = 88
  end
  object ADOConnection1: TADOConnection
    ConnectionString = 
      'Provider=SQLOLEDB.1;Persist Security Info=False;User ID=sa;Initi' +
      'al Catalog=MainDetailDemo;Data Source=192.168.1.100'
    LoginPrompt = False
    Provider = 'SQLOLEDB.1'
    Left = 80
    Top = 88
  end
  object DHibernateIdGenerator1: TCnDHibernateIdGenerator
    Connection = ADOConnection1
    Parameters = <>
    CodeLength = 0
    Left = 176
    Top = 88
  end
  object DataSource1: TDataSource
    DataSet = DHibernateQueryAdvance1
    Left = 112
    Top = 120
  end
  object DataSource2: TDataSource
    DataSet = DHibernateSubQueryAdvance1
    Left = 144
    Top = 120
  end
end
