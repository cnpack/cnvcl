object Form1: TForm1
  Left = 280
  Top = 242
  Width = 656
  Height = 351
  Caption = '多表关联，单表更新Demo'
  Color = clBtnFace
  Font.Charset = GB2312_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = '宋体'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object lbl1: TLabel
    Left = 8
    Top = 8
    Width = 522
    Height = 24
    Caption = 
      '执行ADO多表关联(或视图)更新时，只会更新CnADOUpdateSQL关联的表，' +
      '其它联接的表不会被更新。'#13#10'从而实现多表关联，单表更新'
  end
  object dbgrd1: TDBGrid
    Left = 8
    Top = 48
    Width = 633
    Height = 233
    DataSource = ds1
    TabOrder = 0
    TitleFont.Charset = GB2312_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -12
    TitleFont.Name = '宋体'
    TitleFont.Style = []
  end
  object btn1: TButton
    Left = 568
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Update'
    TabOrder = 1
    OnClick = btn1Click
  end
  object CnADOUpdateSQL1: TCnADOUpdateSQL
    ConnectionType = ctProvider
    Provider = DataSetProvider1
    ModifySQL.Strings = (
      'UPDATE Product'
      ' SET '
      '    ProductName = :ProductName,'
      '    ProductNo = :ProductNo,'
      '    ProductSortNo = :ProductSortNo,'
      '    Remark = :Remark'
      ' WHERE '
      '    ID = :OLD_ID')
    InsertSQL.Strings = (
      'INSERT INTO Product'
      '   (ProductName,ProductNo,ProductSortNo,Remark)'
      'VALUES '
      '   (:ProductName,:ProductNo,:ProductSortNo,:Remark)')
    DeleteSQL.Strings = (
      'DELETE FROM Product'
      ' WHERE '
      '    ID = :OLD_ID')
    Left = 176
    Top = 200
  end
  object DataSetProvider1: TDataSetProvider
    DataSet = qry1
    Constraints = True
    Options = [poAllowCommandText]
    BeforeUpdateRecord = DataSetProvider1BeforeUpdateRecord
    Left = 112
    Top = 200
  end
  object cds1: TClientDataSet
    Aggregates = <>
    CommandText = 'select * from vProduct'
    Params = <>
    ProviderName = 'DataSetProvider1'
    Left = 48
    Top = 200
    object cds1ProductNo: TWideStringField
      FieldName = 'ProductNo'
      Size = 50
    end
    object cds1ProductName: TWideStringField
      FieldName = 'ProductName'
      Size = 50
    end
    object cds1ProductSortNo: TWideStringField
      FieldName = 'ProductSortNo'
      Size = 50
    end
    object cds1Remark: TWideStringField
      FieldName = 'Remark'
      Size = 50
    end
    object cds1ProductSortName: TWideStringField
      FieldName = 'ProductSortName'
      Size = 50
    end
  end
  object ds1: TDataSource
    DataSet = cds1
    Left = 144
    Top = 200
  end
  object con1: TADOConnection
    Connected = True
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=CnADOUpdateSQL.mdb;' +
      'Persist Security Info=False'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 16
    Top = 200
  end
  object qry1: TADOQuery
    Connection = con1
    CursorType = ctStatic
    Parameters = <>
    Left = 80
    Top = 200
  end
end
