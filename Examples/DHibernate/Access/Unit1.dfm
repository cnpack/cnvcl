object Form1: TForm1
  Left = 285
  Top = 147
  Width = 454
  Height = 318
  Caption = 'Access Demo - Reading as an ADOQuery'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    446
    291)
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 8
    Top = 8
    Width = 426
    Height = 268
    Anchors = [akLeft, akTop, akRight, akBottom]
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
    Columns = <
      item
        Expanded = False
        FieldName = 'name'
        Width = 72
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'capital'
        Width = 92
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'place'
        Width = 95
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'area'
        Visible = True
      end
      item
        Expanded = False
        FieldName = 'population'
        Visible = True
      end>
  end
  object DataSource1: TDataSource
    DataSet = DHibernateQuery1
    Left = 96
    Top = 168
  end
  object ADOConnection1: TADOConnection
    Connected = True
    ConnectionString = 
      'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=orz.mdb;Persist Sec' +
      'urity Info=False'
    LoginPrompt = False
    Mode = cmShareDenyNone
    Provider = 'Microsoft.Jet.OLEDB.4.0'
    Left = 32
    Top = 168
  end
  object DHibernateQuery1: TCnDHibernateQuery
    Connection = ADOConnection1
    CursorType = ctStatic
    Parameters = <>
    SQL.Strings = (
      'select * from country')
    Left = 64
    Top = 168
  end
end
