object Form1: TForm1
  Left = 217
  Top = 134
  BorderStyle = bsDialog
  Caption = 'XLS to SQL Server'
  ClientHeight = 142
  ClientWidth = 200
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 40
    Top = 80
    Width = 118
    Height = 26
    Caption = 'You can specify the SQL'#13#10'Server Table to Import'
  end
  object Button1: TButton
    Left = 56
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Import'
    TabOrder = 0
    OnClick = Button1Click
  end
  object DHibernateImport1: TCnDHibernateImport
    FileName = 'country.xls'
    SheetName = 'country'
    Map.Strings = (
      'name=1'
      'Capital=2'
      'Continent=GetDate()'
      'Area=4'
      'Population=5')
    Connection = ADOConnection1
    TableName = 'country'
    Left = 64
    Top = 48
  end
  object ADOConnection1: TADOConnection
    ConnectionString = 
      'Provider=SQLOLEDB.1;Persist Security Info=False;User ID=sa;Initi' +
      'al Catalog=DEmo;Data Source=.'
    LoginPrompt = False
    Provider = 'SQLOLEDB.1'
    Left = 32
    Top = 48
  end
end
