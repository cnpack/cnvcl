object Form1: TForm1
  Left = 285
  Top = 260
  BorderStyle = bsDialog
  Caption = 'SQL Server to XLS'
  ClientHeight = 148
  ClientWidth = 192
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
    Top = 88
    Width = 118
    Height = 26
    Caption = 'You can specify the SQL'#13#10'Server Table to  Export'
  end
  object Button1: TButton
    Left = 56
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Export'
    TabOrder = 0
    OnClick = Button1Click
  end
  object ADOConnection1: TADOConnection
    ConnectionString = 
      'Provider=SQLOLEDB.1;Password="";Persist Security Info=True;User ' +
      'ID=sa;Initial Catalog=QQGroup;Data Source=192.168.1.100'
    LoginPrompt = False
    Provider = 'SQLOLEDB.1'
    Left = 8
    Top = 16
  end
  object DHibernateExport1: TCnDHibernateExport
    Connection = ADOConnection1
    SQL = 'select * from Members'
    FileName = 
      'C:\Program Files\CodeGear\Delphi2009\CnDelphiHibernate\demo\Expo' +
      'rt\Demo.xls'
    SheetName = 'Sheet1'
    Left = 144
    Top = 16
  end
end
