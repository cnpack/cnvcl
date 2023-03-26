object MainFrm: TMainFrm
  Left = 123
  Top = 49
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'CnRedis Client'
  ClientHeight = 424
  ClientWidth = 471
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = #24494#36719#38597#40657
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 0
    Top = 292
    Width = 471
    Height = 4
    Cursor = crVSplit
    Align = alBottom
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 471
    Height = 48
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblHost: TLabel
      Left = 10
      Top = 18
      Width = 284
      Height = 16
      Caption = 'Host:                         Port:                Password:'
    end
    object edtHost: TEdit
      Left = 41
      Top = 15
      Width = 68
      Height = 24
      TabOrder = 0
      Text = '127.0.0.1'
    end
    object edtPort: TEdit
      Left = 152
      Top = 15
      Width = 41
      Height = 24
      TabOrder = 1
      Text = '8888'
    end
    object edtPassword: TEdit
      Left = 258
      Top = 15
      Width = 108
      Height = 24
      TabOrder = 2
      Text = 'mosker123'
    end
    object btnDisconnect: TButton
      Left = 371
      Top = 17
      Width = 77
      Height = 19
      Caption = 'Disconnect'
      TabOrder = 3
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 48
    Width = 471
    Height = 244
    Align = alClient
    TabOrder = 1
    object edtStringKey: TLabeledEdit
      Left = 17
      Top = 20
      Width = 78
      Height = 24
      EditLabel.Width = 31
      EditLabel.Height = 16
      EditLabel.Caption = 'KEY:'
      TabOrder = 0
      Text = 'StringKey'
    end
    object edtStringValue: TLabeledEdit
      Left = 100
      Top = 20
      Width = 102
      Height = 24
      EditLabel.Width = 37
      EditLabel.Height = 16
      EditLabel.Caption = 'Value:'
      TabOrder = 1
      Text = 'This is a Value.'
    end
    object Button1: TButton
      Left = 207
      Top = 21
      Width = 63
      Height = 21
      Caption = 'SET'
      TabOrder = 2
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 275
      Top = 21
      Width = 64
      Height = 21
      Caption = 'GET'
      TabOrder = 3
      OnClick = Button2Click
    end
    object LabeledEdit3: TLabeledEdit
      Left = 17
      Top = 74
      Width = 78
      Height = 24
      EditLabel.Width = 31
      EditLabel.Height = 16
      EditLabel.Caption = 'KEY:'
      TabOrder = 4
      Text = 'HashKey'
    end
    object LabeledEdit4: TLabeledEdit
      Left = 100
      Top = 74
      Width = 102
      Height = 24
      EditLabel.Width = 40
      EditLabel.Height = 16
      EditLabel.Caption = 'FIELD:'
      TabOrder = 5
      Text = 'id1'
    end
    object Button3: TButton
      Left = 314
      Top = 75
      Width = 63
      Height = 21
      Caption = 'HSET'
      TabOrder = 6
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 382
      Top = 75
      Width = 63
      Height = 21
      Caption = 'HGET'
      TabOrder = 7
      OnClick = Button4Click
    end
    object LabeledEdit5: TLabeledEdit
      Left = 207
      Top = 74
      Width = 102
      Height = 24
      EditLabel.Width = 37
      EditLabel.Height = 16
      EditLabel.Caption = 'Value:'
      TabOrder = 8
      Text = 'abcdefg123'
    end
    object Button5: TButton
      Left = 344
      Top = 21
      Width = 63
      Height = 21
      Caption = 'Scan'
      TabOrder = 9
      OnClick = Button5Click
    end
  end
  object mmoResult: TMemo
    Left = 0
    Top = 296
    Width = 471
    Height = 128
    Align = alBottom
    TabOrder = 2
  end
  object PopupMenu1: TPopupMenu
    AutoHotkeys = maManual
    Left = 504
    Top = 368
    object N1: TMenuItem
      Caption = '  '#28165'  '#31354
      OnClick = N1Click
    end
  end
end
