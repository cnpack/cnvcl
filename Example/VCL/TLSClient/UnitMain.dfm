object FormMain: TFormMain
  Left = 212
  Top = 69
  Width = 824
  Height = 632
  Caption = 'TLS Client Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblLog: TLabel
    Left = 8
    Top = 368
    Width = 21
    Height = 13
    Caption = 'Log:'
  end
  object grpConfig: TGroupBox
    Left = 8
    Top = 8
    Width = 400
    Height = 80
    Caption = 'Connection Config'
    TabOrder = 0
    object lblHost: TLabel
      Left = 16
      Top = 24
      Width = 26
      Height = 13
      Caption = 'Host:'
    end
    object lblPort: TLabel
      Left = 216
      Top = 24
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object edtHost: TEdit
      Left = 48
      Top = 21
      Width = 150
      Height = 21
      TabOrder = 0
      Text = 'www.cnpack.org'
    end
    object edtPort: TEdit
      Left = 244
      Top = 21
      Width = 50
      Height = 21
      TabOrder = 1
      Text = '443'
    end
    object btnConnect: TButton
      Left = 16
      Top = 48
      Width = 80
      Height = 25
      Caption = 'Connect'
      TabOrder = 2
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 104
      Top = 48
      Width = 80
      Height = 25
      Caption = 'Disconnect'
      Enabled = False
      TabOrder = 3
      OnClick = btnDisconnectClick
    end
    object btnSend: TButton
      Left = 200
      Top = 48
      Width = 80
      Height = 25
      Caption = 'Send'
      Enabled = False
      TabOrder = 4
      OnClick = btnSendClick
    end
  end
  object grpData: TGroupBox
    Left = 8
    Top = 94
    Width = 784
    Height = 260
    Caption = 'Data'
    TabOrder = 1
    object lblRequest: TLabel
      Left = 16
      Top = 24
      Width = 44
      Height = 13
      Caption = 'Request:'
    end
    object lblResponse: TLabel
      Left = 16
      Top = 139
      Width = 51
      Height = 13
      Caption = 'Response:'
    end
    object memRequest: TMemo
      Left = 16
      Top = 43
      Width = 752
      Height = 80
      Lines.Strings = (
        'GET / HTTP/1.1'
        'Host: www.cnpack.org'
        'Connection: close'
        ''
        '')
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object memResponse: TMemo
      Left = 16
      Top = 158
      Width = 752
      Height = 80
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
  object memLog: TMemo
    Left = 8
    Top = 387
    Width = 784
    Height = 200
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 2
  end
end
