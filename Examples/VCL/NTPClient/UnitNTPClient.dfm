object FormNTP: TFormNTP
  Left = 314
  Top = 195
  BorderStyle = bsDialog
  Caption = 'NTP Client'
  ClientHeight = 141
  ClientWidth = 463
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
  object lblNtp: TLabel
    Left = 32
    Top = 32
    Width = 59
    Height = 13
    Caption = 'NTP Server:'
  end
  object lblPort: TLabel
    Left = 344
    Top = 32
    Width = 22
    Height = 13
    Caption = 'Port:'
  end
  object cbbNtpServers: TComboBox
    Left = 112
    Top = 28
    Width = 217
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    Text = 'cn.ntp.org.cn'
    Items.Strings = (
      'cn.pool.ntp.org'
      '1.cn.pool.ntp.org'
      'cn.ntp.org.cn'
      'edu.ntp.org.cn'
      'ntp.sjtu.edu.cn')
  end
  object btnSend: TButton
    Left = 112
    Top = 80
    Width = 153
    Height = 25
    Caption = 'Send NTP Request'
    TabOrder = 1
    OnClick = btnSendClick
  end
  object edtPort: TEdit
    Left = 376
    Top = 28
    Width = 49
    Height = 21
    TabOrder = 2
    Text = '123'
  end
  object udpNTP: TCnUDP
    RemotePort = 0
    LocalPort = 0
    OnDataReceived = udpNTPDataReceived
    Left = 304
    Top = 80
  end
end
