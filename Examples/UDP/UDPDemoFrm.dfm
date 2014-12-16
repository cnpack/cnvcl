object Form1: TForm1
  Left = 305
  Top = 124
  Width = 439
  Height = 418
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grp1: TGroupBox
    Left = 8
    Top = 8
    Width = 417
    Height = 209
    Caption = 'Send'
    TabOrder = 0
    object lbl1: TLabel
      Left = 8
      Top = 20
      Width = 34
      Height = 13
      Caption = 'Server:'
    end
    object lbl2: TLabel
      Left = 8
      Top = 80
      Width = 24
      Height = 13
      Caption = 'Text:'
    end
    object lbl3: TLabel
      Left = 8
      Top = 148
      Width = 19
      Height = 13
      Caption = 'File:'
    end
    object btnFile: TSpeedButton
      Left = 384
      Top = 144
      Width = 23
      Height = 22
      Caption = '...'
      OnClick = btnFileClick
    end
    object lbl4: TLabel
      Left = 8
      Top = 53
      Width = 22
      Height = 13
      Caption = 'Port:'
    end
    object lbl5: TLabel
      Left = 8
      Top = 180
      Width = 216
      Height = 13
      Caption = 'Note: Increase RecvBufSize if file is too large.'
    end
    object mmoSend: TMemo
      Left = 56
      Top = 80
      Width = 353
      Height = 57
      Lines.Strings = (
        'CnUDP Component Demo.'
        'Test it.')
      TabOrder = 3
    end
    object edtServer: TEdit
      Left = 56
      Top = 20
      Width = 353
      Height = 21
      TabOrder = 0
      Text = 'localhost'
    end
    object edtFile: TEdit
      Left = 56
      Top = 144
      Width = 321
      Height = 21
      TabOrder = 4
    end
    object sePort: TSpinEdit
      Left = 56
      Top = 49
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 14570
    end
    object btnSendText: TButton
      Left = 253
      Top = 176
      Width = 75
      Height = 21
      Caption = 'Send &Text'
      TabOrder = 5
      OnClick = btnSendTextClick
    end
    object btnSendFile: TButton
      Left = 333
      Top = 176
      Width = 75
      Height = 21
      Caption = 'Send &File'
      TabOrder = 6
      OnClick = btnSendFileClick
    end
    object chkBoardCast: TCheckBox
      Left = 328
      Top = 52
      Width = 81
      Height = 17
      Caption = 'BroadCast'
      TabOrder = 2
    end
  end
  object grp2: TGroupBox
    Left = 7
    Top = 224
    Width = 417
    Height = 129
    Caption = 'Receive'
    TabOrder = 1
    object lbl6: TLabel
      Left = 8
      Top = 48
      Width = 43
      Height = 13
      Caption = 'Receive:'
    end
    object lbl8: TLabel
      Left = 8
      Top = 21
      Width = 46
      Height = 13
      Caption = 'Bind Port:'
    end
    object lblStatus: TLabel
      Left = 186
      Top = 21
      Width = 40
      Height = 13
      Caption = 'lblStatus'
    end
    object mmoReceive: TMemo
      Left = 56
      Top = 48
      Width = 353
      Height = 65
      TabOrder = 1
    end
    object seBind: TSpinEdit
      Left = 56
      Top = 17
      Width = 121
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 14570
      OnChange = seBindChange
    end
  end
  object btnClose: TButton
    Left = 344
    Top = 360
    Width = 75
    Height = 21
    Caption = '&Close'
    TabOrder = 2
    OnClick = btnCloseClick
  end
  object CnUDP1: TCnUDP
    RemotePort = 0
    LocalPort = 0
    OnDataReceived = CnUDP1DataReceived
    Left = 15
    Top = 348
  end
  object dlgOpen: TOpenDialog
    Left = 47
    Top = 348
  end
  object dlgSave: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 79
    Top = 348
  end
end
